//// generated from js/*.js by `gleam run -m embed_js`; edit those instead

pub const bootstrap = "(function () {
  \"use strict\";

  const compile = Function;
  const registry = new Map();
  const sources = [];

  const ticks = [];

  const loop = {
    addSource(source) {
      sources.push(source);
    },
    // runs before promise jobs, once the current js has returned
    nextTick(callback) {
      ticks.push(callback);
    },
  };

  // one tick per call, so a throwing tick leaves the rest queued
  function runTick() {
    if (ticks.length === 0) return false;
    ticks.shift()();
    return true;
  }

  registry.set(\"spark:loop\", { hidden: true, module: { exports: loop } });

  // factory is the source already compiled, when the embedder did that ahead
  function register(specifier, source, native, dependencies, hidden, factory) {
    registry.set(specifier, {
      source,
      factory,
      native,
      dependencies,
      hidden,
      module: undefined,
    });
  }

  function lookup(specifier) {
    if (registry.has(specifier)) return specifier;
    const prefixed = \"node:\" + specifier;
    if (registry.has(prefixed)) return prefixed;
    return undefined;
  }

  function notFound(specifier) {
    const error = new Error(\"Cannot find module '\" + specifier + \"'\");
    error.code = \"MODULE_NOT_FOUND\";
    return error;
  }

  function load(name) {
    const entry = registry.get(name);
    if (entry.module !== undefined) return entry.module.exports;
    const module = { exports: {} };
    entry.module = module;
    try {
      const factory =
        entry.factory ??
        compile(\"exports\", \"require\", \"module\", \"native\", entry.source);
      factory(
        module.exports,
        requireWithin(name, entry.dependencies),
        module,
        entry.native,
      );
    } catch (error) {
      entry.module = undefined;
      throw error;
    }
    return module.exports;
  }

  // what scripts get
  function require(specifier) {
    const name = lookup(String(specifier));
    if (name === undefined || registry.get(name).hidden) {
      throw notFound(specifier);
    }
    return load(name);
  }

  // what a builtin gets: its declared dependencies and nothing else
  function requireWithin(owner, dependencies) {
    return function require(specifier) {
      const name = lookup(String(specifier));
      if (name === undefined || !dependencies.includes(name)) {
        throw new Error(
          owner + \" has no dependency on '\" + specifier + \"'\",
        );
      }
      return load(name);
    };
  }

  function runCommonJs(filename, dirname, source) {
    const module = { id: \".\", filename, exports: {}, loaded: false };
    const factory = compile(
      \"exports\",
      \"require\",
      \"module\",
      \"__filename\",
      \"__dirname\",
      source,
    );
    factory.call(module.exports, module.exports, require, module, filename, dirname);
    module.loaded = true;
    return module.exports;
  }

  function step() {
    for (let i = 0; i < sources.length; i++) {
      if (sources[i].step()) return true;
    }
    return false;
  }

  function wait() {
    let soonest = -1;
    for (let i = 0; i < sources.length; i++) {
      const ms = sources[i].wait();
      if (ms >= 0 && (soonest < 0 || ms < soonest)) soonest = ms;
    }
    return soonest;
  }

  function alive() {
    for (let i = 0; i < sources.length; i++) {
      if (sources[i].alive()) return true;
    }
    return false;
  }

  function installGlobals(specifier, names) {
    const exports = load(specifier);
    for (let i = 0; i < names.length; i++) {
      Object.defineProperty(globalThis, names[i], {
        value: exports[names[i]],
        writable: true,
        // as in node: a class is not listed, a function or an object is
        enumerable: names[i][0] === names[i][0].toLowerCase(),
        configurable: true,
      });
    }
  }

  return {
    register,
    require,
    runCommonJs,
    runTick,
    step,
    wait,
    alive,
    installGlobals,
  };
})
"

pub const errors = "\"use strict\";

let realInspect;

// node's util.inspect, loaded the first time a message needs it
function inspect(value, options) {
  realInspect ??= require(\"spark:internal\").load(
    \"internal/util/inspect\",
  ).inspect;
  return realInspect(value, options);
}

const classRegExp = /^[A-Z][a-zA-Z0-9]*$/;

const kTypes = [
  \"string\",
  \"function\",
  \"number\",
  \"object\",
  \"Function\",
  \"Object\",
  \"boolean\",
  \"bigint\",
  \"symbol\",
];

// libuv's message and, where linux and macos agree on it, the errno
const systemErrors = {
  EACCES: [\"permission denied\", -13],
  EAGAIN: [\"resource temporarily unavailable\"],
  EBADF: [\"bad file descriptor\", -9],
  EBUSY: [\"resource busy or locked\", -16],
  EEXIST: [\"file already exists\", -17],
  EINVAL: [\"invalid argument\", -22],
  EIO: [\"i/o error\", -5],
  EISDIR: [\"illegal operation on a directory\", -21],
  ELOOP: [\"too many symbolic links encountered\"],
  EMFILE: [\"too many open files\", -24],
  ENAMETOOLONG: [\"name too long\"],
  ENFILE: [\"file table overflow\", -23],
  ENOENT: [\"no such file or directory\", -2],
  ENOMEM: [\"not enough memory\", -12],
  ENOSPC: [\"no space left on device\", -28],
  ENOTDIR: [\"not a directory\", -20],
  ENOTEMPTY: [\"directory not empty\"],
  EPERM: [\"operation not permitted\", -1],
  EROFS: [\"read-only file system\", -30],
  ETXTBSY: [\"text file is busy\", -26],
};

// as node makes its coded errors: a plain error of the base class, with the
// code on it and a toString that shows the code
function coded(Base, code, message) {
  const made = new Base(message);
  Object.defineProperty(made, \"toString\", {
    value() {
      return `${this.name} [${code}]: ${this.message}`;
    },
    enumerable: false,
    writable: true,
    configurable: true,
  });
  made.code = code;
  return made;
}

function error(code, message) {
  return coded(Error, code, message);
}

function typeError(code, message) {
  return coded(TypeError, code, message);
}

function rangeError(code, message) {
  return coded(RangeError, code, message);
}

function uriError(code, message) {
  return coded(URIError, code, message);
}

function formatList(array, type) {
  switch (array.length) {
    case 0:
      return \"\";
    case 1:
      return `${array[0]}`;
    case 2:
      return `${array[0]} ${type} ${array[1]}`;
    case 3:
      return `${array[0]}, ${array[1]}, ${type} ${array[2]}`;
    default:
      return `${array.slice(0, -1).join(\", \")}, ${type} ${array[array.length - 1]}`;
  }
}

function determineSpecificType(value) {
  if (value === null) {
    return \"null\";
  } else if (value === undefined) {
    return \"undefined\";
  }

  const type = typeof value;

  switch (type) {
    case \"bigint\":
      return `type bigint (${value}n)`;
    case \"number\":
      if (value === 0) {
        return 1 / value === -Infinity ? \"type number (-0)\" : \"type number (0)\";
      } else if (value !== value) {
        return \"type number (NaN)\";
      } else if (value === Infinity) {
        return \"type number (Infinity)\";
      } else if (value === -Infinity) {
        return \"type number (-Infinity)\";
      }
      return `type number (${value})`;
    case \"boolean\":
      return value ? \"type boolean (true)\" : \"type boolean (false)\";
    case \"symbol\":
      return `type symbol (${String(value)})`;
    case \"function\":
      return `function ${value.name}`;
    case \"object\":
      if (value.constructor && \"name\" in value.constructor) {
        return `an instance of ${value.constructor.name}`;
      }
      return `${inspect(value, { depth: -1 })}`;
    case \"string\":
      value.length > 28 && (value = `${value.slice(0, 25)}...`);
      if (value.indexOf(\"'\") === -1) {
        return `type string ('${value}')`;
      }
      return `type string (${JSON.stringify(value)})`;
    default:
      value = inspect(value);
      if (value.length > 28) {
        value = `${value.slice(0, 25)}...`;
      }
      return `type ${type} (${value})`;
  }
}

function invalidArgType(name, expected, actual) {
  if (!Array.isArray(expected)) {
    expected = [expected];
  }

  let msg = \"The \";
  if (name.endsWith(\" argument\")) {
    msg += `${name} `;
  } else {
    const type = name.includes(\".\") ? \"property\" : \"argument\";
    msg += `\"${name}\" ${type} `;
  }
  msg += \"must be \";

  const types = [];
  const instances = [];
  const other = [];

  for (const value of expected) {
    if (kTypes.includes(value)) {
      types.push(value.toLowerCase());
    } else if (classRegExp.exec(value) !== null) {
      instances.push(value);
    } else {
      other.push(value);
    }
  }

  if (instances.length > 0) {
    const pos = types.indexOf(\"object\");
    if (pos !== -1) {
      types.splice(pos, 1);
      instances.push(\"Object\");
    }
  }

  if (types.length > 0) {
    msg += `${types.length > 1 ? \"one of type\" : \"of type\"} ${formatList(types, \"or\")}`;
    if (instances.length > 0 || other.length > 0) msg += \" or \";
  }

  if (instances.length > 0) {
    msg += `an instance of ${formatList(instances, \"or\")}`;
    if (other.length > 0) msg += \" or \";
  }

  if (other.length > 0) {
    if (other.length > 1) {
      msg += `one of ${formatList(other, \"or\")}`;
    } else {
      if (other[0].toLowerCase() !== other[0]) msg += \"an \";
      msg += `${other[0]}`;
    }
  }

  msg += `. Received ${determineSpecificType(actual)}`;

  return typeError(\"ERR_INVALID_ARG_TYPE\", msg);
}

function invalidArgValue(name, value, reason = \"is invalid\") {
  let inspected = inspect(value);
  if (inspected.length > 128) {
    inspected = `${inspected.slice(0, 128)}...`;
  }
  const type = name.includes(\".\") ? \"property\" : \"argument\";
  return typeError(
    \"ERR_INVALID_ARG_VALUE\",
    `The ${type} '${name}' ${reason}. Received ${inspected}`,
  );
}

// a failed want as node reports a failed syscall
function systemError(code, syscall, path) {
  const known = systemErrors[code];
  let message = `${code}: ${known ? known[0] : \"unknown error\"}, ${syscall}`;
  if (path) message += ` '${path}'`;
  const error = new Error(message);
  if (known && known[1] !== undefined) error.errno = known[1];
  error.code = code;
  error.syscall = syscall;
  if (path) error.path = path;
  return error;
}

function addNumericalSeparator(val) {
  let res = \"\";
  let i = val.length;
  const start = val[0] === \"-\" ? 1 : 0;
  for (; i >= start + 4; i -= 3) {
    res = `_${val.slice(i - 3, i)}${res}`;
  }
  return `${val.slice(0, i)}${res}`;
}

function outOfRange(name, range, input, replaceDefaultBoolean = false) {
  let msg = replaceDefaultBoolean
    ? name
    : `The value of \"${name}\" is out of range.`;
  let received;
  if (Number.isInteger(input) && Math.abs(input) > 2 ** 32) {
    received = addNumericalSeparator(String(input));
  } else if (typeof input === \"bigint\") {
    received = String(input);
    const limit = BigInt(2) ** BigInt(32);
    if (input > limit || input < -limit) {
      received = addNumericalSeparator(received);
    }
    received += \"n\";
  } else {
    received = inspect(input);
  }
  msg += ` It must be ${range}. Received ${received}`;
  return rangeError(\"ERR_OUT_OF_RANGE\", msg);
}

function validateInteger(
  value,
  name,
  min = Number.MIN_SAFE_INTEGER,
  max = Number.MAX_SAFE_INTEGER,
) {
  if (typeof value !== \"number\") throw invalidArgType(name, \"number\", value);
  if (!Number.isInteger(value)) throw outOfRange(name, \"an integer\", value);
  if (value < min || value > max) {
    throw outOfRange(name, `>= ${min} && <= ${max}`, value);
  }
}

function validateNumber(value, name, min = undefined, max) {
  if (typeof value !== \"number\") throw invalidArgType(name, \"number\", value);
  if (
    (min != null && value < min) ||
    (max != null && value > max) ||
    ((min != null || max != null) && Number.isNaN(value))
  ) {
    throw outOfRange(
      name,
      `${min != null ? `>= ${min}` : \"\"}${min != null && max != null ? \" && \" : \"\"}${max != null ? `<= ${max}` : \"\"}`,
      value,
    );
  }
}

function validateArray(value, name, minLength = 0) {
  if (!Array.isArray(value)) throw invalidArgType(name, \"Array\", value);
  if (value.length < minLength) {
    throw invalidArgValue(name, value, `must be longer than ${minLength}`);
  }
}

function validateBuffer(buffer, name = \"buffer\") {
  if (!ArrayBuffer.isView(buffer)) {
    throw invalidArgType(name, [\"Buffer\", \"TypedArray\", \"DataView\"], buffer);
  }
}

function validateBoolean(value, name) {
  if (typeof value !== \"boolean\") throw invalidArgType(name, \"boolean\", value);
}

function validateAbortSignal(signal, name) {
  if (
    signal !== undefined &&
    (signal === null || typeof signal !== \"object\" || !(\"aborted\" in signal))
  ) {
    throw invalidArgType(name, \"AbortSignal\", signal);
  }
}

class AbortError extends Error {
  constructor(message = \"The operation was aborted\", options = undefined) {
    if (options !== undefined && typeof options !== \"object\") {
      throw invalidArgType(\"options\", \"Object\", options);
    }
    super(message, options);
    this.code = \"ABORT_ERR\";
    this.name = \"AbortError\";
  }
}

function genericNodeError(message, errorProperties) {
  const err = new Error(message);
  Object.assign(err, errorProperties);
  return err;
}

const kEnhanceStackBeforeInspector = Symbol(\"kEnhanceStackBeforeInspector\");

// objects that structuredClone must refuse, as node marks its URL
const uncloneable = new WeakSet();

function markUncloneable(object) {
  uncloneable.add(object);
}

function isUncloneable(object) {
  return uncloneable.has(object);
}

// node's error classes by code, for code ported from node that says
// `new ERR_X(...)`; each returns the error, which `new` then hands back
const codes = {
  ERR_INVALID_ARG_TYPE: function (name, expected, actual) {
    return invalidArgType(name, expected, actual);
  },
  ERR_INVALID_ARG_VALUE: function (name, value, reason) {
    return invalidArgValue(name, value, reason);
  },
  ERR_OUT_OF_RANGE: function (name, range, input, replaceDefaultBoolean) {
    return outOfRange(name, range, input, replaceDefaultBoolean);
  },
  ERR_INVALID_THIS: function (type) {
    return typeError(
      \"ERR_INVALID_THIS\",
      `Value of \"this\" must be of type ${type}`,
    );
  },
  ERR_METHOD_NOT_IMPLEMENTED: function (method) {
    return error(
      \"ERR_METHOD_NOT_IMPLEMENTED\",
      `The ${method} method is not implemented`,
    );
  },
  ERR_UNKNOWN_ENCODING: function (encoding) {
    return typeError(\"ERR_UNKNOWN_ENCODING\", `Unknown encoding: ${encoding}`);
  },
  ERR_UNHANDLED_ERROR: function (err = undefined) {
    const msg = \"Unhandled error.\";
    return error(
      \"ERR_UNHANDLED_ERROR\",
      err === undefined ? msg : `${msg} (${err})`,
    );
  },
};

function validateFunction(value, name) {
  if (typeof value !== \"function\") {
    throw invalidArgType(name, \"Function\", value);
  }
}

function validateObject(value, name) {
  if (value === null || Array.isArray(value) || typeof value !== \"object\") {
    throw invalidArgType(name, \"Object\", value);
  }
}

function validateString(value, name) {
  if (typeof value !== \"string\") {
    throw invalidArgType(name, \"string\", value);
  }
}

module.exports = {
  error,
  typeError,
  rangeError,
  uriError,
  inspect,
  codes,
  AbortError,
  genericNodeError,
  kEnhanceStackBeforeInspector,
  validateAbortSignal,
  validateBoolean,
  markUncloneable,
  isUncloneable,
  invalidArgType,
  invalidArgValue,
  outOfRange,
  determineSpecificType,
  systemError,
  validateArray,
  validateBuffer,
  validateInteger,
  validateNumber,
  validateFunction,
  validateObject,
  validateString,
};
"

pub const internal = "\"use strict\";

// node's own internals, the files the rest of its library is written
// against: errors, validators, util, inspect and a few small ones. Other
// ports reach them through `load`. What these ask of node's native side is
// answered by `bindings` and `shims` below

const loop = require(\"spark:loop\");
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
    typeof key === \"symbol\"
      ? `Symbol${key.description[7].toUpperCase()}${key.description.slice(8)}`
      : `${key[0].toUpperCase()}${key.slice(1)}`;

  function copyStatics(source, prefix, bind) {
    for (const key of Reflect.ownKeys(source)) {
      if (key === \"prototype\" || key === \"name\" || key === \"length\") continue;
      const desc = Reflect.getOwnPropertyDescriptor(source, key);
      if (!(\"value\" in desc)) continue;
      const { value } = desc;
      primordials[`${prefix}${capital(key)}`] =
        bind && typeof value === \"function\" ? value.bind(source) : value;
    }
  }

  function copyPrototype(prototype, prefix) {
    primordials[`${prefix}Prototype`] = prototype;
    for (const key of Reflect.ownKeys(prototype)) {
      if (key === \"constructor\") continue;
      const desc = Reflect.getOwnPropertyDescriptor(prototype, key);
      const name = `${prefix}Prototype`;
      if (\"value\" in desc) {
        primordials[`${name}${capital(key)}`] =
          typeof desc.value === \"function\"
            ? uncurryThis(desc.value)
            : desc.value;
      } else {
        if (desc.get) primordials[`${name}Get${capital(key)}`] = uncurryThis(desc.get);
        if (desc.set) primordials[`${name}Set${capital(key)}`] = uncurryThis(desc.set);
      }
    }
  }

  for (const name of [\"JSON\", \"Math\", \"Reflect\", \"Atomics\"]) {
    if (globalThis[name] !== undefined) copyStatics(globalThis[name], name, false);
  }
  for (const name of [
    \"AggregateError\", \"Array\", \"ArrayBuffer\", \"BigInt\", \"BigInt64Array\",
    \"BigUint64Array\", \"Boolean\", \"DataView\", \"Date\", \"Error\", \"EvalError\",
    \"FinalizationRegistry\", \"Float16Array\", \"Float32Array\", \"Float64Array\",
    \"Function\", \"Int16Array\", \"Int32Array\", \"Int8Array\", \"Map\", \"Number\",
    \"Object\", \"Promise\", \"RangeError\", \"ReferenceError\", \"RegExp\", \"Set\",
    \"SharedArrayBuffer\", \"String\", \"Symbol\", \"SyntaxError\", \"TypeError\",
    \"URIError\", \"Uint16Array\", \"Uint32Array\", \"Uint8Array\",
    \"Uint8ClampedArray\", \"WeakMap\", \"WeakRef\", \"WeakSet\",
  ]) {
    const original = globalThis[name];
    if (original === undefined) continue;
    primordials[name] = original;
    copyStatics(original, name, true);
    copyPrototype(original.prototype, name);
  }
  primordials.Proxy = Proxy;
  primordials.globalThis = globalThis;
  for (const name of [\"decodeURI\", \"decodeURIComponent\", \"encodeURI\", \"encodeURIComponent\"]) {
    primordials[name] = globalThis[name];
  }

  const TypedArray = Object.getPrototypeOf(Uint8Array);
  primordials.TypedArray = TypedArray;
  copyStatics(TypedArray, \"TypedArray\", true);
  copyPrototype(TypedArray.prototype, \"TypedArray\");

  const ArrayIteratorPrototype = Object.getPrototypeOf([][Symbol.iterator]());
  primordials.ArrayIteratorPrototype = ArrayIteratorPrototype;
  primordials.ArrayIteratorPrototypeNext = uncurryThis(ArrayIteratorPrototype.next);
  primordials.IteratorPrototype = Object.getPrototypeOf(ArrayIteratorPrototype);
  const StringIteratorPrototype = Object.getPrototypeOf(\"\"[Symbol.iterator]());
  primordials.StringIteratorPrototype = StringIteratorPrototype;
  primordials.StringIteratorPrototypeNext = uncurryThis(StringIteratorPrototype.next);
  primordials.AsyncIteratorPrototype = Object.getPrototypeOf(
    Object.getPrototypeOf(async function* () {}).prototype,
  );

  // the helpers that take their arguments as one list
  for (const name of [\"Push\", \"Unshift\", \"Concat\"]) {
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
  if (typeof key !== \"string\") return false;
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
    isDate: kinds(\"date\"),
    isArgumentsObject: kinds(\"arguments\"),
    isBigIntObject: kinds(\"bigint_object\"),
    isBooleanObject: kinds(\"boolean_object\"),
    isNumberObject: kinds(\"number_object\"),
    isStringObject: kinds(\"string_object\"),
    isSymbolObject: kinds(\"symbol_object\"),
    isBoxedPrimitive: kinds(
      \"bigint_object\",
      \"boolean_object\",
      \"number_object\",
      \"string_object\",
      \"symbol_object\",
    ),
    isNativeError: kinds(\"error\"),
    isRegExp: kinds(\"regexp\"),
    isAsyncFunction: kinds(\"async_function\", \"async_generator_function\"),
    isGeneratorFunction: kinds(
      \"generator_function\",
      \"async_generator_function\",
    ),
    isGeneratorObject: kinds(\"generator_object\"),
    isPromise: kinds(\"promise\"),
    isMap: kinds(\"map\"),
    isSet: kinds(\"set\"),
    isMapIterator: kinds(\"map_iterator\"),
    isSetIterator: kinds(\"set_iterator\"),
    isWeakMap: kinds(\"weak_map\"),
    isWeakSet: kinds(\"weak_set\"),
    isArrayBuffer: kinds(\"array_buffer\"),
    isSharedArrayBuffer: kinds(\"shared_array_buffer\"),
    isAnyArrayBuffer: kinds(\"array_buffer\", \"shared_array_buffer\"),
    isProxy: kinds(\"proxy\"),
    isModuleNamespaceObject: kinds(\"module_namespace\"),
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
    // v8 remembers the class an object was made by; here only \"Object\" is known
    getConstructorName: () => \"Object\",
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
    guessHandleType: () => \"UNKNOWN\",
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
        Object.defineProperty(set, \"name\", { __proto__: null, value: `set ${key}` });
        const get = () => {
          mod ??= loadAnywhere(id);
          if (lazyLoadedValue === undefined) {
            lazyLoadedValue = mod[key];
            set(lazyLoadedValue);
          }
          return lazyLoadedValue;
        };
        Object.defineProperty(get, \"name\", { __proto__: null, value: `get ${key}` });
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
      arrow_message_private_symbol: Symbol(\"node:arrowMessage\"),
      decorated_private_symbol: Symbol(\"node:decorated\"),
      untransferable_object_private_symbol: Symbol(\"node:untransferableObject\"),
      transfer_mode_private_symbol: Symbol(\"node:transfer_mode\"),
      entry_point_module_private_symbol: Symbol(\"node:entry_point_module\"),
      host_defined_option_symbol: Symbol(\"node:host_defined_option_symbol\"),
      module_first_parent_private_symbol: Symbol(\"node:module_first_parent\"),
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
    encodings: [\"ascii\", \"utf8\", \"base64\", \"utf16le\", \"latin1\", \"hex\", \"buffer\", \"base64url\"],
  }),
  symbols: () => ({}),
  errors: () => ({ exitCodes: { kNoFailure: 0, kGenericUserError: 1 } }),
};

// errno to [name, message], libuv's numbering on linux
const uvErrors = new Map([
  [-7, [\"E2BIG\", \"argument list too long\"]],
  [-13, [\"EACCES\", \"permission denied\"]],
  [-98, [\"EADDRINUSE\", \"address already in use\"]],
  [-99, [\"EADDRNOTAVAIL\", \"address not available\"]],
  [-11, [\"EAGAIN\", \"resource temporarily unavailable\"]],
  [-9, [\"EBADF\", \"bad file descriptor\"]],
  [-16, [\"EBUSY\", \"resource busy or locked\"]],
  [-125, [\"ECANCELED\", \"operation canceled\"]],
  [-103, [\"ECONNABORTED\", \"software caused connection abort\"]],
  [-111, [\"ECONNREFUSED\", \"connection refused\"]],
  [-104, [\"ECONNRESET\", \"connection reset by peer\"]],
  [-17, [\"EEXIST\", \"file already exists\"]],
  [-14, [\"EFAULT\", \"bad address in system call argument\"]],
  [-113, [\"EHOSTUNREACH\", \"host is unreachable\"]],
  [-4, [\"EINTR\", \"interrupted system call\"]],
  [-22, [\"EINVAL\", \"invalid argument\"]],
  [-5, [\"EIO\", \"i/o error\"]],
  [-106, [\"EISCONN\", \"socket is already connected\"]],
  [-21, [\"EISDIR\", \"illegal operation on a directory\"]],
  [-40, [\"ELOOP\", \"too many symbolic links encountered\"]],
  [-24, [\"EMFILE\", \"too many open files\"]],
  [-36, [\"ENAMETOOLONG\", \"name too long\"]],
  [-100, [\"ENETDOWN\", \"network is down\"]],
  [-101, [\"ENETUNREACH\", \"network is unreachable\"]],
  [-23, [\"ENFILE\", \"file table overflow\"]],
  [-2, [\"ENOENT\", \"no such file or directory\"]],
  [-12, [\"ENOMEM\", \"not enough memory\"]],
  [-28, [\"ENOSPC\", \"no space left on device\"]],
  [-38, [\"ENOSYS\", \"function not implemented\"]],
  [-107, [\"ENOTCONN\", \"socket is not connected\"]],
  [-20, [\"ENOTDIR\", \"not a directory\"]],
  [-39, [\"ENOTEMPTY\", \"directory not empty\"]],
  [-88, [\"ENOTSOCK\", \"socket operation on non-socket\"]],
  [-95, [\"ENOTSUP\", \"operation not supported on socket\"]],
  [-1, [\"EPERM\", \"operation not permitted\"]],
  [-32, [\"EPIPE\", \"broken pipe\"]],
  [-34, [\"ERANGE\", \"result too large\"]],
  [-30, [\"EROFS\", \"read-only file system\"]],
  [-29, [\"ESPIPE\", \"invalid seek\"]],
  [-3, [\"ESRCH\", \"no such process\"]],
  [-110, [\"ETIMEDOUT\", \"connection timed out\"]],
  [-26, [\"ETXTBSY\", \"text file is busy\"]],
  [-18, [\"EXDEV\", \"cross-device link not permitted\"]],
  [-4095, [\"EOF\", \"end of file\"]],
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
      if (!String(error?.message).startsWith(\"no module \")) throw error;
    }
  }
  return load(id);
}

function internalBinding(name) {
  if (!(name in bindings)) throw new Error(\"no binding \" + name + \" here\");
  return bindings[name]();
}

// what the files see as `process`: the real one when it was passed
let seenProcess;
function processForFiles() {
  if (seenProcess !== undefined) return seenProcess;
  const real = optional(\"node:process\");
  seenProcess = {
    __proto__: real ?? null,
    env: real?.env ?? {},
    platform: real?.platform ?? \"linux\",
    versions: real?.versions ?? {},
    execArgv: [],
    nextTick(callback, ...args) {
      loop.nextTick(() => callback(...args));
    },
    emitWarning(...args) {
      real?.emitWarning?.(...args);
    },
    cwd: () => real?.cwd?.() ?? \"/\",
  };
  return seenProcess;
}

const shims = {
  // node runs this once at startup
  afterLoad: {
    \"internal/util/debuglog\": (debuglog) =>
      debuglog.initializeDebugEnv(processForFiles().env.NODE_DEBUG),
  },
  process: processForFiles,
  primordials,
  internalBinding,
  buffer: () => optional(\"node:buffer\") ?? {},
  path: () => optional(\"node:path\") ?? {},
  vm: () => {
    throw new Error(\"vm is not here\");
  },
  util: () => load(\"internal/util/inspect\"),
  \"internal/url\": () => ({
    ...optional(\"node:url\"),
    isURL: (self) =>
      Boolean(
        self?.href &&
        self.protocol &&
        self.auth === undefined &&
        self.path === undefined,
      ),
  }),
  \"internal/options\": () => ({
    getOptionValue: (name) => optionDefaults[name],
    getEmbedderOptions: () => ({}),
  }),
  \"internal/process/warning\": () => ({
    emitWarning: (...args) => processForFiles().emitWarning(...args),
    emitWarningSync: (...args) => processForFiles().emitWarning(...args),
  }),
  \"internal/process/execution\": () => ({}),
  \"internal/source_map/source_map_cache\": () => ({ findSourceMap: () => undefined }),
  \"internal/trace_events\": () => ({
    isTraceCategoryEnabled: () => false,
    trace() {},
  }),
  \"internal/tty\": () => ({ getColorDepth: () => 1, hasColors: () => false }),
  \"internal/bootstrap/realm\": () => ({
    BuiltinModule: { exists: () => false, canBeRequiredByUsers: () => false },
  }),
  \"internal/modules/helpers\": () => ({}),
  \"internal/crypto/keys\": () => ({
    isCryptoKey: () => false,
    isKeyObject: () => false,
  }),
  \"internal/v8/startup_snapshot\": () => ({
    namespace: {
      addSerializeCallback() {},
      addDeserializeCallback() {},
      isBuildingSnapshot: () => false,
    },
  }),
};

const optionDefaults = {
  __proto__: null,
  \"--eval\": \"\",
  \"--pending-deprecation\": false,
  \"--no-deprecation\": false,
  \"--trace-deprecation\": false,
  \"--throw-deprecation\": false,
  \"--experimental-require-module\": false,
  \"--enable-source-maps\": false,
  \"--stack-trace-limit\": 10,
};

const files = {
  \"internal/errors\": function (module, exports, require, process, primordials, internalBinding) {
    /* eslint node-core/documented-errors: \"error\" */
    /* eslint node-core/alphabetize-errors: [\"error\", {checkErrorDeclarations: true}] */
    /* eslint node-core/prefer-util-format-errors: \"error\" */

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
      return `${errorString}\\n    at ${trace.join('\\n    at ')}`;
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
        return frames.join('\\n    at ');
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
          if (value.indexOf(\"'\") === -1) {
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
    E('ERR_AMBIGUOUS_ARGUMENT', 'The \"%s\" argument is ambiguous. %s', TypeError);
    E('ERR_ARG_NOT_ITERABLE', '%s must be iterable', TypeError);
    E('ERR_ASSERTION', '%s', Error);
    E('ERR_ASYNC_CALLBACK', '%s must be a function', TypeError);
    E('ERR_ASYNC_LOADER_REQUEST_NEVER_SETTLED',
      'Async loader request never settled', Error);
    E('ERR_ASYNC_TYPE', 'Invalid name for async \"type\": %s', TypeError);
    E('ERR_BROTLI_INVALID_PARAM', '%s is not a valid Brotli parameter', RangeError);
    E('ERR_BUFFER_OUT_OF_BOUNDS',
      (name = undefined) => {
        if (name) {
          return `\"${name}\" is outside of buffer bounds`;
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
      \"Forked processes must have an IPC channel, missing value 'ipc' in %s\",
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
    E('ERR_CRYPTO_ENGINE_UNKNOWN', 'Engine \"%s\" was not found', Error);
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
    E('ERR_DNS_SET_SERVERS_FAILED', 'c-ares failed to set servers: \"%s\" [%s]',
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
    E('ERR_ENCODING_NOT_SUPPORTED', 'The \"%s\" encoding is not supported',
      RangeError);
    E('ERR_EVAL_ESM_CANNOT_PRINT', '--print cannot be used with ESM input', Error);
    E('ERR_EVENT_RECURSION', 'The event \"%s\" is already being dispatched', Error);
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
      'Header field \"%s\" must only have a single value', TypeError);
    E('ERR_HTTP2_INFO_STATUS_NOT_ALLOWED',
      'Informational status codes cannot be used', RangeError);
    E('ERR_HTTP2_INVALID_CONNECTION_HEADERS',
      'HTTP/1 Connection specific headers are forbidden: \"%s\"', TypeError);
    E('ERR_HTTP2_INVALID_HEADER_VALUE',
      'Invalid value \"%s\" for header \"%s\"', TypeError, HideStackFramesError);
    E('ERR_HTTP2_INVALID_INFO_STATUS',
      'Invalid informational status code: %s', RangeError);
    E('ERR_HTTP2_INVALID_ORIGIN',
      'HTTP/2 ORIGIN frames require a valid origin', TypeError);
    E('ERR_HTTP2_INVALID_PACKED_SETTINGS_LENGTH',
      'Packed settings length must be a multiple of six', RangeError);
    E('ERR_HTTP2_INVALID_PSEUDOHEADER',
      '\"%s\" is an invalid pseudoheader or is used incorrectly', TypeError, HideStackFramesError);
    E('ERR_HTTP2_INVALID_SESSION', 'The session has been destroyed', Error);
    E('ERR_HTTP2_INVALID_SETTING_VALUE',
      function(name, actual, min = undefined, max = undefined) {
        this.actual = actual;
        if (min !== undefined) {
          this.min = min;
          this.max = max;
        }
        return `Invalid value for setting \"${name}\": ${actual}`;
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
    E('ERR_HTTP2_UNSUPPORTED_PROTOCOL', 'protocol \"%s\" is unsupported.', Error);
    E('ERR_HTTP_BODY_NOT_ALLOWED',
      'Adding content for this request method or response status is not allowed.', Error);
    E('ERR_HTTP_CONTENT_LENGTH_MISMATCH',
      'Response body\\'s content-length of %s byte(s) does not match the content-length of %s byte(s) set in header', Error);
    E('ERR_HTTP_HEADERS_SENT',
      'Cannot %s headers after they are sent to the client', Error);
    E('ERR_HTTP_INVALID_HEADER_VALUE',
      'Invalid value \"%s\" for header \"%s\"', TypeError, HideStackFramesError);
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
      'Module \"%s\" needs an import attribute of \"%s: %s\"', TypeError);
    E('ERR_IMPORT_ATTRIBUTE_TYPE_INCOMPATIBLE',
      'Module \"%s\" is not of type \"%s\"', TypeError);
    E('ERR_IMPORT_ATTRIBUTE_UNSUPPORTED',
      function error(attribute, value, url = undefined) {
        if (url === undefined) {
          return `Import attribute \"${attribute}\" with value \"${value}\" is not supported`;
        }
        return `Import attribute \"${attribute}\" with value \"${value}\" is not supported in ${url}`;
      }, TypeError);
    E('ERR_INCOMPATIBLE_OPTION_PAIR',
      'Option \"%s\" cannot be used in combination with option \"%s\"', TypeError, HideStackFramesError);
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
        'or incorrect usage of Node.js internals.\\n' +
        'Please open an issue with this stack trace at ' +
        'https://github.com/nodejs/node/issues\\n';
      return message === undefined ? suffix : `${message}\\n${suffix}`;
    }, Error);
    E('ERR_INVALID_ADDRESS_FAMILY', function(addressType, host, port) {
      this.host = host;
      this.port = port;
      return `Invalid address family: ${addressType} ${host}:${port}`;
    }, RangeError);
    E('ERR_INVALID_ARG_TYPE',
      (name, expected, actual) => {
        assert(typeof name === 'string', \"'name' must be a string\");
        if (!Array.isArray(expected)) {
          expected = [expected];
        }

        let msg = 'The ';
        if (name.endsWith(' argument')) {
          msg += `${name} `;
        } else {
          const type = name.includes('.') ? 'property' : 'argument';
          msg += `\"${name}\" ${type} `;
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
                   'The value \"object\" should be written as \"Object\"');
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
          msg += ` [\"${field}\"]`;
        }
        return msg;
      }, TypeError, HideStackFramesError);
    E('ERR_INVALID_CURSOR_POS',
      'Cannot set cursor row without setting its column', TypeError);
    E('ERR_INVALID_FD',
      '\"fd\" must be a positive integer: %s', RangeError);
    E('ERR_INVALID_FD_TYPE', 'Unsupported fd type: %s', TypeError);
    E('ERR_INVALID_FILE_URL_HOST',
      'File URL host must be \"localhost\" or empty on %s', TypeError);
    E('ERR_INVALID_FILE_URL_PATH', function(reason, input) {
      this.input = input;
      return `File URL path ${reason}`;
    }, TypeError);
    E('ERR_INVALID_HANDLE_TYPE', 'This handle type cannot be sent', TypeError);
    E('ERR_INVALID_HTTP_TOKEN', '%s must be a valid HTTP token [\"%s\"]', TypeError, HideStackFramesError);
    E('ERR_INVALID_IP_ADDRESS', 'Invalid IP address: %s', TypeError);
    E('ERR_INVALID_MIME_SYNTAX', (production, str, invalidIndex) => {
      const msg = invalidIndex !== -1 ? ` at ${invalidIndex}` : '';
      return `The MIME syntax for a ${production} in \"${str}\" is invalid` + msg;
    }, TypeError);
    E('ERR_INVALID_MODULE_SPECIFIER', (request, reason, base = undefined) => {
      return `Invalid module \"${request}\" ${reason}${base ?
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
          return `Invalid \"exports\" main target ${JSON.stringify(target)} defined ` +
            `in the package config ${pkgPath}package.json${base ?
              ` imported from ${base}` : ''}${relError ?
              '; targets must start with \"./\"' : ''}`;
        }
        return `Invalid \"${isImport ? 'imports' : 'exports'}\" target ${
          JSON.stringify(target)} defined for '${key}' in the package config ${
          pkgPath}package.json${base ? ` imported from ${base}` : ''}${relError ?
          '; targets must start with \"./\"' : ''}`;
      }, Error);
    E('ERR_INVALID_PROTOCOL',
      'Protocol \"%s\" not supported. Expected \"%s\"',
      TypeError);
    E('ERR_INVALID_REPL_EVAL_CONFIG',
      'Cannot specify both \"breakEvalOnSigint\" and \"eval\" for REPL', TypeError);
    E('ERR_INVALID_REPL_INPUT', '%s', TypeError);
    E('ERR_INVALID_RETURN_PROPERTY', (input, name, prop, value) => {
      return `Expected a valid ${input} to be returned for the \"${prop}\" from the` +
             ` \"${name}\" hook but got ${determineSpecificType(value)}.`;
    }, TypeError);
    E('ERR_INVALID_RETURN_PROPERTY_VALUE', (input, name, prop, value) => {
      return `Expected ${input} to be returned for the \"${prop}\" from the` +
             ` \"${name}\" hook but got ${determineSpecificType(value)}.`;
    }, TypeError);
    E('ERR_INVALID_RETURN_VALUE', (input, name, value) => {
      const type = determineSpecificType(value);

      return `Expected ${input} to be returned from the \"${name}\"` +
             ` function but got ${type}.`;
    }, TypeError, RangeError);
    E('ERR_INVALID_STATE', 'Invalid state: %s', Error, TypeError, RangeError);
    E('ERR_INVALID_SYNC_FORK_INPUT',
      'Asynchronous forks do not support ' +
        'Buffer, TypedArray, DataView or string input: %s',
      TypeError);
    E('ERR_INVALID_THIS', 'Value of \"this\" must be of type %s', TypeError, HideStackFramesError);
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
      '\"%s\" did not call the next hook in its chain and did not' +
      ' explicitly signal a short circuit. If this is intentional, include' +
      ' `shortCircuit: true` in the hook\\'s return.',
      Error,
    );
    E('ERR_METHOD_NOT_IMPLEMENTED', 'The %s method is not implemented', Error);
    E('ERR_MISSING_ARGS',
      (...args) => {
        assert(args.length > 0, 'At least one arg needs to be specified');
        let msg = 'The ';
        const len = args.length;
        const wrap = (a) => `\"${a}\"`;
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
        assert(range, 'Missing \"range\" argument');
        let msg = replaceDefaultBoolean ? str :
          `The value of \"${str}\" is out of range.`;
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
      return `Package import specifier \"${specifier}\" is not defined${packagePath ?
        ` in package ${packagePath}package.json` : ''} imported from ${base}`;
    }, TypeError);
    E('ERR_PACKAGE_MAP_EXTERNAL_FILE', (specifier, parentPath, configPath) => {
      return `Cannot resolve \"${specifier}\" from \"${parentPath}\": file is not within any package defined in ${configPath}`;
    }, Error);
    E('ERR_PACKAGE_MAP_INVALID', (configPath, reason) => {
      return `Invalid package-map.json at \"${configPath}\": ${reason}`;
    }, SyntaxError);
    E('ERR_PACKAGE_MAP_KEY_NOT_FOUND', (key, configPath) => {
      return `Package key \"${key}\" referenced in dependencies but not defined in ${configPath}`;
    }, Error);
    E('ERR_PACKAGE_PATH_NOT_EXPORTED', (pkgPath, subpath, base = undefined) => {
      if (subpath === '.')
        return `No \"exports\" main defined in ${pkgPath}package.json${base ?
          ` imported from ${base}` : ''}`;
      return `Package subpath '${subpath}' is not defined by \"exports\" in ${
        pkgPath}package.json${base ? ` imported from ${base}` : ''}`;
    }, Error);
    E('ERR_PARSE_ARGS_INVALID_OPTION_VALUE', '%s', TypeError);
    E('ERR_PARSE_ARGS_UNEXPECTED_POSITIONAL', \"Unexpected argument '%s'. This \" +
      'command does not take positional arguments', TypeError);
    E('ERR_PARSE_ARGS_UNKNOWN_OPTION', (option, allowPositionals) => {
      const suggestDashDash = allowPositionals ? '. To specify a positional ' +
        \"argument starting with a '-', place it at the end of the command after \" +
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
        message += `\\nRequired module: ${filename}`;
      }
      if (parent) {
        const { getRequireStack } = require('internal/modules/helpers');
        const requireStack = getRequireStack(parent);
        if (requireStack.length > 0) {
          message += '\\nRequire stack:\\n- ' +
                    requireStack.join('\\n- ');
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
          `${urlToFilename(url)}:${line}\\n\\n${sourceLine}\\n${' '.repeat(column - 1)}^\\n`);
        setArrowMessage(this, frames.join('\\n'));
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
            msg += `\\nInstead change the require of ${filename} to a dynamic ` +
                'import() which is available in all CommonJS modules.';
          return msg;
        }
        const path = require('path');
        const basename = parentPath && path.basename(filename) ===
          path.basename(parentPath) ? filename : path.basename(filename);
        if (hasEsmSyntax) {
          msg += `\\nInstead change the require of ${basename} in ${parentPath} to` +
            ' a dynamic import() which is available in all CommonJS modules.';
          return msg;
        }
        msg += `\\n${basename} is treated as an ES module file as it is a .js ` +
          'file whose nearest parent package.json contains \"type\": \"module\" ' +
          'which declares all .js files in that package scope as ES modules.' +
          `\\nInstead either rename ${basename} to end in .cjs, change the requiring ` +
          'code to use dynamic import() which is available in all CommonJS ' +
          'modules, or change \"type\": \"module\" to \"type\": \"commonjs\" in ' +
          `${packageJsonPath} to treat all .js files as CommonJS (using .mjs for ` +
          'all ES modules instead).\\n';
        return msg;
      }, Error);
    E('ERR_REQUIRE_ESM_RACE_CONDITION', (filename, parentFilename, isForAsyncLoaderHookWorker) => {
      let raceMessage = `Cannot require() ES Module ${filename} because it is not yet fully loaded.\\n`;
      raceMessage += 'This may be caused by a race condition if the module is simultaneously dynamically ';
      raceMessage += 'import()-ed via Promise.all().\\n';
      raceMessage += 'Try await-ing the import() sequentially in a loop instead.\\n';
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
             \"The 'allowZero' argument must be of type boolean.\");
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
             \"The 'failureType' argument must be of type string or symbol.\");

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
      '\"servername\" is required parameter for Server.addContext', Error);
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
    E('ERR_UNKNOWN_FILE_EXTENSION', 'Unknown file extension \"%s\" for %s', TypeError);
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
      'Stripping types is currently unsupported for files under node_modules, for \"%s\"',
      Error);
    E('ERR_UNSUPPORTED_RESOLVE_REQUEST',
      'Failed to resolve module specifier \"%s\" from \"%s\": Invalid relative URL or base scheme is not hierarchical.',
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
      'relative path starting with \\'./\\' or \\'../\\'.' +
      (filename.startsWith('file://') ?
        ' Wrap file:// URLs with `new URL`.' : ''
      ) +
      (filename.startsWith('data:text/javascript') ?
        ' Wrap data: URLs with `new URL`.' : ''
      ) +
      ` Received \"${filename}\"`,
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
  \"internal/validators\": function (module, exports, require, process, primordials, internalBinding) {
    /* eslint jsdoc/require-jsdoc: \"error\" */

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
      if (value.includes('\\u0000')) {
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
    const linkValueRegExp = /^(?:<[^>\\r\\n]*>)(?:\\s*;\\s*[^;\"\\s]+(?:=(\")?[^;\"\\s]*\\1)?)*$/;

    const validateLinkHeaderFormat = hideStackFrames((value, name) => {
      if (
        typeof value === 'undefined' ||
        !linkValueRegExp.exec(value)
      ) {
        throw new ERR_INVALID_ARG_VALUE(
          name,
          value,
          'must be an array or string of format \"</styles.css>; rel=preload; as=style\"',
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
        'must be an array or string of format \"</styles.css>; rel=preload; as=style\"',
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
  \"internal/util\": function (module, exports, require, process, primordials, internalBinding) {
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

    const colorRegExp = /\\u001b\\[\\d\\d?m/g; // eslint-disable-line no-control-regex

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

    const kNodeModulesRE = /^(?:.*)[\\\\/]node_modules[\\\\/]/;

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
  \"internal/util/types\": function (module, exports, require, process, primordials, internalBinding) {
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
  \"internal/util/inspect\": function (module, exports, require, process, primordials, internalBinding) {
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
    const strEscapeSequencesRegExp = /[\\x00-\\x1f\\x27\\x5c\\x7f-\\x9f]|[\\ud800-\\udbff](?![\\udc00-\\udfff])|(?<![\\ud800-\\udbff])[\\udc00-\\udfff]/;
    const strEscapeSequencesReplacer = /[\\x00-\\x1f\\x27\\x5c\\x7f-\\x9f]|[\\ud800-\\udbff](?![\\udc00-\\udfff])|(?<![\\ud800-\\udbff])[\\udc00-\\udfff]/g;
    const strEscapeSequencesRegExpSingle = /[\\x00-\\x1f\\x5c\\x7f-\\x9f]|[\\ud800-\\udbff](?![\\udc00-\\udfff])|(?<![\\ud800-\\udbff])[\\udc00-\\udfff]/;
    const strEscapeSequencesReplacerSingle = /[\\x00-\\x1f\\x5c\\x7f-\\x9f]|[\\ud800-\\udbff](?![\\udc00-\\udfff])|(?<![\\ud800-\\udbff])[\\udc00-\\udfff]/g;
    /* eslint-enable no-control-regex */

    const keyStrRegExp = /^[a-zA-Z_][a-zA-Z_0-9]*$/;
    const numberRegExp = /^(0|[1-9][0-9]*)$/;

    const coreModuleRegExp = /^ {4}at (?:[^/\\\\(]+ \\(|)node:(.+):\\d+:\\d+\\)?$/;

    const classRegExp = /^(\\s+[^(]*?)\\s*{/;
    const stripCommentsRegExp = /(\\/\\/.*?\\n)|(\\/\\*(.|\\n)*?\\*\\/)/g;

    const kMinLineLength = 16;

    const kWeak = 0;
    const kIterator = 1;
    const kMapEntries = 2;

    const meta = [
      '\\\\x00', '\\\\x01', '\\\\x02', '\\\\x03', '\\\\x04', '\\\\x05', '\\\\x06', '\\\\x07', // x07
      '\\\\b', '\\\\t', '\\\\n', '\\\\x0B', '\\\\f', '\\\\r', '\\\\x0E', '\\\\x0F',           // x0F
      '\\\\x10', '\\\\x11', '\\\\x12', '\\\\x13', '\\\\x14', '\\\\x15', '\\\\x16', '\\\\x17', // x17
      '\\\\x18', '\\\\x19', '\\\\x1A', '\\\\x1B', '\\\\x1C', '\\\\x1D', '\\\\x1E', '\\\\x1F', // x1F
      '', '', '', '', '', '', '', \"\\\\'\", '', '', '', '', '', '', '', '',      // x2F
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',         // x3F
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',         // x4F
      '', '', '', '', '', '', '', '', '', '', '', '', '\\\\\\\\', '', '', '',     // x5F
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',         // x6F
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '\\\\x7F',    // x7F
      '\\\\x80', '\\\\x81', '\\\\x82', '\\\\x83', '\\\\x84', '\\\\x85', '\\\\x86', '\\\\x87', // x87
      '\\\\x88', '\\\\x89', '\\\\x8A', '\\\\x8B', '\\\\x8C', '\\\\x8D', '\\\\x8E', '\\\\x8F', // x8F
      '\\\\x90', '\\\\x91', '\\\\x92', '\\\\x93', '\\\\x94', '\\\\x95', '\\\\x96', '\\\\x97', // x97
      '\\\\x98', '\\\\x99', '\\\\x9A', '\\\\x9B', '\\\\x9C', '\\\\x9D', '\\\\x9E', '\\\\x9F', // x9F
    ];

    const ansi = new RegExp(
      '(?:\\\\u001B\\\\][\\\\s\\\\S]*?(?:\\\\u0007|\\\\u001B\\\\u005C|\\\\u009C))' +
      '|[\\\\u001B\\\\u009B][[\\\\]()#;?]*' +
      '(?:\\\\d{1,4}(?:[;:]\\\\d{0,4})*)?' +
      '[\\\\dA-PR-TZcf-nq-uy=><~]', 'g',
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
        if (color) acc.push([`\\u001b[${color[0]}m`, `\\u001b[${color[1]}m`]);
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
          if (ch === '\\\\') {
            let seq = '\\\\';
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
        } else if (ch === '\\\\') {
          let seq = '\\\\';
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
        return `\"${str}\"`;
      }
      if (quotes === -2) {
        return `\\`${str}\\``;
      }
      return `'${str}'`;
    }

    function escapeFn(str) {
      const charCode = str.charCodeAt();
      return meta.length > charCode ? meta[charCode] : `\\\\u${charCode.toString(16)}`;
    }

    function strEscape(str) {
      let escapeTest = strEscapeSequencesRegExp;
      let escapeReplace = strEscapeSequencesReplacer;
      let singleQuote = 39;

      if (str.includes(\"'\")) {
        if (!str.includes('\"')) {
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
          result += `${str.slice(last, i)}\\\\u${point.toString(16)}`;
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
          return `\\u001b[${color[0]}m${str}\\u001b[${color[1]}m`;
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
            output.push(`\\u001b[2m${value}\\u001b[22m`);
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
            return ret.replaceAll('\\n', `\\n${' '.repeat(ctx.indentationLvl)}`);
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
        return `${ErrorPrototypeToString(error)}\\n    ${result}`;
      }
      return ErrorPrototypeToString(error);
    }

    function getStackFrames(ctx, err, stack) {
      const frames = stack.split('\\n');

      let cause;
      try {
        ({ cause } = err);
      } catch {
      }

      if (cause != null && isError(cause)) {
        const causeStack = getStackString(ctx, cause);
        const causeStackStart = causeStack.indexOf('\\n    at');
        if (causeStackStart !== -1) {
          const causeFrames = causeStack.slice(causeStackStart + 1).split('\\n');
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
          (stack.length === len || stack[len] === ':' || stack[len] === '\\n'))) {
        let fallback = 'Error';
        if (constructor === null) {
          const start = (/^([A-Z][a-z_ A-Z0-9[\\]()-]+)(?::|\\n {4}at)/).exec(stack) ||
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

        if ((after !== '/' && after !== '\\\\') || (separator !== '/' && separator !== '\\\\')) {
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
      const stackStart = stack.indexOf('\\n    at', pos);
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
              newStack += `\\n${ctx.stylize(line, 'undefined')}`;
            } else {
              newStack += '\\n';

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
          newStack += `\\n${lines.join('\\n')}`;
        }
        stack = newStack;
      }
      if (ctx.indentationLvl !== 0) {
        const indentation = ' '.repeat(ctx.indentationLvl);
        stack = stack.replaceAll('\\n', `\\n${indentation}`);
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
          return ((RegExpPrototypeSymbolSplit(/(?<=\\n)/, value)).map((line) => fn(strEscape(line), 'string'))).join(` +\\n${' '.repeat(ctx.indentationLvl + 2)}`) + trailer;
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
          extra = `\\n${' '.repeat(ctx.indentationLvl)}`;
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
        name = key === '__proto__' ? \"['__proto__']\" : ctx.stylize(key, 'name');
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
        if (base !== '' && base.includes('\\n')) return false;
        for (let i = 0; i < output.length; i++) {
          if (typeof output[i] === 'string' && output[i].includes('\\n')) return false;
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
      return base === '' || !base.includes('\\n');
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
              if (!joinedOutput.includes('\\n')) {
                return `${base ? `${base} ` : ''}${braces[0]} ${joinedOutput}` +
                  ` ${braces[1]}`;
              }
            }
          }
        }
        const indentation = `\\n${' '.repeat(ctx.indentationLvl)}`;
        return `${base ? `${base} ` : ''}${braces[0]}${indentation}  ` +
          `${join(output, `,${indentation}  `)}${indentation}${braces[1]}`;
      }
      if (isBelowBreakLength(ctx, output, 0, base)) {
        return `${braces[0]}${base ? ` ${base}` : ''} ${join(output, ', ')} ` +
          braces[1];
      }
      const indentation = ' '.repeat(ctx.indentationLvl);
      const ln = base === '' && braces[0].length === 1 ?
        ' ' : `${base ? ` ${base}` : ''}\\n${indentation}  `;
      return `${braces[0]}${ln}${join(output, `,\\n${indentation}  `)} ${braces[1]}`;
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

    const firstErrorLine = (error) => error.message.split('\\n', 1)[0];
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

      if (str.indexOf('\\u001B') === -1 &&
          str.indexOf('\\u009B') === -1)
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
  \"internal/assert\": function (module, exports, require, process, primordials, internalBinding) {
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
  \"internal/constants\": function (module, exports, require, process, primordials, internalBinding) {
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
      CHAR_BACKWARD_SLASH: 92, /* \\ */
      CHAR_VERTICAL_LINE: 124, /* | */
      CHAR_COLON: 58, /* : */
      CHAR_QUESTION_MARK: 63, /* ? */
      CHAR_UNDERSCORE: 95, /* _ */
      CHAR_LINE_FEED: 10, /* \\n */
      CHAR_CARRIAGE_RETURN: 13, /* \\r */
      CHAR_TAB: 9, /* \\t */
      CHAR_FORM_FEED: 12, /* \\f */
      CHAR_EXCLAMATION_MARK: 33, /* ! */
      CHAR_HASH: 35, /* # */
      CHAR_SPACE: 32, /*   */
      CHAR_NO_BREAK_SPACE: 160, /* \\u00A0 */
      CHAR_ZERO_WIDTH_NOBREAK_SPACE: 65279, /* \\uFEFF */
      CHAR_LEFT_SQUARE_BRACKET: 91, /* [ */
      CHAR_RIGHT_SQUARE_BRACKET: 93, /* ] */
      CHAR_LEFT_ANGLE_BRACKET: 60, /* < */
      CHAR_RIGHT_ANGLE_BRACKET: 62, /* > */
      CHAR_LEFT_CURLY_BRACKET: 123, /* { */
      CHAR_RIGHT_CURLY_BRACKET: 125, /* } */
      CHAR_HYPHEN_MINUS: 45, /* - */
      CHAR_PLUS: 43, /* + */
      CHAR_DOUBLE_QUOTE: 34, /* \" */
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

      EOL: isWindows ? '\\r\\n' : '\\n',
    };
  },
  \"internal/util/colors\": function (module, exports, require, process, primordials, internalBinding) {
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
          module.exports.blue = '\\u001b[34m';
          module.exports.green = '\\u001b[32m';
          module.exports.white = '\\u001b[39m';
          module.exports.yellow = '\\u001b[33m';
          module.exports.red = '\\u001b[31m';
          module.exports.gray = '\\u001b[90m';
          module.exports.clear = '\\u001bc';
          module.exports.reset = '\\u001b[0m';
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
  \"internal/util/debuglog\": function (module, exports, require, process, primordials, internalBinding) {
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
        debugEnv = debugEnv.replace(/[|\\\\{}()[\\]^$+?.]/g, '\\\\$&')
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
          'to \\'' + set.toLowerCase() + '\\' can expose sensitive ' +
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
            process.stderr.write(format('%s %s: %s\\n', set, coloredPID, msg));
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
      return label.replaceAll('\\\\', '\\\\\\\\').replaceAll('\"', '\\\\\"');
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
          process.stderr.write(format('%s %s %s: %s\\n', set, coloredPID, label, timeFormatted));
        else
          process.stderr.write(
            format(
              '%s %s %s: %s\\n',
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
    throw new Error(\"no module \" + name + \" in this port\");
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
  find: loadAnywhere,
  addLoader,
  primordials,
  internalBinding,
  process: processForFiles,
};
"

pub const async_hooks = "\"use strict\";

const { getContext, setContext } = native;

const {
  rangeError,
  validateFunction,
  validateObject,
  validateString,
} = require(\"spark:errors\");

class AsyncContextFrame extends Map {
  constructor(store, data) {
    super(getContext());
    this.set(store, data);
  }
}

let lastAsyncId = 1;

class AsyncResource {
  #frame;
  #asyncId;
  #triggerAsyncId;

  constructor(type, options = {}) {
    validateString(type, \"type\");
    let triggerAsyncId = options;
    if (typeof options !== \"number\") {
      triggerAsyncId =
        options.triggerAsyncId === undefined ? 0 : options.triggerAsyncId;
    }
    if (!Number.isSafeInteger(triggerAsyncId) || triggerAsyncId < -1) {
      throw rangeError(
        \"ERR_INVALID_ASYNC_ID\",
        `Invalid triggerAsyncId value: ${triggerAsyncId}`,
      );
    }
    this.#frame = getContext();
    this.#asyncId = ++lastAsyncId;
    this.#triggerAsyncId = triggerAsyncId;
  }

  runInAsyncScope(fn, thisArg, ...args) {
    const prior = setContext(this.#frame);
    try {
      return Reflect.apply(fn, thisArg, args);
    } finally {
      setContext(prior);
    }
  }

  emitDestroy() {
    return this;
  }

  asyncId() {
    return this.#asyncId;
  }

  triggerAsyncId() {
    return this.#triggerAsyncId;
  }

  bind(fn, thisArg) {
    validateFunction(fn, \"fn\");
    let bound;
    if (thisArg === undefined) {
      const resource = this;
      bound = function (...args) {
        args.unshift(fn, this);
        return Reflect.apply(resource.runInAsyncScope, resource, args);
      };
    } else {
      bound = this.runInAsyncScope.bind(this, fn, thisArg);
    }
    Object.defineProperty(bound, \"length\", {
      configurable: true,
      enumerable: false,
      value: fn.length,
      writable: false,
    });
    return bound;
  }

  static bind(fn, type, thisArg) {
    type ||= fn.name;
    return new AsyncResource(type || \"bound-anonymous-fn\").bind(fn, thisArg);
  }
}

class AsyncLocalStorage {
  #defaultValue = undefined;
  #name = undefined;

  constructor(options = {}) {
    validateObject(options, \"options\");
    this.#defaultValue = options.defaultValue;
    if (options.name !== undefined) {
      this.#name = `${options.name}`;
    }
  }

  get name() {
    return this.#name || \"\";
  }

  static bind(fn) {
    return AsyncResource.bind(fn);
  }

  static snapshot() {
    return AsyncLocalStorage.bind((cb, ...args) => cb(...args));
  }

  disable() {
    const frame = getContext();
    if (frame !== undefined) frame.delete(this);
  }

  enterWith(data) {
    setContext(new AsyncContextFrame(this, data));
  }

  run(data, fn, ...args) {
    const prior = this.getStore();
    if (Object.is(prior, data)) {
      return Reflect.apply(fn, null, args);
    }
    this.enterWith(data);
    try {
      return Reflect.apply(fn, null, args);
    } finally {
      this.enterWith(prior);
    }
  }

  exit(fn, ...args) {
    return this.run(undefined, fn, ...args);
  }

  getStore() {
    const frame = getContext();
    if (frame === undefined || !frame.has(this)) {
      return this.#defaultValue;
    }
    return frame.get(this);
  }
}

// no async_hooks tracking: hooks never fire and ids stay 0, like bun
function createHook(hooks) {
  validateObject(hooks, \"hookCallbacks\");
  return {
    enable() {
      return this;
    },
    disable() {
      return this;
    },
  };
}

function executionAsyncId() {
  return 0;
}

function triggerAsyncId() {
  return 0;
}

function executionAsyncResource() {
  return {};
}

module.exports = {
  AsyncLocalStorage,
  AsyncResource,
  createHook,
  executionAsyncId,
  triggerAsyncId,
  executionAsyncResource,
  asyncWrapProviders: Object.freeze({ __proto__: null }),
};
"

pub const timers = "\"use strict\";

const { now, getContext, setContext } = native;
const { validateFunction } = require(\"spark:errors\");

const TIMEOUT_MAX = 2 ** 31 - 1;

const active = new Map();
const heap = [];
const immediates = [];
let nextId = 1;
let nextSeq = 1;
let refCount = 0;

function before(a, b) {
  return a.deadline < b.deadline ||
    (a.deadline === b.deadline && a.seq < b.seq);
}

function heapPush(entry) {
  let i = heap.length;
  heap.push(entry);
  while (i > 0) {
    const parent = (i - 1) >> 1;
    if (!before(entry, heap[parent])) break;
    heap[i] = heap[parent];
    i = parent;
  }
  heap[i] = entry;
}

function heapPop() {
  const top = heap[0];
  const last = heap.pop();
  const size = heap.length;
  if (size === 0) return top;
  let i = 0;
  for (;;) {
    let child = 2 * i + 1;
    if (child >= size) break;
    if (child + 1 < size && before(heap[child + 1], heap[child])) child++;
    if (!before(heap[child], last)) break;
    heap[i] = heap[child];
    i = child;
  }
  heap[i] = last;
  return top;
}

class Timeout {
  #id = nextId++;
  #callback;
  #args;
  #delay;
  #repeat;
  #seq = 0;
  #refed = true;
  #active = false;
  #destroyed = false;
  #frame = getContext();

  constructor(callback, delay, args, repeat) {
    delay *= 1;
    if (!(delay >= 1 && delay <= TIMEOUT_MAX)) delay = 1;
    this.#callback = callback;
    this.#args = args;
    this.#delay = delay;
    this.#repeat = repeat;
    Timeout.#schedule(this);
  }

  static #schedule(timer) {
    timer.#seq = nextSeq++;
    if (!timer.#active) {
      timer.#active = true;
      active.set(timer.#id, timer);
      if (timer.#refed) refCount++;
    }
    heapPush({ deadline: now() + timer.#delay, seq: timer.#seq, timer });
  }

  static #cancel(timer) {
    if (!timer.#active) return;
    timer.#active = false;
    active.delete(timer.#id);
    if (timer.#refed) refCount--;
  }

  static isTimeout(value) {
    return typeof value === \"object\" && value !== null && #active in value;
  }

  static destroy(timer) {
    timer.#destroyed = true;
    Timeout.#cancel(timer);
  }

  // the earliest live entry, with stale ones dropped
  static peek() {
    while (heap.length > 0) {
      const entry = heap[0];
      if (entry.timer.#active && entry.timer.#seq === entry.seq) return entry;
      heapPop();
    }
    return undefined;
  }

  static fire(timer) {
    heapPop();
    if (timer.#repeat) {
      Timeout.#schedule(timer);
    } else {
      Timeout.#cancel(timer);
    }
    const prior = setContext(timer.#frame);
    try {
      Reflect.apply(timer.#callback, timer, timer.#args);
    } finally {
      setContext(prior);
    }
  }

  ref() {
    if (!this.#refed) {
      this.#refed = true;
      if (this.#active) refCount++;
    }
    return this;
  }

  unref() {
    if (this.#refed) {
      this.#refed = false;
      if (this.#active) refCount--;
    }
    return this;
  }

  hasRef() {
    return this.#refed;
  }

  refresh() {
    if (!this.#destroyed) Timeout.#schedule(this);
    return this;
  }

  close() {
    Timeout.destroy(this);
    return this;
  }

  [Symbol.toPrimitive]() {
    return this.#id;
  }

  [Symbol.dispose]() {
    Timeout.destroy(this);
  }
}

class Immediate {
  #callback;
  #args;
  #refed = true;
  #queued = true;
  #frame = getContext();

  constructor(callback, args) {
    this.#callback = callback;
    this.#args = args;
    refCount++;
    immediates.push(this);
  }

  static isImmediate(value) {
    return typeof value === \"object\" && value !== null && #queued in value;
  }

  static destroy(immediate) {
    if (!immediate.#queued) return;
    immediate.#queued = false;
    if (immediate.#refed) refCount--;
  }

  // false when it was cleared while queued
  static fire(immediate) {
    if (!immediate.#queued) return false;
    Immediate.destroy(immediate);
    const prior = setContext(immediate.#frame);
    try {
      Reflect.apply(immediate.#callback, immediate, immediate.#args);
    } finally {
      setContext(prior);
    }
    return true;
  }

  ref() {
    if (!this.#refed) {
      this.#refed = true;
      if (this.#queued) refCount++;
    }
    return this;
  }

  unref() {
    if (this.#refed) {
      this.#refed = false;
      if (this.#queued) refCount--;
    }
    return this;
  }

  hasRef() {
    return this.#refed;
  }

  [Symbol.dispose]() {
    Immediate.destroy(this);
  }
}

function setTimeout(callback, delay, ...args) {
  validateFunction(callback, \"callback\");
  return new Timeout(callback, delay, args, false);
}

function setInterval(callback, delay, ...args) {
  validateFunction(callback, \"callback\");
  return new Timeout(callback, delay, args, true);
}

function clearTimeout(timer) {
  if (Timeout.isTimeout(timer)) {
    Timeout.destroy(timer);
  } else if (typeof timer === \"number\" || typeof timer === \"string\") {
    const found = active.get(Number(timer));
    if (found !== undefined) Timeout.destroy(found);
  }
}

function setImmediate(callback, ...args) {
  validateFunction(callback, \"callback\");
  return new Immediate(callback, args);
}

function clearImmediate(immediate) {
  if (Immediate.isImmediate(immediate)) Immediate.destroy(immediate);
}

require(\"spark:loop\").addSource({
  step() {
    const entry = Timeout.peek();
    if (entry !== undefined && entry.deadline <= now()) {
      Timeout.fire(entry.timer);
      return true;
    }
    while (immediates.length > 0) {
      if (Immediate.fire(immediates.shift())) return true;
    }
    return false;
  },
  wait() {
    if (immediates.length > 0) return 0;
    const entry = Timeout.peek();
    if (entry === undefined) return -1;
    return Math.max(entry.deadline - now(), 0);
  },
  alive() {
    return refCount > 0;
  },
});

module.exports = {
  setTimeout,
  clearTimeout,
  setInterval,
  clearInterval: clearTimeout,
  setImmediate,
  clearImmediate,
};
"

pub const fs = "\"use strict\";

const {
  AbortError,
  codes,
  invalidArgType,
  invalidArgValue,
  systemError,
  validateAbortSignal,
  validateBoolean,
  validateFunction,
  validateObject,
} = require(\"spark:errors\");
const { Buffer } = require(\"node:buffer\");

// a module the embedder may not have passed
function optional(specifier) {
  try {
    return require(specifier);
  } catch {
    return undefined;
  }
}

const F_OK = 0;
const R_OK = 4;
const W_OK = 2;
const X_OK = 1;

const S_IFMT = 0o170000;
const S_IFREG = 0o100000;
const S_IFDIR = 0o040000;
const S_IFCHR = 0o020000;
const S_IFBLK = 0o060000;
const S_IFIFO = 0o010000;
const S_IFLNK = 0o120000;
const S_IFSOCK = 0o140000;

const UV_DIRENT_UNKNOWN = 0;
const UV_DIRENT_FILE = 1;
const UV_DIRENT_DIR = 2;
const UV_DIRENT_LINK = 3;
const UV_DIRENT_FIFO = 4;
const UV_DIRENT_SOCKET = 5;
const UV_DIRENT_CHAR = 6;
const UV_DIRENT_BLOCK = 7;

const constants = {
  __proto__: null,
  UV_DIRENT_UNKNOWN,
  UV_DIRENT_FILE,
  UV_DIRENT_DIR,
  UV_DIRENT_LINK,
  UV_DIRENT_FIFO,
  UV_DIRENT_SOCKET,
  UV_DIRENT_CHAR,
  UV_DIRENT_BLOCK,
  S_IFMT,
  S_IFREG,
  S_IFDIR,
  S_IFCHR,
  S_IFBLK,
  S_IFIFO,
  S_IFLNK,
  S_IFSOCK,
  F_OK,
  R_OK,
  W_OK,
  X_OK,
};

const kEmptyObject = Object.freeze({ __proto__: null });

function isURL(self) {
  return Boolean(
    self?.href &&
    self.protocol &&
    self.auth === undefined &&
    self.path === undefined,
  );
}

// a path may be a string, bytes or a file: URL; the embedder is asked with text
function getValidatedPath(fileURLOrPath, name = \"path\") {
  let path = fileURLOrPath;
  if (isURL(path)) {
    const url = optional(\"node:url\");
    if (url !== undefined) path = url.fileURLToPath(path);
  }
  const isBytes = ArrayBuffer.isView(path) && path.BYTES_PER_ELEMENT === 1;
  if (typeof path !== \"string\" && !isBytes) {
    throw invalidArgType(name, [\"string\", \"Buffer\", \"URL\"], path);
  }
  if (isBytes) {
    path = Buffer.from(path.buffer, path.byteOffset, path.byteLength).toString();
  }
  if (path.includes(\"\\u0000\")) {
    throw invalidArgValue(
      name,
      fileURLOrPath,
      \"must be a string, Uint8Array, or URL without null bytes\",
    );
  }
  return path;
}

function assertEncoding(encoding) {
  if (encoding && !Buffer.isEncoding(encoding)) {
    throw invalidArgValue(\"encoding\", encoding, \"is invalid encoding\");
  }
}

function getOptions(options, defaultOptions = kEmptyObject) {
  if (options == null || typeof options === \"function\") {
    return defaultOptions;
  }

  if (typeof options === \"string\") {
    defaultOptions = { ...defaultOptions };
    defaultOptions.encoding = options;
    options = defaultOptions;
  } else if (typeof options !== \"object\") {
    throw invalidArgType(\"options\", [\"string\", \"Object\"], options);
  }

  if (options.encoding !== \"buffer\") assertEncoding(options.encoding);

  if (options.signal !== undefined) {
    validateAbortSignal(options.signal, \"options.signal\");
  }

  return options;
}

// a name or a path comes from the embedder as text; node hands it on as
// text, as a Buffer, or as that Buffer read in another encoding
function inEncoding(text, encoding) {
  if (!encoding || encoding === \"utf8\" || encoding === \"utf-8\") return text;
  const bytes = Buffer.from(text);
  return encoding === \"buffer\" ? bytes : bytes.toString(encoding);
}

function checkAborted(signal, callback) {
  if (signal?.aborted) {
    callback(new AbortError(undefined, { cause: signal.reason }));
    return true;
  }
  return false;
}

// node raises these from its binding, so they are plain errors with a code
function modeError(Base, code, message) {
  const error = new Base(message);
  error.code = code;
  return error;
}

function getValidMode(mode) {
  if (mode === undefined || mode === null) return F_OK;
  if (typeof mode !== \"number\") {
    throw modeError(
      TypeError,
      \"ERR_INVALID_ARG_TYPE\",
      \"mode must be int32 or null/undefined\",
    );
  }
  if (!Number.isFinite(mode)) {
    throw modeError(RangeError, \"ERR_OUT_OF_RANGE\", \"mode is out of range\");
  }
  const int = Math.trunc(mode);
  if (int < 0 || int > 7) {
    throw modeError(
      RangeError,
      \"ERR_OUT_OF_RANGE\",
      \"mode is out of range: >= 0 && <= 7\",
    );
  }
  return int;
}

// a want that failed is thrown or rejected as its error code, a string
function failed(thrown, syscall, path) {
  return typeof thrown === \"string\" ? systemError(thrown, syscall, path) : thrown;
}

function accessSync(path, mode) {
  path = getValidatedPath(path);
  mode = getValidMode(mode);
  try {
    native.access(path, mode);
  } catch (thrown) {
    throw failed(thrown, \"access\", path);
  }
}

function existsSync(path) {
  try {
    path = getValidatedPath(path);
  } catch {
    return false;
  }
  try {
    native.access(path, F_OK);
    return true;
  } catch (thrown) {
    if (typeof thrown === \"string\") return false;
    throw thrown;
  }
}

function access(path, mode, callback) {
  if (typeof mode === \"function\") {
    callback = mode;
    mode = F_OK;
  }

  path = getValidatedPath(path);
  validateFunction(callback, \"cb\");
  mode = getValidMode(mode);

  native.accessLater(path, mode).then(
    () => callback(null),
    (thrown) => callback(failed(thrown, \"access\", path)),
  );
}

function exists(path, callback) {
  validateFunction(callback, \"cb\");

  function suppressedCallback(err) {
    callback(!err);
  }

  try {
    access(path, F_OK, suppressedCallback);
  } catch {
    return callback(false);
  }
}

function realpathSyncNative(path, options) {
  options = getOptions(options);
  path = getValidatedPath(path);
  try {
    return inEncoding(native.realpath(path), options.encoding);
  } catch (thrown) {
    throw failed(thrown, \"realpath\", path);
  }
}

// one question to the embedder, where node walks the path itself; a failure
// is reported for the whole path with syscall realpath
function realpathSync(path, options) {
  options = getOptions(options);
  if (typeof path !== \"string\" && !isURL(path) && !ArrayBuffer.isView(path)) {
    path += \"\";
  }
  return realpathSyncNative(path, options);
}
realpathSync.native = realpathSyncNative;

function realpathNative(path, options, callback) {
  if (typeof options === \"function\") {
    callback = options;
    options = undefined;
  }
  validateFunction(callback, \"cb\");
  options = getOptions(options);
  path = getValidatedPath(path);
  native.realpathLater(path).then(
    (resolved) => callback(null, inEncoding(resolved, options.encoding)),
    (thrown) => callback(failed(thrown, \"realpath\", path)),
  );
}

function realpath(path, options, callback) {
  if (typeof options === \"function\") {
    callback = options;
    options = undefined;
  }
  if (typeof path !== \"string\" && !isURL(path) && !ArrayBuffer.isView(path)) {
    path += \"\";
  }
  realpathNative(path, options, callback);
}
realpath.native = realpathNative;

// ---- what stat returns, as node's lib/internal/fs/utils.js builds it

const kPartialAtimeNs = Symbol(\"kPartialAtimeNs\");
const kPartialMtimeNs = Symbol(\"kPartialMtimeNs\");
const kPartialCtimeNs = Symbol(\"kPartialCtimeNs\");
const kPartialBirthtimeNs = Symbol(\"kPartialBirthtimeNs\");

const kNsPerMsBigInt = 1_000_000n;
const kNsPerSecBigInt = 1_000_000_000n;
const kMsPerSec = 1_000;
const kNsPerMs = 1_000_000;

function setOwnProperty(obj, key, value) {
  return Object.defineProperty(obj, key, {
    __proto__: null,
    configurable: true,
    enumerable: true,
    value,
    writable: true,
  })[key];
}

function StatsBase(dev, mode, nlink, uid, gid, rdev, blksize, ino, size, blocks) {
  this.dev = dev;
  this.mode = mode;
  this.nlink = nlink;
  this.uid = uid;
  this.gid = gid;
  this.rdev = rdev;
  this.blksize = blksize;
  this.ino = ino;
  this.size = size;
  this.blocks = blocks;
}

StatsBase.prototype.isDirectory = function () {
  return this._checkModeProperty(S_IFDIR);
};

StatsBase.prototype.isFile = function () {
  return this._checkModeProperty(S_IFREG);
};

StatsBase.prototype.isBlockDevice = function () {
  return this._checkModeProperty(S_IFBLK);
};

StatsBase.prototype.isCharacterDevice = function () {
  return this._checkModeProperty(S_IFCHR);
};

StatsBase.prototype.isSymbolicLink = function () {
  return this._checkModeProperty(S_IFLNK);
};

StatsBase.prototype.isFIFO = function () {
  return this._checkModeProperty(S_IFIFO);
};

StatsBase.prototype.isSocket = function () {
  return this._checkModeProperty(S_IFSOCK);
};

function msFromTimeSpec(sec, nsec) {
  return sec * kMsPerSec + nsec / kNsPerMs;
}

function nsFromTimeSpecBigInt(sec, nsec) {
  return sec * kNsPerSecBigInt + nsec;
}

function instantFromNs(nsec) {
  return new Temporal.Instant(nsec);
}

function instantFromTimeSpecMs(msec, nsec) {
  return new Temporal.Instant(
    BigInt(Math.floor(msec / kMsPerSec)) * kNsPerSecBigInt + BigInt(nsec),
  );
}

function dateFromMs(ms) {
  return new Date(Math.round(Number(ms)));
}

// each is made on first read and then kept on the object
function lazyFields(names, make) {
  const fields = { __proto__: null };
  for (const name of names) {
    fields[name] = {
      __proto__: null,
      enumerable: true,
      configurable: true,
      get() {
        return setOwnProperty(this, name, make(this, name));
      },
      set(value) {
        setOwnProperty(this, name, value);
      },
    };
  }
  return fields;
}

const lazyDateFields = lazyFields(
  [\"atime\", \"mtime\", \"ctime\", \"birthtime\"],
  (stats, name) => dateFromMs(stats[`${name}Ms`]),
);

const partialNs = {
  __proto__: null,
  atimeInstant: kPartialAtimeNs,
  mtimeInstant: kPartialMtimeNs,
  ctimeInstant: kPartialCtimeNs,
  birthtimeInstant: kPartialBirthtimeNs,
};
const instantNames = [\"atimeInstant\", \"mtimeInstant\", \"ctimeInstant\", \"birthtimeInstant\"];

const lazyTemporalFields = lazyFields(instantNames, (stats, name) =>
  instantFromTimeSpecMs(stats[`${name.slice(0, -7)}Ms`], stats[partialNs[name]]),
);

const lazyTemporalBigIntFields = lazyFields(instantNames, (stats, name) =>
  instantFromNs(stats[`${name.slice(0, -7)}Ns`]),
);

function BigIntStats(dev, mode, nlink, uid, gid, rdev, blksize, ino, size, blocks, atimeNs, mtimeNs, ctimeNs, birthtimeNs) {
  StatsBase.call(this, dev, mode, nlink, uid, gid, rdev, blksize, ino, size, blocks);

  this.atimeMs = atimeNs / kNsPerMsBigInt;
  this.mtimeMs = mtimeNs / kNsPerMsBigInt;
  this.ctimeMs = ctimeNs / kNsPerMsBigInt;
  this.birthtimeMs = birthtimeNs / kNsPerMsBigInt;
  this.atimeNs = atimeNs;
  this.mtimeNs = mtimeNs;
  this.ctimeNs = ctimeNs;
  this.birthtimeNs = birthtimeNs;
}

Object.setPrototypeOf(BigIntStats.prototype, StatsBase.prototype);
Object.setPrototypeOf(BigIntStats, StatsBase);
Object.defineProperties(BigIntStats.prototype, lazyDateFields);
Object.defineProperties(BigIntStats.prototype, lazyTemporalBigIntFields);

BigIntStats.prototype._checkModeProperty = function (property) {
  return (this.mode & BigInt(S_IFMT)) === BigInt(property);
};

function Stats(dev, mode, nlink, uid, gid, rdev, blksize, ino, size, blocks, atimeS, atimeNs, mtimeS, mtimeNs, ctimeS, ctimeNs, birthtimeS, birthtimeNs) {
  StatsBase.call(this, dev, mode, nlink, uid, gid, rdev, blksize, ino, size, blocks);
  this.atimeMs = msFromTimeSpec(atimeS, atimeNs);
  this.mtimeMs = msFromTimeSpec(mtimeS, mtimeNs);
  this.ctimeMs = msFromTimeSpec(ctimeS, ctimeNs);
  this.birthtimeMs = msFromTimeSpec(birthtimeS, birthtimeNs);
  this[kPartialAtimeNs] = atimeNs;
  this[kPartialMtimeNs] = mtimeNs;
  this[kPartialCtimeNs] = ctimeNs;
  this[kPartialBirthtimeNs] = birthtimeNs;
}

Object.setPrototypeOf(Stats.prototype, StatsBase.prototype);
Object.setPrototypeOf(Stats, StatsBase);
Object.defineProperties(Stats.prototype, lazyDateFields);
Object.defineProperties(Stats.prototype, lazyTemporalFields);

Stats.prototype._checkModeProperty = function (property) {
  return (this.mode & S_IFMT) === property;
};

// `fields` is the eighteen numbers the Stat want is answered with
function getStatsFromBinding(fields, bigint) {
  if (bigint) {
    const big = fields.map(BigInt);
    return new BigIntStats(
      big[0], big[1], big[2], big[3], big[4], big[5], big[6], big[7], big[8], big[9],
      nsFromTimeSpecBigInt(big[10], big[11]),
      nsFromTimeSpecBigInt(big[12], big[13]),
      nsFromTimeSpecBigInt(big[14], big[15]),
      nsFromTimeSpecBigInt(big[16], big[17]),
    );
  }
  return new Stats(...fields);
}

const kType = Symbol(\"type\");

class Dirent {
  constructor(name, type, path) {
    this.name = name;
    this.parentPath = path;
    this[kType] = type;
  }

  isDirectory() {
    return this[kType] === UV_DIRENT_DIR;
  }

  isFile() {
    return this[kType] === UV_DIRENT_FILE;
  }

  isBlockDevice() {
    return this[kType] === UV_DIRENT_BLOCK;
  }

  isCharacterDevice() {
    return this[kType] === UV_DIRENT_CHAR;
  }

  isSymbolicLink() {
    return this[kType] === UV_DIRENT_LINK;
  }

  isFIFO() {
    return this[kType] === UV_DIRENT_FIFO;
  }

  isSocket() {
    return this[kType] === UV_DIRENT_SOCKET;
  }
}

// ---- stat and lstat

// with throwIfNoEntry false, a path that is not there is undefined, not an
// error; for stat that includes a path that runs through a file
function isNoEntry(thrown, follow) {
  return thrown === \"ENOENT\" || (follow && thrown === \"ENOTDIR\");
}

function statSyncOf(follow, syscall, path, options) {
  path = getValidatedPath(path);
  let fields;
  try {
    fields = native.stat(path, follow);
  } catch (thrown) {
    if (options.throwIfNoEntry === false && isNoEntry(thrown, follow)) {
      return undefined;
    }
    throw failed(thrown, syscall, path);
  }
  return getStatsFromBinding(fields, options.bigint);
}

function lstatSync(path, options = { __proto__: null, bigint: false, throwIfNoEntry: true }) {
  return statSyncOf(false, \"lstat\", path, options);
}

function statSync(path, options = { __proto__: null, bigint: false, throwIfNoEntry: true }) {
  return statSyncOf(true, \"stat\", path, options);
}

function statOf(follow, syscall, path, options, callback) {
  if (typeof options === \"function\") {
    callback = options;
    options = kEmptyObject;
  } else if (options === null || typeof options !== \"object\") {
    options = kEmptyObject;
  } else {
    options = getOptions(options, { bigint: false });
  }
  validateFunction(callback, \"cb\");
  path = getValidatedPath(path);
  if (options.signal !== undefined) {
    validateAbortSignal(options.signal, \"options.signal\");
  }
  if (checkAborted(options.signal, callback)) return;
  native.statLater(path, follow).then(
    (fields) => callback(null, getStatsFromBinding(fields, options.bigint)),
    (thrown) => {
      if (follow && options.throwIfNoEntry === false && isNoEntry(thrown, true)) {
        callback(null, undefined);
      } else {
        callback(failed(thrown, syscall, path));
      }
    },
  );
}

function lstat(path, options = { __proto__: null, bigint: false }, callback) {
  statOf(false, \"lstat\", path, options, callback);
}

function stat(path, options = { __proto__: null, bigint: false, throwIfNoEntry: true }, callback) {
  statOf(true, \"stat\", path, options, callback);
}

async function statPromise(follow, syscall, path, options) {
  validateObject(options, \"options\");
  const { signal } = options;
  if (signal !== undefined) validateAbortSignal(signal, \"options.signal\");
  if (signal?.aborted) throw new AbortError(undefined, { cause: signal.reason });
  path = getValidatedPath(path);
  let fields;
  try {
    fields = await native.statLater(path, follow);
  } catch (thrown) {
    if (follow && options.throwIfNoEntry === false && isNoEntry(thrown, true)) {
      return undefined;
    }
    throw failed(thrown, syscall, path);
  }
  return getStatsFromBinding(fields, options.bigint);
}

// ---- readlink

function readlinkSync(path, options) {
  options = getOptions(options);
  path = getValidatedPath(path);
  try {
    return inEncoding(native.readlink(path), options.encoding);
  } catch (thrown) {
    throw failed(thrown, \"readlink\", path);
  }
}

function readlink(path, options, callback) {
  if (typeof options === \"function\") {
    callback = options;
    options = undefined;
  }
  validateFunction(callback, \"cb\");
  options = getOptions(options);
  path = getValidatedPath(path);
  native.readlinkLater(path).then(
    (target) => callback(null, inEncoding(target, options.encoding)),
    (thrown) => callback(failed(thrown, \"readlink\", path)),
  );
}

// ---- readdir

// the Readdir want is answered with name, kind, name, kind, ...
function listed(flat) {
  const names = [];
  const types = [];
  for (let i = 0; i < flat.length; i += 2) {
    names.push(flat[i]);
    types.push(flat[i + 1]);
  }
  return [names, types];
}

function joined(dir, name) {
  const path = optional(\"node:path\");
  if (path !== undefined) return path.join(dir, name);
  return dir.endsWith(\"/\") ? dir + name : `${dir}/${name}`;
}

function entriesOf(path, names, types, options) {
  const { encoding } = options;
  if (!options.withFileTypes) {
    return names.map((name) => inEncoding(name, encoding));
  }
  return names.map(
    (name, i) => new Dirent(inEncoding(name, encoding), types[i], path),
  );
}

function scandirSync(path) {
  try {
    return listed(native.readdir(path));
  } catch (thrown) {
    throw failed(thrown, \"scandir\", path);
  }
}

async function scandir(path) {
  try {
    return listed(await native.readdirLater(path));
  } catch (thrown) {
    throw failed(thrown, \"scandir\", path);
  }
}

// a symlink counts as a directory to walk into when it leads to one
function leadsToDirectorySync(path) {
  try {
    return (native.stat(path, true)[1] & S_IFMT) === S_IFDIR;
  } catch (thrown) {
    if (typeof thrown === \"string\") return false;
    throw thrown;
  }
}

async function leadsToDirectory(path) {
  try {
    return ((await native.statLater(path, true))[1] & S_IFMT) === S_IFDIR;
  } catch (thrown) {
    if (typeof thrown === \"string\") return false;
    throw thrown;
  }
}

// one directory's entries into the results, and its sub-directories onto the
// list still to visit
function collect(walk, i, names, types, options, isDirectoryLink) {
  const dir = walk.dirs[i];
  const prefix = walk.prefixes[i];
  const pending = [];
  for (let k = 0; k < names.length; k++) {
    const name = names[k];
    const type = types[k];
    const relative = prefix === \"\" ? name : `${prefix}/${name}`;
    walk.results.push(
      options.withFileTypes
        ? new Dirent(inEncoding(name, options.encoding), type, dir)
        : inEncoding(relative, options.encoding),
    );
    if (type === UV_DIRENT_DIR) {
      pending.push([joined(dir, name), relative, true]);
    } else if (type === UV_DIRENT_LINK) {
      pending.push([joined(dir, name), relative, isDirectoryLink(joined(dir, name))]);
    }
  }
  return pending;
}

function readdirSyncRecursive(basePath, options) {
  const walk = { results: [], dirs: [basePath], prefixes: [\"\"] };
  for (let i = 0; i < walk.dirs.length; i++) {
    const [names, types] = scandirSync(walk.dirs[i]);
    for (const [dir, prefix, isDirectory] of collect(walk, i, names, types, options, leadsToDirectorySync)) {
      if (isDirectory) {
        walk.dirs.push(dir);
        walk.prefixes.push(prefix);
      }
    }
  }
  return walk.results;
}

async function readdirRecursive(basePath, options) {
  const walk = { results: [], dirs: [basePath], prefixes: [\"\"] };
  for (let i = 0; i < walk.dirs.length; i++) {
    const [names, types] = await scandir(walk.dirs[i]);
    for (const [dir, prefix, isDirectory] of collect(walk, i, names, types, options, leadsToDirectory)) {
      if (await isDirectory) {
        walk.dirs.push(dir);
        walk.prefixes.push(prefix);
      }
    }
  }
  return walk.results;
}

function readdirSync(path, options) {
  options = getOptions(options);
  path = getValidatedPath(path);
  if (options.recursive != null) {
    validateBoolean(options.recursive, \"options.recursive\");
  }
  if (options.recursive) return readdirSyncRecursive(path, options);
  const [names, types] = scandirSync(path);
  return entriesOf(path, names, types, options);
}

async function readdirPromise(path, options) {
  options = getOptions(options);
  path = getValidatedPath(path);
  if (options.recursive != null) {
    validateBoolean(options.recursive, \"options.recursive\");
  }
  if (options.recursive) return readdirRecursive(path, options);
  const [names, types] = await scandir(path);
  return entriesOf(path, names, types, options);
}

function readdir(path, options, callback) {
  if (typeof options === \"function\") {
    callback = options;
    options = undefined;
  }
  validateFunction(callback, \"cb\");
  options = getOptions(options);
  path = getValidatedPath(path);
  if (options.recursive != null) {
    validateBoolean(options.recursive, \"options.recursive\");
  }
  readdirPromise(path, options).then(
    (entries) => callback(null, entries),
    (error) => callback(error),
  );
}

// ---- readFile

// only reading is here, so only the flags that read
function checkReadFlag(flag) {
  if (flag === undefined || flag === null || flag === \"r\" || flag === \"rs\" || flag === \"sr\") {
    return;
  }
  if (typeof flag === \"number\") {
    throw new codes.ERR_METHOD_NOT_IMPLEMENTED(\"readFile with numeric flags\");
  }
  if (![\"r+\", \"rs+\", \"sr+\", \"w\", \"wx\", \"xw\", \"w+\", \"wx+\", \"xw+\", \"a\", \"ax\", \"xa\", \"as\", \"sa\", \"a+\", \"ax+\", \"xa+\", \"as+\", \"sa+\"].includes(flag)) {
    throw invalidArgValue(\"flags\", flag);
  }
  throw new codes.ERR_METHOD_NOT_IMPLEMENTED(`readFile with the flag '${flag}'`);
}

function readTarget(path) {
  if (typeof path === \"number\") {
    throw new codes.ERR_METHOD_NOT_IMPLEMENTED(\"readFile from a file descriptor\");
  }
  return getValidatedPath(path);
}

// node opens, then reads: a missing file fails at open, a directory at
// read. Its sync read names no path in that error; the others do
function readFailed(thrown, path, namesPath) {
  if (thrown === \"EISDIR\") {
    return systemError(\"EISDIR\", \"read\", namesPath ? path : undefined);
  }
  return failed(thrown, \"open\", path);
}

function fileContent(bytes, options) {
  const buffer = Buffer.from(bytes);
  return options.encoding ? buffer.toString(options.encoding) : buffer;
}

function readFileSync(path, options) {
  options = getOptions(options, { flag: \"r\" });
  path = readTarget(path);
  checkReadFlag(options.flag);
  let bytes;
  try {
    bytes = native.readFile(path);
  } catch (thrown) {
    throw readFailed(thrown, path, false);
  }
  return fileContent(bytes, options);
}

function readFile(path, options, callback) {
  callback ||= options;
  validateFunction(callback, \"cb\");
  options = getOptions(options, { flag: \"r\" });
  if (checkAborted(options.signal, callback)) return;
  checkReadFlag(options.flag);
  path = readTarget(path);
  native.readFileLater(path).then(
    (bytes) => {
      if (checkAborted(options.signal, callback)) return;
      let content;
      try {
        content = fileContent(bytes, options);
      } catch (error) {
        callback(error);
        return;
      }
      callback(null, content);
    },
    (thrown) => callback(readFailed(thrown, path, true)),
  );
}

async function readFilePromise(path, options) {
  options = getOptions(options, { flag: \"r\" });
  const { signal } = options;
  if (signal?.aborted) throw new AbortError(undefined, { cause: signal.reason });
  checkReadFlag(options.flag);
  path = readTarget(path);
  let bytes;
  try {
    bytes = await native.readFileLater(path);
  } catch (thrown) {
    throw readFailed(thrown, path, true);
  }
  if (signal?.aborted) throw new AbortError(undefined, { cause: signal.reason });
  return fileContent(bytes, options);
}

const promises = {
  async realpath(path, options) {
    options = getOptions(options);
    path = getValidatedPath(path);
    try {
      return inEncoding(await native.realpathLater(path), options.encoding);
    } catch (thrown) {
      throw failed(thrown, \"realpath\", path);
    }
  },
  async access(path, mode = F_OK) {
    path = getValidatedPath(path);
    mode = getValidMode(mode);
    try {
      await native.accessLater(path, mode);
    } catch (thrown) {
      throw failed(thrown, \"access\", path);
    }
  },
  stat(path, options = { bigint: false }) {
    return statPromise(true, \"stat\", path, options);
  },
  lstat(path, options = { bigint: false }) {
    return statPromise(false, \"lstat\", path, options);
  },
  async readlink(path, options) {
    options = getOptions(options);
    path = getValidatedPath(path, \"oldPath\");
    try {
      return inEncoding(await native.readlinkLater(path), options.encoding);
    } catch (thrown) {
      throw failed(thrown, \"readlink\", path);
    }
  },
  readdir: readdirPromise,
  readFile: readFilePromise,
  constants,
};

module.exports = {
  access,
  accessSync,
  exists,
  existsSync,
  realpath,
  realpathSync,
  stat,
  statSync,
  lstat,
  lstatSync,
  readlink,
  readlinkSync,
  readdir,
  readdirSync,
  readFile,
  readFileSync,
  Stats,
  Dirent,
  constants,
  promises,
  F_OK,
  R_OK,
  W_OK,
  X_OK,
};
"

pub const process = "\"use strict\";

const { facts, write, now, getContext, setContext } = native;
const { invalidArgType, systemError, validateFunction } = require(\"spark:errors\");
const loop = require(\"spark:loop\");
const EventEmitter = require(\"node:events\");

const given = facts();
const startedAt = now();

function nextTick(callback, ...args) {
  validateFunction(callback, \"callback\");
  const frame = getContext();
  loop.nextTick(() => {
    const prior = setContext(frame);
    try {
      Reflect.apply(callback, undefined, args);
    } finally {
      setContext(prior);
    }
  });
}

function stream(fd, isTTY) {
  const out = {
    fd,
    write(chunk, encoding, callback) {
      if (typeof encoding === \"function\") {
        callback = encoding;
      }
      if (typeof chunk !== \"string\") {
        throw invalidArgType(
          \"chunk\",
          [\"string\", \"Buffer\", \"TypedArray\", \"DataView\"],
          chunk,
        );
      }
      let error = null;
      try {
        write(fd, chunk);
      } catch (thrown) {
        if (typeof thrown !== \"string\") throw thrown;
        error = systemError(thrown, \"write\");
      }
      if (typeof callback === \"function\") {
        nextTick(callback, error);
      } else if (error !== null) {
        throw error;
      }
      return true;
    },
  };
  if (isTTY) out.isTTY = true;
  return out;
}

// node's process is an EventEmitter two steps up: its own prototype is an
// object named process, and that one's prototype is EventEmitter's
function process() {}
Object.setPrototypeOf(process.prototype, EventEmitter.prototype);

module.exports = Object.assign(Object.create(process.prototype), {
  env: given.env,
  argv: given.argv,
  platform: given.platform,
  arch: given.arch,
  pid: given.pid,
  version: given.version,
  versions: given.versions,
  cwd() {
    return given.cwd;
  },
  // seconds since this module was first loaded
  uptime() {
    return (now() - startedAt) / 1000;
  },
  nextTick,
  stdout: stream(1, given.stdoutIsTTY),
  stderr: stream(2, given.stderrIsTTY),
});

EventEmitter.call(module.exports);
Object.defineProperty(module.exports, Symbol.toStringTag, {
  enumerable: false,
  writable: true,
  configurable: false,
  value: \"process\",
});
"

pub const os = "\"use strict\";

const given = native.facts();

// what node reports for os.constants on each platform; the numbers are part
// of that platform's conventions, nothing here is read from a machine
const constantsFor = {
  linux: {
    __proto__: null,
    UV_UDP_REUSEADDR: 4,
    dlopen: {
      __proto__: null,
      RTLD_LAZY: 1,
      RTLD_NOW: 2,
      RTLD_GLOBAL: 256,
      RTLD_LOCAL: 0,
      RTLD_DEEPBIND: 8,
    },
    errno: {
      __proto__: null,
      E2BIG: 7,
      EACCES: 13,
      EADDRINUSE: 98,
      EADDRNOTAVAIL: 99,
      EAFNOSUPPORT: 97,
      EAGAIN: 11,
      EALREADY: 114,
      EBADF: 9,
      EBADMSG: 74,
      EBUSY: 16,
      ECANCELED: 125,
      ECHILD: 10,
      ECONNABORTED: 103,
      ECONNREFUSED: 111,
      ECONNRESET: 104,
      EDEADLK: 35,
      EDESTADDRREQ: 89,
      EDOM: 33,
      EDQUOT: 122,
      EEXIST: 17,
      EFAULT: 14,
      EFBIG: 27,
      EHOSTUNREACH: 113,
      EIDRM: 43,
      EILSEQ: 84,
      EINPROGRESS: 115,
      EINTR: 4,
      EINVAL: 22,
      EIO: 5,
      EISCONN: 106,
      EISDIR: 21,
      ELOOP: 40,
      EMFILE: 24,
      EMLINK: 31,
      EMSGSIZE: 90,
      EMULTIHOP: 72,
      ENAMETOOLONG: 36,
      ENETDOWN: 100,
      ENETRESET: 102,
      ENETUNREACH: 101,
      ENFILE: 23,
      ENOBUFS: 105,
      ENODATA: 61,
      ENODEV: 19,
      ENOENT: 2,
      ENOEXEC: 8,
      ENOLCK: 37,
      ENOLINK: 67,
      ENOMEM: 12,
      ENOMSG: 42,
      ENOPROTOOPT: 92,
      ENOSPC: 28,
      ENOSR: 63,
      ENOSTR: 60,
      ENOSYS: 38,
      ENOTCONN: 107,
      ENOTDIR: 20,
      ENOTEMPTY: 39,
      ENOTSOCK: 88,
      ENOTSUP: 95,
      ENOTTY: 25,
      ENXIO: 6,
      EOPNOTSUPP: 95,
      EOVERFLOW: 75,
      EPERM: 1,
      EPIPE: 32,
      EPROTO: 71,
      EPROTONOSUPPORT: 93,
      EPROTOTYPE: 91,
      ERANGE: 34,
      EROFS: 30,
      ESPIPE: 29,
      ESRCH: 3,
      ESTALE: 116,
      ETIME: 62,
      ETIMEDOUT: 110,
      ETXTBSY: 26,
      EWOULDBLOCK: 11,
      EXDEV: 18,
    },
    signals: {
      __proto__: null,
      SIGHUP: 1,
      SIGINT: 2,
      SIGQUIT: 3,
      SIGILL: 4,
      SIGTRAP: 5,
      SIGABRT: 6,
      SIGIOT: 6,
      SIGBUS: 7,
      SIGFPE: 8,
      SIGKILL: 9,
      SIGUSR1: 10,
      SIGSEGV: 11,
      SIGUSR2: 12,
      SIGPIPE: 13,
      SIGALRM: 14,
      SIGTERM: 15,
      SIGCHLD: 17,
      SIGSTKFLT: 16,
      SIGCONT: 18,
      SIGSTOP: 19,
      SIGTSTP: 20,
      SIGTTIN: 21,
      SIGTTOU: 22,
      SIGURG: 23,
      SIGXCPU: 24,
      SIGXFSZ: 25,
      SIGVTALRM: 26,
      SIGPROF: 27,
      SIGWINCH: 28,
      SIGIO: 29,
      SIGPOLL: 29,
      SIGPWR: 30,
      SIGSYS: 31,
    },
    priority: {
      __proto__: null,
      PRIORITY_LOW: 19,
      PRIORITY_BELOW_NORMAL: 10,
      PRIORITY_NORMAL: 0,
      PRIORITY_ABOVE_NORMAL: -7,
      PRIORITY_HIGH: -14,
      PRIORITY_HIGHEST: -20,
    },
  },
  darwin: {
    __proto__: null,
    UV_UDP_REUSEADDR: 4,
    dlopen: {
      __proto__: null,
      RTLD_LAZY: 1,
      RTLD_NOW: 2,
      RTLD_GLOBAL: 8,
      RTLD_LOCAL: 4,
    },
    errno: {
      __proto__: null,
      E2BIG: 7,
      EACCES: 13,
      EADDRINUSE: 48,
      EADDRNOTAVAIL: 49,
      EAFNOSUPPORT: 47,
      EAGAIN: 35,
      EALREADY: 37,
      EBADF: 9,
      EBADMSG: 94,
      EBUSY: 16,
      ECANCELED: 89,
      ECHILD: 10,
      ECONNABORTED: 53,
      ECONNREFUSED: 61,
      ECONNRESET: 54,
      EDEADLK: 11,
      EDESTADDRREQ: 39,
      EDOM: 33,
      EDQUOT: 69,
      EEXIST: 17,
      EFAULT: 14,
      EFBIG: 27,
      EHOSTUNREACH: 65,
      EIDRM: 90,
      EILSEQ: 92,
      EINPROGRESS: 36,
      EINTR: 4,
      EINVAL: 22,
      EIO: 5,
      EISCONN: 56,
      EISDIR: 21,
      ELOOP: 62,
      EMFILE: 24,
      EMLINK: 31,
      EMSGSIZE: 40,
      EMULTIHOP: 95,
      ENAMETOOLONG: 63,
      ENETDOWN: 50,
      ENETRESET: 52,
      ENETUNREACH: 51,
      ENFILE: 23,
      ENOBUFS: 55,
      ENODATA: 96,
      ENODEV: 19,
      ENOENT: 2,
      ENOEXEC: 8,
      ENOLCK: 77,
      ENOLINK: 97,
      ENOMEM: 12,
      ENOMSG: 91,
      ENOPROTOOPT: 42,
      ENOSPC: 28,
      ENOSR: 98,
      ENOSTR: 99,
      ENOSYS: 78,
      ENOTCONN: 57,
      ENOTDIR: 20,
      ENOTEMPTY: 66,
      ENOTSOCK: 38,
      ENOTSUP: 45,
      ENOTTY: 25,
      ENXIO: 6,
      EOPNOTSUPP: 102,
      EOVERFLOW: 84,
      EPERM: 1,
      EPIPE: 32,
      EPROTO: 100,
      EPROTONOSUPPORT: 43,
      EPROTOTYPE: 41,
      ERANGE: 34,
      EROFS: 30,
      ESPIPE: 29,
      ESRCH: 3,
      ESTALE: 70,
      ETIME: 101,
      ETIMEDOUT: 60,
      ETXTBSY: 26,
      EWOULDBLOCK: 35,
      EXDEV: 18,
    },
    signals: {
      __proto__: null,
      SIGHUP: 1,
      SIGINT: 2,
      SIGQUIT: 3,
      SIGILL: 4,
      SIGTRAP: 5,
      SIGABRT: 6,
      SIGIOT: 6,
      SIGBUS: 10,
      SIGFPE: 8,
      SIGKILL: 9,
      SIGUSR1: 30,
      SIGSEGV: 11,
      SIGUSR2: 31,
      SIGPIPE: 13,
      SIGALRM: 14,
      SIGTERM: 15,
      SIGCHLD: 20,
      SIGCONT: 19,
      SIGSTOP: 17,
      SIGTSTP: 18,
      SIGTTIN: 21,
      SIGTTOU: 22,
      SIGURG: 16,
      SIGXCPU: 24,
      SIGXFSZ: 25,
      SIGVTALRM: 26,
      SIGPROF: 27,
      SIGWINCH: 28,
      SIGIO: 23,
      SIGINFO: 29,
      SIGSYS: 12,
    },
    priority: {
      __proto__: null,
      PRIORITY_LOW: 19,
      PRIORITY_BELOW_NORMAL: 10,
      PRIORITY_NORMAL: 0,
      PRIORITY_ABOVE_NORMAL: -7,
      PRIORITY_HIGH: -14,
      PRIORITY_HIGHEST: -20,
    },
  },
};

const constants = constantsFor[given.platform];

// node lets these be used where a string is wanted: `${os.platform}`
function fact(value) {
  const read = () => value;
  read[Symbol.toPrimitive] = read;
  return read;
}

module.exports = {
  arch: fact(given.arch),
  availableParallelism: fact(given.parallelism),
  endianness: fact(given.endianness),
  homedir: fact(given.homedir),
  hostname: fact(given.hostname),
  platform: fact(given.platform),
  release: fact(given.release),
  tmpdir: fact(given.tmpdir),
  type: fact(given.platform === \"darwin\" ? \"Darwin\" : \"Linux\"),
  version: fact(given.version),
  machine: fact(given.machine),
};

Object.defineProperties(module.exports, {
  constants: {
    configurable: false,
    enumerable: true,
    value: constants,
  },
  EOL: {
    configurable: true,
    enumerable: true,
    writable: false,
    value: \"\\n\",
  },
  devNull: {
    configurable: true,
    enumerable: true,
    writable: false,
    value: \"/dev/null\",
  },
});
"

pub const encoding = "\"use strict\";

const {
  typeError,
  rangeError,
  invalidArgType,
  validateString,
  inspect,
} = require(\"spark:errors\");

const utf8Labels = new Set([
  \"unicode-1-1-utf-8\",
  \"unicode11utf8\",
  \"unicode20utf8\",
  \"utf8\",
  \"utf-8\",
  \"x-unicode20utf8\",
]);

function trimAsciiWhitespace(label) {
  let s = 0;
  let e = label.length;
  while (
    s < e &&
    (label[s] === \"\\u0009\" ||
      label[s] === \"\\u000a\" ||
      label[s] === \"\\u000c\" ||
      label[s] === \"\\u000d\" ||
      label[s] === \" \")
  ) {
    s++;
  }
  while (
    e > s &&
    (label[e - 1] === \"\\u0009\" ||
      label[e - 1] === \"\\u000a\" ||
      label[e - 1] === \"\\u000c\" ||
      label[e - 1] === \"\\u000d\" ||
      label[e - 1] === \" \")
  ) {
    e--;
  }
  return label.slice(s, e);
}

function invalidThis(type) {
  return typeError(\"ERR_INVALID_THIS\", `Value of \"this\" must be of type ${type}`);
}

const kInspect = Symbol.for(\"nodejs.util.inspect.custom\");

function getConstructorOf(obj) {
  while (obj) {
    const descriptor = Object.getOwnPropertyDescriptor(obj, \"constructor\");
    if (
      descriptor !== undefined &&
      typeof descriptor.value === \"function\" &&
      descriptor.value.name !== \"\"
    ) {
      return descriptor.value;
    }
    obj = Object.getPrototypeOf(obj);
  }
  return null;
}

class TextEncoder {
  #encoding = \"utf-8\";

  [kInspect](depth, opts) {
    if (typeof depth === \"number\" && depth < 0) return this;
    const ctor = getConstructorOf(this);
    const obj = {
      __proto__: {
        constructor: ctor === null ? TextEncoder : ctor,
      },
    };
    obj.encoding = this.#encoding;
    return inspect(obj, opts);
  }

  #encode(input) {
    return new Uint8Array(native.encode(`${input}`));
  }

  // read counts string positions as this engine indexes them
  #encodeInto(src, dest) {
    const whole = new Uint8Array(native.encode(src));
    if (whole.length <= dest.length) {
      dest.set(whole);
      return { read: src.length, written: whole.length };
    }
    let read = 0;
    let written = 0;
    for (const character of src) {
      const size = native.encode(character).byteLength;
      if (written + size > dest.length) break;
      written += size;
      read += character.length;
    }
    dest.set(whole.subarray(0, written));
    return { read, written };
  }

  get encoding() {
    return this.#encoding;
  }

  encode(input = \"\") {
    return this.#encode(input);
  }

  encodeInto(src, dest) {
    validateString(src, \"src\");
    if (!dest || !(dest instanceof Uint8Array)) {
      throw invalidArgType(\"dest\", \"Uint8Array\", dest);
    }
    return this.#encodeInto(src, dest);
  }
}

const empty = new Uint8Array(0);

const kDecoder = Symbol(\"decoder\");

class TextDecoder {
  // node marks its decoders this way, and inspect shows it
  [kDecoder] = true;
  #fatal;
  #ignoreBOM;
  #bomSeen = false;
  #pending = empty;

  constructor(encoding = \"utf-8\", options = {}) {
    encoding = `${encoding}`;
    if (options !== null && typeof options !== \"object\" && typeof options !== \"function\") {
      throw invalidArgType(\"options\", \"Object\", options);
    }
    if (
      !utf8Labels.has(encoding) &&
      !utf8Labels.has(trimAsciiWhitespace(encoding.toLowerCase()))
    ) {
      throw rangeError(
        \"ERR_ENCODING_NOT_SUPPORTED\",
        `The \"${encoding}\" encoding is not supported`,
      );
    }
    this.#fatal = Boolean(options?.fatal);
    this.#ignoreBOM = Boolean(options?.ignoreBOM);
  }

  static #check(decoder) {
    if (decoder == null || !(#fatal in decoder)) throw invalidThis(\"TextDecoder\");
  }

  get encoding() {
    TextDecoder.#check(this);
    return \"utf-8\";
  }

  get fatal() {
    TextDecoder.#check(this);
    return this.#fatal;
  }

  [kInspect](depth) {
    TextDecoder.#check(this);
    if (typeof depth === \"number\" && depth < 0) return this;
    const constructor = getConstructorOf(this) || TextDecoder;
    const obj = { __proto__: { constructor } };
    obj.encoding = this.encoding;
    obj.fatal = this.fatal;
    obj.ignoreBOM = this.ignoreBOM;
    return `${constructor.name} ${inspect(obj)}`;
  }

  get ignoreBOM() {
    TextDecoder.#check(this);
    return this.#ignoreBOM;
  }

  decode(input = empty, options = {}) {
    TextDecoder.#check(this);
    let bytes;
    if (input instanceof ArrayBuffer || input instanceof SharedArrayBuffer) {
      bytes = new Uint8Array(input);
    } else if (ArrayBuffer.isView(input)) {
      bytes = new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
    } else {
      throw invalidArgType(\"input\", [\"ArrayBuffer\", \"ArrayBufferView\"], input);
    }
    if (options !== null && typeof options !== \"object\" && typeof options !== \"function\") {
      throw invalidArgType(\"options\", \"Object\", options);
    }
    const stream = Boolean(options?.stream);

    if (this.#pending.length > 0) {
      const joined = new Uint8Array(this.#pending.length + bytes.length);
      joined.set(this.#pending);
      joined.set(bytes, this.#pending.length);
      bytes = joined;
      this.#pending = empty;
    }

    const decoded = native.decode(bytes, this.#fatal, stream);
    if (decoded === undefined) {
      this.#bomSeen = false;
      const error = typeError(
        \"ERR_ENCODING_INVALID_ENCODED_DATA\",
        \"The encoded data was not valid for encoding utf-8\",
      );
      throw error;
    }

    let text = decoded[0];
    const leftover = decoded[1];
    if (leftover > 0) {
      this.#pending = bytes.slice(bytes.length - leftover);
    }

    if (!this.#bomSeen && text.length > 0) {
      if (!this.#ignoreBOM && text[0] === \"﻿\") text = text.slice(1);
      this.#bomSeen = true;
    }
    if (!stream) this.#bomSeen = false;
    return text;
  }
}

module.exports = { TextEncoder, TextDecoder };
"

pub const perf_hooks = "\"use strict\";

const {
  error,
  typeError,
  invalidArgType,
  invalidArgValue,
  validateObject,
  validateString,
} = require(\"spark:errors\");
const { DOMException } = require(\"spark:domexception\");
const { now, wallClock } = native;

const startedAt = now();
const timeOrigin = wallClock();
const sinceStart = () => now() - startedAt;

const kSkipThrow = Symbol(\"skipThrow\");
const kName = Symbol(\"name\");
const kEntryType = Symbol(\"entryType\");
const kStartTime = Symbol(\"startTime\");
const kDuration = Symbol(\"duration\");
const kDetail = Symbol(\"detail\");
const kBrand = Symbol(\"performance\");

function illegalConstructor() {
  return typeError(\"ERR_ILLEGAL_CONSTRUCTOR\", \"Illegal constructor\");
}

function missingArgs(name) {
  return typeError(
    \"ERR_MISSING_ARGS\",
    `The \"${name}\" argument must be specified`,
  );
}

function checkField(value, key, type) {
  if (typeof value !== \"object\" || value === null || !(key in value)) {
    throw typeError(
      \"ERR_INVALID_THIS\",
      `Value of \"this\" must be of type ${type}`,
    );
  }
}

const enumerable = { enumerable: true };

class PerformanceEntry {
  constructor(
    skipThrowSymbol = undefined,
    name = undefined,
    type = undefined,
    start = undefined,
    duration = undefined,
  ) {
    if (skipThrowSymbol !== kSkipThrow) throw illegalConstructor();
    this[kName] = name;
    this[kEntryType] = type;
    this[kStartTime] = start;
    this[kDuration] = duration;
  }

  get name() {
    checkField(this, kName, \"PerformanceEntry\");
    return this[kName];
  }

  get entryType() {
    checkField(this, kEntryType, \"PerformanceEntry\");
    return this[kEntryType];
  }

  get startTime() {
    checkField(this, kStartTime, \"PerformanceEntry\");
    return this[kStartTime];
  }

  get duration() {
    checkField(this, kDuration, \"PerformanceEntry\");
    return this[kDuration];
  }

  toJSON() {
    checkField(this, kName, \"PerformanceEntry\");
    return {
      name: this[kName],
      entryType: this[kEntryType],
      startTime: this[kStartTime],
      duration: this[kDuration],
    };
  }
}

Object.defineProperties(PerformanceEntry.prototype, {
  name: enumerable,
  entryType: enumerable,
  startTime: enumerable,
  duration: enumerable,
  toJSON: enumerable,
});

const markTimings = new Map();
const reservedMarkNames = new Set([
  \"nodeStart\",
  \"v8Start\",
  \"environment\",
  \"loopStart\",
  \"loopExit\",
  \"bootstrapComplete\",
]);

let markEntryBuffer = [];
let measureEntryBuffer = [];

function invalidTimestamp(value) {
  return typeError(
    \"ERR_PERFORMANCE_INVALID_TIMESTAMP\",
    `${value} is not a valid timestamp`,
  );
}

function getMark(name) {
  if (name === undefined) return;
  if (typeof name === \"number\") {
    if (name < 0) throw invalidTimestamp(name);
    return name;
  }
  name = `${name}`;
  const ts = markTimings.get(name);
  if (ts === undefined) {
    throw new DOMException(
      `The \"${name}\" performance mark has not been set`,
      \"SyntaxError\",
    );
  }
  return ts;
}

// a detail is copied, which needs the embedder to have given structuredClone
function cloneDetail(detail) {
  if (detail == null) return null;
  if (typeof globalThis.structuredClone !== \"function\") {
    throw error(
      \"ERR_METHOD_NOT_IMPLEMENTED\",
      \"The structuredClone method is not implemented\",
    );
  }
  return globalThis.structuredClone(detail);
}

class PerformanceMark extends PerformanceEntry {
  constructor(name, options = undefined) {
    if (arguments.length === 0) throw missingArgs(\"name\");
    name = `${name}`;
    if (reservedMarkNames.has(name)) throw invalidArgValue(\"name\", name);
    if (options != null) validateObject(options, \"options\");
    const startTime = options?.startTime ?? sinceStart();
    if (typeof startTime !== \"number\") {
      throw invalidArgType(\"startTime\", \"number\", startTime);
    }
    if (startTime < 0) throw invalidTimestamp(startTime);
    markTimings.set(name, startTime);
    const detail = cloneDetail(options?.detail);

    super(kSkipThrow, name, \"mark\", startTime, 0);
    this[kDetail] = detail;
  }

  get detail() {
    checkField(this, kDetail, \"PerformanceMark\");
    return this[kDetail];
  }

  toJSON() {
    return {
      name: this.name,
      entryType: this.entryType,
      startTime: this.startTime,
      duration: this.duration,
      detail: this[kDetail],
    };
  }
}

Object.defineProperties(PerformanceMark.prototype, {
  detail: enumerable,
  [Symbol.toStringTag]: { configurable: true, value: \"PerformanceMark\" },
});

class PerformanceMeasure extends PerformanceEntry {
  constructor(
    skipThrowSymbol = undefined,
    name = undefined,
    type = undefined,
    start = undefined,
    duration = undefined,
  ) {
    if (skipThrowSymbol !== kSkipThrow) throw illegalConstructor();
    super(skipThrowSymbol, name, type, start, duration);
  }

  get detail() {
    checkField(this, kDetail, \"PerformanceMeasure\");
    return this[kDetail];
  }

  toJSON() {
    return {
      name: this.name,
      entryType: this.entryType,
      startTime: this.startTime,
      duration: this.duration,
      detail: this[kDetail],
    };
  }
}

Object.defineProperties(PerformanceMeasure.prototype, {
  detail: enumerable,
  [Symbol.toStringTag]: { configurable: true, value: \"PerformanceMeasure\" },
});

function invalidMeasureOptions(message) {
  return typeError(\"ERR_PERFORMANCE_MEASURE_INVALID_OPTIONS\", message);
}

function calculateStartDuration(startOrMeasureOptions, endMark) {
  startOrMeasureOptions ??= 0;
  let start;
  let end;
  let duration;
  let optionsValid = false;
  if (typeof startOrMeasureOptions === \"object\") {
    ({ start, end, duration } = startOrMeasureOptions);
    optionsValid = start !== undefined || end !== undefined;
  }
  if (optionsValid) {
    if (endMark !== undefined) {
      throw invalidMeasureOptions(\"endMark must not be specified\");
    }
    if (start === undefined && end === undefined) {
      throw invalidMeasureOptions(
        \"One of options.start or options.end is required\",
      );
    }
    if (start !== undefined && end !== undefined && duration !== undefined) {
      throw invalidMeasureOptions(
        \"Must not have options.start, options.end, and \" +
          \"options.duration specified\",
      );
    }
  }

  if (endMark !== undefined) {
    end = getMark(endMark);
  } else if (optionsValid && end !== undefined) {
    end = getMark(end);
  } else if (optionsValid && start !== undefined && duration !== undefined) {
    end = getMark(start) + getMark(duration);
  } else {
    end = sinceStart();
  }

  if (typeof startOrMeasureOptions === \"string\") {
    start = getMark(startOrMeasureOptions);
  } else if (optionsValid && start !== undefined) {
    start = getMark(start);
  } else if (optionsValid && duration !== undefined && end !== undefined) {
    start = end - getMark(duration);
  } else {
    start = 0;
  }

  duration = end - start;
  return { start, duration };
}

function clearEntriesFromBuffer(type, name) {
  if (type === \"mark\") {
    markEntryBuffer =
      name === undefined
        ? []
        : markEntryBuffer.filter((entry) => entry.name !== name);
  } else if (type === \"measure\") {
    measureEntryBuffer =
      name === undefined
        ? []
        : measureEntryBuffer.filter((entry) => entry.name !== name);
  }
}

function filterBufferByNameAndType(name, type) {
  let bufferList;
  if (type === \"mark\") {
    bufferList = markEntryBuffer;
  } else if (type === \"measure\") {
    bufferList = measureEntryBuffer;
  } else if (type !== undefined) {
    return [];
  } else {
    bufferList = [...markEntryBuffer, ...measureEntryBuffer];
  }
  if (name !== undefined) {
    bufferList = bufferList.filter((entry) => entry.name === name);
  } else if (type !== undefined) {
    bufferList = bufferList.slice();
  }
  return bufferList.sort((first, second) => first.startTime - second.startTime);
}

let constructing = false;

class Performance {
  constructor() {
    if (!constructing) throw illegalConstructor();
    this[kBrand] = true;
  }

  clearMarks(name = undefined) {
    checkField(this, kBrand, \"Performance\");
    if (name !== undefined) {
      name = `${name}`;
      if (reservedMarkNames.has(name)) throw invalidArgValue(\"name\", name);
      markTimings.delete(name);
    } else {
      markTimings.clear();
    }
    clearEntriesFromBuffer(\"mark\", name);
  }

  clearMeasures(name = undefined) {
    checkField(this, kBrand, \"Performance\");
    if (name !== undefined) name = `${name}`;
    clearEntriesFromBuffer(\"measure\", name);
  }

  getEntries() {
    checkField(this, kBrand, \"Performance\");
    return filterBufferByNameAndType();
  }

  getEntriesByName(name, type = undefined) {
    checkField(this, kBrand, \"Performance\");
    if (arguments.length === 0) throw missingArgs(\"name\");
    name = `${name}`;
    if (type !== undefined) type = `${type}`;
    return filterBufferByNameAndType(name, type);
  }

  getEntriesByType(type) {
    checkField(this, kBrand, \"Performance\");
    if (arguments.length === 0) throw missingArgs(\"type\");
    type = `${type}`;
    return filterBufferByNameAndType(undefined, type);
  }

  mark(name, options = {}) {
    checkField(this, kBrand, \"Performance\");
    if (arguments.length === 0) throw missingArgs(\"name\");
    const entry = new PerformanceMark(name, options);
    markEntryBuffer.push(entry);
    return entry;
  }

  measure(name, startOrMeasureOptions = {}, endMark = undefined) {
    checkField(this, kBrand, \"Performance\");
    if (arguments.length === 0) throw missingArgs(\"name\");
    validateString(name, \"name\");
    const { start, duration } = calculateStartDuration(
      startOrMeasureOptions,
      endMark,
    );
    const detail = cloneDetail(startOrMeasureOptions?.detail);
    const entry = new PerformanceMeasure(
      kSkipThrow,
      name,
      \"measure\",
      start,
      duration,
    );
    entry[kDetail] = detail;
    measureEntryBuffer.push(entry);
    return entry;
  }

  // milliseconds since this module was first loaded
  now() {
    checkField(this, kBrand, \"Performance\");
    return sinceStart();
  }

  get timeOrigin() {
    checkField(this, kBrand, \"Performance\");
    return timeOrigin;
  }

  toJSON() {
    checkField(this, kBrand, \"Performance\");
    return { timeOrigin };
  }

  get [Symbol.toStringTag]() {
    return \"Performance\";
  }
}

constructing = true;
const performance = new Performance();
constructing = false;

module.exports = {
  performance,
  PerformanceEntry,
  PerformanceMark,
  PerformanceMeasure,
};
"

pub const fs_promises = "\"use strict\";

module.exports = require(\"node:fs\").promises;
"

pub const global = "\"use strict\";

module.exports = { global: globalThis };
"

pub const domexception = "\"use strict\";

const internals = new WeakMap();
const nameToCode = new Map();
const disusedNames = new Set([
  \"DOMStringSizeError\",
  \"NoDataAllowedError\",
  \"ValidationError\",
]);

function throwInvalidThis(type) {
  const error = new TypeError();
  Object.defineProperties(error, {
    message: {
      value: `Value of \"this\" must be of ${type}`,
      enumerable: false,
      writable: true,
      configurable: true,
    },
    toString: {
      value: function toString() {
        return `${this.name} [ERR_INVALID_THIS]: ${this.message}`;
      },
      enumerable: false,
      writable: true,
      configurable: true,
    },
  });
  error.code = \"ERR_INVALID_THIS\";
  throw error;
}

function internalsOf(exception) {
  const found = internals.get(exception);
  if (found === undefined) throwInvalidThis(\"DOMException\");
  return found;
}

class DOMException {
  constructor(message = \"\", options = \"Error\") {
    const self = new Error();
    Object.setPrototypeOf(self, new.target.prototype);
    if (options && typeof options === \"object\") {
      const { name } = options;
      internals.set(self, { message: `${message}`, name: `${name}` });
      if (\"cause\" in options) {
        Object.defineProperty(self, \"cause\", {
          value: options.cause,
          configurable: true,
          writable: true,
          enumerable: false,
        });
      }
    } else {
      internals.set(self, { message: `${message}`, name: `${options}` });
    }
    return self;
  }

  get name() {
    return internalsOf(this).name;
  }

  get message() {
    return internalsOf(this).message;
  }

  get code() {
    const { name } = internalsOf(this);
    if (disusedNames.has(name)) return 0;
    const code = nameToCode.get(name);
    return code === undefined ? 0 : code;
  }
}

Object.setPrototypeOf(DOMException.prototype, Error.prototype);
Object.defineProperties(DOMException.prototype, {
  [Symbol.toStringTag]: { configurable: true, value: \"DOMException\" },
  name: { enumerable: true, configurable: true },
  message: { enumerable: true, configurable: true },
  code: { enumerable: true, configurable: true },
});

for (const [name, codeName, value] of [
  [\"IndexSizeError\", \"INDEX_SIZE_ERR\", 1],
  [\"DOMStringSizeError\", \"DOMSTRING_SIZE_ERR\", 2],
  [\"HierarchyRequestError\", \"HIERARCHY_REQUEST_ERR\", 3],
  [\"WrongDocumentError\", \"WRONG_DOCUMENT_ERR\", 4],
  [\"InvalidCharacterError\", \"INVALID_CHARACTER_ERR\", 5],
  [\"NoDataAllowedError\", \"NO_DATA_ALLOWED_ERR\", 6],
  [\"NoModificationAllowedError\", \"NO_MODIFICATION_ALLOWED_ERR\", 7],
  [\"NotFoundError\", \"NOT_FOUND_ERR\", 8],
  [\"NotSupportedError\", \"NOT_SUPPORTED_ERR\", 9],
  [\"InUseAttributeError\", \"INUSE_ATTRIBUTE_ERR\", 10],
  [\"InvalidStateError\", \"INVALID_STATE_ERR\", 11],
  [\"SyntaxError\", \"SYNTAX_ERR\", 12],
  [\"InvalidModificationError\", \"INVALID_MODIFICATION_ERR\", 13],
  [\"NamespaceError\", \"NAMESPACE_ERR\", 14],
  [\"InvalidAccessError\", \"INVALID_ACCESS_ERR\", 15],
  [\"ValidationError\", \"VALIDATION_ERR\", 16],
  [\"TypeMismatchError\", \"TYPE_MISMATCH_ERR\", 17],
  [\"SecurityError\", \"SECURITY_ERR\", 18],
  [\"NetworkError\", \"NETWORK_ERR\", 19],
  [\"AbortError\", \"ABORT_ERR\", 20],
  [\"URLMismatchError\", \"URL_MISMATCH_ERR\", 21],
  [\"QuotaExceededError\", \"QUOTA_EXCEEDED_ERR\", 22],
  [\"TimeoutError\", \"TIMEOUT_ERR\", 23],
  [\"InvalidNodeTypeError\", \"INVALID_NODE_TYPE_ERR\", 24],
  [\"DataCloneError\", \"DATA_CLONE_ERR\", 25],
]) {
  const descriptor = { enumerable: true, value };
  Object.defineProperty(DOMException, codeName, descriptor);
  Object.defineProperty(DOMException.prototype, codeName, descriptor);
  nameToCode.set(name, value);
}

class QuotaExceededError extends DOMException {
  #quota;
  #requested;

  constructor(message = \"\", options = {}) {
    super(message, \"QuotaExceededError\");

    let quota = null;
    let requested = null;

    if (options !== null && options !== undefined) {
      if (\"quota\" in options) {
        quota = +options.quota;
        if (!Number.isFinite(quota)) {
          throw new TypeError(
            `Cannot convert options.quota to a double: the value is ${Number.isNaN(quota) ? \"NaN\" : \"Infinity\"}`,
          );
        }
        if (quota < 0) {
          throw new RangeError(\"options.quota must not be negative\");
        }
      }

      if (\"requested\" in options) {
        requested = +options.requested;
        if (!Number.isFinite(requested)) {
          throw new TypeError(
            `Cannot convert options.requested to a double: the value is ${Number.isNaN(requested) ? \"NaN\" : \"Infinity\"}`,
          );
        }
        if (requested < 0) {
          throw new RangeError(\"options.requested must not be negative\");
        }
      }
    }

    if (quota !== null && requested !== null && requested < quota) {
      throw new RangeError(
        \"options.requested must not be less than options.quota\",
      );
    }

    this.#quota = quota;
    this.#requested = requested;
  }

  get quota() {
    if (!(#quota in this)) throwInvalidThis(\"QuotaExceededError\");
    return this.#quota;
  }

  get requested() {
    if (!(#requested in this)) throwInvalidThis(\"QuotaExceededError\");
    return this.#requested;
  }
}

Object.defineProperties(QuotaExceededError.prototype, {
  [Symbol.toStringTag]: { configurable: true, value: \"QuotaExceededError\" },
  quota: { enumerable: true, configurable: true },
  requested: { enumerable: true, configurable: true },
});

module.exports = { DOMException, QuotaExceededError };
"

pub const dom_events = "\"use strict\";

const {
  error,
  typeError,
  rangeError,
  invalidArgType,
  validateObject,
  inspect,
} = require(\"spark:errors\");

const kInspect = Symbol.for(\"nodejs.util.inspect.custom\");

function customInspect(self, obj, depth, options) {
  if (depth < 0) return self;
  const opts = Object.assign({}, options, {
    depth: options.depth === null ? null : options.depth - 1,
  });
  return `${self.constructor.name} ${inspect(obj, opts)}`;
}
const loop = require(\"spark:loop\");
const { DOMException } = require(\"spark:domexception\");
const { setTimeout, clearTimeout } = require(\"node:timers\");

const startedAt = native.now();

const kType = Symbol(\"type\");
const kTarget = Symbol(\"target\");
const kIsBeingDispatched = Symbol(\"isBeingDispatched\");
const kInPassiveListener = Symbol(\"inPassiveListener\");
const kStop = Symbol(\"stop\");
const kDetail = Symbol(\"detail\");
const kTrustEvent = Symbol(\"trustEvent\");
const kEvents = Symbol(\"events\");
const kHandlers = Symbol(\"handlers\");
const kIsEventTarget = Symbol(\"isEventTarget\");
const kWeakHandler = Symbol(\"weakHandler\");
const kResistStopPropagation = Symbol(\"resistStopPropagation\");
const kNewListener = Symbol(\"newListener\");
const kRemoveListener = Symbol(\"removeListener\");
const kRemoveWeakListenerHelper = Symbol(\"removeWeakListenerHelper\");

function missingArgs(...names) {
  const quoted = names.map((name) => `\"${name}\"`);
  const list =
    quoted.length === 1
      ? `${quoted[0]} argument`
      : `${quoted.slice(0, -1).join(\", \")} and ${quoted[quoted.length - 1]} arguments`;
  return typeError(\"ERR_MISSING_ARGS\", `The ${list} must be specified`);
}

function invalidThis(type) {
  return typeError(\"ERR_INVALID_THIS\", `Value of \"this\" must be of type ${type}`);
}

// an error out of a listener never reaches the dispatcher; it is thrown later
function emitUncaughtException(thrown) {
  loop.nextTick(() => {
    throw thrown;
  });
}

function addCatch(promise) {
  const then = promise.then;
  if (typeof then === \"function\") {
    then.call(promise, undefined, emitUncaughtException);
  }
}

const isTrustedSet = new WeakSet();
const isTrusted = Object.getOwnPropertyDescriptor(
  {
    get isTrusted() {
      return isTrustedSet.has(this);
    },
  },
  \"isTrusted\",
).get;

function isEvent(value) {
  return typeof value?.[kType] === \"string\";
}

class Event {
  #cancelable = false;
  #bubbles = false;
  #composed = false;
  #defaultPrevented = false;
  #timestamp = native.now() - startedAt;
  #propagationStopped = false;

  constructor(type, options = undefined) {
    if (arguments.length === 0) throw missingArgs(\"type\");
    if (options != null) validateObject(options, \"options\");
    this.#bubbles = !!options?.bubbles;
    this.#cancelable = !!options?.cancelable;
    this.#composed = !!options?.composed;
    this[kType] = `${type}`;
    if (options?.[kTrustEvent]) isTrustedSet.add(this);
    this[kTarget] = null;
    this[kIsBeingDispatched] = false;
    this[kInPassiveListener] = false;
  }

  initEvent(type, bubbles = false, cancelable = false) {
    if (arguments.length === 0) throw missingArgs(\"type\");
    if (this[kIsBeingDispatched]) return;
    this[kType] = `${type}`;
    this.#bubbles = !!bubbles;
    this.#cancelable = !!cancelable;
  }

  [kInspect](depth, options) {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    const name = this.constructor.name;
    if (depth < 0) return name;
    const opts = Object.assign({}, options, {
      depth: Number.isInteger(options.depth)
        ? options.depth - 1
        : options.depth,
    });
    return `${name} ${inspect(
      {
        type: this[kType],
        defaultPrevented: this.#defaultPrevented,
        cancelable: this.#cancelable,
        timeStamp: this.#timestamp,
      },
      opts,
    )}`;
  }

  stopImmediatePropagation() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    this.stopPropagation();
    this[kStop] = true;
  }

  preventDefault() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    if (!this.#cancelable || this[kInPassiveListener]) return;
    this.#defaultPrevented = true;
  }

  get target() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this[kTarget];
  }

  get currentTarget() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this[kIsBeingDispatched] ? this[kTarget] : null;
  }

  get srcElement() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this[kTarget];
  }

  get type() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this[kType];
  }

  get cancelable() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this.#cancelable;
  }

  get defaultPrevented() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this.#cancelable && this.#defaultPrevented;
  }

  get timeStamp() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this.#timestamp;
  }

  composedPath() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this[kIsBeingDispatched] ? [this[kTarget]] : [];
  }

  get returnValue() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return !this.#cancelable || !this.#defaultPrevented;
  }

  set returnValue(value) {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    if (!value) {
      if (!this.#cancelable || this[kInPassiveListener]) return;
      this.#defaultPrevented = true;
    }
  }

  get bubbles() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this.#bubbles;
  }

  get composed() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this.#composed;
  }

  get eventPhase() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this[kIsBeingDispatched] ? Event.AT_TARGET : Event.NONE;
  }

  get cancelBubble() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    return this.#propagationStopped;
  }

  set cancelBubble(value) {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    if (value) this.#propagationStopped = true;
  }

  stopPropagation() {
    if (!isEvent(this)) throw invalidThis(\"Event\");
    this.#propagationStopped = true;
  }
}

const enumerable = { enumerable: true };

Object.defineProperties(Event.prototype, {
  [Symbol.toStringTag]: {
    writable: false,
    enumerable: false,
    configurable: true,
    value: \"Event\",
  },
  initEvent: enumerable,
  stopImmediatePropagation: enumerable,
  preventDefault: enumerable,
  target: enumerable,
  currentTarget: enumerable,
  srcElement: enumerable,
  type: enumerable,
  cancelable: enumerable,
  defaultPrevented: enumerable,
  timeStamp: enumerable,
  composedPath: enumerable,
  returnValue: enumerable,
  bubbles: enumerable,
  composed: enumerable,
  eventPhase: enumerable,
  cancelBubble: enumerable,
  stopPropagation: enumerable,
  isTrusted: { configurable: false, enumerable: true, get: isTrusted },
});

[\"NONE\", \"CAPTURING_PHASE\", \"AT_TARGET\", \"BUBBLING_PHASE\"].forEach(
  (name, index) => {
    Object.defineProperty(Event, name, {
      writable: false,
      configurable: false,
      enumerable: true,
      value: index,
    });
  },
);

function isCustomEvent(value) {
  return isEvent(value) && value?.[kDetail] !== undefined;
}

class CustomEvent extends Event {
  constructor(type, options = {}) {
    if (arguments.length === 0) throw missingArgs(\"type\");
    super(type, options);
    this[kDetail] = options?.detail ?? null;
  }

  get detail() {
    if (!isCustomEvent(this)) throw invalidThis(\"CustomEvent\");
    return this[kDetail];
  }
}

Object.defineProperties(CustomEvent.prototype, {
  [Symbol.toStringTag]: {
    writable: false,
    enumerable: false,
    configurable: true,
    value: \"CustomEvent\",
  },
  detail: enumerable,
});

// listeners held only weakly: kept alive by the object that asked for them
let weakListenersState;
let objectToWeakListenerMap;
function weakListeners() {
  weakListenersState ??= new FinalizationRegistry(
    ({ eventTarget, listener, eventType }) =>
      eventTarget.deref()?.[kRemoveWeakListenerHelper](eventType, listener),
  );
  objectToWeakListenerMap ??= new WeakMap();
  return { registry: weakListenersState, map: objectToWeakListenerMap };
}

class Listener {
  constructor(
    eventTarget,
    eventType,
    previous,
    listener,
    once,
    capture,
    passive,
    weak,
    resistStopPropagation,
  ) {
    this.next = undefined;
    if (previous !== undefined) previous.next = this;
    this.previous = previous;
    this.listener = listener;
    this.once = Boolean(once);
    this.capture = Boolean(capture);
    this.passive = Boolean(passive);
    this.weak = Boolean(weak);
    this.resistStopPropagation = Boolean(resistStopPropagation);
    this.removed = false;

    if (this.weak) {
      this.callback = new WeakRef(listener);
      weakListeners().registry.register(
        listener,
        { eventTarget: new WeakRef(eventTarget), listener: this, eventType },
        this,
      );
      this.weakKeyRef = new WeakRef(weak);
      let retained = weakListeners().map.get(weak);
      if (retained === undefined) {
        retained = new Set();
        weakListeners().map.set(weak, retained);
      }
      retained.add(listener);
      this.listener = this.callback;
    } else if (typeof listener === \"function\") {
      this.callback = listener;
    } else {
      this.callback = async (...args) => {
        if (listener.handleEvent) {
          await Reflect.apply(listener.handleEvent, listener, args);
        }
      };
    }
  }

  same(listener, capture) {
    const mine = this.weak ? this.listener.deref() : this.listener;
    return mine === listener && this.capture === capture;
  }

  remove() {
    if (this.previous !== undefined) this.previous.next = this.next;
    if (this.next !== undefined) this.next.previous = this.previous;
    this.removed = true;
    if (this.weak) {
      weakListeners().registry.unregister(this);
      const weakKey = this.weakKeyRef?.deref();
      const listener = this.callback?.deref();
      if (weakKey !== undefined && listener !== undefined) {
        const retained = weakListeners().map.get(weakKey);
        if (retained !== undefined) {
          retained.delete(listener);
          if (retained.size === 0) weakListeners().map.delete(weakKey);
        }
      }
    }
  }
}

function validateEventListener(listener) {
  if (
    typeof listener === \"function\" ||
    typeof listener?.handleEvent === \"function\"
  ) {
    return true;
  }
  if (listener == null) return false;
  if (typeof listener === \"object\") return true;
  throw invalidArgType(\"listener\", \"EventListener\", listener);
}

function validateEventListenerOptions(options) {
  if (typeof options === \"boolean\") return { capture: options };
  if (options === null) return {};
  if (typeof options !== \"object\" && typeof options !== \"function\") {
    throw invalidArgType(\"options\", \"Object\", options);
  }
  return {
    once: Boolean(options.once),
    capture: Boolean(options.capture),
    passive: Boolean(options.passive),
    signal: options.signal,
    weak: options[kWeakHandler],
    resistStopPropagation: options[kResistStopPropagation] ?? false,
  };
}

function validateAbortSignal(signal, name) {
  if (
    signal !== undefined &&
    (signal === null || typeof signal !== \"object\" || !(\"aborted\" in signal))
  ) {
    throw invalidArgType(name, \"AbortSignal\", signal);
  }
}

function isEventTarget(value) {
  return value?.constructor?.[kIsEventTarget];
}

const noOptions = {};

class EventTarget {
  static [kIsEventTarget] = true;

  constructor() {
    this[kEvents] = undefined;
    this[kHandlers] = undefined;
  }

  [kNewListener](size, type, listener, once, capture, passive, weak) {}

  [kRemoveListener](size, type, listener, capture) {}

  addEventListener(type, listener, options = noOptions) {
    if (!isEventTarget(this)) throw invalidThis(\"EventTarget\");
    if (arguments.length < 2) throw missingArgs(\"type\", \"listener\");

    let once = false;
    let capture = false;
    let passive = false;
    let weak = false;
    let resistStopPropagation = false;

    if (options !== noOptions) {
      options = validateEventListenerOptions(options);
      once = options.once;
      capture = options.capture;
      passive = options.passive;
      weak = options.weak;
      resistStopPropagation = options.resistStopPropagation;

      const signal = options.signal;
      validateAbortSignal(signal, \"options.signal\");
      if (signal) {
        if (signal.aborted) return;
        // not prevented by stopImmediatePropagation, and gone when this is
        signal.addEventListener(
          \"abort\",
          () => {
            this.removeEventListener(type, listener, options);
          },
          { once: true, [kWeakHandler]: this, [kResistStopPropagation]: true },
        );
      }
    }

    if (!validateEventListener(listener)) return;
    type = `${type}`;

    this[kEvents] ??= new Map();
    let root = this[kEvents].get(type);

    if (root === undefined) {
      root = {
        size: 1,
        next: undefined,
        resistStopPropagation: Boolean(resistStopPropagation),
      };
      new Listener(
        this,
        type,
        root,
        listener,
        once,
        capture,
        passive,
        weak,
        resistStopPropagation,
      );
      this[kNewListener](root.size, type, listener, once, capture, passive, weak);
      this[kEvents].set(type, root);
      return;
    }

    let handler = root.next;
    let previous = root;
    while (handler !== undefined && !handler.same(listener, capture)) {
      previous = handler;
      handler = handler.next;
    }
    if (handler !== undefined) return;

    new Listener(
      this,
      type,
      previous,
      listener,
      once,
      capture,
      passive,
      weak,
      resistStopPropagation,
    );
    root.size++;
    root.resistStopPropagation ||= Boolean(resistStopPropagation);
    this[kNewListener](root.size, type, listener, once, capture, passive, weak);
  }

  removeEventListener(type, listener, options = noOptions) {
    if (!isEventTarget(this)) throw invalidThis(\"EventTarget\");
    if (arguments.length < 2) throw missingArgs(\"type\", \"listener\");
    if (!validateEventListener(listener)) return;

    type = `${type}`;
    const capture =
      typeof options === \"boolean\" ? options : options?.capture === true;

    if (this[kEvents] === undefined) return;
    const root = this[kEvents].get(type);
    if (root === undefined || root.next === undefined) return;

    let handler = root.next;
    while (handler !== undefined) {
      if (handler.same(listener, capture)) {
        handler.remove();
        root.size--;
        if (root.size === 0) this[kEvents].delete(type);
        this[kRemoveListener](root.size, type, listener, capture);
        break;
      }
      handler = handler.next;
    }
  }

  [kRemoveWeakListenerHelper](type, listener) {
    if (this[kEvents] === undefined) return;
    const root = this[kEvents].get(type);
    if (root === undefined || root.next === undefined) return;

    const capture = listener.capture === true;
    let handler = root.next;
    while (handler !== undefined) {
      if (handler === listener) {
        handler.remove();
        root.size--;
        if (root.size === 0) this[kEvents].delete(type);
        this[kRemoveListener](root.size, type, undefined, capture);
        break;
      }
      handler = handler.next;
    }
  }

  [kInspect](depth, options) {
    const name = this.constructor.name;
    if (depth < 0) return name;
    const opts = Object.assign({}, options, {
      depth: Number.isInteger(options.depth)
        ? options.depth - 1
        : options.depth,
    });
    return `${name} ${inspect({}, opts)}`;
  }

  dispatchEvent(event) {
    if (!isEventTarget(this)) throw invalidThis(\"EventTarget\");
    if (arguments.length < 1) throw missingArgs(\"event\");
    if (!(event instanceof Event)) {
      throw invalidArgType(\"event\", \"Event\", event);
    }
    if (event[kIsBeingDispatched]) {
      throw error(
        \"ERR_EVENT_RECURSION\",
        `The event \"${event.type}\" is already being dispatched`,
      );
    }

    const type = event.type;
    event[kTarget] = this;
    event[kIsBeingDispatched] = true;

    const root = this[kEvents]?.get(type);
    if (root === undefined || root.next === undefined) {
      event[kIsBeingDispatched] = false;
      return event.defaultPrevented !== true;
    }

    let handler = root.next;
    let next;
    while (
      handler !== undefined &&
      (root.resistStopPropagation || handler.passive || event[kStop] !== true)
    ) {
      next = handler.next;

      if (
        handler.removed ||
        (event[kStop] === true && !handler.resistStopPropagation)
      ) {
        handler = next;
        continue;
      }
      if (handler.once) {
        handler.remove();
        root.size--;
        const { listener, capture } = handler;
        this[kRemoveListener](root.size, type, listener, capture);
      }

      try {
        const callback = handler.weak
          ? handler.callback.deref()
          : handler.callback;
        let result;
        if (callback) {
          if (handler.passive) event[kInPassiveListener] = true;
          result = callback.call(this, event);
          event[kIsBeingDispatched] = false;
        }
        if (result !== undefined && result !== null) addCatch(result);
      } catch (thrown) {
        emitUncaughtException(thrown);
      } finally {
        if (event[kInPassiveListener]) event[kInPassiveListener] = false;
      }

      handler = next;
    }

    event[kIsBeingDispatched] = false;
    return event.defaultPrevented !== true;
  }
}

Object.defineProperties(EventTarget.prototype, {
  addEventListener: enumerable,
  removeEventListener: enumerable,
  dispatchEvent: enumerable,
  [Symbol.toStringTag]: {
    writable: false,
    enumerable: false,
    configurable: true,
    value: \"EventTarget\",
  },
});

function makeEventHandler(handler) {
  function eventHandler(...args) {
    if (typeof eventHandler.handler !== \"function\") return;
    return Reflect.apply(eventHandler.handler, this, args);
  }
  eventHandler.handler = handler;
  return eventHandler;
}

// the `on<name>` attribute, one handler slot kept among the listeners
function defineEventHandler(emitter, name) {
  const propName = `on${name}`;
  function get() {
    if (typeof this !== \"object\" || this === null || !(kHandlers in this)) {
      throw invalidThis(\"EventTarget\");
    }
    return this[kHandlers]?.get(name)?.handler ?? null;
  }
  Object.defineProperty(get, \"name\", { value: `get ${propName}` });

  function set(value) {
    if (typeof this !== \"object\" || this === null || !(kHandlers in this)) {
      throw invalidThis(\"EventTarget\");
    }
    // anything that is not callable or an object switches the handler off
    if (
      typeof value !== \"function\" &&
      (typeof value !== \"object\" || value === null)
    ) {
      value = null;
    }
    this[kHandlers] ??= new Map();
    let wrapped = this[kHandlers].get(name);
    if (wrapped) {
      this[kEvents] ??= new Map();
      if (typeof wrapped.handler === \"function\") {
        const root = this[kEvents].get(name);
        if (root !== undefined) {
          root.size--;
          this[kRemoveListener](root.size, name, wrapped.handler, false);
        }
      }
      wrapped.handler = value;
      if (typeof wrapped.handler === \"function\") {
        const root = this[kEvents].get(name);
        if (root !== undefined) {
          root.size++;
          this[kNewListener](root.size, name, value, false, false, false, false);
        } else {
          this.addEventListener(name, wrapped);
        }
      }
    } else {
      wrapped = makeEventHandler(value);
      this.addEventListener(name, wrapped);
    }
    this[kHandlers].set(name, wrapped);
  }
  Object.defineProperty(set, \"name\", { value: `set ${propName}` });

  Object.defineProperty(emitter, propName, {
    get,
    set,
    configurable: true,
    enumerable: true,
  });
}

const kDontThrowSymbol = Symbol(\"dontThrow\");
const kAborted = Symbol(\"aborted\");
const kReason = Symbol(\"reason\");
const kTimeout = Symbol(\"timeout\");
const kComposite = Symbol(\"composite\");
const kFollowing = Symbol(\"following\");
const kResultSignalWeakRef = Symbol(\"resultSignalWeakRef\");
const kSourceSignals = Symbol(\"sourceSignals\");
const kDependantSignals = Symbol(\"dependantSignals\");

const clearTimeoutRegistry = new FinalizationRegistry(clearTimeout);
const dependantSignalsCleanupRegistry = new FinalizationRegistry(
  ({ sourceSignalRef, dependantSignalRef, sourceSignalsCleanupToken }) => {
    sourceSignalsCleanupRegistry.unregister(sourceSignalsCleanupToken);
    const sourceSignal = sourceSignalRef.deref();
    if (sourceSignal === undefined) return;
    sourceSignal[kDependantSignals].delete(dependantSignalRef);
  },
);
// signals something still listens to, held here so they outlive their makers
const gcPersistentSignals = new Set();
const sourceSignalsCleanupRegistry = new FinalizationRegistry(
  ({ sourceSignalRef, composedSignalRef }) => {
    const composedSignal = composedSignalRef.deref();
    if (composedSignal !== undefined) {
      composedSignal[kSourceSignals].delete(sourceSignalRef);
      if (composedSignal[kSourceSignals].size === 0) {
        gcPersistentSignals.delete(composedSignal);
      }
    }
  },
);

function validateThisAbortSignal(value) {
  if (value?.[kAborted] === undefined) throw invalidThis(\"AbortSignal\");
}

function refreshCompositeSignal(signal) {
  if (!signal[kComposite] || signal[kAborted] || !signal[kSourceSignals]?.size) {
    return;
  }
  for (const sourceSignalWeakRef of signal[kSourceSignals]) {
    const sourceSignal = sourceSignalWeakRef.deref();
    if (sourceSignal === undefined) {
      signal[kSourceSignals].delete(sourceSignalWeakRef);
      continue;
    }
    if (sourceSignal[kAborted]) {
      abortSignal(signal, sourceSignal[kReason]);
      return;
    }
  }
}

function followCompositeSignal(signal) {
  if (signal[kFollowing] || signal[kAborted] || !signal[kSourceSignals]?.size) {
    return;
  }
  const resultSignalWeakRef = (signal[kResultSignalWeakRef] ??= new WeakRef(
    signal,
  ));
  for (const sourceSignalWeakRef of signal[kSourceSignals]) {
    const sourceSignal = sourceSignalWeakRef.deref();
    if (sourceSignal === undefined) {
      signal[kSourceSignals].delete(sourceSignalWeakRef);
      continue;
    }
    if (sourceSignal[kAborted]) {
      abortSignal(signal, sourceSignal[kReason]);
      return;
    }
    sourceSignal[kDependantSignals] ??= new Set();
    sourceSignal[kDependantSignals].add(resultSignalWeakRef);
    dependantSignalsCleanupRegistry.register(signal, {
      sourceSignalRef: sourceSignalWeakRef,
      dependantSignalRef: resultSignalWeakRef,
      sourceSignalsCleanupToken: sourceSignalWeakRef,
    });
    sourceSignalsCleanupRegistry.register(
      sourceSignal,
      { sourceSignalRef: sourceSignalWeakRef, composedSignalRef: resultSignalWeakRef },
      sourceSignalWeakRef,
    );
  }
  signal[kFollowing] = true;
}

function setWeakAbortSignalTimeout(weakRef, delay) {
  const timeout = setTimeout(() => {
    const signal = weakRef.deref();
    if (signal !== undefined) {
      clearTimeoutRegistry.unregister(signal);
      gcPersistentSignals.delete(signal);
      abortSignal(
        signal,
        new DOMException(
          \"The operation was aborted due to timeout\",
          \"TimeoutError\",
        ),
      );
    }
  }, delay);
  timeout.unref();
  return timeout;
}

function validateUint32(value, name) {
  if (typeof value !== \"number\") throw invalidArgType(name, \"number\", value);
  if (!Number.isInteger(value)) {
    throw rangeError(
      \"ERR_OUT_OF_RANGE\",
      `The value of \"${name}\" is out of range. It must be an integer. Received ${String(value)}`,
    );
  }
  if (value < 0 || value > 4294967295) {
    throw rangeError(
      \"ERR_OUT_OF_RANGE\",
      `The value of \"${name}\" is out of range. It must be >= 0 && <= 4294967295. Received ${String(value)}`,
    );
  }
}

let isSignal;

class AbortSignal extends EventTarget {
  #brand;

  static {
    isSignal = (value) =>
      typeof value === \"object\" && value !== null && #brand in value;
  }

  constructor(dontThrowSymbol = undefined, init = {}) {
    if (dontThrowSymbol !== kDontThrowSymbol) {
      throw typeError(\"ERR_ILLEGAL_CONSTRUCTOR\", \"Illegal constructor\");
    }
    super();
    const { aborted = false, reason = undefined, composite = false } = init;
    this[kAborted] = aborted;
    this[kReason] = reason;
    this[kComposite] = composite;
  }

  [kInspect](depth, options) {
    return customInspect(this, { aborted: this.aborted }, depth, options);
  }

  get aborted() {
    validateThisAbortSignal(this);
    refreshCompositeSignal(this);
    return !!this[kAborted];
  }

  get reason() {
    validateThisAbortSignal(this);
    refreshCompositeSignal(this);
    return this[kReason];
  }

  throwIfAborted() {
    validateThisAbortSignal(this);
    refreshCompositeSignal(this);
    if (this[kAborted]) throw this[kReason];
  }

  static abort(
    reason = new DOMException(\"This operation was aborted\", \"AbortError\"),
  ) {
    return new AbortSignal(kDontThrowSymbol, { aborted: true, reason });
  }

  static timeout(delay) {
    validateUint32(delay, \"delay\");
    const signal = new AbortSignal(kDontThrowSymbol);
    signal[kTimeout] = true;
    clearTimeoutRegistry.register(
      signal,
      setWeakAbortSignalTimeout(new WeakRef(signal), delay),
      signal,
    );
    return signal;
  }

  static any(signals) {
    const signalsArray = signalSequence(signals);
    const resultSignal = new AbortSignal(kDontThrowSymbol, { composite: true });
    if (!signalsArray.length) return resultSignal;

    resultSignal[kSourceSignals] = new Set();
    let hasTimeoutSignals = false;
    for (let i = 0; i < signalsArray.length; i++) {
      const signal = signalsArray[i];
      if (signal[kTimeout]) {
        hasTimeoutSignals = true;
        gcPersistentSignals.add(signal);
      }
      refreshCompositeSignal(signal);
      if (signal[kAborted]) {
        abortSignal(resultSignal, signal[kReason]);
        return resultSignal;
      }
      if (!signal[kComposite]) {
        resultSignal[kSourceSignals].add(new WeakRef(signal));
      } else if (!signal[kSourceSignals]) {
        continue;
      } else {
        for (const sourceSignalWeakRef of signal[kSourceSignals]) {
          const sourceSignal = sourceSignalWeakRef.deref();
          if (!sourceSignal) continue;
          if (sourceSignal[kAborted]) {
            abortSignal(resultSignal, sourceSignal[kReason]);
            return resultSignal;
          }
          if (resultSignal[kSourceSignals].has(sourceSignalWeakRef)) continue;
          resultSignal[kSourceSignals].add(sourceSignalWeakRef);
        }
      }
    }
    if (hasTimeoutSignals && resultSignal[kSourceSignals].size > 0) {
      resultSignal[kTimeout] = true;
    }
    // keeps the abort state even if the sources are collected before anyone
    // looks; following holds them weakly, so unused signals can still go
    followCompositeSignal(resultSignal);
    return resultSignal;
  }

  [kNewListener](size, type, listener, once, capture, passive, weak) {
    if (this[kComposite] && type === \"abort\" && !this.aborted && size === 1) {
      followCompositeSignal(this);
    }
    const keeps =
      this[kTimeout] || (this[kComposite] && this[kSourceSignals]?.size);
    if (keeps && type === \"abort\" && !this.aborted && !weak && size === 1) {
      gcPersistentSignals.add(this);
    }
  }

  [kRemoveListener](size, type, listener, capture) {
    const keeps =
      this[kTimeout] || (this[kComposite] && this[kSourceSignals]?.size);
    if (keeps && type === \"abort\" && size === 0) {
      gcPersistentSignals.delete(this);
    }
  }
}

// webidl sequence<AbortSignal>
function signalSequence(signals) {
  const notSequence = () =>
    typeError(
      \"ERR_INVALID_ARG_TYPE\",
      \"signals cannot be converted to sequence.\",
    );
  if (
    signals === null ||
    (typeof signals !== \"object\" && typeof signals !== \"function\")
  ) {
    throw notSequence();
  }
  const method = signals[Symbol.iterator];
  if (typeof method !== \"function\") throw notSequence();
  const iterator = method.call(signals);
  const nextMethod = iterator?.next;
  if (typeof nextMethod !== \"function\") throw notSequence();

  const list = [];
  while (true) {
    const next = nextMethod.call(iterator);
    if (next === null || (typeof next !== \"object\" && typeof next !== \"function\")) {
      throw notSequence();
    }
    if (next.done) break;
    if (!isSignal(next.value)) {
      throw typeError(
        \"ERR_INVALID_ARG_TYPE\",
        `signals[${list.length}] is not of type AbortSignal.`,
      );
    }
    list.push(next.value);
  }
  return list;
}

Object.defineProperties(AbortSignal.prototype, {
  aborted: enumerable,
});
Object.defineProperty(AbortSignal.prototype, Symbol.toStringTag, {
  writable: false,
  enumerable: false,
  configurable: true,
  value: \"AbortSignal\",
});
defineEventHandler(AbortSignal.prototype, \"abort\");

function abortSignal(signal, reason) {
  if (signal[kAborted]) return;
  signal[kAborted] = true;
  signal[kReason] = reason;

  const dependentSignalsToAbort = [];
  signal[kDependantSignals]?.forEach((weakRef) => {
    const dependentSignal = weakRef.deref();
    if (dependentSignal && !dependentSignal[kAborted]) {
      dependentSignal[kReason] = reason;
      dependentSignal[kAborted] = true;
      dependentSignalsToAbort.push(dependentSignal);
    }
  });

  runAbort(signal);
  for (let i = 0; i < dependentSignalsToAbort.length; i++) {
    const dependentSignal = dependentSignalsToAbort[i];
    runAbort(dependentSignal);
    gcPersistentSignals.delete(dependentSignal);
  }

  gcPersistentSignals.delete(signal);
  if (signal[kComposite] && signal[kSourceSignals]) {
    signal[kSourceSignals].forEach((sourceWeakRef) => {
      const sourceSignal = sourceWeakRef.deref();
      if (sourceSignal) gcPersistentSignals.delete(sourceSignal);
    });
  }
}

function runAbort(signal) {
  signal.dispatchEvent(new Event(\"abort\", { [kTrustEvent]: true }));
}

class AbortController {
  #signal;

  [kInspect](depth, options) {
    return customInspect(this, { signal: this.signal }, depth, options);
  }

  get signal() {
    this.#signal ??= new AbortSignal(kDontThrowSymbol);
    return this.#signal;
  }

  abort(reason = new DOMException(\"This operation was aborted\", \"AbortError\")) {
    abortSignal((this.#signal ??= new AbortSignal(kDontThrowSymbol)), reason);
  }
}

Object.defineProperties(AbortController.prototype, {
  signal: enumerable,
  abort: enumerable,
});
Object.defineProperty(AbortController.prototype, Symbol.toStringTag, {
  writable: false,
  enumerable: false,
  configurable: true,
  value: \"AbortController\",
});

module.exports = {
  Event,
  CustomEvent,
  EventTarget,
  AbortController,
  AbortSignal,
};

// for the events module; not an export
Object.defineProperty(module.exports, \"internals\", {
  value: {
    isEventTarget,
    kEvents,
    kIsBeingDispatched,
    kResistStopPropagation,
    kTarget,
    kWeakHandler,
    kTrustEvent,
  },
});
"

pub const buffer = "\"use strict\";

// node's lib/buffer.js. Two things differ on purpose: small buffers are not
// carved out of a shared pool, so each has its own memory (byteOffset 0), and
// Buffer() gives no deprecation warning

const {
  typeError,
  rangeError,
  invalidArgType,
  invalidArgValue,
  outOfRange,
  validateArray,
  validateBuffer,
  validateInteger,
  validateNumber,
  validateString,
} = require(\"spark:errors\");
const { DOMException } = require(\"spark:domexception\");
const {
  encode,
  decode,
  byteLengthUtf8,
  compare: compareBytes,
  indexOf: indexOfBytes,
  swap: swapBytes,
  atob: decodeBase64Strict,
  isAscii: bytesAreAscii,
  isUtf8: bytesAreUtf8,
  isLatin1: stringIsLatin1,
  utf16Units,
} = native;

const TypedArrayPrototype = Object.getPrototypeOf(Uint8Array.prototype);
const getter = (proto, name) =>
  Object.getOwnPropertyDescriptor(proto, name).get;
const typedArrayTag = getter(TypedArrayPrototype, Symbol.toStringTag);
const typedArrayLength = getter(TypedArrayPrototype, \"length\");
const typedArrayByteLength = getter(TypedArrayPrototype, \"byteLength\");
const typedArrayByteOffset = getter(TypedArrayPrototype, \"byteOffset\");
const typedArrayBuffer = getter(TypedArrayPrototype, \"buffer\");
const typedArraySet = TypedArrayPrototype.set;
const typedArrayFill = TypedArrayPrototype.fill;
const typedArraySubarray = TypedArrayPrototype.subarray;
const typedArrayCopyWithin = TypedArrayPrototype.copyWithin;
const arrayBufferByteLength = getter(ArrayBuffer.prototype, \"byteLength\");
const sharedArrayBufferByteLength =
  typeof SharedArrayBuffer === \"function\"
    ? getter(SharedArrayBuffer.prototype, \"byteLength\")
    : undefined;

const customInspectSymbol = Symbol.for(\"nodejs.util.inspect.custom\");
const kIsEncodingSymbol = Symbol(\"kIsEncodingSymbol\");

const kMaxLength = Number.MAX_SAFE_INTEGER;
const kStringMaxLength = 2 ** 29 - 24;
const kMaxAlignment = 2 ** 30;

function isUint8Array(value) {
  return typedArrayTag.call(value) === \"Uint8Array\";
}

function isTypedArray(value) {
  return typedArrayTag.call(value) !== undefined;
}

function hasByteLength(read, value) {
  try {
    read.call(value);
    return true;
  } catch {
    return false;
  }
}

function isAnyArrayBuffer(value) {
  if (typeof value !== \"object\" || value === null) return false;
  return (
    hasByteLength(arrayBufferByteLength, value) ||
    (sharedArrayBufferByteLength !== undefined &&
      hasByteLength(sharedArrayBufferByteLength, value))
  );
}

function bufferOutOfBounds(name) {
  return rangeError(
    \"ERR_BUFFER_OUT_OF_BOUNDS\",
    name
      ? `\"${name}\" is outside of buffer bounds`
      : \"Attempt to access memory outside buffer bounds\",
  );
}

function unknownEncoding(encoding) {
  return typeError(\"ERR_UNKNOWN_ENCODING\", `Unknown encoding: ${encoding}`);
}

function invalidBufferSize(unit) {
  return rangeError(
    \"ERR_INVALID_BUFFER_SIZE\",
    `Buffer size must be a multiple of ${unit}`,
  );
}

function missingArgs(name) {
  return typeError(
    \"ERR_MISSING_ARGS\",
    `The \"${name}\" argument must be specified`,
  );
}

function indexOutOfRange() {
  return rangeError(\"ERR_OUT_OF_RANGE\", \"Index out of range\");
}

const validateOffset = (value, name, min = 0, max = kMaxLength) =>
  validateInteger(value, name, min, max);

function normalizeEncoding(enc) {
  if (enc == null || enc === \"utf8\" || enc === \"utf-8\") return \"utf8\";
  return slowCases(enc);
}

function slowCases(enc) {
  switch (enc.length) {
    case 4:
      if (enc === \"UTF8\") return \"utf8\";
      if (enc === \"ucs2\" || enc === \"UCS2\") return \"utf16le\";
      enc = `${enc}`.toLowerCase();
      if (enc === \"utf8\") return \"utf8\";
      if (enc === \"ucs2\") return \"utf16le\";
      break;
    case 3:
      if (enc === \"hex\" || enc === \"HEX\" || `${enc}`.toLowerCase() === \"hex\") {
        return \"hex\";
      }
      break;
    case 5:
      if (enc === \"ascii\") return \"ascii\";
      if (enc === \"ucs-2\") return \"utf16le\";
      if (enc === \"UTF-8\") return \"utf8\";
      if (enc === \"ASCII\") return \"ascii\";
      if (enc === \"UCS-2\") return \"utf16le\";
      enc = `${enc}`.toLowerCase();
      if (enc === \"utf-8\") return \"utf8\";
      if (enc === \"ascii\") return \"ascii\";
      if (enc === \"ucs-2\") return \"utf16le\";
      break;
    case 6:
      if (enc === \"base64\") return \"base64\";
      if (enc === \"latin1\" || enc === \"binary\") return \"latin1\";
      if (enc === \"BASE64\") return \"base64\";
      if (enc === \"LATIN1\" || enc === \"BINARY\") return \"latin1\";
      enc = `${enc}`.toLowerCase();
      if (enc === \"base64\") return \"base64\";
      if (enc === \"latin1\" || enc === \"binary\") return \"latin1\";
      break;
    case 7:
      if (
        enc === \"utf16le\" ||
        enc === \"UTF16LE\" ||
        `${enc}`.toLowerCase() === \"utf16le\"
      ) {
        return \"utf16le\";
      }
      break;
    case 8:
      if (
        enc === \"utf-16le\" ||
        enc === \"UTF-16LE\" ||
        `${enc}`.toLowerCase() === \"utf-16le\"
      ) {
        return \"utf16le\";
      }
      break;
    case 9:
      if (
        enc === \"base64url\" ||
        enc === \"BASE64URL\" ||
        `${enc}`.toLowerCase() === \"base64url\"
      ) {
        return \"base64url\";
      }
      break;
    default:
      if (enc === \"\") return \"utf8\";
  }
}

// what node's native slice functions do with their start and end
function sliceIndex(value, fallback) {
  if (value === undefined) return fallback;
  const index = Math.trunc(Number(value)) || 0;
  if (index < 0) throw indexOutOfRange();
  return index;
}

function slicer(encoding) {
  return function slice(buf, start, end) {
    const length = typedArrayByteLength.call(buf);
    start = sliceIndex(start, 0);
    end = sliceIndex(end, length);
    if (end <= start) return \"\";
    if (end > length) throw indexOutOfRange();
    return decode(buf, encoding, start, end);
  };
}

const asciiSlice = slicer(\"ascii\");
const base64Slice = slicer(\"base64\");
const base64urlSlice = slicer(\"base64url\");
const latin1Slice = slicer(\"latin1\");
const hexSlice = slicer(\"hex\");
const ucs2Slice = slicer(\"utf16le\");
const utf8Slice = slicer(\"utf8\");

// writes what fits of the string, never part of a character, and says how
// many bytes that was
function writeBytes(buf, string, encoding, offset, length) {
  if (typeof string !== \"string\") {
    throw typeError(\"ERR_INVALID_ARG_TYPE\", \"argument must be a string\");
  }
  if (length <= 0) return 0;
  const bytes = new Uint8Array(encode(string, encoding, length));
  typedArraySet.call(buf, bytes, offset);
  return bytes.length;
}

// node's native write functions: base64, base64url, hex and ucs2
function writer(encoding) {
  return function write(buf, string, offset, length) {
    if (typeof string !== \"string\") {
      throw typeError(\"ERR_INVALID_ARG_TYPE\", \"argument must be a string\");
    }
    const size = typedArrayByteLength.call(buf);
    offset = sliceIndex(offset, 0);
    if (offset > size) throw bufferOutOfBounds(\"offset\");
    length = Math.min(size - offset, sliceIndex(length, size - offset));
    return writeBytes(buf, string, encoding, offset, length);
  };
}

const base64Write = writer(\"base64\");
const base64urlWrite = writer(\"base64url\");
const hexWrite = writer(\"hex\");
const ucs2Write = writer(\"utf16le\");

// lib/internal/buffer.js: ascii, latin1 and utf8
function checkedWriter(encoding) {
  return function write(buf, string, offset = 0, length) {
    offset = Number(offset);
    if (offset < 0 || offset > buf.byteLength) {
      throw bufferOutOfBounds(\"offset\");
    }
    if (length === undefined) length = buf.byteLength - offset;
    else length = Number(length);
    if (length < 0 || length > buf.byteLength - offset) {
      throw bufferOutOfBounds(\"length\");
    }
    return writeBytes(
      buf,
      string,
      encoding,
      Math.trunc(offset) || 0,
      Math.trunc(length) || 0,
    );
  };
}

const asciiWrite = checkedWriter(\"ascii\");
const latin1Write = checkedWriter(\"latin1\");
const utf8Write = checkedWriter(\"utf8\");

function _compare(a, b) {
  return compareBytes(a, b);
}

function compareOffset(
  source,
  target,
  targetStart,
  sourceStart,
  targetEnd,
  sourceEnd,
) {
  const sourceLength = typedArrayByteLength.call(source);
  if (targetStart > typedArrayByteLength.call(target)) {
    throw rangeError(
      \"ERR_OUT_OF_RANGE\",
      'The value of \"targetStart\" is out of range.',
    );
  }
  if (sourceStart > sourceLength) {
    throw rangeError(
      \"ERR_OUT_OF_RANGE\",
      'The value of \"sourceStart\" is out of range.',
    );
  }
  const sourceSpan = sourceEnd - sourceStart;
  const targetSpan = targetEnd - targetStart;
  const count = Math.min(sourceSpan, targetSpan, sourceLength - sourceStart);
  const order =
    count > 0
      ? compareBytes(
          typedArraySubarray.call(source, sourceStart, sourceStart + count),
          typedArraySubarray.call(target, targetStart, targetStart + count),
        )
      : 0;
  if (order !== 0) return order;
  if (sourceSpan > targetSpan) return 1;
  if (sourceSpan < targetSpan) return -1;
  return 0;
}

// the bytes of any view, as a Uint8Array over the same memory
function bytesOf(view) {
  if (isUint8Array(view)) return view;
  return new Uint8Array(view.buffer, view.byteOffset, view.byteLength);
}

function _copy(source, target, targetStart, sourceStart, nb) {
  const from = bytesOf(source);
  typedArraySet.call(
    bytesOf(target),
    typedArraySubarray.call(from, sourceStart, sourceStart + nb),
    targetStart,
  );
  return nb;
}

// -1 for a value with no bytes, -2 for a range outside the buffer
function bindingFill(buf, value, offset, end, encoding) {
  if (offset > end || end > typedArrayByteLength.call(buf)) return -2;
  const fillLength = end - offset;
  let pattern;
  if (typeof value === \"string\") {
    pattern = new Uint8Array(
      encode(value, normalizeEncoding(encoding) ?? \"utf8\"),
    );
  } else if (ArrayBuffer.isView(value)) {
    pattern = bytesOf(value);
  } else {
    typedArrayFill.call(buf, (value >>> 0) & 255, offset, end);
    return 0;
  }
  let done = Math.min(pattern.length, fillLength);
  if (done > 0) {
    typedArraySet.call(buf, typedArraySubarray.call(pattern, 0, done), offset);
  }
  if (done >= fillLength) return 0;
  if (done === 0) return -1;
  // each pass doubles the stretch already filled
  while (done < fillLength) {
    const more = Math.min(done, fillLength - done);
    typedArrayCopyWithin.call(buf, offset + done, offset, offset + more);
    done += more;
  }
  return 0;
}

// `end` cuts the haystack short
function haystack(buf, end) {
  const length = typedArrayByteLength.call(buf);
  end = +end;
  if (!(end < length)) return bytesOf(buf);
  return typedArraySubarray.call(bytesOf(buf), 0, end > 0 ? Math.trunc(end) : 0);
}

function searchOffset(byteOffset) {
  return Math.trunc(byteOffset) || 0;
}

function indexOfBuffer(buf, val, byteOffset, step, dir, end) {
  return indexOfBytes(
    haystack(buf, end),
    bytesOf(val),
    searchOffset(byteOffset),
    dir,
    step,
  );
}

const oneByte = new Uint8Array(1);

function indexOfNumber(buf, val, byteOffset, dir, end) {
  oneByte[0] = val;
  return indexOfBytes(
    haystack(buf, end),
    oneByte,
    searchOffset(byteOffset),
    dir,
    1,
  );
}

function indexOfString(buf, val, byteOffset, encoding, dir, end) {
  return indexOfBytes(
    haystack(buf, end),
    encode(val, encoding),
    searchOffset(byteOffset),
    dir,
    encoding === \"utf16le\" ? 2 : 1,
  );
}

class FastBuffer extends Uint8Array {}

function createUnsafeBuffer(size) {
  return new FastBuffer(size);
}

const constants = Object.defineProperties(
  {},
  {
    MAX_LENGTH: {
      __proto__: null,
      value: kMaxLength,
      writable: false,
      enumerable: true,
    },
    MAX_STRING_LENGTH: {
      __proto__: null,
      value: kStringMaxLength,
      writable: false,
      enumerable: true,
    },
  },
);

function toInteger(n, defaultVal) {
  n = +n;
  if (
    !Number.isNaN(n) &&
    n >= Number.MIN_SAFE_INTEGER &&
    n <= Number.MAX_SAFE_INTEGER
  ) {
    return n % 1 === 0 ? n : Math.floor(n);
  }
  return defaultVal;
}

function copyImpl(source, target, targetStart, sourceStart, sourceEnd) {
  if (!ArrayBuffer.isView(source)) {
    throw invalidArgType(\"source\", [\"Buffer\", \"Uint8Array\"], source);
  }
  if (!ArrayBuffer.isView(target)) {
    throw invalidArgType(\"target\", [\"Buffer\", \"Uint8Array\"], target);
  }

  if (targetStart === undefined) {
    targetStart = 0;
  } else {
    targetStart = Number.isInteger(targetStart)
      ? targetStart
      : toInteger(targetStart, 0);
    if (targetStart < 0) throw outOfRange(\"targetStart\", \">= 0\", targetStart);
  }

  if (sourceStart === undefined) {
    sourceStart = 0;
  } else {
    sourceStart = Number.isInteger(sourceStart)
      ? sourceStart
      : toInteger(sourceStart, 0);
    if (sourceStart < 0 || sourceStart > source.byteLength) {
      throw outOfRange(
        \"sourceStart\",
        `>= 0 && <= ${source.byteLength}`,
        sourceStart,
      );
    }
  }

  if (sourceEnd === undefined) {
    sourceEnd = source.byteLength;
  } else {
    sourceEnd = Number.isInteger(sourceEnd)
      ? sourceEnd
      : toInteger(sourceEnd, 0);
    if (sourceEnd < 0) throw outOfRange(\"sourceEnd\", \">= 0\", sourceEnd);
  }

  if (targetStart >= target.byteLength || sourceStart >= sourceEnd) return 0;

  return _copyActual(source, target, targetStart, sourceStart, sourceEnd);
}

function _copyActual(source, target, targetStart, sourceStart, sourceEnd) {
  if (sourceEnd - sourceStart > target.byteLength - targetStart) {
    sourceEnd = sourceStart + target.byteLength - targetStart;
  }

  let nb = sourceEnd - sourceStart;
  const sourceLen = source.byteLength - sourceStart;
  if (nb > sourceLen) nb = sourceLen;

  if (nb <= 0) return 0;

  return _copy(source, target, targetStart, sourceStart, nb);
}

function Buffer(arg, encodingOrOffset, length) {
  if (typeof arg === \"number\") {
    if (typeof encodingOrOffset === \"string\") {
      throw invalidArgType(\"string\", \"string\", arg);
    }
    return Buffer.alloc(arg);
  }
  return Buffer.from(arg, encodingOrOffset, length);
}

FastBuffer.prototype.constructor = Buffer;
Buffer.prototype = FastBuffer.prototype;

Object.defineProperty(Buffer, Symbol.species, {
  __proto__: null,
  enumerable: false,
  configurable: true,
  get() {
    return FastBuffer;
  },
});

Buffer.poolSize = 64 * 1024;

Buffer.from = function from(value, encodingOrOffset, length) {
  if (typeof value === \"string\") return fromString(value, encodingOrOffset);

  if (typeof value === \"object\" && value !== null) {
    if (isAnyArrayBuffer(value)) {
      return fromArrayBuffer(value, encodingOrOffset, length);
    }

    const valueOf = value.valueOf && value.valueOf();
    if (
      valueOf != null &&
      valueOf !== value &&
      (typeof valueOf === \"string\" || typeof valueOf === \"object\")
    ) {
      return from(valueOf, encodingOrOffset, length);
    }

    const b = fromObject(value);
    if (b) return b;

    if (typeof value[Symbol.toPrimitive] === \"function\") {
      const primitive = value[Symbol.toPrimitive](\"string\");
      if (typeof primitive === \"string\") {
        return fromString(primitive, encodingOrOffset);
      }
    }
  }

  throw invalidArgType(
    \"first argument\",
    [\"string\", \"Buffer\", \"ArrayBuffer\", \"Array\", \"Array-like Object\"],
    value,
  );
};

Buffer.copyBytesFrom = function copyBytesFrom(view, offset, length) {
  if (!isTypedArray(view)) {
    throw invalidArgType(\"view\", [\"TypedArray\"], view);
  }

  const viewLength = typedArrayLength.call(view);
  if (viewLength === 0) return new FastBuffer();

  let start = 0;
  let end = viewLength;

  if (offset !== undefined) {
    validateInteger(offset, \"offset\", 0);
    if (offset >= viewLength) return new FastBuffer();
    start = offset;
  }

  if (length !== undefined) {
    validateInteger(length, \"length\", 0);
    end = Math.min(start + length, viewLength);
  }

  if (end <= start) return new FastBuffer();

  const viewByteLength = typedArrayByteLength.call(view);
  const elementSize = viewByteLength / viewLength;
  const srcByteOffset = typedArrayByteOffset.call(view) + start * elementSize;
  const srcByteLength = (end - start) * elementSize;

  return fromArrayLike(
    new Uint8Array(typedArrayBuffer.call(view), srcByteOffset, srcByteLength),
  );
};

const of = (...items) => {
  const len = items.length;
  const newObj = new FastBuffer(len);
  for (let k = 0; k < len; k++) newObj[k] = items[k];
  return newObj;
};
Buffer.of = of;

Object.setPrototypeOf(Buffer, Uint8Array);

Buffer.alloc = function alloc(size, fill, encoding) {
  validateNumber(size, \"size\", 0, kMaxLength);
  if (fill !== undefined && fill !== 0 && size > 0) {
    const buf = createUnsafeBuffer(size);
    return _fill(buf, fill, 0, buf.length, encoding);
  }
  return new FastBuffer(size);
};

Buffer.allocUnsafe = function allocUnsafe(size, alignment) {
  validateNumber(size, \"size\", 0, kMaxLength);
  if (alignment !== undefined) validateAlignment(size, alignment);
  return allocate(size);
};

Buffer.allocUnsafeSlow = function allocUnsafeSlow(size, alignment) {
  validateNumber(size, \"size\", 0, kMaxLength);
  if (alignment !== undefined) validateAlignment(size, alignment);
  return createUnsafeBuffer(size);
};

function validateAlignment(size, alignment) {
  validateInteger(alignment, \"alignment\", 1, kMaxAlignment);
  if ((alignment & (alignment - 1)) !== 0) {
    throw invalidArgValue(\"alignment\", alignment, \"must be a power of two\");
  }
  const padding = Math.max(alignment, 8);
  if (size > kMaxLength - padding) {
    throw outOfRange(\"size\", `<= ${kMaxLength - padding}`, size);
  }
}

function allocate(size) {
  if (size <= 0) return new FastBuffer();
  return createUnsafeBuffer(size);
}

function fromString(string, encoding) {
  let ops;
  if (!encoding || encoding === \"utf8\" || typeof encoding !== \"string\") {
    ops = encodingOps.utf8;
  } else {
    ops = getEncodingOps(encoding);
    if (ops === undefined) throw unknownEncoding(encoding);
  }

  return string.length === 0
    ? new FastBuffer()
    : new FastBuffer(encode(string, ops.encodingVal));
}

function fromArrayBuffer(obj, byteOffset, length) {
  if (byteOffset === undefined) {
    byteOffset = 0;
  } else {
    byteOffset = +byteOffset;
    if (Number.isNaN(byteOffset)) byteOffset = 0;
  }

  const maxLength = obj.byteLength - byteOffset;

  if (maxLength < 0) throw bufferOutOfBounds(\"offset\");

  if (length !== undefined) {
    length = +length;
    if (length > 0) {
      if (length > maxLength) throw bufferOutOfBounds(\"length\");
    } else {
      length = 0;
    }
  }

  return new FastBuffer(obj, byteOffset, length);
}

function fromArrayLike(obj) {
  const { length } = obj;
  if (length <= 0) return new FastBuffer();
  if (length < Buffer.poolSize >>> 1) {
    const b = new FastBuffer(length);
    typedArraySet.call(b, obj, 0);
    return b;
  }
  return new FastBuffer(obj);
}

function fromObject(obj) {
  if (obj.length !== undefined || isAnyArrayBuffer(obj.buffer)) {
    if (typeof obj.length !== \"number\") return new FastBuffer();
    return fromArrayLike(obj);
  }

  if (obj.type === \"Buffer\" && Array.isArray(obj.data)) {
    return fromArrayLike(obj.data);
  }
}

Buffer.isBuffer = function isBuffer(b) {
  return b instanceof Buffer;
};

Buffer.compare = function compare(buf1, buf2) {
  if (!isUint8Array(buf1)) {
    throw invalidArgType(\"buf1\", [\"Buffer\", \"Uint8Array\"], buf1);
  }

  if (!isUint8Array(buf2)) {
    throw invalidArgType(\"buf2\", [\"Buffer\", \"Uint8Array\"], buf2);
  }

  if (buf1 === buf2) return 0;

  return _compare(buf1, buf2);
};

Buffer.isEncoding = function isEncoding(encoding) {
  return (
    typeof encoding === \"string\" &&
    encoding.length !== 0 &&
    normalizeEncoding(encoding) !== undefined
  );
};
Buffer[kIsEncodingSymbol] = Buffer.isEncoding;

Buffer.concat = function concat(list, length) {
  validateArray(list, \"list\");

  if (list.length === 0) return new FastBuffer();

  if (length === undefined) {
    length = 0;
    for (let i = 0; i < list.length; i++) {
      const buf = list[i];
      if (!isUint8Array(buf)) {
        throw invalidArgType(`list[${i}]`, [\"Buffer\", \"Uint8Array\"], buf);
      }
      length += typedArrayByteLength.call(buf);
    }

    const buffer = allocate(length);
    let pos = 0;
    for (let i = 0; i < list.length; i++) {
      const buf = list[i];
      const bufLength = typedArrayByteLength.call(buf);
      typedArraySet.call(buffer, buf, pos);
      pos += bufLength;
    }

    if (pos < length) typedArrayFill.call(buffer, 0, pos, length);
    return buffer;
  }

  validateOffset(length, \"length\");
  for (let i = 0; i < list.length; i++) {
    if (!isUint8Array(list[i])) {
      throw invalidArgType(`list[${i}]`, [\"Buffer\", \"Uint8Array\"], list[i]);
    }
  }

  const buffer = allocate(length);
  let pos = 0;
  for (let i = 0; i < list.length; i++) {
    const buf = list[i];
    const bufLength = typedArrayByteLength.call(buf);
    if (pos + bufLength > length) {
      typedArraySet.call(
        buffer,
        typedArraySubarray.call(buf, 0, length - pos),
        pos,
      );
      pos = length;
      break;
    }
    typedArraySet.call(buffer, buf, pos);
    pos += bufLength;
  }

  if (pos < length) typedArrayFill.call(buffer, 0, pos, length);
  return buffer;
};

function base64ByteLength(str, bytes) {
  if (str.charCodeAt(bytes - 1) === 0x3d) bytes--;
  if (bytes > 1 && str.charCodeAt(bytes - 1) === 0x3d) bytes--;

  // base64 ratio: 3/4
  return (bytes * 3) >>> 2;
}

// a string as the bytes to look for; none of them are utf-16 text, where a
// match has to sit on a unit edge
function searchEncoded(encoding) {
  return (buf, val, byteOffset, dir, end) =>
    indexOfBuffer(
      buf,
      new Uint8Array(encode(val, encoding)),
      byteOffset,
      1,
      dir,
      end,
    );
}

const encodingOps = {
  utf8: {
    encoding: \"utf8\",
    encodingVal: \"utf8\",
    byteLength: byteLengthUtf8,
    write: utf8Write,
    slice: utf8Slice,
    indexOf: (buf, val, byteOffset, dir, end) =>
      indexOfString(buf, val, byteOffset, \"utf8\", dir, end),
  },
  ucs2: {
    encoding: \"ucs2\",
    encodingVal: \"utf16le\",
    byteLength: (string) => string.length * 2,
    write: ucs2Write,
    slice: ucs2Slice,
    indexOf: (buf, val, byteOffset, dir, end) =>
      indexOfString(buf, val, byteOffset, \"utf16le\", dir, end),
  },
  utf16le: {
    encoding: \"utf16le\",
    encodingVal: \"utf16le\",
    byteLength: (string) => string.length * 2,
    write: ucs2Write,
    slice: ucs2Slice,
    indexOf: (buf, val, byteOffset, dir, end) =>
      indexOfString(buf, val, byteOffset, \"utf16le\", dir, end),
  },
  latin1: {
    encoding: \"latin1\",
    encodingVal: \"latin1\",
    byteLength: (string) => string.length,
    write: latin1Write,
    slice: latin1Slice,
    indexOf: (buf, val, byteOffset, dir, end) =>
      indexOfString(buf, val, byteOffset, \"latin1\", dir, end),
  },
  ascii: {
    encoding: \"ascii\",
    encodingVal: \"ascii\",
    byteLength: (string) => string.length,
    write: asciiWrite,
    slice: asciiSlice,
    indexOf: searchEncoded(\"ascii\"),
  },
  base64: {
    encoding: \"base64\",
    encodingVal: \"base64\",
    byteLength: (string) => base64ByteLength(string, string.length),
    write: base64Write,
    slice: base64Slice,
    indexOf: searchEncoded(\"base64\"),
  },
  base64url: {
    encoding: \"base64url\",
    encodingVal: \"base64url\",
    byteLength: (string) => base64ByteLength(string, string.length),
    write: base64urlWrite,
    slice: base64urlSlice,
    indexOf: searchEncoded(\"base64url\"),
  },
  hex: {
    encoding: \"hex\",
    encodingVal: \"hex\",
    byteLength: (string) => string.length >>> 1,
    write: hexWrite,
    slice: hexSlice,
    indexOf: searchEncoded(\"hex\"),
  },
};

function getEncodingOps(encoding) {
  encoding += \"\";
  switch (encoding.length) {
    case 4:
      if (encoding === \"utf8\") return encodingOps.utf8;
      if (encoding === \"ucs2\") return encodingOps.ucs2;
      encoding = encoding.toLowerCase();
      if (encoding === \"utf8\") return encodingOps.utf8;
      if (encoding === \"ucs2\") return encodingOps.ucs2;
      break;
    case 5:
      if (encoding === \"utf-8\") return encodingOps.utf8;
      if (encoding === \"ascii\") return encodingOps.ascii;
      if (encoding === \"ucs-2\") return encodingOps.ucs2;
      encoding = encoding.toLowerCase();
      if (encoding === \"utf-8\") return encodingOps.utf8;
      if (encoding === \"ascii\") return encodingOps.ascii;
      if (encoding === \"ucs-2\") return encodingOps.ucs2;
      break;
    case 7:
      if (encoding === \"utf16le\" || encoding.toLowerCase() === \"utf16le\") {
        return encodingOps.utf16le;
      }
      break;
    case 8:
      if (encoding === \"utf-16le\" || encoding.toLowerCase() === \"utf-16le\") {
        return encodingOps.utf16le;
      }
      break;
    case 6:
      if (encoding === \"latin1\" || encoding === \"binary\") {
        return encodingOps.latin1;
      }
      if (encoding === \"base64\") return encodingOps.base64;
      encoding = encoding.toLowerCase();
      if (encoding === \"latin1\" || encoding === \"binary\") {
        return encodingOps.latin1;
      }
      if (encoding === \"base64\") return encodingOps.base64;
      break;
    case 3:
      if (encoding === \"hex\" || encoding.toLowerCase() === \"hex\") {
        return encodingOps.hex;
      }
      break;
    case 9:
      if (encoding === \"base64url\" || encoding.toLowerCase() === \"base64url\") {
        return encodingOps.base64url;
      }
      break;
  }
}

function byteLength(string, encoding) {
  if (typeof string !== \"string\") {
    if (ArrayBuffer.isView(string) || isAnyArrayBuffer(string)) {
      return string.byteLength;
    }

    throw invalidArgType(\"string\", [\"string\", \"Buffer\", \"ArrayBuffer\"], string);
  }

  const len = string.length;
  if (len === 0) return 0;

  if (!encoding || encoding === \"utf8\") return byteLengthUtf8(string);

  if (encoding === \"ascii\") return len;

  const ops = getEncodingOps(encoding);
  if (ops === undefined) return byteLengthUtf8(string);

  return ops.byteLength(string);
}

Buffer.byteLength = byteLength;
Buffer.stringLength = stringLength;

Object.defineProperty(Buffer.prototype, \"parent\", {
  __proto__: null,
  enumerable: true,
  get() {
    if (!(this instanceof Buffer)) return undefined;
    return this.buffer;
  },
});
Object.defineProperty(Buffer.prototype, \"offset\", {
  __proto__: null,
  enumerable: true,
  get() {
    if (!(this instanceof Buffer)) return undefined;
    return this.byteOffset;
  },
});

Buffer.prototype.copy = function copy(
  target,
  targetStart,
  sourceStart,
  sourceEnd,
) {
  return copyImpl(this, target, targetStart, sourceStart, sourceEnd);
};

Buffer.prototype.toString = function toString(encoding, start, end) {
  if (arguments.length === 0) {
    return utf8Slice(this, 0, this.length);
  }

  const bufferLength = typedArrayLength.call(this);

  if (start <= 0) start = 0;
  else if (start >= bufferLength) return \"\";
  else start = Math.trunc(start) || 0;

  if (end === undefined || end > bufferLength) end = bufferLength;
  else end = Math.trunc(end) || 0;

  if (end <= start) return \"\";

  if (encoding === undefined) return utf8Slice(this, start, end);

  const ops = getEncodingOps(encoding);
  if (ops === undefined) throw unknownEncoding(encoding);

  return ops.slice(this, start, end);
};

Buffer.prototype.equals = function equals(otherBuffer) {
  if (!isUint8Array(otherBuffer)) {
    throw invalidArgType(
      \"otherBuffer\",
      [\"Buffer\", \"Uint8Array\"],
      otherBuffer,
    );
  }

  if (this === otherBuffer) return true;
  const len = typedArrayByteLength.call(this);
  if (len !== typedArrayByteLength.call(otherBuffer)) return false;

  return len === 0 || _compare(this, otherBuffer) === 0;
};

let INSPECT_MAX_BYTES = 50;

// own properties beyond the bytes are not listed, that needs util.inspect
Buffer.prototype[customInspectSymbol] = function inspect(recurseTimes, ctx) {
  const max = INSPECT_MAX_BYTES;
  const actualMax = Math.min(max, this.length);
  const remaining = this.length - max;
  let str = hexSlice(this, 0, actualMax)
    .replace(/(.{2})/g, \"$1 \")
    .trim();
  if (remaining > 0) {
    str += ` ... ${remaining} more byte${remaining > 1 ? \"s\" : \"\"}`;
  }
  let constructorName = \"Buffer\";
  try {
    const { constructor } = this;
    if (
      typeof constructor === \"function\" &&
      Object.prototype.hasOwnProperty.call(constructor, \"name\")
    ) {
      constructorName = constructor.name;
    }
  } catch {
    // keep the default name
  }
  return `<${constructorName} ${str}>`;
};
Buffer.prototype.inspect = Buffer.prototype[customInspectSymbol];

Buffer.prototype.compare = function compare(
  target,
  targetStart,
  targetEnd,
  sourceStart,
  sourceEnd,
) {
  if (!isUint8Array(target)) {
    throw invalidArgType(\"target\", [\"Buffer\", \"Uint8Array\"], target);
  }
  if (arguments.length === 1) return _compare(this, target);

  if (targetStart === undefined) targetStart = 0;
  else validateOffset(targetStart, \"targetStart\");

  if (targetEnd === undefined) targetEnd = target.length;
  else validateOffset(targetEnd, \"targetEnd\", 0, target.length);

  if (sourceStart === undefined) sourceStart = 0;
  else validateOffset(sourceStart, \"sourceStart\");

  if (sourceEnd === undefined) sourceEnd = this.length;
  else validateOffset(sourceEnd, \"sourceEnd\", 0, this.length);

  if (sourceStart >= sourceEnd) return targetStart >= targetEnd ? 0 : -1;

  if (targetStart >= targetEnd) return 1;

  return compareOffset(
    this,
    target,
    targetStart,
    sourceStart,
    targetEnd,
    sourceEnd,
  );
};

function bidirectionalIndexOf(buffer, val, byteOffset, end, encoding, dir) {
  validateBuffer(buffer);

  if (typeof byteOffset === \"string\") {
    encoding = byteOffset;
    byteOffset = undefined;
  } else if (byteOffset > 0x7fffffff) {
    byteOffset = 0x7fffffff;
  } else if (byteOffset < -0x80000000) {
    byteOffset = -0x80000000;
  }
  // Coerce to Number. Values like null and [] become 0.
  byteOffset = +byteOffset;
  // If the offset is undefined, \"foo\", {}, coerces to NaN, search whole buffer.
  if (Number.isNaN(byteOffset)) {
    byteOffset = dir ? 0 : buffer.length || buffer.byteLength;
  }
  dir = !!dir;

  if (typeof val === \"number\") {
    return indexOfNumber(buffer, val >>> 0, byteOffset, dir, end);
  }

  let ops;
  if (encoding === undefined) ops = encodingOps.utf8;
  else ops = getEncodingOps(encoding);

  if (typeof val === \"string\") {
    if (ops === undefined) throw unknownEncoding(encoding);
    return ops.indexOf(buffer, val, byteOffset, dir, end);
  }

  if (isUint8Array(val)) {
    const step = ops !== undefined && ops.encodingVal === \"utf16le\" ? 2 : 1;
    return indexOfBuffer(buffer, val, byteOffset, step, dir, end);
  }

  throw invalidArgType(
    \"value\",
    [\"number\", \"string\", \"Buffer\", \"Uint8Array\"],
    val,
  );
}

Buffer.prototype.indexOf = function indexOf(val, offset, end, encoding) {
  if (typeof end === \"string\") {
    encoding = end;
    end = this.length;
  } else if (end === undefined) {
    end = this.length;
  }
  return bidirectionalIndexOf(this, val, offset, end, encoding, true);
};

Buffer.prototype.lastIndexOf = function lastIndexOf(
  val,
  offset,
  end,
  encoding,
) {
  if (typeof end === \"string\") {
    encoding = end;
    end = this.length;
  } else if (end === undefined) {
    end = this.length;
  }
  return bidirectionalIndexOf(this, val, offset, end, encoding, false);
};

Buffer.prototype.includes = function includes(val, offset, end, encoding) {
  if (typeof end === \"string\") {
    encoding = end;
    end = this.length;
  } else if (end === undefined) {
    end = this.length;
  }
  return bidirectionalIndexOf(this, val, offset, end, encoding, true) !== -1;
};

Buffer.prototype.fill = function fill(value, offset, end, encoding) {
  return _fill(this, value, offset, end, encoding);
};

function _fill(buf, value, offset, end, encoding) {
  if (typeof value === \"string\") {
    if (offset === undefined || typeof offset === \"string\") {
      encoding = offset;
      offset = 0;
      end = buf.length;
    } else if (typeof end === \"string\") {
      encoding = end;
      end = buf.length;
    }

    const normalizedEncoding = normalizeEncoding(encoding);
    if (normalizedEncoding === undefined) {
      validateString(encoding, \"encoding\");
      throw unknownEncoding(encoding);
    }

    if (value.length === 0) {
      // If value === '' default to zero.
      value = 0;
    } else if (value.length === 1) {
      // Fast path: If `value` fits into a single byte, use that numeric value.
      if (normalizedEncoding === \"utf8\" || normalizedEncoding === \"ascii\") {
        const code = value.charCodeAt(0);
        if (code < 128) value = code;
      } else if (normalizedEncoding === \"latin1\") {
        value = value.charCodeAt(0);
      }
    }
  } else {
    encoding = undefined;
  }

  if (offset === undefined) {
    offset = 0;
    end = buf.length;
  } else {
    validateOffset(offset, \"offset\");
    // Invalid ranges are not set to a default, so can range check early.
    if (end === undefined) {
      end = buf.length;
    } else {
      validateOffset(end, \"end\", 0, buf.length);
    }
    if (offset >= end) return buf;
  }

  if (typeof value === \"number\") {
    const byteLen = typedArrayByteLength.call(buf);
    const fillLength = end - offset;
    if (offset > end || fillLength + offset > byteLen) {
      throw bufferOutOfBounds();
    }

    typedArrayFill.call(buf, value, offset, end);
  } else {
    const res = bindingFill(buf, value, offset, end, encoding);
    if (res < 0) {
      if (res === -1) throw invalidArgValue(\"value\", value);
      throw bufferOutOfBounds();
    }
  }

  return buf;
}

Buffer.prototype.write = function write(string, offset, length, encoding) {
  const bufferLength = typedArrayLength.call(this);
  // Buffer#write(string);
  if (offset === undefined) {
    return utf8Write(this, string, 0, bufferLength);
  }
  // Buffer#write(string, encoding)
  if (length === undefined && typeof offset === \"string\") {
    encoding = offset;
    length = bufferLength;
    offset = 0;

    // Buffer#write(string, offset[, length][, encoding])
  } else {
    validateOffset(offset, \"offset\", 0, bufferLength);

    const remaining = bufferLength - offset;

    if (length === undefined) {
      length = remaining;
    } else if (typeof length === \"string\") {
      encoding = length;
      length = remaining;
    } else {
      validateOffset(length, \"length\", 0, bufferLength);
      if (length > remaining) length = remaining;
    }
  }

  if (!encoding || encoding === \"utf8\") {
    return utf8Write(this, string, offset, length);
  }
  if (encoding === \"ascii\") return asciiWrite(this, string, offset, length);

  const ops = getEncodingOps(encoding);
  if (ops === undefined) throw unknownEncoding(encoding);
  return ops.write(this, string, offset, length);
};

Buffer.prototype.toJSON = function toJSON() {
  const bufferLength = typedArrayLength.call(this);
  if (bufferLength > 0) {
    const data = new Array(bufferLength);
    for (let i = 0; i < bufferLength; ++i) data[i] = this[i];
    return { type: \"Buffer\", data };
  }
  return { type: \"Buffer\", data: [] };
};

function adjustOffset(offset, length) {
  // Use Math.trunc() to convert offset to an integer value that can be larger
  // than an Int32. Hence, don't use offset | 0 or similar techniques.
  offset = Math.trunc(offset);
  if (offset === 0) return 0;
  if (offset < 0) {
    offset += length;
    return offset > 0 ? offset : 0;
  }
  if (offset < length) return offset;
  return Number.isNaN(offset) ? 0 : length;
}

Buffer.prototype.subarray = function subarray(start, end) {
  const srcLength = typedArrayLength.call(this);
  start = adjustOffset(start, srcLength);
  end = end !== undefined ? adjustOffset(end, srcLength) : srcLength;
  const newLength = end > start ? end - start : 0;
  return new FastBuffer(this.buffer, this.byteOffset + start, newLength);
};

Buffer.prototype.slice = function slice(start, end) {
  return this.subarray(start, end);
};

function swap(b, n, m) {
  const i = b[n];
  b[n] = b[m];
  b[m] = i;
}

function swapWhole(buf, bits) {
  typedArraySet.call(buf, new Uint8Array(swapBytes(buf, bits)), 0);
}

Buffer.prototype.swap16 = function swap16() {
  // For Buffer.length <= 32, it's generally faster to
  // do the swap in javascript.
  const len = typedArrayLength.call(this);
  if (len % 2 !== 0) throw invalidBufferSize(\"16-bits\");
  if (len <= 32) {
    for (let i = 0; i < len; i += 2) swap(this, i, i + 1);
    return this;
  }
  swapWhole(this, 16);
  return this;
};

Buffer.prototype.swap32 = function swap32() {
  const len = typedArrayLength.call(this);
  if (len % 4 !== 0) throw invalidBufferSize(\"32-bits\");
  if (len <= 32) {
    for (let i = 0; i < len; i += 4) {
      swap(this, i, i + 3);
      swap(this, i + 1, i + 2);
    }
    return this;
  }
  swapWhole(this, 32);
  return this;
};

Buffer.prototype.swap64 = function swap64() {
  const len = typedArrayLength.call(this);
  if (len % 8 !== 0) throw invalidBufferSize(\"64-bits\");
  if (len < 48) {
    for (let i = 0; i < len; i += 8) {
      swap(this, i, i + 7);
      swap(this, i + 1, i + 6);
      swap(this, i + 2, i + 5);
      swap(this, i + 3, i + 4);
    }
    return this;
  }
  swapWhole(this, 64);
  return this;
};

Buffer.prototype.toLocaleString = Buffer.prototype.toString;

// node's lib/internal/buffer.js: reading and writing numbers

const float32Array = new Float32Array(1);
const uInt8Float32Array = new Uint8Array(float32Array.buffer);
const float64Array = new Float64Array(1);
const uInt8Float64Array = new Uint8Array(float64Array.buffer);

float32Array[0] = -1;
const bigEndian = uInt8Float32Array[3] === 0;

function checkBounds(buf, offset, byteLength) {
  validateNumber(offset, \"offset\");
  if (buf[offset] === undefined || buf[offset + byteLength] === undefined)
    boundsError(offset, buf.length - (byteLength + 1));
}

function checkInt(value, min, max, buf, offset, byteLength) {
  if (value > max || value < min) {
    const n = typeof min === \"bigint\" ? \"n\" : \"\";
    let range;
    if (byteLength > 3) {
      if (min === 0 || min === 0n) {
        range = `>= 0${n} and < 2${n} ** ${(byteLength + 1) * 8}${n}`;
      } else {
        range = `>= -(2${n} ** ${(byteLength + 1) * 8 - 1}${n}) and ` +
                `< 2${n} ** ${(byteLength + 1) * 8 - 1}${n}`;
      }
    } else {
      range = `>= ${min}${n} and <= ${max}${n}`;
    }
    throw outOfRange(\"value\", range, value);
  }
  checkBounds(buf, offset, byteLength);
}

function boundsError(value, length, type) {
  if (Math.floor(value) !== value) {
    validateNumber(value, type);
    throw outOfRange(type || \"offset\", \"an integer\", value);
  }

  if (length < 0)
    throw bufferOutOfBounds();

  throw outOfRange(type || \"offset\",
                             `>= ${type ? 1 : 0} and <= ${length}`,
                             value);
}

function readBigUInt64LE(offset = 0) {
  validateNumber(offset, \"offset\");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  const lo = first +
    this[++offset] * 2 ** 8 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 24;

  const hi = this[++offset] +
    this[++offset] * 2 ** 8 +
    this[++offset] * 2 ** 16 +
    last * 2 ** 24;

  return BigInt(lo) + (BigInt(hi) << 32n);
}

function readBigUInt64BE(offset = 0) {
  validateNumber(offset, \"offset\");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  const hi = first * 2 ** 24 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 8 +
    this[++offset];

  const lo = this[++offset] * 2 ** 24 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 8 +
    last;

  return (BigInt(hi) << 32n) + BigInt(lo);
}

function readBigInt64LE(offset = 0) {
  validateNumber(offset, \"offset\");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  const val = this[offset + 4] +
    this[offset + 5] * 2 ** 8 +
    this[offset + 6] * 2 ** 16 +
    (last << 24);
  return (BigInt(val) << 32n) +
    BigInt(first +
    this[++offset] * 2 ** 8 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 24);
}

function readBigInt64BE(offset = 0) {
  validateNumber(offset, \"offset\");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  const val = (first << 24) + // Overflow
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 8 +
    this[++offset];
  return (BigInt(val) << 32n) +
    BigInt(this[++offset] * 2 ** 24 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 8 +
    last);
}

function readUIntLE(offset, byteLength) {
  if (offset === undefined)
    throw invalidArgType(\"offset\", \"number\", offset);
  if (byteLength === 6)
    return readUInt48LE(this, offset);
  if (byteLength === 5)
    return readUInt40LE(this, offset);
  if (byteLength === 3)
    return readUInt24LE(this, offset);
  if (byteLength === 4)
    return readUInt32LE(this, offset);
  if (byteLength === 2)
    return readUInt16LE(this, offset);
  if (byteLength === 1)
    return readUInt8(this, offset);

  boundsError(byteLength, 6, \"byteLength\");
}

function readUInt48LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 5];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 6);

  return first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 24 +
    (buf[++offset] + last * 2 ** 8) * 2 ** 32;
}

function readUInt40LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 4];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 5);

  return first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 24 +
    last * 2 ** 32;
}

function readUInt32LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 4);

  return first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    last * 2 ** 24;
}

function readUInt24LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 2];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 3);

  return first + buf[++offset] * 2 ** 8 + last * 2 ** 16;
}

function readUInt16LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 1];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 2);

  return first + last * 2 ** 8;
}

function readUInt8(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const val = buf[offset];
  if (val === undefined)
    boundsError(offset, buf.length - 1);

  return val;
}

function readUIntBE(offset, byteLength) {
  if (offset === undefined)
    throw invalidArgType(\"offset\", \"number\", offset);
  if (byteLength === 6)
    return readUInt48BE(this, offset);
  if (byteLength === 5)
    return readUInt40BE(this, offset);
  if (byteLength === 3)
    return readUInt24BE(this, offset);
  if (byteLength === 4)
    return readUInt32BE(this, offset);
  if (byteLength === 2)
    return readUInt16BE(this, offset);
  if (byteLength === 1)
    return readUInt8(this, offset);

  boundsError(byteLength, 6, \"byteLength\");
}

function readUInt48BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 5];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 6);

  return (first * 2 ** 8 + buf[++offset]) * 2 ** 32 +
    buf[++offset] * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readUInt40BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 4];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 5);

  return first * 2 ** 32 +
    buf[++offset] * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readUInt32BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 4);

  return first * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readUInt24BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 2];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 3);

  return first * 2 ** 16 + buf[++offset] * 2 ** 8 + last;
}

function readUInt16BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 1];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 2);

  return first * 2 ** 8 + last;
}

function readIntLE(offset, byteLength) {
  if (offset === undefined)
    throw invalidArgType(\"offset\", \"number\", offset);
  if (byteLength === 6)
    return readInt48LE(this, offset);
  if (byteLength === 5)
    return readInt40LE(this, offset);
  if (byteLength === 3)
    return readInt24LE(this, offset);
  if (byteLength === 4)
    return readInt32LE(this, offset);
  if (byteLength === 2)
    return readInt16LE(this, offset);
  if (byteLength === 1)
    return readInt8(this, offset);

  boundsError(byteLength, 6, \"byteLength\");
}

function readInt48LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 5];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 6);

  const val = buf[offset + 4] + last * 2 ** 8;
  return (val | (val & 2 ** 15) * 0x1fffe) * 2 ** 32 +
    first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 24;
}

function readInt40LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 4];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 5);

  return (last | (last & 2 ** 7) * 0x1fffffe) * 2 ** 32 +
    first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 24;
}

function readInt32LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 4);

  return first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    (last << 24);
}

function readInt24LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 2];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 3);

  const val = first + buf[++offset] * 2 ** 8 + last * 2 ** 16;
  return val | (val & 2 ** 23) * 0x1fe;
}

function readInt16LE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 1];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 2);

  const val = first + last * 2 ** 8;
  return val | (val & 2 ** 15) * 0x1fffe;
}

function readInt8(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const val = buf[offset];
  if (val === undefined)
    boundsError(offset, buf.length - 1);

  return val | (val & 2 ** 7) * 0x1fffffe;
}

function readIntBE(offset, byteLength) {
  if (offset === undefined)
    throw invalidArgType(\"offset\", \"number\", offset);
  if (byteLength === 6)
    return readInt48BE(this, offset);
  if (byteLength === 5)
    return readInt40BE(this, offset);
  if (byteLength === 3)
    return readInt24BE(this, offset);
  if (byteLength === 4)
    return readInt32BE(this, offset);
  if (byteLength === 2)
    return readInt16BE(this, offset);
  if (byteLength === 1)
    return readInt8(this, offset);

  boundsError(byteLength, 6, \"byteLength\");
}

function readInt48BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 5];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 6);

  const val = buf[++offset] + first * 2 ** 8;
  return (val | (val & 2 ** 15) * 0x1fffe) * 2 ** 32 +
    buf[++offset] * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readInt40BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 4];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 5);

  return (first | (first & 2 ** 7) * 0x1fffffe) * 2 ** 32 +
    buf[++offset] * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readInt32BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 4);

  return (first << 24) + // Overflow
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readInt24BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 2];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 3);

  const val = first * 2 ** 16 + buf[++offset] * 2 ** 8 + last;
  return val | (val & 2 ** 23) * 0x1fe;
}

function readInt16BE(buf, offset = 0) {
  validateNumber(offset, \"offset\");
  const first = buf[offset];
  const last = buf[offset + 1];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 2);

  const val = first * 2 ** 8 + last;
  return val | (val & 2 ** 15) * 0x1fffe;
}

function readFloatBackwards(offset = 0) {
  validateNumber(offset, \"offset\");
  const first = this[offset];
  const last = this[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 4);

  uInt8Float32Array[3] = first;
  uInt8Float32Array[2] = this[++offset];
  uInt8Float32Array[1] = this[++offset];
  uInt8Float32Array[0] = last;
  return float32Array[0];
}

function readFloatForwards(offset = 0) {
  validateNumber(offset, \"offset\");
  const first = this[offset];
  const last = this[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 4);

  uInt8Float32Array[0] = first;
  uInt8Float32Array[1] = this[++offset];
  uInt8Float32Array[2] = this[++offset];
  uInt8Float32Array[3] = last;
  return float32Array[0];
}

function readDoubleBackwards(offset = 0) {
  validateNumber(offset, \"offset\");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  uInt8Float64Array[7] = first;
  uInt8Float64Array[6] = this[++offset];
  uInt8Float64Array[5] = this[++offset];
  uInt8Float64Array[4] = this[++offset];
  uInt8Float64Array[3] = this[++offset];
  uInt8Float64Array[2] = this[++offset];
  uInt8Float64Array[1] = this[++offset];
  uInt8Float64Array[0] = last;
  return float64Array[0];
}

function readDoubleForwards(offset = 0) {
  validateNumber(offset, \"offset\");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  uInt8Float64Array[0] = first;
  uInt8Float64Array[1] = this[++offset];
  uInt8Float64Array[2] = this[++offset];
  uInt8Float64Array[3] = this[++offset];
  uInt8Float64Array[4] = this[++offset];
  uInt8Float64Array[5] = this[++offset];
  uInt8Float64Array[6] = this[++offset];
  uInt8Float64Array[7] = last;
  return float64Array[0];
}

function writeBigU_Int64LE(buf, value, offset, min, max) {
  checkInt(value, min, max, buf, offset, 7);

  let lo = Number(value & 0xffffffffn);
  buf[offset++] = lo;
  lo = lo >> 8;
  buf[offset++] = lo;
  lo = lo >> 8;
  buf[offset++] = lo;
  lo = lo >> 8;
  buf[offset++] = lo;
  let hi = Number(value >> 32n & 0xffffffffn);
  buf[offset++] = hi;
  hi = hi >> 8;
  buf[offset++] = hi;
  hi = hi >> 8;
  buf[offset++] = hi;
  hi = hi >> 8;
  buf[offset++] = hi;
  return offset;
}

function writeBigUInt64LE(value, offset = 0) {
  return writeBigU_Int64LE(this, value, offset, 0n, 0xffffffffffffffffn);
}

function writeBigU_Int64BE(buf, value, offset, min, max) {
  checkInt(value, min, max, buf, offset, 7);

  let lo = Number(value & 0xffffffffn);
  buf[offset + 7] = lo;
  lo = lo >> 8;
  buf[offset + 6] = lo;
  lo = lo >> 8;
  buf[offset + 5] = lo;
  lo = lo >> 8;
  buf[offset + 4] = lo;
  let hi = Number(value >> 32n & 0xffffffffn);
  buf[offset + 3] = hi;
  hi = hi >> 8;
  buf[offset + 2] = hi;
  hi = hi >> 8;
  buf[offset + 1] = hi;
  hi = hi >> 8;
  buf[offset] = hi;
  return offset + 8;
}

function writeBigUInt64BE(value, offset = 0) {
  return writeBigU_Int64BE(this, value, offset, 0n, 0xffffffffffffffffn);
}

function writeBigInt64LE(value, offset = 0) {
  return writeBigU_Int64LE(
    this, value, offset, -0x8000000000000000n, 0x7fffffffffffffffn);
}

function writeBigInt64BE(value, offset = 0) {
  return writeBigU_Int64BE(
    this, value, offset, -0x8000000000000000n, 0x7fffffffffffffffn);
}

function writeUIntLE(value, offset, byteLength) {
  if (byteLength === 6)
    return writeU_Int48LE(this, value, offset, 0, 0xffffffffffff);
  if (byteLength === 5)
    return writeU_Int40LE(this, value, offset, 0, 0xffffffffff);
  if (byteLength === 3)
    return writeU_Int24LE(this, value, offset, 0, 0xffffff);
  if (byteLength === 4)
    return writeU_Int32LE(this, value, offset, 0, 0xffffffff);
  if (byteLength === 2)
    return writeU_Int16LE(this, value, offset, 0, 0xffff);
  if (byteLength === 1)
    return writeU_Int8(this, value, offset, 0, 0xff);

  boundsError(byteLength, 6, \"byteLength\");
}

function writeU_Int48LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 5);

  const newVal = Math.floor(value * 2 ** -32);
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  buf[offset++] = newVal;
  buf[offset++] = (newVal >>> 8);
  return offset;
}

function writeU_Int40LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 4);

  const newVal = value;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  buf[offset++] = Math.floor(newVal * 2 ** -32);
  return offset;
}

function writeU_Int32LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 3);

  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  return offset;
}

function writeUInt32LE(value, offset = 0) {
  return writeU_Int32LE(this, value, offset, 0, 0xffffffff);
}

function writeU_Int24LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 2);

  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  return offset;
}

function writeU_Int16LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 1);

  buf[offset++] = value;
  buf[offset++] = (value >>> 8);
  return offset;
}

function writeUInt16LE(value, offset = 0) {
  return writeU_Int16LE(this, value, offset, 0, 0xffff);
}

function writeU_Int8(buf, value, offset, min, max) {
  value = +value;
  validateNumber(offset, \"offset\");
  if (value > max || value < min) {
    throw outOfRange(\"value\", `>= ${min} and <= ${max}`, value);
  }
  if (buf[offset] === undefined)
    boundsError(offset, buf.length - 1);

  buf[offset] = value;
  return offset + 1;
}

function writeUInt8(value, offset = 0) {
  return writeU_Int8(this, value, offset, 0, 0xff);
}

function writeUIntBE(value, offset, byteLength) {
  if (byteLength === 6)
    return writeU_Int48BE(this, value, offset, 0, 0xffffffffffff);
  if (byteLength === 5)
    return writeU_Int40BE(this, value, offset, 0, 0xffffffffff);
  if (byteLength === 3)
    return writeU_Int24BE(this, value, offset, 0, 0xffffff);
  if (byteLength === 4)
    return writeU_Int32BE(this, value, offset, 0, 0xffffffff);
  if (byteLength === 2)
    return writeU_Int16BE(this, value, offset, 0, 0xffff);
  if (byteLength === 1)
    return writeU_Int8(this, value, offset, 0, 0xff);

  boundsError(byteLength, 6, \"byteLength\");
}

function writeU_Int48BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 5);

  const newVal = Math.floor(value * 2 ** -32);
  buf[offset++] = (newVal >>> 8);
  buf[offset++] = newVal;
  buf[offset + 3] = value;
  value = value >>> 8;
  buf[offset + 2] = value;
  value = value >>> 8;
  buf[offset + 1] = value;
  value = value >>> 8;
  buf[offset] = value;
  return offset + 4;
}

function writeU_Int40BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 4);

  buf[offset++] = Math.floor(value * 2 ** -32);
  buf[offset + 3] = value;
  value = value >>> 8;
  buf[offset + 2] = value;
  value = value >>> 8;
  buf[offset + 1] = value;
  value = value >>> 8;
  buf[offset] = value;
  return offset + 4;
}

function writeU_Int32BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 3);

  buf[offset + 3] = value;
  value = value >>> 8;
  buf[offset + 2] = value;
  value = value >>> 8;
  buf[offset + 1] = value;
  value = value >>> 8;
  buf[offset] = value;
  return offset + 4;
}

function writeUInt32BE(value, offset = 0) {
  return writeU_Int32BE(this, value, offset, 0, 0xffffffff);
}

function writeU_Int24BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 2);

  buf[offset + 2] = value;
  value = value >>> 8;
  buf[offset + 1] = value;
  value = value >>> 8;
  buf[offset] = value;
  return offset + 3;
}

function writeU_Int16BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 1);

  buf[offset++] = (value >>> 8);
  buf[offset++] = value;
  return offset;
}

function writeUInt16BE(value, offset = 0) {
  return writeU_Int16BE(this, value, offset, 0, 0xffff);
}

function writeIntLE(value, offset, byteLength) {
  if (byteLength === 6)
    return writeU_Int48LE(this, value, offset, -0x800000000000, 0x7fffffffffff);
  if (byteLength === 5)
    return writeU_Int40LE(this, value, offset, -0x8000000000, 0x7fffffffff);
  if (byteLength === 3)
    return writeU_Int24LE(this, value, offset, -0x800000, 0x7fffff);
  if (byteLength === 4)
    return writeU_Int32LE(this, value, offset, -0x80000000, 0x7fffffff);
  if (byteLength === 2)
    return writeU_Int16LE(this, value, offset, -0x8000, 0x7fff);
  if (byteLength === 1)
    return writeU_Int8(this, value, offset, -0x80, 0x7f);

  boundsError(byteLength, 6, \"byteLength\");
}

function writeInt32LE(value, offset = 0) {
  return writeU_Int32LE(this, value, offset, -0x80000000, 0x7fffffff);
}

function writeInt16LE(value, offset = 0) {
  return writeU_Int16LE(this, value, offset, -0x8000, 0x7fff);
}

function writeInt8(value, offset = 0) {
  return writeU_Int8(this, value, offset, -0x80, 0x7f);
}

function writeIntBE(value, offset, byteLength) {
  if (byteLength === 6)
    return writeU_Int48BE(this, value, offset, -0x800000000000, 0x7fffffffffff);
  if (byteLength === 5)
    return writeU_Int40BE(this, value, offset, -0x8000000000, 0x7fffffffff);
  if (byteLength === 3)
    return writeU_Int24BE(this, value, offset, -0x800000, 0x7fffff);
  if (byteLength === 4)
    return writeU_Int32BE(this, value, offset, -0x80000000, 0x7fffffff);
  if (byteLength === 2)
    return writeU_Int16BE(this, value, offset, -0x8000, 0x7fff);
  if (byteLength === 1)
    return writeU_Int8(this, value, offset, -0x80, 0x7f);

  boundsError(byteLength, 6, \"byteLength\");
}

function writeInt32BE(value, offset = 0) {
  return writeU_Int32BE(this, value, offset, -0x80000000, 0x7fffffff);
}

function writeInt16BE(value, offset = 0) {
  return writeU_Int16BE(this, value, offset, -0x8000, 0x7fff);
}

function writeDoubleForwards(val, offset = 0) {
  val = +val;
  checkBounds(this, offset, 7);

  float64Array[0] = val;
  this[offset++] = uInt8Float64Array[0];
  this[offset++] = uInt8Float64Array[1];
  this[offset++] = uInt8Float64Array[2];
  this[offset++] = uInt8Float64Array[3];
  this[offset++] = uInt8Float64Array[4];
  this[offset++] = uInt8Float64Array[5];
  this[offset++] = uInt8Float64Array[6];
  this[offset++] = uInt8Float64Array[7];
  return offset;
}

function writeDoubleBackwards(val, offset = 0) {
  val = +val;
  checkBounds(this, offset, 7);

  float64Array[0] = val;
  this[offset++] = uInt8Float64Array[7];
  this[offset++] = uInt8Float64Array[6];
  this[offset++] = uInt8Float64Array[5];
  this[offset++] = uInt8Float64Array[4];
  this[offset++] = uInt8Float64Array[3];
  this[offset++] = uInt8Float64Array[2];
  this[offset++] = uInt8Float64Array[1];
  this[offset++] = uInt8Float64Array[0];
  return offset;
}

function writeFloatForwards(val, offset = 0) {
  val = +val;
  checkBounds(this, offset, 3);

  float32Array[0] = val;
  this[offset++] = uInt8Float32Array[0];
  this[offset++] = uInt8Float32Array[1];
  this[offset++] = uInt8Float32Array[2];
  this[offset++] = uInt8Float32Array[3];
  return offset;
}

function writeFloatBackwards(val, offset = 0) {
  val = +val;
  checkBounds(this, offset, 3);

  float32Array[0] = val;
  this[offset++] = uInt8Float32Array[3];
  this[offset++] = uInt8Float32Array[2];
  this[offset++] = uInt8Float32Array[1];
  this[offset++] = uInt8Float32Array[0];
  return offset;
}

const proto = Buffer.prototype;
proto.readBigUInt64LE = readBigUInt64LE;
proto.readBigUInt64BE = readBigUInt64BE;
proto.readBigUint64LE = readBigUInt64LE;
proto.readBigUint64BE = readBigUInt64BE;
proto.readBigInt64LE = readBigInt64LE;
proto.readBigInt64BE = readBigInt64BE;
proto.writeBigUInt64LE = writeBigUInt64LE;
proto.writeBigUInt64BE = writeBigUInt64BE;
proto.writeBigUint64LE = writeBigUInt64LE;
proto.writeBigUint64BE = writeBigUInt64BE;
proto.writeBigInt64LE = writeBigInt64LE;
proto.writeBigInt64BE = writeBigInt64BE;
proto.readUIntLE = readUIntLE;
proto.readUInt32LE = function(offset) { return readUInt32LE(this, offset); };
proto.readUInt16LE = function(offset) { return readUInt16LE(this, offset); };
proto.readUInt8 = function(offset) { return readUInt8(this, offset); };
proto.readUIntBE = readUIntBE;
proto.readUInt32BE = function(offset) { return readUInt32BE(this, offset); };
proto.readUInt16BE = function(offset) { return readUInt16BE(this, offset); };
proto.readUintLE = readUIntLE;
proto.readUint32LE = proto.readUInt32LE;
proto.readUint16LE = proto.readUInt16LE;
proto.readUint8 = proto.readUInt8;
proto.readUintBE = readUIntBE;
proto.readUint32BE = proto.readUInt32BE;
proto.readUint16BE = proto.readUInt16BE;
proto.readIntLE = readIntLE;
proto.readInt32LE = function(offset) { return readInt32LE(this, offset); };
proto.readInt16LE = function(offset) { return readInt16LE(this, offset); };
proto.readInt8 = function(offset) { return readInt8(this, offset); };
proto.readIntBE = readIntBE;
proto.readInt32BE = function(offset) { return readInt32BE(this, offset); };
proto.readInt16BE = function(offset) { return readInt16BE(this, offset); };
proto.writeUIntLE = writeUIntLE;
proto.writeUInt32LE = writeUInt32LE;
proto.writeUInt16LE = writeUInt16LE;
proto.writeUInt8 = writeUInt8;
proto.writeUIntBE = writeUIntBE;
proto.writeUInt32BE = writeUInt32BE;
proto.writeUInt16BE = writeUInt16BE;
proto.writeUintLE = writeUIntLE;
proto.writeUint32LE = writeUInt32LE;
proto.writeUint16LE = writeUInt16LE;
proto.writeUint8 = writeUInt8;
proto.writeUintBE = writeUIntBE;
proto.writeUint32BE = writeUInt32BE;
proto.writeUint16BE = writeUInt16BE;
proto.writeIntLE = writeIntLE;
proto.writeInt32LE = writeInt32LE;
proto.writeInt16LE = writeInt16LE;
proto.writeInt8 = writeInt8;
proto.writeIntBE = writeIntBE;
proto.writeInt32BE = writeInt32BE;
proto.writeInt16BE = writeInt16BE;
proto.readFloatLE = bigEndian ? readFloatBackwards : readFloatForwards;
proto.readFloatBE = bigEndian ? readFloatForwards : readFloatBackwards;
proto.readDoubleLE = bigEndian ? readDoubleBackwards : readDoubleForwards;
proto.readDoubleBE = bigEndian ? readDoubleForwards : readDoubleBackwards;
proto.writeFloatLE = bigEndian ? writeFloatBackwards : writeFloatForwards;
proto.writeFloatBE = bigEndian ? writeFloatForwards : writeFloatBackwards;
proto.writeDoubleLE = bigEndian ? writeDoubleBackwards : writeDoubleForwards;
proto.writeDoubleBE = bigEndian ? writeDoubleForwards : writeDoubleBackwards;
proto.asciiSlice = function (start, end) {
  return asciiSlice(this, start, end);
};
proto.base64Slice = function (start, end) {
  return base64Slice(this, start, end);
};
proto.base64urlSlice = function (start, end) {
  return base64urlSlice(this, start, end);
};
proto.latin1Slice = function (start, end) {
  return latin1Slice(this, start, end);
};
proto.hexSlice = function (start, end) {
  return hexSlice(this, start, end);
};
proto.ucs2Slice = function (start, end) {
  return ucs2Slice(this, start, end);
};
proto.utf8Slice = function (start, end) {
  return utf8Slice(this, start, end);
};
proto.asciiWrite = function (string, offset, length) {
  return asciiWrite(this, string, offset, length);
};
proto.base64Write = function (string, offset, length) {
  return base64Write(this, string, offset, length);
};
proto.base64urlWrite = function (string, offset, length) {
  return base64urlWrite(this, string, offset, length);
};
proto.latin1Write = function (string, offset, length) {
  return latin1Write(this, string, offset, length);
};
proto.hexWrite = function (string, offset, length) {
  return hexWrite(this, string, offset, length);
};
proto.ucs2Write = function (string, offset, length) {
  return ucs2Write(this, string, offset, length);
};
proto.utf8Write = function (string, offset, length) {
  return utf8Write(this, string, offset, length);
};

function btoa(input) {
  if (arguments.length === 0) throw missingArgs(\"input\");
  const text = `${input}`;
  if (!stringIsLatin1(text)) {
    throw new DOMException(\"Invalid character\", \"InvalidCharacterError\");
  }
  return decode(encode(text, \"latin1\"), \"base64\", 0, text.length);
}

function atob(input) {
  if (arguments.length === 0) throw missingArgs(\"input\");

  const result = decodeBase64Strict(`${input}`);

  switch (result) {
    case -2: // Invalid character
      throw new DOMException(\"Invalid character\", \"InvalidCharacterError\");
    case -1: // Single character remained
      throw new DOMException(
        \"The string to be decoded is not correctly encoded.\",
        \"InvalidCharacterError\",
      );
    default:
      return result;
  }
}

function isUtf8(input) {
  if (isTypedArray(input) || isAnyArrayBuffer(input)) {
    return bytesAreUtf8(input);
  }

  throw invalidArgType(\"input\", [\"ArrayBuffer\", \"Buffer\", \"TypedArray\"], input);
}

function isAscii(input) {
  if (isTypedArray(input) || isAnyArrayBuffer(input)) {
    return bytesAreAscii(input);
  }

  throw invalidArgType(\"input\", [\"ArrayBuffer\", \"Buffer\", \"TypedArray\"], input);
}

function isLatin1(input) {
  validateString(input, \"input\");
  return stringIsLatin1(input);
}

function stringLength(input, encoding = \"utf8\") {
  if (!isTypedArray(input) && !isAnyArrayBuffer(input)) {
    throw invalidArgType(
      \"input\",
      [\"ArrayBuffer\", \"Buffer\", \"TypedArray\"],
      input,
    );
  }
  validateString(encoding, \"encoding\");
  const ops = getEncodingOps(encoding);
  if (ops === undefined) throw unknownEncoding(encoding);
  const length = input.byteLength;
  switch (ops.encodingVal) {
    case \"utf8\":
      return length === 0 ? 0 : utf16Units(input);
    case \"utf16le\":
      return Math.floor(length / 2);
    case \"hex\":
      return length * 2;
    case \"base64\":
      return Math.ceil(length / 3) * 4;
    case \"base64url\":
      return Math.ceil((length * 4) / 3);
    default: // latin1, ascii
      return length;
  }
}

module.exports = {
  Buffer,
  isUtf8,
  isAscii,
  isLatin1,
  kMaxLength,
  kStringMaxLength,
  btoa,
  atob,
};

Object.defineProperties(module.exports, {
  constants: {
    __proto__: null,
    configurable: false,
    enumerable: true,
    value: constants,
  },
  INSPECT_MAX_BYTES: {
    __proto__: null,
    configurable: true,
    enumerable: true,
    get() {
      return INSPECT_MAX_BYTES;
    },
    set(val) {
      validateNumber(val, \"INSPECT_MAX_BYTES\", 0);
      INSPECT_MAX_BYTES = val;
    },
  },
});
"

pub const path = "\"use strict\";

// node's lib/path.js. matchesGlob is not here yet

const { error, validateObject, validateString } = require(\"spark:errors\");
const process = require(\"node:process\");

const CHAR_UPPERCASE_A = 65;
const CHAR_LOWERCASE_A = 97;
const CHAR_UPPERCASE_Z = 90;
const CHAR_LOWERCASE_Z = 122;
const CHAR_DOT = 46;
const CHAR_FORWARD_SLASH = 47;
const CHAR_BACKWARD_SLASH = 92;
const CHAR_COLON = 58;
const CHAR_QUESTION_MARK = 63;

const isWindows = process.platform === \"win32\";

function matchesGlobMissing() {
  return error(
    \"ERR_METHOD_NOT_IMPLEMENTED\",
    \"The matchesGlob() method is not implemented\",
  );
}

function isPathSeparator(code) {
  return code === CHAR_FORWARD_SLASH || code === CHAR_BACKWARD_SLASH;
}

function isPosixPathSeparator(code) {
  return code === CHAR_FORWARD_SLASH;
}

const WINDOWS_RESERVED_NAMES = [
  \"CON\", \"PRN\", \"AUX\", \"NUL\",
  \"COM1\", \"COM2\", \"COM3\", \"COM4\", \"COM5\", \"COM6\", \"COM7\", \"COM8\", \"COM9\",
  \"LPT1\", \"LPT2\", \"LPT3\", \"LPT4\", \"LPT5\", \"LPT6\", \"LPT7\", \"LPT8\", \"LPT9\",
  \"COM\\xb9\", \"COM\\xb2\", \"COM\\xb3\",
  \"LPT\\xb9\", \"LPT\\xb2\", \"LPT\\xb3\",
];

function isWindowsReservedName(path, colonIndex) {
  const devicePart = path.slice(0, colonIndex).toUpperCase();
  return WINDOWS_RESERVED_NAMES.includes(devicePart);
}

function isWindowsDeviceRoot(code) {
  return (code >= CHAR_UPPERCASE_A && code <= CHAR_UPPERCASE_Z) ||
         (code >= CHAR_LOWERCASE_A && code <= CHAR_LOWERCASE_Z);
}

function normalizeString(path, allowAboveRoot, separator, isPathSeparator) {
  let res = \"\";
  let lastSegmentLength = 0;
  let lastSlash = -1;
  let dots = 0;
  let code = 0;
  for (let i = 0; i <= path.length; ++i) {
    if (i < path.length)
      code = path.charCodeAt(i);
    else if (isPathSeparator(code))
      break;
    else
      code = CHAR_FORWARD_SLASH;

    if (isPathSeparator(code)) {
      if (lastSlash === i - 1 || dots === 1) {
      } else if (dots === 2) {
        if (res.length < 2 || lastSegmentLength !== 2 ||
            res.charCodeAt(res.length - 1) !== CHAR_DOT ||
            res.charCodeAt(res.length - 2) !== CHAR_DOT) {
          if (res.length > 2) {
            const lastSlashIndex = res.length - lastSegmentLength - 1;
            if (lastSlashIndex === -1) {
              res = \"\";
              lastSegmentLength = 0;
            } else {
              res = res.slice(0, lastSlashIndex);
              lastSegmentLength =
                res.length - 1 - res.lastIndexOf(separator);
            }
            lastSlash = i;
            dots = 0;
            continue;
          } else if (res.length !== 0) {
            res = \"\";
            lastSegmentLength = 0;
            lastSlash = i;
            dots = 0;
            continue;
          }
        }
        if (allowAboveRoot) {
          res += res.length > 0 ? `${separator}..` : \"..\";
          lastSegmentLength = 2;
        }
      } else {
        if (res.length > 0)
          res += `${separator}${path.slice(lastSlash + 1, i)}`;
        else
          res = path.slice(lastSlash + 1, i);
        lastSegmentLength = i - lastSlash - 1;
      }
      lastSlash = i;
      dots = 0;
    } else if (code === CHAR_DOT && dots !== -1) {
      ++dots;
    } else {
      dots = -1;
    }
  }
  return res;
}

function formatExt(ext) {
  return ext ? `${ext[0] === \".\" ? \"\" : \".\"}${ext}` : \"\";
}

function _format(sep, pathObject) {
  validateObject(pathObject, \"pathObject\");
  const dir = pathObject.dir || pathObject.root;
  const base = pathObject.base ||
    `${pathObject.name || \"\"}${formatExt(pathObject.ext)}`;
  if (!dir) {
    return base;
  }
  return dir === pathObject.root ? `${dir}${base}` : `${dir}${sep}${base}`;
}

const forwardSlashRegExp = /\\//g;

const win32 = {
    resolve(...args) {
    let resolvedDevice = \"\";
    let resolvedTail = \"\";
    let resolvedAbsolute = false;

    for (let i = args.length - 1; i >= -1; i--) {
      let path;
      if (i >= 0) {
        path = args[i];
        validateString(path, `paths[${i}]`);

        if (path.length === 0) {
          continue;
        }
      } else if (resolvedDevice.length === 0) {
        path = process.cwd();
        if (args.length === 0 || ((args.length === 1 && (args[0] === \"\" || args[0] === \".\")) &&
            isPathSeparator(path.charCodeAt(0)))) {
          if (!isWindows) {
            path = path.replace(forwardSlashRegExp, \"\\\\\");
          }
          return path;
        }
      } else {
        path = process.env[`=${resolvedDevice}`] || process.cwd();

        if (path === undefined ||
            (path.slice(0, 2).toLowerCase() !==
            resolvedDevice.toLowerCase() &&
            path.charCodeAt(2) === CHAR_BACKWARD_SLASH)) {
          path = `${resolvedDevice}\\\\`;
        }
      }

      const len = path.length;
      let rootEnd = 0;
      let device = \"\";
      let isAbsolute = false;
      const code = path.charCodeAt(0);

      if (len === 1) {
        if (isPathSeparator(code)) {
          rootEnd = 1;
          isAbsolute = true;
        }
      } else if (isPathSeparator(code)) {

        isAbsolute = true;

        if (isPathSeparator(path.charCodeAt(1))) {
          let j = 2;
          let last = j;
          while (j < len &&
                 !isPathSeparator(path.charCodeAt(j))) {
            j++;
          }
          if (j < len && j !== last) {
            const firstPart = path.slice(last, j);
            last = j;
            while (j < len &&
                   isPathSeparator(path.charCodeAt(j))) {
              j++;
            }
            if (j < len && j !== last) {
              last = j;
              while (j < len &&
                     !isPathSeparator(path.charCodeAt(j))) {
                j++;
              }
              if (j === len || j !== last) {
                if (firstPart !== \".\" && firstPart !== \"?\") {
                  device =
                    `\\\\\\\\${firstPart}\\\\${path.slice(last, j)}`;
                  rootEnd = j;
                } else {
                  device = `\\\\\\\\${firstPart}`;
                  rootEnd = 4;
                }
              }
            }
          }
        } else {
          rootEnd = 1;
        }
      } else if (isWindowsDeviceRoot(code) &&
                  path.charCodeAt(1) === CHAR_COLON) {
        device = path.slice(0, 2);
        rootEnd = 2;
        if (len > 2 && isPathSeparator(path.charCodeAt(2))) {
          isAbsolute = true;
          rootEnd = 3;
        }
      }

      if (device.length > 0) {
        if (resolvedDevice.length > 0) {
          if (device.toLowerCase() !==
              resolvedDevice.toLowerCase())
            continue;
        } else {
          resolvedDevice = device;
        }
      }

      if (resolvedAbsolute) {
        if (resolvedDevice.length > 0)
          break;
      } else {
        resolvedTail =
          `${path.slice(rootEnd)}\\\\${resolvedTail}`;
        resolvedAbsolute = isAbsolute;
        if (isAbsolute && resolvedDevice.length > 0) {
          break;
        }
      }
    }

    resolvedTail = normalizeString(resolvedTail, !resolvedAbsolute, \"\\\\\",
                                   isPathSeparator);

    return resolvedAbsolute ?
      `${resolvedDevice}\\\\${resolvedTail}` :
      `${resolvedDevice}${resolvedTail}` || \".\";
  },

    normalize(path) {
    validateString(path, \"path\");
    const len = path.length;
    if (len === 0)
      return \".\";
    let rootEnd = 0;
    let device;
    let isAbsolute = false;
    const code = path.charCodeAt(0);

    if (len === 1) {
      return isPosixPathSeparator(code) ? \"\\\\\" : path;
    }
    if (isPathSeparator(code)) {

      isAbsolute = true;

      if (isPathSeparator(path.charCodeAt(1))) {
        let j = 2;
        let last = j;
        while (j < len &&
               !isPathSeparator(path.charCodeAt(j))) {
          j++;
        }
        if (j < len && j !== last) {
          const firstPart = path.slice(last, j);
          last = j;
          while (j < len &&
                 isPathSeparator(path.charCodeAt(j))) {
            j++;
          }
          if (j < len && j !== last) {
            last = j;
            while (j < len &&
                   !isPathSeparator(path.charCodeAt(j))) {
              j++;
            }
            if (j === len || j !== last) {
              if (firstPart === \".\" || firstPart === \"?\") {
                device = `\\\\\\\\${firstPart}`;
                rootEnd = 4;
                const colonIndex = path.indexOf(\":\");
                const possibleDevice = path.slice(4, colonIndex + 1);
                if (isWindowsReservedName(possibleDevice, possibleDevice.length - 1)) {
                  device = `\\\\\\\\?\\\\${possibleDevice}`;
                  rootEnd = 4 + possibleDevice.length;
                }
              } else if (j === len) {
                return `\\\\\\\\${firstPart}\\\\${path.slice(last)}\\\\`;
              } else {
                device =
                  `\\\\\\\\${firstPart}\\\\${path.slice(last, j)}`;
                rootEnd = j;
              }
            }
          }
        }
      } else {
        rootEnd = 1;
      }
    } else {
      const colonIndex = path.indexOf(\":\");
      if (colonIndex > 0) {
        if (isWindowsDeviceRoot(code) && colonIndex === 1) {
          device = path.slice(0, 2);
          rootEnd = 2;
          if (len > 2 && isPathSeparator(path.charCodeAt(2))) {
            isAbsolute = true;
            rootEnd = 3;
          }
        } else if (isWindowsReservedName(path, colonIndex)) {
          device = path.slice(0, colonIndex + 1);
          rootEnd = colonIndex + 1;

        }
      }
    }

    let tail = rootEnd < len ?
      normalizeString(path.slice(rootEnd),
                      !isAbsolute, \"\\\\\", isPathSeparator) :
      \"\";
    if (tail.length === 0 && !isAbsolute)
      tail = \".\";
    if (tail.length > 0 &&
        isPathSeparator(path.charCodeAt(len - 1)))
      tail += \"\\\\\";
    if (!isAbsolute && device === undefined && path.includes(\":\")) {
      if (tail.length >= 2 &&
          isWindowsDeviceRoot(tail.charCodeAt(0)) &&
          tail.charCodeAt(1) === CHAR_COLON) {
        return `.\\\\${tail}`;
      }
      let index = path.indexOf(\":\");

      do {
        if (index === len - 1 || isPathSeparator(path.charCodeAt(index + 1))) {
          return `.\\\\${tail}`;
        }
      } while ((index = path.indexOf(\":\", index + 1)) !== -1);
    }
    const colonIndex = path.indexOf(\":\");
    if (isWindowsReservedName(path, colonIndex)) {
      return `.\\\\${device ?? \"\"}${tail}`;
    }
    if (device === undefined) {
      return isAbsolute ? `\\\\${tail}` : tail;
    }
    return isAbsolute ? `${device}\\\\${tail}` : `${device}${tail}`;
  },

    isAbsolute(path) {
    validateString(path, \"path\");
    const len = path.length;
    if (len === 0)
      return false;

    const code = path.charCodeAt(0);
    return isPathSeparator(code) ||
      (len > 2 &&
      isWindowsDeviceRoot(code) &&
      path.charCodeAt(1) === CHAR_COLON &&
      isPathSeparator(path.charCodeAt(2)));
  },

    join(...args) {
    if (args.length === 0)
      return \".\";

    const path = [];
    for (let i = 0; i < args.length; ++i) {
      const arg = args[i];
      validateString(arg, \"path\");
      if (arg.length > 0) {
        path.push(arg);
      }
    }

    if (path.length === 0)
      return \".\";

    const firstPart = path[0];
    let joined = path.join(\"\\\\\");

    let needsReplace = true;
    let slashCount = 0;
    if (isPathSeparator(firstPart.charCodeAt(0))) {
      ++slashCount;
      const firstLen = firstPart.length;
      if (firstLen > 1 &&
          isPathSeparator(firstPart.charCodeAt(1))) {
        ++slashCount;
        if (firstLen > 2) {
          if (isPathSeparator(firstPart.charCodeAt(2)))
            ++slashCount;
          else {
            needsReplace = false;
          }
        }
      }
    }
    if (needsReplace) {
      while (slashCount < joined.length &&
             isPathSeparator(joined.charCodeAt(slashCount))) {
        slashCount++;
      }

      if (slashCount >= 2)
        joined = `\\\\${joined.slice(slashCount)}`;
    }

    const parts = [];
    let part = \"\";

    for (let i = 0; i < joined.length; i++) {
      if (joined[i] === \"\\\\\") {
        if (part) parts.push(part);
        part = \"\";
        while (i + 1 < joined.length && joined[i + 1] === \"\\\\\") i++;
      } else {
        part += joined[i];
      }
    }
    if (part) parts.push(part);

    if (parts.some((p) => {
      const colonIndex = p.indexOf(\":\");
      return colonIndex !== -1 && isWindowsReservedName(p, colonIndex);
    })) {
      let result = \"\";
      for (let i = 0; i < joined.length; i++) {
        result += joined[i] === \"/\" ? \"\\\\\" : joined[i];
      }
      return result;
    }

    return win32.normalize(joined);
  },

    relative(from, to) {
    validateString(from, \"from\");
    validateString(to, \"to\");

    if (from === to)
      return \"\";

    const fromOrig = win32.resolve(from);
    const toOrig = win32.resolve(to);

    if (fromOrig === toOrig)
      return \"\";

    from = fromOrig.toLowerCase();
    to = toOrig.toLowerCase();

    if (from === to)
      return \"\";

    if (fromOrig.length !== from.length || toOrig.length !== to.length) {
      const fromSplit = fromOrig.split(\"\\\\\");
      const toSplit = toOrig.split(\"\\\\\");
      if (fromSplit[fromSplit.length - 1] === \"\") {
        fromSplit.pop();
      }
      if (toSplit[toSplit.length - 1] === \"\") {
        toSplit.pop();
      }

      const fromLen = fromSplit.length;
      const toLen = toSplit.length;
      const length = fromLen < toLen ? fromLen : toLen;

      let i;
      for (i = 0; i < length; i++) {
        if (fromSplit[i].toLowerCase() !== toSplit[i].toLowerCase()) {
          break;
        }
      }

      if (i === 0) {
        return toOrig;
      } else if (i === length) {
        if (toLen > length) {
          return toSplit.slice(i).join(\"\\\\\");
        }
        if (fromLen > length) {
          return \"..\\\\\".repeat(fromLen - 1 - i) + \"..\";
        }
        return \"\";
      }

      return \"..\\\\\".repeat(fromLen - i) + toSplit.slice(i).join(\"\\\\\");
    }

    let fromStart = 0;
    while (fromStart < from.length &&
           from.charCodeAt(fromStart) === CHAR_BACKWARD_SLASH) {
      fromStart++;
    }
    let fromEnd = from.length;
    while (
      fromEnd - 1 > fromStart &&
      from.charCodeAt(fromEnd - 1) === CHAR_BACKWARD_SLASH
    ) {
      fromEnd--;
    }
    const fromLen = fromEnd - fromStart;

    let toStart = 0;
    while (toStart < to.length &&
           to.charCodeAt(toStart) === CHAR_BACKWARD_SLASH) {
      toStart++;
    }
    let toEnd = to.length;
    while (toEnd - 1 > toStart &&
           to.charCodeAt(toEnd - 1) === CHAR_BACKWARD_SLASH) {
      toEnd--;
    }
    const toLen = toEnd - toStart;

    const length = fromLen < toLen ? fromLen : toLen;
    let lastCommonSep = -1;
    let i = 0;
    for (; i < length; i++) {
      const fromCode = from.charCodeAt(fromStart + i);
      if (fromCode !== to.charCodeAt(toStart + i))
        break;
      else if (fromCode === CHAR_BACKWARD_SLASH)
        lastCommonSep = i;
    }

    if (i !== length) {
      if (lastCommonSep === -1)
        return toOrig;
    } else {
      if (toLen > length) {
        if (to.charCodeAt(toStart + i) ===
            CHAR_BACKWARD_SLASH) {
          return toOrig.slice(toStart + i + 1);
        }
        if (i === 2) {
          return toOrig.slice(toStart + i);
        }
      }
      if (fromLen > length) {
        if (from.charCodeAt(fromStart + i) ===
            CHAR_BACKWARD_SLASH) {
          lastCommonSep = i;
        } else if (i === 2) {
          lastCommonSep = 3;
        }
      }
      if (lastCommonSep === -1)
        lastCommonSep = 0;
    }

    let out = \"\";
    for (i = fromStart + lastCommonSep + 1; i <= fromEnd; ++i) {
      if (i === fromEnd ||
          from.charCodeAt(i) === CHAR_BACKWARD_SLASH) {
        out += out.length === 0 ? \"..\" : \"\\\\..\";
      }
    }

    toStart += lastCommonSep;

    if (out.length > 0)
      return `${out}${toOrig.slice(toStart, toEnd)}`;

    if (toOrig.charCodeAt(toStart) === CHAR_BACKWARD_SLASH)
      ++toStart;
    return toOrig.slice(toStart, toEnd);
  },

    toNamespacedPath(path) {
    if (typeof path !== \"string\" || path.length === 0)
      return path;

    const resolvedPath = win32.resolve(path);

    if (resolvedPath.length <= 2)
      return path;

    if (resolvedPath.charCodeAt(0) === CHAR_BACKWARD_SLASH) {
      if (resolvedPath.charCodeAt(1) === CHAR_BACKWARD_SLASH) {
        const code = resolvedPath.charCodeAt(2);
        if (code !== CHAR_QUESTION_MARK && code !== CHAR_DOT) {
          return `\\\\\\\\?\\\\UNC\\\\${resolvedPath.slice(2)}`;
        }
      }
    } else if (
      isWindowsDeviceRoot(resolvedPath.charCodeAt(0)) &&
      resolvedPath.charCodeAt(1) === CHAR_COLON &&
      resolvedPath.charCodeAt(2) === CHAR_BACKWARD_SLASH
    ) {
      return `\\\\\\\\?\\\\${resolvedPath}`;
    }

    return resolvedPath;
  },

    dirname(path) {
    validateString(path, \"path\");
    const len = path.length;
    if (len === 0)
      return \".\";
    let rootEnd = -1;
    let offset = 0;
    const code = path.charCodeAt(0);

    if (len === 1) {
      return isPathSeparator(code) ? path : \".\";
    }

    if (isPathSeparator(code)) {

      rootEnd = offset = 1;

      if (isPathSeparator(path.charCodeAt(1))) {
        let j = 2;
        let last = j;
        while (j < len &&
               !isPathSeparator(path.charCodeAt(j))) {
          j++;
        }
        if (j < len && j !== last) {
          last = j;
          while (j < len &&
                 isPathSeparator(path.charCodeAt(j))) {
            j++;
          }
          if (j < len && j !== last) {
            last = j;
            while (j < len &&
                   !isPathSeparator(path.charCodeAt(j))) {
              j++;
            }
            if (j === len) {
              return path;
            }
            if (j !== last) {

              rootEnd = offset = j + 1;
            }
          }
        }
      }
    } else if (isWindowsDeviceRoot(code) &&
               path.charCodeAt(1) === CHAR_COLON) {
      rootEnd =
        len > 2 && isPathSeparator(path.charCodeAt(2)) ? 3 : 2;
      offset = rootEnd;
    }

    let end = -1;
    let matchedSlash = true;
    for (let i = len - 1; i >= offset; --i) {
      if (isPathSeparator(path.charCodeAt(i))) {
        if (!matchedSlash) {
          end = i;
          break;
        }
      } else {
        matchedSlash = false;
      }
    }

    if (end === -1) {
      if (rootEnd === -1)
        return \".\";

      end = rootEnd;
    }
    return path.slice(0, end);
  },

    basename(path, suffix) {
    if (suffix !== undefined)
      validateString(suffix, \"suffix\");
    validateString(path, \"path\");
    let start = 0;
    let end = -1;
    let matchedSlash = true;

    if (path.length >= 2 &&
        isWindowsDeviceRoot(path.charCodeAt(0)) &&
        path.charCodeAt(1) === CHAR_COLON) {
      start = 2;
    }

    if (suffix !== undefined && suffix.length > 0 && suffix.length <= path.length) {
      if (suffix === path)
        return \"\";
      let extIdx = suffix.length - 1;
      let firstNonSlashEnd = -1;
      for (let i = path.length - 1; i >= start; --i) {
        const code = path.charCodeAt(i);
        if (isPathSeparator(code)) {
          if (!matchedSlash) {
            start = i + 1;
            break;
          }
        } else {
          if (firstNonSlashEnd === -1) {
            matchedSlash = false;
            firstNonSlashEnd = i + 1;
          }
          if (extIdx >= 0) {
            if (code === suffix.charCodeAt(extIdx)) {
              if (--extIdx === -1) {
                end = i;
              }
            } else {
              extIdx = -1;
              end = firstNonSlashEnd;
            }
          }
        }
      }

      if (start === end)
        end = firstNonSlashEnd;
      else if (end === -1)
        end = path.length;
      return path.slice(start, end);
    }
    for (let i = path.length - 1; i >= start; --i) {
      if (isPathSeparator(path.charCodeAt(i))) {
        if (!matchedSlash) {
          start = i + 1;
          break;
        }
      } else if (end === -1) {
        matchedSlash = false;
        end = i + 1;
      }
    }

    if (end === -1)
      return \"\";
    return path.slice(start, end);
  },

    extname(path) {
    validateString(path, \"path\");
    let start = 0;
    let startDot = -1;
    let startPart = 0;
    let end = -1;
    let matchedSlash = true;
    let preDotState = 0;

    if (path.length >= 2 &&
        path.charCodeAt(1) === CHAR_COLON &&
        isWindowsDeviceRoot(path.charCodeAt(0))) {
      start = startPart = 2;
    }

    for (let i = path.length - 1; i >= start; --i) {
      const code = path.charCodeAt(i);
      if (isPathSeparator(code)) {
        if (!matchedSlash) {
          startPart = i + 1;
          break;
        }
        continue;
      }
      if (end === -1) {
        matchedSlash = false;
        end = i + 1;
      }
      if (code === CHAR_DOT) {
        if (startDot === -1)
          startDot = i;
        else if (preDotState !== 1)
          preDotState = 1;
      } else if (startDot !== -1) {
        preDotState = -1;
      }
    }

    if (startDot === -1 ||
        end === -1 ||
        preDotState === 0 ||
        (preDotState === 1 &&
         startDot === end - 1 &&
         startDot === startPart + 1)) {
      return \"\";
    }
    return path.slice(startDot, end);
  },

  format: _format.bind(null, \"\\\\\"),

    parse(path) {
    validateString(path, \"path\");

    const ret = { root: \"\", dir: \"\", base: \"\", ext: \"\", name: \"\" };
    if (path.length === 0)
      return ret;

    const len = path.length;
    let rootEnd = 0;
    let code = path.charCodeAt(0);

    if (len === 1) {
      if (isPathSeparator(code)) {
        ret.root = ret.dir = path;
        return ret;
      }
      ret.base = ret.name = path;
      return ret;
    }
    if (isPathSeparator(code)) {

      rootEnd = 1;
      if (isPathSeparator(path.charCodeAt(1))) {
        let j = 2;
        let last = j;
        while (j < len &&
               !isPathSeparator(path.charCodeAt(j))) {
          j++;
        }
        if (j < len && j !== last) {
          last = j;
          while (j < len &&
                 isPathSeparator(path.charCodeAt(j))) {
            j++;
          }
          if (j < len && j !== last) {
            last = j;
            while (j < len &&
                   !isPathSeparator(path.charCodeAt(j))) {
              j++;
            }
            if (j === len) {
              rootEnd = j;
            } else if (j !== last) {
              rootEnd = j + 1;
            }
          }
        }
      }
    } else if (isWindowsDeviceRoot(code) &&
               path.charCodeAt(1) === CHAR_COLON) {
      if (len <= 2) {
        ret.root = ret.dir = path;
        return ret;
      }
      rootEnd = 2;
      if (isPathSeparator(path.charCodeAt(2))) {
        if (len === 3) {
          ret.root = ret.dir = path;
          return ret;
        }
        rootEnd = 3;
      }
    }
    if (rootEnd > 0)
      ret.root = path.slice(0, rootEnd);

    let startDot = -1;
    let startPart = rootEnd;
    let end = -1;
    let matchedSlash = true;
    let i = path.length - 1;

    let preDotState = 0;

    for (; i >= rootEnd; --i) {
      code = path.charCodeAt(i);
      if (isPathSeparator(code)) {
        if (!matchedSlash) {
          startPart = i + 1;
          break;
        }
        continue;
      }
      if (end === -1) {
        matchedSlash = false;
        end = i + 1;
      }
      if (code === CHAR_DOT) {
        if (startDot === -1)
          startDot = i;
        else if (preDotState !== 1)
          preDotState = 1;
      } else if (startDot !== -1) {
        preDotState = -1;
      }
    }

    if (end !== -1) {
      if (startDot === -1 ||
          preDotState === 0 ||
          (preDotState === 1 &&
           startDot === end - 1 &&
           startDot === startPart + 1)) {
        ret.base = ret.name = path.slice(startPart, end);
      } else {
        ret.name = path.slice(startPart, startDot);
        ret.base = path.slice(startPart, end);
        ret.ext = path.slice(startDot, end);
      }
    }

    if (startPart > 0 && startPart !== rootEnd)
      ret.dir = path.slice(0, startPart - 1);
    else
      ret.dir = ret.root;

    return ret;
  },

  matchesGlob(path, pattern) {
    throw matchesGlobMissing();
  },

  sep: \"\\\\\",
  delimiter: \";\",
  win32: null,
  posix: null,
};

const posixCwd = (() => {
  if (isWindows) {
    const regexp = /\\\\/g;
    return () => {
      const cwd = process.cwd().replace(regexp, \"/\");
      return cwd.slice(cwd.indexOf(\"/\"));
    };
  }

  return () => process.cwd();
})();

const posix = {
    resolve(...args) {
    if (args.length === 0 || (args.length === 1 && (args[0] === \"\" || args[0] === \".\"))) {
      const cwd = posixCwd();
      if (cwd.charCodeAt(0) === CHAR_FORWARD_SLASH) {
        return cwd;
      }
    }
    let resolvedPath = \"\";
    let resolvedAbsolute = false;

    for (let i = args.length - 1; i >= 0 && !resolvedAbsolute; i--) {
      const path = args[i];
      validateString(path, `paths[${i}]`);

      if (path.length === 0) {
        continue;
      }

      resolvedPath = `${path}/${resolvedPath}`;
      resolvedAbsolute = path[0] === \"/\";
    }

    if (!resolvedAbsolute) {
      const cwd = posixCwd();
      resolvedPath = `${cwd}/${resolvedPath}`;
      resolvedAbsolute =
        cwd.charCodeAt(0) === CHAR_FORWARD_SLASH;
    }

    resolvedPath = normalizeString(resolvedPath, !resolvedAbsolute, \"/\",
                                   isPosixPathSeparator);

    if (resolvedAbsolute) {
      return `/${resolvedPath}`;
    }
    return resolvedPath.length > 0 ? resolvedPath : \".\";
  },

    normalize(path) {
    validateString(path, \"path\");

    if (path.length === 0)
      return \".\";

    const isAbsolute = path[0] === \"/\";
    const trailingSeparator = path[path.length - 1] === \"/\";

    path = normalizeString(path, !isAbsolute, \"/\", isPosixPathSeparator);

    if (path.length === 0) {
      if (isAbsolute)
        return \"/\";
      return trailingSeparator ? \"./\" : \".\";
    }
    if (trailingSeparator)
      path += \"/\";

    return isAbsolute ? `/${path}` : path;
  },

    isAbsolute(path) {
    validateString(path, \"path\");
    return path.length > 0 &&
           path.charCodeAt(0) === CHAR_FORWARD_SLASH;
  },

    join(...args) {
    if (args.length === 0)
      return \".\";

    const path = [];
    for (let i = 0; i < args.length; ++i) {
      const arg = args[i];
      validateString(arg, \"path\");
      if (arg.length > 0) {
        path.push(arg);
      }
    }

    if (path.length === 0)
      return \".\";

    return posix.normalize(path.join(\"/\"));
  },

    relative(from, to) {
    validateString(from, \"from\");
    validateString(to, \"to\");

    if (from === to)
      return \"\";

    from = posix.resolve(from);
    to = posix.resolve(to);

    if (from === to)
      return \"\";

    const fromStart = 1;
    const fromEnd = from.length;
    const fromLen = fromEnd - fromStart;
    const toStart = 1;
    const toLen = to.length - toStart;

    const length = (fromLen < toLen ? fromLen : toLen);
    let lastCommonSep = -1;
    let i = 0;
    for (; i < length; i++) {
      const fromCode = from.charCodeAt(fromStart + i);
      if (fromCode !== to.charCodeAt(toStart + i))
        break;
      else if (fromCode === CHAR_FORWARD_SLASH)
        lastCommonSep = i;
    }
    if (i === length) {
      if (toLen > length) {
        if (to.charCodeAt(toStart + i) === CHAR_FORWARD_SLASH) {
          return to.slice(toStart + i + 1);
        }
        if (i === 0) {
          return to.slice(toStart + i);
        }
      } else if (fromLen > length) {
        if (from.charCodeAt(fromStart + i) ===
            CHAR_FORWARD_SLASH) {
          lastCommonSep = i;
        } else if (i === 0) {
          lastCommonSep = 0;
        }
      }
    }

    let out = \"\";
    for (i = fromStart + lastCommonSep + 1; i <= fromEnd; ++i) {
      if (i === fromEnd ||
          from.charCodeAt(i) === CHAR_FORWARD_SLASH) {
        out += out.length === 0 ? \"..\" : \"/..\";
      }
    }

    return `${out}${to.slice(toStart + lastCommonSep)}`;
  },

    toNamespacedPath(path) {
    return path;
  },

    dirname(path) {
    validateString(path, \"path\");
    if (path.length === 0)
      return \".\";
    const hasRoot = path.charCodeAt(0) === CHAR_FORWARD_SLASH;
    let end = -1;
    let matchedSlash = true;
    for (let i = path.length - 1; i >= 1; --i) {
      if (path.charCodeAt(i) === CHAR_FORWARD_SLASH) {
        if (!matchedSlash) {
          end = i;
          break;
        }
      } else {
        matchedSlash = false;
      }
    }

    if (end === -1)
      return hasRoot ? \"/\" : \".\";
    if (hasRoot && end === 1)
      return \"//\";
    return path.slice(0, end);
  },

    basename(path, suffix) {
    if (suffix !== undefined)
      validateString(suffix, \"suffix\");
    validateString(path, \"path\");

    let start = 0;
    let end = -1;
    let matchedSlash = true;

    if (suffix !== undefined && suffix.length > 0 && suffix.length <= path.length) {
      if (suffix === path)
        return \"\";
      let extIdx = suffix.length - 1;
      let firstNonSlashEnd = -1;
      for (let i = path.length - 1; i >= 0; --i) {
        const code = path.charCodeAt(i);
        if (code === CHAR_FORWARD_SLASH) {
          if (!matchedSlash) {
            start = i + 1;
            break;
          }
        } else {
          if (firstNonSlashEnd === -1) {
            matchedSlash = false;
            firstNonSlashEnd = i + 1;
          }
          if (extIdx >= 0) {
            if (code === suffix.charCodeAt(extIdx)) {
              if (--extIdx === -1) {
                end = i;
              }
            } else {
              extIdx = -1;
              end = firstNonSlashEnd;
            }
          }
        }
      }

      if (start === end)
        end = firstNonSlashEnd;
      else if (end === -1)
        end = path.length;
      return path.slice(start, end);
    }
    for (let i = path.length - 1; i >= 0; --i) {
      if (path.charCodeAt(i) === CHAR_FORWARD_SLASH) {
        if (!matchedSlash) {
          start = i + 1;
          break;
        }
      } else if (end === -1) {
        matchedSlash = false;
        end = i + 1;
      }
    }

    if (end === -1)
      return \"\";
    return path.slice(start, end);
  },

    extname(path) {
    validateString(path, \"path\");
    let startDot = -1;
    let startPart = 0;
    let end = -1;
    let matchedSlash = true;
    let preDotState = 0;
    for (let i = path.length - 1; i >= 0; --i) {
      const char = path[i];
      if (char === \"/\") {
        if (!matchedSlash) {
          startPart = i + 1;
          break;
        }
        continue;
      }
      if (end === -1) {
        matchedSlash = false;
        end = i + 1;
      }
      if (char === \".\") {
        if (startDot === -1)
          startDot = i;
        else if (preDotState !== 1)
          preDotState = 1;
      } else if (startDot !== -1) {
        preDotState = -1;
      }
    }

    if (startDot === -1 ||
        end === -1 ||
        preDotState === 0 ||
        (preDotState === 1 &&
         startDot === end - 1 &&
         startDot === startPart + 1)) {
      return \"\";
    }
    return path.slice(startDot, end);
  },

  format: _format.bind(null, \"/\"),

    parse(path) {
    validateString(path, \"path\");

    const ret = { root: \"\", dir: \"\", base: \"\", ext: \"\", name: \"\" };
    if (path.length === 0)
      return ret;
    const isAbsolute =
      path.charCodeAt(0) === CHAR_FORWARD_SLASH;
    let start;
    if (isAbsolute) {
      ret.root = \"/\";
      start = 1;
    } else {
      start = 0;
    }
    let startDot = -1;
    let startPart = 0;
    let end = -1;
    let matchedSlash = true;
    let i = path.length - 1;

    let preDotState = 0;

    for (; i >= start; --i) {
      const char = path[i];
      if (char === \"/\") {
        if (!matchedSlash) {
          startPart = i + 1;
          break;
        }
        continue;
      }
      if (end === -1) {
        matchedSlash = false;
        end = i + 1;
      }
      if (char === \".\") {
        if (startDot === -1)
          startDot = i;
        else if (preDotState !== 1)
          preDotState = 1;
      } else if (startDot !== -1) {
        preDotState = -1;
      }
    }

    if (end !== -1) {
      const start = startPart === 0 && isAbsolute ? 1 : startPart;
      if (startDot === -1 ||
          preDotState === 0 ||
          (preDotState === 1 &&
          startDot === end - 1 &&
          startDot === startPart + 1)) {
        ret.base = ret.name = path.slice(start, end);
      } else {
        ret.name = path.slice(start, startDot);
        ret.base = path.slice(start, end);
        ret.ext = path.slice(startDot, end);
      }
    }

    if (startPart > 0)
      ret.dir = path.slice(0, startPart - 1);
    else if (isAbsolute)
      ret.dir = \"/\";

    return ret;
  },

  matchesGlob(path, pattern) {
    throw matchesGlobMissing();
  },

  sep: \"/\",
  delimiter: \":\",
  win32: null,
  posix: null,
};

posix.win32 = win32.win32 = win32;
posix.posix = win32.posix = posix;

win32._makeLong = win32.toNamespacedPath;
posix._makeLong = posix.toNamespacedPath;

module.exports = isWindows ? win32 : posix;
"

pub const path_posix = "\"use strict\";

module.exports = require(\"node:path\").posix;
"

pub const path_win32 = "\"use strict\";

module.exports = require(\"node:path\").win32;
"

pub const net = "\"use strict\";

// the parts of node's net module that open nothing: isIP and friends,
// SocketAddress and BlockList. The address parsing and printing is libuv's
// inet_pton and inet_ntop; the matching rules are node's own

const {
  error,
  rangeError,
  invalidArgType,
  invalidArgValue,
  outOfRange,
  determineSpecificType,
  validateArray,
  validateObject,
  validateString,
} = require(\"spark:errors\");

const v4Seg = \"(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\";
const v4Str = `(?:${v4Seg}\\\\.){3}${v4Seg}`;
const IPv4Reg = new RegExp(`^${v4Str}$`);

const v6Seg = \"(?:[0-9a-fA-F]{1,4})\";
const IPv6Reg = new RegExp(
  \"^(?:\" +
    `(?:${v6Seg}:){7}(?:${v6Seg}|:)|` +
    `(?:${v6Seg}:){6}(?:${v4Str}|:${v6Seg}|:)|` +
    `(?:${v6Seg}:){5}(?::${v4Str}|(?::${v6Seg}){1,2}|:)|` +
    `(?:${v6Seg}:){4}(?:(?::${v6Seg}){0,1}:${v4Str}|(?::${v6Seg}){1,3}|:)|` +
    `(?:${v6Seg}:){3}(?:(?::${v6Seg}){0,2}:${v4Str}|(?::${v6Seg}){1,4}|:)|` +
    `(?:${v6Seg}:){2}(?:(?::${v6Seg}){0,3}:${v4Str}|(?::${v6Seg}){1,5}|:)|` +
    `(?:${v6Seg}:){1}(?:(?::${v6Seg}){0,4}:${v4Str}|(?::${v6Seg}){1,6}|:)|` +
    `(?::(?:(?::${v6Seg}){0,5}:${v4Str}|(?::${v6Seg}){1,7}|:))` +
    \")(?:%[0-9a-zA-Z-.:]{1,})?$\",
);

function isIPv4(s) {
  return IPv4Reg.test(s);
}

function isIPv6(s) {
  if (typeof s === \"string\") {
    if (s.length < 2 || s.indexOf(\":\") === -1) return false;
  }
  return IPv6Reg.test(s);
}

function isIP(s) {
  if (isIPv4(s)) return 4;
  if (isIPv6(s)) return 6;
  return 0;
}

const AF_INET = 4;
const AF_INET6 = 6;

// four bytes, or undefined: no leading zeros, exactly four parts
function parseIPv4(text) {
  const bytes = [0];
  let sawDigit = false;
  let octets = 0;
  for (let i = 0; i < text.length; i++) {
    const ch = text.charCodeAt(i);
    if (ch >= 48 && ch <= 57) {
      const at = bytes.length - 1;
      const value = bytes[at] * 10 + (ch - 48);
      if (sawDigit && bytes[at] === 0) return undefined;
      if (value > 255) return undefined;
      bytes[at] = value;
      if (!sawDigit) {
        if (++octets > 4) return undefined;
        sawDigit = true;
      }
    } else if (ch === 46 && sawDigit) {
      if (octets === 4) return undefined;
      bytes.push(0);
      sawDigit = false;
    } else {
      return undefined;
    }
  }
  if (octets < 4) return undefined;
  return bytes;
}

function hexValue(ch) {
  if (ch >= 48 && ch <= 57) return ch - 48;
  if (ch >= 97 && ch <= 102) return ch - 87;
  if (ch >= 65 && ch <= 70) return ch - 55;
  return -1;
}

// sixteen bytes, or undefined; anything from a '%' on is a zone and ignored
function parseIPv6(text) {
  const zone = text.indexOf(\"%\");
  if (zone !== -1) text = text.slice(0, zone);
  const bytes = [];
  let gap = -1;
  let i = 0;
  if (text.charCodeAt(0) === 58) {
    if (text.charCodeAt(1) !== 58) return undefined;
    i = 1;
  }
  let token = i;
  let digits = 0;
  let value = 0;
  let endedByQuad = false;
  while (i < text.length) {
    const ch = text.charCodeAt(i++);
    const digit = hexValue(ch);
    if (digit !== -1) {
      value = (value << 4) | digit;
      if (++digits > 4) return undefined;
      continue;
    }
    if (ch === 58) {
      token = i;
      if (digits === 0) {
        if (gap !== -1) return undefined;
        gap = bytes.length;
        continue;
      }
      if (i === text.length) return undefined;
      if (bytes.length + 2 > 16) return undefined;
      bytes.push((value >> 8) & 0xff, value & 0xff);
      digits = 0;
      value = 0;
      continue;
    }
    if (ch === 46 && bytes.length + 4 <= 16) {
      const quad = parseIPv4(text.slice(token));
      if (quad !== undefined) {
        bytes.push(...quad);
        digits = 0;
        endedByQuad = true;
        break;
      }
    }
    return undefined;
  }
  if (!endedByQuad && digits !== 0) {
    if (bytes.length + 2 > 16) return undefined;
    bytes.push((value >> 8) & 0xff, value & 0xff);
  }
  if (gap !== -1) {
    if (bytes.length === 16) return undefined;
    const tail = bytes.splice(gap);
    while (bytes.length + tail.length < 16) bytes.push(0);
    bytes.push(...tail);
  }
  if (bytes.length !== 16) return undefined;
  return bytes;
}

function formatIPv6(bytes) {
  const words = [];
  for (let i = 0; i < 16; i += 2) words.push((bytes[i] << 8) | bytes[i + 1]);
  // the longest run of zero words becomes \"::\"
  let bestBase = -1;
  let bestLen = 0;
  let curBase = -1;
  let curLen = 0;
  for (let i = 0; i < 8; i++) {
    if (words[i] === 0) {
      if (curBase === -1) {
        curBase = i;
        curLen = 1;
      } else {
        curLen++;
      }
    } else if (curBase !== -1) {
      if (bestBase === -1 || curLen > bestLen) {
        bestBase = curBase;
        bestLen = curLen;
      }
      curBase = -1;
    }
  }
  if (curBase !== -1 && (bestBase === -1 || curLen > bestLen)) {
    bestBase = curBase;
    bestLen = curLen;
  }
  if (bestBase !== -1 && bestLen < 2) bestBase = -1;

  let text = \"\";
  for (let i = 0; i < 8; i++) {
    if (bestBase !== -1 && i >= bestBase && i < bestBase + bestLen) {
      if (i === bestBase) text += \":\";
      continue;
    }
    if (i !== 0) text += \":\";
    // an address that wraps an ipv4 one ends in dotted form
    if (
      i === 6 &&
      bestBase === 0 &&
      (bestLen === 6 ||
        (bestLen === 7 && words[7] !== 0x0001) ||
        (bestLen === 5 && words[5] === 0xffff))
    ) {
      return text + bytes.slice(12).join(\".\");
    }
    text += words[i].toString(16);
  }
  if (bestBase !== -1 && bestBase + bestLen === 8) text += \":\";
  return text;
}

function parseAddress(text, family) {
  return family === AF_INET ? parseIPv4(text) : parseIPv6(text);
}

function formatAddress(handle) {
  return handle.family === AF_INET
    ? handle.bytes.join(\".\")
    : formatIPv6(handle.bytes);
}

function familyName(handle) {
  return handle.family === AF_INET ? \"IPv4\" : \"IPv6\";
}

function sameBytes(a, b, count, aFrom = 0, bFrom = 0) {
  for (let i = 0; i < count; i++) {
    if (a[aFrom + i] !== b[bFrom + i]) return false;
  }
  return true;
}

// -1, 0 or 1 over `count` bytes
function orderBytes(a, b, count, aFrom = 0, bFrom = 0) {
  for (let i = 0; i < count; i++) {
    const d = a[aFrom + i] - b[bFrom + i];
    if (d !== 0) return d < 0 ? -1 : 1;
  }
  return 0;
}

const mappedPrefix = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff];

function isMapped(v6) {
  return sameBytes(v6, mappedPrefix, 12);
}

function isMatch(a, b) {
  if (a.family === b.family) {
    return sameBytes(a.bytes, b.bytes, a.bytes.length);
  }
  const v4 = a.family === AF_INET ? a : b;
  const v6 = a.family === AF_INET ? b : a;
  return isMapped(v6.bytes) && sameBytes(v6.bytes, v4.bytes, 4, 12, 0);
}

// undefined when an ipv4 address meets an ipv6 one that does not wrap one
function compare(a, b) {
  if (a.family === b.family) {
    return orderBytes(a.bytes, b.bytes, a.bytes.length);
  }
  if (a.family === AF_INET) {
    if (!isMapped(b.bytes)) return undefined;
    return orderBytes(a.bytes, b.bytes, 4, 0, 12);
  }
  if (!isMapped(a.bytes)) return undefined;
  return orderBytes(a.bytes, b.bytes, 4, 12, 0);
}

function prefixMatches(a, b, prefix) {
  const whole = prefix >> 3;
  const rest = prefix & 7;
  if (!sameBytes(a, b, whole)) return false;
  if (rest === 0) return true;
  const mask = (0xff << (8 - rest)) & 0xff;
  return (a[whole] & mask) === (b[whole] & mask);
}

function inNetwork(address, network, prefix) {
  if (address.family === network.family) {
    return prefixMatches(address.bytes, network.bytes, prefix);
  }
  if (address.family === AF_INET) {
    return prefixMatches(
      [...mappedPrefix, ...address.bytes],
      network.bytes,
      prefix,
    );
  }
  if (!isMapped(address.bytes)) return false;
  return prefixMatches(address.bytes.slice(12), network.bytes, prefix);
}

function invalidAddress() {
  return error(\"ERR_INVALID_ADDRESS\", \"Invalid socket address\");
}

function validatePort(port, name = \"Port\", allowZero = true) {
  if (
    (typeof port !== \"number\" && typeof port !== \"string\") ||
    (typeof port === \"string\" && port.trim().length === 0) ||
    +port !== +port >>> 0 ||
    port > 0xffff ||
    (port === 0 && !allowZero)
  ) {
    const operator = allowZero ? \">=\" : \">\";
    throw rangeError(
      \"ERR_SOCKET_BAD_PORT\",
      `${name} should be ${operator} 0 and < 65536. Received ${determineSpecificType(port)}.`,
    );
  }
  return port | 0;
}

function validateUint32(value, name, positive = false) {
  if (typeof value !== \"number\") throw invalidArgType(name, \"number\", value);
  if (!Number.isInteger(value)) throw outOfRange(name, \"an integer\", value);
  const min = positive ? 1 : 0;
  const max = 4294967295;
  if (value < min || value > max) {
    throw outOfRange(name, `>= ${min} && <= ${max}`, value);
  }
}

function validateInt32(value, name, min = -2147483648, max = 2147483647) {
  if (typeof value !== \"number\") throw invalidArgType(name, \"number\", value);
  if (!Number.isInteger(value)) throw outOfRange(name, \"an integer\", value);
  if (value < min || value > max) {
    throw outOfRange(name, `>= ${min} && <= ${max}`, value);
  }
}

const kSocketAddressHandle = Symbol(\"kHandle\");

class SocketAddress {
  static isSocketAddress(value) {
    return value?.[kSocketAddressHandle] !== undefined;
  }

  constructor(options = {}) {
    validateObject(options, \"options\");
    let { family = \"ipv4\" } = options;
    const {
      address = family === \"ipv4\" ? \"127.0.0.1\" : \"::\",
      port = 0,
      flowlabel = 0,
    } = options;

    let type;
    if (typeof family?.toLowerCase === \"function\") {
      family = family.toLowerCase();
    }
    switch (family) {
      case \"ipv4\":
        type = AF_INET;
        break;
      case \"ipv6\":
        type = AF_INET6;
        break;
      default:
        throw invalidArgValue(\"options.family\", options.family);
    }

    validateString(address, \"options.address\");
    validatePort(port, \"options.port\");
    validateUint32(flowlabel, \"options.flowlabel\", false);

    const bytes = parseAddress(address, type);
    if (bytes === undefined) throw invalidAddress();
    this[kSocketAddressHandle] = {
      family: type,
      bytes,
      port: port | 0,
      flowlabel: type === AF_INET6 ? flowlabel | 0 : 0,
    };
  }

  get address() {
    return formatAddress(this[kSocketAddressHandle]);
  }

  get port() {
    return this[kSocketAddressHandle].port;
  }

  get family() {
    return this[kSocketAddressHandle].family === AF_INET ? \"ipv4\" : \"ipv6\";
  }

  get flowlabel() {
    return this[kSocketAddressHandle].flowlabel;
  }

  toJSON() {
    return {
      address: this.address,
      port: this.port,
      family: this.family,
      flowlabel: this.flowlabel,
    };
  }

  // node reads the input as a url, which is not here yet
  static parse(input) {
    validateString(input, \"input\");
    throw error(
      \"ERR_METHOD_NOT_IMPLEMENTED\",
      \"The parse() method is not implemented\",
    );
  }
}

function parseCIDR(cidr) {
  validateString(cidr, \"cidr\");
  const slash = cidr.lastIndexOf(\"/\");
  if (slash === -1) {
    throw invalidArgValue(
      \"cidr\",
      cidr,
      'must contain a prefix length (e.g. \"10.0.0.0/8\")',
    );
  }
  const address = cidr.slice(0, slash);
  const prefixStr = cidr.slice(slash + 1);
  const prefix = Number.parseInt(prefixStr, 10);
  if (Number.isNaN(prefix) || `${prefix}` !== prefixStr) {
    throw invalidArgValue(
      \"cidr\",
      cidr,
      \"prefix length must be a valid integer\",
    );
  }
  const family = address.includes(\":\") ? \"ipv6\" : \"ipv4\";
  return { address, prefix, family };
}

function addressKey(handle) {
  return `${handle.family} ${handle.bytes.join(\".\")}`;
}

function toHandle(value, name, family) {
  if (SocketAddress.isSocketAddress(value)) return value;
  validateString(value, name);
  validateString(family, \"family\");
  return new SocketAddress({ address: value, family });
}

function validatePrefix(network, prefix) {
  switch (network.family) {
    case \"ipv4\":
      validateInt32(prefix, \"prefix\", 0, 32);
      break;
    case \"ipv6\":
      validateInt32(prefix, \"prefix\", 0, 128);
      break;
  }
}

const kHandle = Symbol(\"kHandle\");

class BlockList {
  constructor() {
    // newest first in each: addresses by key, then subnets, then ranges
    this[kHandle] = { addresses: new Map(), subnets: [], ranges: [] };
  }

  static isBlockList(value) {
    return value?.[kHandle] !== undefined;
  }

  static PRIVATE_RANGES = Object.freeze([
    \"10.0.0.0/8\",
    \"172.16.0.0/12\",
    \"192.168.0.0/16\",
    \"127.0.0.0/8\",
    \"::1/128\",
    \"169.254.0.0/16\",
    \"fe80::/10\",
    \"fc00::/7\",
  ]);

  addAddress(address, family = \"ipv4\") {
    address = toHandle(address, \"address\", family);
    const handle = address[kSocketAddressHandle];
    const { addresses } = this[kHandle];
    const key = addressKey(handle);
    addresses.delete(key);
    addresses.set(key, handle);
  }

  addAddresses(addresses, family = \"ipv4\") {
    validateArray(addresses, \"addresses\");
    validateString(family, \"family\");
    const handles = [];
    for (let i = 0; i < addresses.length; i++) {
      let address = addresses[i];
      if (!SocketAddress.isSocketAddress(address)) {
        validateString(address, `addresses[${i}]`);
        address = new SocketAddress({ address, family });
      }
      handles.push(address[kSocketAddressHandle]);
    }
    for (const handle of handles) {
      const key = addressKey(handle);
      this[kHandle].addresses.delete(key);
      this[kHandle].addresses.set(key, handle);
    }
  }

  addRange(start, end, family = \"ipv4\") {
    start = toHandle(start, \"start\", family);
    end = toHandle(end, \"end\", family);
    const from = start[kSocketAddressHandle];
    const to = end[kSocketAddressHandle];
    if (compare(from, to) === 1) {
      throw invalidArgValue(\"start\", start, \"must come before end\");
    }
    this[kHandle].ranges.unshift({ start: from, end: to });
  }

  addSubnet(network, prefix, family = \"ipv4\") {
    network = toHandle(network, \"network\", family);
    validatePrefix(network, prefix);
    prefix += 0;
    this[kHandle].subnets.unshift({
      network: network[kSocketAddressHandle],
      prefix,
    });
  }

  addCIDR(cidr) {
    const { address, prefix, family } = parseCIDR(cidr);
    this.addSubnet(address, prefix, family);
  }

  addCIDRs(cidrs) {
    validateArray(cidrs, \"cidrs\");
    const parsed = [];
    for (let i = 0; i < cidrs.length; i++) {
      validateString(cidrs[i], `cidrs[${i}]`);
      parsed.push(parseCIDR(cidrs[i]));
    }
    for (let i = 0; i < parsed.length; i++) {
      const { address, prefix, family } = parsed[i];
      this.addSubnet(address, prefix, family);
    }
  }

  removeAddress(address, family = \"ipv4\") {
    address = toHandle(address, \"address\", family);
    this[kHandle].addresses.delete(addressKey(address[kSocketAddressHandle]));
  }

  removeRange(start, end, family = \"ipv4\") {
    start = toHandle(start, \"start\", family);
    end = toHandle(end, \"end\", family);
    const from = addressKey(start[kSocketAddressHandle]);
    const to = addressKey(end[kSocketAddressHandle]);
    const { ranges } = this[kHandle];
    const at = ranges.findIndex(
      (range) => addressKey(range.start) === from && addressKey(range.end) === to,
    );
    if (at !== -1) ranges.splice(at, 1);
  }

  removeSubnet(network, prefix, family = \"ipv4\") {
    network = toHandle(network, \"network\", family);
    validatePrefix(network, prefix);
    prefix += 0;
    const key = addressKey(network[kSocketAddressHandle]);
    const { subnets } = this[kHandle];
    const at = subnets.findIndex(
      (subnet) => addressKey(subnet.network) === key && subnet.prefix === prefix,
    );
    if (at !== -1) subnets.splice(at, 1);
  }

  removeCIDR(cidr) {
    const { address, prefix, family } = parseCIDR(cidr);
    this.removeSubnet(address, prefix, family);
  }

  check(address, family = \"ipv4\") {
    let handle;
    if (!SocketAddress.isSocketAddress(address)) {
      validateString(address, \"address\");
      validateString(family, \"family\");
      let af;
      if (family === \"ipv4\" || family === \"IPv4\") {
        af = AF_INET;
      } else if (family === \"ipv6\" || family === \"IPv6\") {
        af = AF_INET6;
      } else {
        af = family.toLowerCase() === \"ipv4\" ? AF_INET : AF_INET6;
      }
      const bytes = parseAddress(address, af);
      if (bytes === undefined) return false;
      handle = { family: af, bytes };
    } else {
      handle = address[kSocketAddressHandle];
    }
    const { addresses, subnets, ranges } = this[kHandle];
    for (const rule of addresses.values()) {
      if (isMatch(rule, handle)) return true;
    }
    for (const { network, prefix } of subnets) {
      if (inNetwork(handle, network, prefix)) return true;
    }
    for (const { start, end } of ranges) {
      const low = compare(handle, start);
      const high = compare(handle, end);
      if (low !== undefined && high !== undefined && low >= 0 && high <= 0) {
        return true;
      }
    }
    return false;
  }

  clear() {
    this[kHandle] = { addresses: new Map(), subnets: [], ranges: [] };
  }

  #parseIPInfo(data) {
    for (const item of data) {
      if (item.includes(\"IPv4\")) {
        const subnetMatch = item.match(
          /Subnet: IPv4 (\\d{1,3}(?:\\.\\d{1,3}){3})\\/(\\d{1,2})/,
        );
        if (subnetMatch) {
          const { 1: network, 2: prefix } = subnetMatch;
          this.addSubnet(network, Number.parseInt(prefix));
          continue;
        }
        const addressMatch = item.match(
          /Address: IPv4 (\\d{1,3}(?:\\.\\d{1,3}){3})/,
        );
        if (addressMatch) {
          const { 1: address } = addressMatch;
          this.addAddress(address);
          continue;
        }

        const rangeMatch = item.match(
          /Range: IPv4 (\\d{1,3}(?:\\.\\d{1,3}){3})-(\\d{1,3}(?:\\.\\d{1,3}){3})/,
        );
        if (rangeMatch) {
          const { 1: start, 2: end } = rangeMatch;
          this.addRange(start, end);
          continue;
        }
      }
      // IPv6 parsing with support for compressed addresses
      if (item.includes(\"IPv6\")) {
        // IPv6 subnet pattern: supports both full and compressed formats
        const ipv6SubnetMatch = item.match(
          /Subnet: IPv6 ([0-9a-fA-F:.]{1,45})\\/([0-9]{1,3})/i,
        );
        if (ipv6SubnetMatch) {
          const { 1: network, 2: prefix } = ipv6SubnetMatch;
          this.addSubnet(network, Number.parseInt(prefix), \"ipv6\");
          continue;
        }

        const ipv6AddressMatch = item.match(
          /Address: IPv6 ([0-9a-fA-F:.]{1,45})/i,
        );
        if (ipv6AddressMatch) {
          const { 1: address } = ipv6AddressMatch;
          this.addAddress(address, \"ipv6\");
          continue;
        }

        const ipv6RangeMatch = item.match(
          /Range: IPv6 ([0-9a-fA-F:.]{1,45})-([0-9a-fA-F:.]{1,45})/i,
        );
        if (ipv6RangeMatch) {
          const { 1: start, 2: end } = ipv6RangeMatch;
          this.addRange(start, end, \"ipv6\");
          continue;
        }
      }
    }
  }

  toJSON() {
    return this.rules;
  }

  fromJSON(data) {
    // The data argument must be a string, or an array of strings that
    // is JSON parseable.
    if (Array.isArray(data)) {
      for (const n of data) {
        if (typeof n !== \"string\") {
          throw invalidArgType(\"data\", [\"string\", \"string[]\"], data);
        }
      }
    } else if (typeof data !== \"string\") {
      throw invalidArgType(\"data\", [\"string\", \"string[]\"], data);
    } else {
      data = JSON.parse(data);
      if (!Array.isArray(data)) {
        throw invalidArgType(\"data\", [\"string\", \"string[]\"], data);
      }
      for (const n of data) {
        if (typeof n !== \"string\") {
          throw invalidArgType(\"data\", [\"string\", \"string[]\"], data);
        }
      }
    }

    this.#parseIPInfo(data);
  }

  get rules() {
    const { addresses, subnets, ranges } = this[kHandle];
    const rules = [];
    for (const handle of [...addresses.values()].reverse()) {
      rules.push(`Address: ${familyName(handle)} ${formatAddress(handle)}`);
    }
    for (const { network, prefix } of subnets) {
      rules.push(
        `Subnet: ${familyName(network)} ${formatAddress(network)}/${prefix}`,
      );
    }
    for (const { start, end } of ranges) {
      rules.push(
        `Range: ${familyName(start)} ${formatAddress(start)}-${formatAddress(end)}`,
      );
    }
    return rules;
  }

  get size() {
    const { addresses, subnets, ranges } = this[kHandle];
    return addresses.size + subnets.length + ranges.length;
  }
}

module.exports = { BlockList, SocketAddress, isIP, isIPv4, isIPv6 };
"

pub const querystring_internal = "\"use strict\";

// node's lib/internal/querystring.js. One addition: a character beyond
// U+FFFF arrives as one code point here, not as a surrogate pair

const { uriError } = require(\"spark:errors\");

function ERR_INVALID_URI() {
  return uriError(\"ERR_INVALID_URI\", \"URI malformed\");
}

const hexTable = new Array(256);
for (let i = 0; i < 256; ++i)
  hexTable[i] = \"%\" +
                ((i < 16 ? \"0\" : \"\") +
                                           i.toString(16)).toUpperCase();

const isHexTable = new Int8Array([
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0 - 15
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 16 - 31
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 32 - 47
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, // 48 - 63
  0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 64 - 79
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 80 - 95
  0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 96 - 111
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 112 - 127
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 128 ...
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  // ... 256
]);

function encodeStr(str, noEscapeTable, hexTable) {
  const len = str.length;
  if (len === 0)
    return \"\";

  let out = \"\";
  let lastPos = 0;
  let i = 0;

  outer:
  for (; i < len; i++) {
    let c = str.charCodeAt(i);

    while (c < 0x80) {
      if (noEscapeTable[c] !== 1) {
        if (lastPos < i)
          out += str.slice(lastPos, i);
        lastPos = i + 1;
        out += hexTable[c];
      }

      if (++i === len)
        break outer;

      c = str.charCodeAt(i);
    }

    if (lastPos < i)
      out += str.slice(lastPos, i);

    if (c < 0x800) {
      lastPos = i + 1;
      out += hexTable[0xC0 | (c >> 6)] +
             hexTable[0x80 | (c & 0x3F)];
      continue;
    }
    if (c > 0xFFFF) {
      lastPos = i + 1;
      out += hexTable[0xF0 | (c >> 18)] +
             hexTable[0x80 | ((c >> 12) & 0x3F)] +
             hexTable[0x80 | ((c >> 6) & 0x3F)] +
             hexTable[0x80 | (c & 0x3F)];
      continue;
    }
    if (c < 0xD800 || c >= 0xE000) {
      lastPos = i + 1;
      out += hexTable[0xE0 | (c >> 12)] +
             hexTable[0x80 | ((c >> 6) & 0x3F)] +
             hexTable[0x80 | (c & 0x3F)];
      continue;
    }
    ++i;

    if (i >= len)
      throw new ERR_INVALID_URI();

    const c2 = str.charCodeAt(i) & 0x3FF;

    lastPos = i + 1;
    c = 0x10000 + (((c & 0x3FF) << 10) | c2);
    out += hexTable[0xF0 | (c >> 18)] +
           hexTable[0x80 | ((c >> 12) & 0x3F)] +
           hexTable[0x80 | ((c >> 6) & 0x3F)] +
           hexTable[0x80 | (c & 0x3F)];
  }
  if (lastPos === 0)
    return str;
  if (lastPos < len)
    return out + str.slice(lastPos);
  return out;
}

module.exports = { encodeStr, hexTable, isHexTable };
"

pub const querystring = "\"use strict\";

// node's lib/querystring.js

const { Buffer } = require(\"node:buffer\");
const { encodeStr, hexTable, isHexTable } = require(\"spark:querystring\");

const QueryString = module.exports = {
  unescapeBuffer,
  unescape: qsUnescape,

  escape: qsEscape,

  stringify,
  encode: stringify,

  parse,
  decode: parse,
};

const unhexTable = new Int8Array([
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 0 - 15
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 16 - 31
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 32 - 47
  +0, +1, +2, +3, +4, +5, +6, +7, +8, +9, -1, -1, -1, -1, -1, -1, // 48 - 63
  -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 64 - 79
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 80 - 95
  -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 96 - 111
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 112 - 127
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // 128 ...
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ... 255
]);
function unescapeBuffer(s, decodeSpaces) {
  if (s.indexOf(\"%\") === -1) {
    if (!decodeSpaces || s.indexOf(\"+\") === -1)
      return Buffer.from(s, \"latin1\");
  }
  const out = Buffer.allocUnsafe(s.length);
  let index = 0;
  let outIndex = 0;
  let currentChar;
  let nextChar;
  let hexHigh;
  let hexLow;
  const maxLength = s.length - 2;
  let hasHex = false;
  while (index < s.length) {
    currentChar = s.charCodeAt(index);
    if (currentChar === 43 /* \"+\" */ && decodeSpaces) {
      out[outIndex++] = 32; // \" \"
      index++;
      continue;
    }
    if (currentChar === 37 /* \"%\" */ && index < maxLength) {
      currentChar = s.charCodeAt(++index);
      hexHigh = unhexTable[currentChar];
      if (!(hexHigh >= 0)) {
        out[outIndex++] = 37; // \"%\"
        continue;
      } else {
        nextChar = s.charCodeAt(++index);
        hexLow = unhexTable[nextChar];
        if (!(hexLow >= 0)) {
          out[outIndex++] = 37; // \"%\"
          index--;
        } else {
          hasHex = true;
          currentChar = hexHigh * 16 + hexLow;
        }
      }
    }
    out[outIndex++] = currentChar;
    index++;
  }
  return hasHex ? out.slice(0, outIndex) : out;
}

function qsUnescape(s, decodeSpaces) {
  try {
    return decodeURIComponent(s);
  } catch {
    return QueryString.unescapeBuffer(s, decodeSpaces).toString();
  }
}

const noEscape = new Int8Array([
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0 - 15
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 16 - 31
  0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, // 32 - 47
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, // 48 - 63
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 64 - 79
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, // 80 - 95
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 96 - 111
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0,  // 112 - 127
]);

function qsEscape(str) {
  if (typeof str !== \"string\") {
    if (typeof str === \"object\")
      str = String(str);
    else
      str += \"\";
  }

  return encodeStr(str, noEscape, hexTable);
}

function stringifyPrimitive(v) {
  if (typeof v === \"string\")
    return v;
  if (typeof v === \"number\" && Number.isFinite(v))
    return \"\" + v;
  if (typeof v === \"bigint\")
    return \"\" + v;
  if (typeof v === \"boolean\")
    return v ? \"true\" : \"false\";
  return \"\";
}

function encodeStringified(v, encode) {
  if (typeof v === \"string\")
    return (v.length ? encode(v) : \"\");
  if (typeof v === \"number\" && Number.isFinite(v)) {
    return (Math.abs(v) < 1e21 ? \"\" + v : encode(\"\" + v));
  }
  if (typeof v === \"bigint\")
    return \"\" + v;
  if (typeof v === \"boolean\")
    return v ? \"true\" : \"false\";
  return \"\";
}

function encodeStringifiedCustom(v, encode) {
  return encode(stringifyPrimitive(v));
}

function stringify(obj, sep, eq, options) {
  sep ||= \"&\";
  eq ||= \"=\";

  let encode = QueryString.escape;
  if (options && typeof options.encodeURIComponent === \"function\") {
    encode = options.encodeURIComponent;
  }
  const convert =
    (encode === qsEscape ? encodeStringified : encodeStringifiedCustom);

  if (obj !== null && typeof obj === \"object\") {
    const keys = Object.keys(obj);
    const len = keys.length;
    let fields = \"\";
    for (let i = 0; i < len; ++i) {
      const k = keys[i];
      const v = obj[k];
      let ks = convert(k, encode);
      ks += eq;

      if (Array.isArray(v)) {
        const vlen = v.length;
        if (vlen === 0) continue;
        if (fields)
          fields += sep;
        for (let j = 0; j < vlen; ++j) {
          if (j)
            fields += sep;
          fields += ks;
          fields += convert(v[j], encode);
        }
      } else {
        if (fields)
          fields += sep;
        fields += ks;
        fields += convert(v, encode);
      }
    }
    return fields;
  }
  return \"\";
}

function charCodes(str) {
  if (str.length === 0) return [];
  if (str.length === 1) return [str.charCodeAt(0)];
  const ret = new Array(str.length);
  for (let i = 0; i < str.length; ++i)
    ret[i] = str.charCodeAt(i);
  return ret;
}
const defSepCodes = [38]; // &
const defEqCodes = [61]; // =

function addKeyVal(obj, key, value, keyEncoded, valEncoded, decode) {
  if (key.length > 0 && keyEncoded)
    key = decodeStr(key, decode);
  if (value.length > 0 && valEncoded)
    value = decodeStr(value, decode);

  if (obj[key] === undefined) {
    obj[key] = value;
  } else {
    const curValue = obj[key];
    if (curValue.pop)
      curValue[curValue.length] = value;
    else
      obj[key] = [curValue, value];
  }
}

function parseAmpEq(qs, obj) {
  let pairs = 1000;
  let lastPos = 0;
  let eqPos = -1;
  let key = \"\";
  let value = \"\";
  let keyEncoded = false;
  let valEncoded = false;
  let encodeCheck = 0;
  const plusChar = \" \";
  const len = qs.length;

  for (let i = 0; i < len; ++i) {
    const code = qs.charCodeAt(i);

    if (code === 38) { // \"&\"
      const end = i;
      if (eqPos === -1) {
        if (lastPos < end)
          key += qs.slice(lastPos, end);
        else if (key.length === 0) {
          if (--pairs === 0)
            return obj;
          lastPos = i + 1;
          encodeCheck = 0;
          continue;
        }
      } else if (lastPos < end) {
        value += qs.slice(lastPos, end);
      }

      addKeyVal(obj, key, value, keyEncoded, valEncoded, qsUnescape);

      if (--pairs === 0)
        return obj;
      keyEncoded = false;
      valEncoded = false;
      key = \"\";
      value = \"\";
      encodeCheck = 0;
      lastPos = i + 1;
      eqPos = -1;
      continue;
    }

    if (eqPos === -1) {
      if (code === 61) { // \"=\"
        if (lastPos < i)
          key += qs.slice(lastPos, i);
        encodeCheck = 0;
        lastPos = i + 1;
        eqPos = i;
        continue;
      }
      if (!keyEncoded) {
        if (code === 37) {
          encodeCheck = 1;
        } else if (encodeCheck > 0) {
          if (isHexTable[code] === 1) {
            if (++encodeCheck === 3)
              keyEncoded = true;
          } else {
            encodeCheck = 0;
          }
        }
      }
      if (code === 43) {
        if (lastPos < i)
          key += qs.slice(lastPos, i);
        key += plusChar;
        lastPos = i + 1;
      }
      continue;
    }

    if (code === 43) {
      if (lastPos < i)
        value += qs.slice(lastPos, i);
      value += plusChar;
      lastPos = i + 1;
    } else if (!valEncoded) {
      if (code === 37) {
        encodeCheck = 1;
      } else if (encodeCheck > 0) {
        if (isHexTable[code] === 1) {
          if (++encodeCheck === 3)
            valEncoded = true;
        } else {
          encodeCheck = 0;
        }
      }
    }
  }

  if (lastPos < len) {
    if (eqPos === -1)
      key += qs.slice(lastPos);
    else
      value += qs.slice(lastPos);
  } else if (eqPos === -1 && key.length === 0) {
    return obj;
  }

  addKeyVal(obj, key, value, keyEncoded, valEncoded, qsUnescape);
  return obj;
}

function parse(qs, sep, eq, options) {
  const obj = { __proto__: null };

  if (typeof qs !== \"string\" || qs.length === 0) {
    return obj;
  }

  if ((sep === undefined || sep === \"&\") &&
      (eq === undefined || eq === \"=\") &&
      QueryString.unescape === qsUnescape &&
      (!options ||
       (options.maxKeys === undefined &&
        options.decodeURIComponent === undefined))) {
    return parseAmpEq(qs, obj);
  }

  const sepCodes = (!sep ? defSepCodes : charCodes(String(sep)));
  const eqCodes = (!eq ? defEqCodes : charCodes(String(eq)));
  const sepLen = sepCodes.length;
  const eqLen = eqCodes.length;

  let pairs = 1000;
  if (options && typeof options.maxKeys === \"number\") {
    pairs = (options.maxKeys > 0 ? options.maxKeys : -1);
  }

  let decode = QueryString.unescape;
  if (options && typeof options.decodeURIComponent === \"function\") {
    decode = options.decodeURIComponent;
  }
  const customDecode = (decode !== qsUnescape);

  let lastPos = 0;
  let sepIdx = 0;
  let eqIdx = 0;
  let key = \"\";
  let value = \"\";
  let keyEncoded = customDecode;
  let valEncoded = customDecode;
  const plusChar = (customDecode ? \"%20\" : \" \");
  let encodeCheck = 0;
  for (let i = 0; i < qs.length; ++i) {
    const code = qs.charCodeAt(i);

    if (code === sepCodes[sepIdx]) {
      if (++sepIdx === sepLen) {
        const end = i - sepIdx + 1;
        if (eqIdx < eqLen) {
          if (lastPos < end) {
            key += qs.slice(lastPos, end);
          } else if (key.length === 0) {
            if (--pairs === 0)
              return obj;
            lastPos = i + 1;
            sepIdx = eqIdx = 0;
            continue;
          }
        } else if (lastPos < end) {
          value += qs.slice(lastPos, end);
        }

        addKeyVal(obj, key, value, keyEncoded, valEncoded, decode);

        if (--pairs === 0)
          return obj;
        keyEncoded = valEncoded = customDecode;
        key = value = \"\";
        encodeCheck = 0;
        lastPos = i + 1;
        sepIdx = eqIdx = 0;
      }
    } else {
      sepIdx = 0;
      if (eqIdx < eqLen) {
        if (code === eqCodes[eqIdx]) {
          if (++eqIdx === eqLen) {
            const end = i - eqIdx + 1;
            if (lastPos < end)
              key += qs.slice(lastPos, end);
            encodeCheck = 0;
            lastPos = i + 1;
          }
          continue;
        } else {
          eqIdx = 0;
          if (!keyEncoded) {
            if (code === 37/* % */) {
              encodeCheck = 1;
              continue;
            } else if (encodeCheck > 0) {
              if (isHexTable[code] === 1) {
                if (++encodeCheck === 3)
                  keyEncoded = true;
                continue;
              } else {
                encodeCheck = 0;
              }
            }
          }
        }
        if (code === 43/* + */) {
          if (lastPos < i)
            key += qs.slice(lastPos, i);
          key += plusChar;
          lastPos = i + 1;
          continue;
        }
      }
      if (code === 43/* + */) {
        if (lastPos < i)
          value += qs.slice(lastPos, i);
        value += plusChar;
        lastPos = i + 1;
      } else if (!valEncoded) {
        if (code === 37/* % */) {
          encodeCheck = 1;
        } else if (encodeCheck > 0) {
          if (isHexTable[code] === 1) {
            if (++encodeCheck === 3)
              valEncoded = true;
          } else {
            encodeCheck = 0;
          }
        }
      }
    }
  }

  if (lastPos < qs.length) {
    if (eqIdx < eqLen)
      key += qs.slice(lastPos);
    else if (sepIdx < sepLen)
      value += qs.slice(lastPos);
  } else if (eqIdx === 0 && key.length === 0) {
    return obj;
  }

  addKeyVal(obj, key, value, keyEncoded, valEncoded, decode);

  return obj;
}

function decodeStr(s, decoder) {
  try {
    return decoder(s);
  } catch {
    return QueryString.unescape(s, true);
  }
}
"

pub const url = "\"use strict\";

// node's lib/internal/url.js and lib/url.js over a native parser. Not here:
// URLPattern, URL.createObjectURL and URL.revokeObjectURL, and the warning
// url.parse gives

const {
  typeError,
  invalidArgType,
  invalidArgValue,
  inspect: describe,
  markUncloneable,
  validateFunction,
  validateString,
} = require(\"spark:errors\");
const path = require(\"node:path\");
const { Buffer } = require(\"node:buffer\");
const querystring = require(\"node:querystring\");
const { encodeStr, hexTable, isHexTable } = require(\"spark:querystring\");
const process = require(\"node:process\");

const CHAR_TAB = 9;
const CHAR_LINE_FEED = 10;
const CHAR_CARRIAGE_RETURN = 13;
const CHAR_SPACE = 32;
const CHAR_DOUBLE_QUOTE = 34;
const CHAR_HASH = 35;
const CHAR_PERCENT = 37;
const CHAR_SINGLE_QUOTE = 39;
const CHAR_FORWARD_SLASH = 47;
const CHAR_COLON = 58;
const CHAR_SEMICOLON = 59;
const CHAR_LEFT_ANGLE_BRACKET = 60;
const CHAR_RIGHT_ANGLE_BRACKET = 62;
const CHAR_QUESTION_MARK = 63;
const CHAR_AT = 64;
const CHAR_LEFT_SQUARE_BRACKET = 91;
const CHAR_BACKWARD_SLASH = 92;
const CHAR_RIGHT_SQUARE_BRACKET = 93;
const CHAR_CIRCUMFLEX_ACCENT = 94;
const CHAR_GRAVE_ACCENT = 96;
const CHAR_LOWERCASE_A = 97;
const CHAR_LOWERCASE_Z = 122;
const CHAR_LEFT_CURLY_BRACKET = 123;
const CHAR_VERTICAL_LINE = 124;
const CHAR_RIGHT_CURLY_BRACKET = 125;
const CHAR_NO_BREAK_SPACE = 160;
const CHAR_ZERO_WIDTH_NOBREAK_SPACE = 65279;

const isWindows = process.platform === \"win32\";
const platform = process.platform;
const kEnumerableProperty = { __proto__: null, enumerable: true };
const kEmptyObject = Object.freeze({ __proto__: null });
const kValidateObjectAllowObjects = true;
const IteratorPrototype = Object.getPrototypeOf(
  Object.getPrototypeOf([][Symbol.iterator]()),
);

const inspect = Object.assign((value, options) => describe(value, options), {
  custom: Symbol.for(\"nodejs.util.inspect.custom\"),
});

function removeColors(str) {
  return String(str).replace(/\\u001b\\[\\d\\d?m/g, \"\");
}

function getConstructorOf(obj) {
  while (obj) {
    const descriptor = Object.getOwnPropertyDescriptor(obj, \"constructor\");
    if (
      descriptor !== undefined &&
      typeof descriptor.value === \"function\" &&
      descriptor.value.name !== \"\"
    ) {
      return descriptor.value;
    }
    obj = Object.getPrototypeOf(obj);
  }
  return null;
}

function markTransferMode(object, cloneable) {
  if (!cloneable) markUncloneable(object);
}

function spliceOne(list, index) {
  for (; index + 1 < list.length; index++) list[index] = list[index + 1];
  list.pop();
}

// with allowObjects, arrays and functions pass too
function validateObject(value, name, allowObjects = false) {
  const fine = allowObjects
    ? value !== null &&
      (typeof value === \"object\" || typeof value === \"function\")
    : value !== null && !Array.isArray(value) && typeof value === \"object\";
  if (!fine) throw invalidArgType(name, \"Object\", value);
}

function isHexByte(byte) {
  return (
    (byte >= 0x30 && byte <= 0x39) ||
    (byte >= 0x41 && byte <= 0x46) ||
    (byte >= 0x61 && byte <= 0x66)
  );
}

// node's internal/data_url percentDecode
function percentDecode(input) {
  const length = input.length;
  const output = new Uint8Array(length);
  let j = 0;
  for (let i = 0; i < length; ++i) {
    const byte = input[i];
    if (byte !== 0x25) {
      output[j++] = byte;
    } else if (!(isHexByte(input[i + 1]) && isHexByte(input[i + 2]))) {
      output[j++] = 0x25;
    } else {
      output[j++] = Number.parseInt(
        String.fromCharCode(input[i + 1], input[i + 2]),
        16,
      );
      i += 2;
    }
  }
  return length === j ? output : output.subarray(0, j);
}

// called with `new` in the code below, as node's error classes are
function ERR_ARG_NOT_ITERABLE(name) {
  return typeError(\"ERR_ARG_NOT_ITERABLE\", `${name} must be iterable`);
}

function ERR_INVALID_ARG_TYPE(name, expected, actual) {
  return invalidArgType(name, expected, actual);
}

function ERR_INVALID_ARG_VALUE(name, value, reason) {
  return invalidArgValue(name, value, reason);
}

function ERR_INVALID_FILE_URL_HOST(os) {
  return typeError(
    \"ERR_INVALID_FILE_URL_HOST\",
    `File URL host must be \"localhost\" or empty on ${os}`,
  );
}

function ERR_INVALID_FILE_URL_PATH(reason, input) {
  const error = typeError(
    \"ERR_INVALID_FILE_URL_PATH\",
    `File URL path ${reason}`,
  );
  if (input !== undefined) error.input = input;
  return error;
}

function ERR_INVALID_THIS(type) {
  return typeError(
    \"ERR_INVALID_THIS\",
    `Value of \"this\" must be of type ${type}`,
  );
}

function ERR_INVALID_TUPLE(name, kind) {
  return typeError(
    \"ERR_INVALID_TUPLE\",
    `${name} must be an iterable ${kind} tuple`,
  );
}

function ERR_INVALID_URL(input, base = null) {
  const error = typeError(\"ERR_INVALID_URL\", \"Invalid URL\");
  error.input = input;
  if (base != null) error.base = base;
  return error;
}

function ERR_INVALID_URL_SCHEME(expected) {
  if (typeof expected === \"string\") expected = [expected];
  const res =
    expected.length === 2
      ? `one of scheme ${expected[0]} or ${expected[1]}`
      : `of scheme ${expected[0]}`;
  return typeError(\"ERR_INVALID_URL_SCHEME\", `The URL must be ${res}`);
}

function ERR_MISSING_ARGS(...args) {
  let msg = \"The \";
  const len = args.length;
  const wrap = (a) => `\"${a}\"`;
  args = args.map((a) =>
    Array.isArray(a) ? a.map(wrap).join(\" or \") : wrap(a),
  );
  switch (len) {
    case 1:
      msg += `${args[0]} argument`;
      break;
    case 2:
      msg += `${args[0]} and ${args[1]} arguments`;
      break;
    default:
      msg += args.slice(0, len - 1).join(\", \");
      msg += `, and ${args[len - 1]} arguments`;
      break;
  }
  return typeError(\"ERR_MISSING_ARGS\", `${msg} must be specified`);
}

const kHostnameAction = 2;
const kPathnameAction = 6;

// what a file path has escaped before it becomes a url path
function escapeUnsafeChars(text, windows) {
  let out = \"\";
  for (const ch of text) {
    switch (ch) {
      case \"\\0\":
        out += \"%00\";
        break;
      case \"\\t\":
        out += \"%09\";
        break;
      case \"\\n\":
        out += \"%0A\";
        break;
      case \"\\r\":
        out += \"%0D\";
        break;
      case \" \":
        out += \"%20\";
        break;
      case '\"':
        out += \"%22\";
        break;
      case \"#\":
        out += \"%23\";
        break;
      case \"%\":
        out += \"%25\";
        break;
      case \"?\":
        out += \"%3F\";
        break;
      case \"[\":
        out += \"%5B\";
        break;
      case \"\\\\\":
        out += windows ? \"/\" : \"%5C\";
        break;
      case \"]\":
        out += \"%5D\";
        break;
      case \"^\":
        out += \"%5E\";
        break;
      case \"|\":
        out += \"%7C\";
        break;
      case \"~\":
        out += \"%7E\";
        break;
      default:
        out += ch;
    }
  }
  return out;
}

// node's native url binding, over the parser. A parse or an update leaves the
// positions of the url's parts in urlComponents
const bindingUrl = {
  urlComponents: [0, 0, 0, 0, 0, 0, 0, 0, 1],

  publish(result) {
    this.urlComponents = result[1];
    return result[0];
  },

  parse(input, base, raiseException) {
    const result = native.parse(input, base);
    if (result === undefined) {
      if (raiseException) throw new ERR_INVALID_URL(input, base);
      return undefined;
    }
    return this.publish(result);
  },

  canParse(input, base) {
    return native.canParse(input, base);
  },

  update(href, action, value) {
    const result = native.update(href, action, value);
    return result === undefined ? false : this.publish(result);
  },

  pathToFileURL(input, windows, hostname) {
    let result = native.parse(\"file://\", undefined);
    if (windows && hostname !== undefined) {
      result = native.update(result[0], kHostnameAction, hostname) ?? result;
    }
    result =
      native.update(
        result[0],
        kPathnameAction,
        escapeUnsafeChars(input, windows),
      ) ?? result;
    return this.publish(result);
  },

  // a url as text with parts left out, and the host as unicode on request
  format(href, fragment, unicode, search, auth) {
    let result = native.parse(href, undefined);
    const drop = (action) => {
      result = native.update(result[0], action, \"\") ?? result;
    };
    if (!fragment) drop(8);
    if (!search) drop(7);
    if (!auth) {
      drop(4);
      drop(5);
    }
    const [text, positions] = result;
    if (!unicode) return text;
    const start = positions[2] + (text[positions[2]] === \"@\" ? 1 : 0);
    return (
      text.slice(0, start) +
      native.domainToUnicode(text.slice(start, positions[3])) +
      text.slice(positions[3])
    );
  },

  getOrigin(url) {
    return new URL(url).origin;
  },

  domainToASCII: native.domainToASCII,
  domainToUnicode: native.domainToUnicode,
  toASCII: native.toASCII,
};

const FORWARD_SLASH = /\\//g;

const contextForInspect = Symbol(\"context\");

const unsafeProtocol = new Set([
  \"javascript\",
  \"javascript:\",
]);
const hostlessProtocol = new Set([
  \"javascript\",
  \"javascript:\",
]);
const slashedProtocol = new Set([
  \"http\",
  \"http:\",
  \"https\",
  \"https:\",
  \"ftp\",
  \"ftp:\",
  \"gopher\",
  \"gopher:\",
  \"file\",
  \"file:\",
  \"ws\",
  \"ws:\",
  \"wss\",
  \"wss:\",
]);

const updateActions = {
  kProtocol: 0,
  kHost: 1,
  kHostname: 2,
  kPort: 3,
  kUsername: 4,
  kPassword: 5,
  kPathname: 6,
  kSearch: 7,
  kHash: 8,
  kHref: 9,
};

function setURLContextFromBinding(ctx, href) {
  const c = bindingUrl.urlComponents;
  ctx.href = href;
  ctx.protocol_end = c[0];
  ctx.username_end = c[1];
  ctx.host_start = c[2];
  ctx.host_end = c[3];
  ctx.pathname_start = c[5];
  ctx.search_start = c[6];
  ctx.hash_start = c[7];
  ctx.port = c[4];
  ctx.scheme_type = c[8];
}

class URLContext {
  static #omitted = 4294967295;

    constructor(href) {
    if (href === undefined) {
      this.href = \"\";
      this.protocol_end = 0;
      this.username_end = 0;
      this.host_start = 0;
      this.host_end = 0;
      this.pathname_start = 0;
      this.search_start = 0;
      this.hash_start = 0;
      this.port = 0;
      this.scheme_type = 1;
      return;
    }
    setURLContextFromBinding(this, href);
  }

  get hasPort() {
    return this.port !== URLContext.#omitted;
  }

  get hasSearch() {
    return this.search_start !== URLContext.#omitted;
  }

  get hasHash() {
    return this.hash_start !== URLContext.#omitted;
  }
}

let setURLSearchParamsModified;
let setURLSearchParamsContext;
let getURLSearchParamsList;
let setURLSearchParams;
let isURLInstance;
let getURLHref;

class URLSearchParamsIterator {
  #target;
  #kind;
  #index;

  constructor(target, kind) {
    this.#target = target;
    this.#kind = kind;
    this.#index = 0;
  }

  next() {
    if (typeof this !== \"object\" || this === null || !(#target in this))
      throw new ERR_INVALID_THIS(\"URLSearchParamsIterator\");

    const index = this.#index;
    const values = getURLSearchParamsList(this.#target);
    const len = values.length;
    if (index >= len) {
      return {
        done: true,
        value: undefined,
      };
    }

    const name = values[index];
    const value = values[index + 1];
    this.#index = index + 2;

    let result;
    if (this.#kind === \"key\") {
      result = name;
    } else if (this.#kind === \"value\") {
      result = value;
    } else {
      result = [name, value];
    }

    return {
      done: false,
      value: result,
    };
  }

  [inspect.custom](recurseTimes, ctx) {
    if (!this || typeof this !== \"object\" || !(#target in this))
      throw new ERR_INVALID_THIS(\"URLSearchParamsIterator\");

    if (typeof recurseTimes === \"number\" && recurseTimes < 0)
      return ctx.stylize(\"[Object]\", \"special\");

    const innerOpts = { ...ctx };
    if (recurseTimes !== null) {
      innerOpts.depth = recurseTimes - 1;
    }
    const index = this.#index;
    const values = getURLSearchParamsList(this.#target);
    const output = [];
    for (let i = index; i < values.length; i++) {
      const isKey = ((i - index) % 2) === 0;
      if (this.#kind === \"key\") {
        if (isKey) output.push(values[i]);
      } else if (!isKey) {
        output.push(this.#kind === \"value\" ? values[i] : [values[i - 1], values[i]]);
      }
    }
    const hasBreak = inspect(output, innerOpts).includes(\"\\n\");
    const outputStrs = output.map((p) => inspect(p, innerOpts));
    let outputStr;
    if (hasBreak) {
      outputStr = `\\n  ${outputStrs.join(\",\\n  \")}`;
    } else {
      outputStr = ` ${outputStrs.join(\", \")}`;
    }
    return `${this[Symbol.toStringTag]} {${outputStr} }`;
  }
}

delete URLSearchParamsIterator.prototype.constructor;
Object.setPrototypeOf(URLSearchParamsIterator.prototype, IteratorPrototype);

Object.defineProperties(URLSearchParamsIterator.prototype, {
  [Symbol.toStringTag]: { __proto__: null, configurable: true, value: \"URLSearchParams Iterator\" },
  next: kEnumerableProperty,
});

class URLSearchParams {
  #searchParams = [];

  #context;

  #serialized;

  static {
    setURLSearchParamsContext = (obj, ctx) => {
      obj.#context = ctx;
    };
    getURLSearchParamsList = (obj) => obj.#searchParams;
    setURLSearchParams = (obj, query) => {
      obj.#serialized = undefined;
      if (query === undefined) {
        obj.#searchParams = [];
      } else {
        obj.#searchParams = parseParams(query);
      }
    };
  }

  #markUpdated() {
    this.#serialized = undefined;
    if (this.#context) {
      setURLSearchParamsModified(this.#context);
    }
  }

  constructor(init = undefined) {
    markTransferMode(this, false, false);

    if (init === undefined) {
    } else if (init !== null && (typeof init === \"object\" || typeof init === \"function\")) {
      const method = init[Symbol.iterator];
      if (method === this[Symbol.iterator] && #searchParams in init) {
        const childParams = init.#searchParams;
        this.#searchParams = childParams.slice();
        this.#serialized = init.#serialized;
      } else if (method != null) {
        if (typeof method !== \"function\") {
          throw new ERR_ARG_NOT_ITERABLE(\"Query pairs\");
        }

        for (const pair of init) {
          if (pair == null) {
            throw new ERR_INVALID_TUPLE(\"Each query pair\", \"[name, value]\");
          } else if (Array.isArray(pair)) {
            if (pair.length !== 2) {
              throw new ERR_INVALID_TUPLE(\"Each query pair\", \"[name, value]\");
            }
            this.#searchParams.push(toUSVString(pair[0]), toUSVString(pair[1]));
          } else {
            if (((typeof pair !== \"object\" && typeof pair !== \"function\") ||
                typeof pair[Symbol.iterator] !== \"function\")) {
              throw new ERR_INVALID_TUPLE(\"Each query pair\", \"[name, value]\");
            }

            let length = 0;

            for (const element of pair) {
              length++;
              this.#searchParams.push(toUSVString(element));
            }

            if (length !== 2) {
              throw new ERR_INVALID_TUPLE(\"Each query pair\", \"[name, value]\");
            }
          }
        }
      } else {
        let visited;
        const keys = Reflect.ownKeys(init);
        for (let i = 0; i < keys.length; i++) {
          const key = keys[i];
          const desc = Reflect.getOwnPropertyDescriptor(init, key);
          if (desc !== undefined && desc.enumerable) {
            const typedKey = toUSVString(key);
            const typedValue = toUSVString(init[key]);

            if (visited === undefined && typedKey === key) {
              this.#searchParams.push(typedKey, typedValue);
              continue;
            }

            if (visited === undefined) {
              visited = new Map();
              for (let j = 0; j < this.#searchParams.length; j += 2) {
                visited.set(this.#searchParams[j], j + 1);
              }
            }

            const keyIdx = visited.get(typedKey);
            if (keyIdx !== undefined) {
              this.#searchParams[keyIdx] = typedValue;
            } else {
              visited.set(typedKey, this.#searchParams.push(typedKey, typedValue) - 1);
            }
          }
        }
      }
    } else {
      init = toUSVString(init);
      this.#searchParams = init ? parseParams(init) : [];
    }
  }

  [inspect.custom](recurseTimes, ctx) {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    if (typeof recurseTimes === \"number\" && recurseTimes < 0)
      return ctx.stylize(\"[Object]\", \"special\");

    const separator = \", \";
    const innerOpts = { ...ctx };
    if (recurseTimes !== null) {
      innerOpts.depth = recurseTimes - 1;
    }
    const innerInspect = (v) => inspect(v, innerOpts);

    const list = this.#searchParams;
    const output = [];
    for (let i = 0; i < list.length; i += 2)
      output.push(`${innerInspect(list[i])} => ${innerInspect(list[i + 1])}`);

    let length = -separator.length;
    for (let i = 0; i < output.length; i++) {
      length += removeColors(output[i]).length + separator.length;
    }
    if (length > ctx.breakLength) {
      return `${this.constructor.name} {\\n` +
      `  ${output.join(\",\\n  \")} }`;
    } else if (output.length) {
      return `${this.constructor.name} { ` +
      `${output.join(separator)} }`;
    }
    return `${this.constructor.name} {}`;
  }

  get size() {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");
    return this.#searchParams.length / 2;
  }

  append(name, value) {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    if (arguments.length < 2) {
      throw new ERR_MISSING_ARGS(\"name\", \"value\");
    }

    name = toUSVString(name);
    value = toUSVString(value);
    this.#searchParams.push(name, value);
    this.#markUpdated();
  }

  delete(name, value = undefined) {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    if (arguments.length < 1) {
      throw new ERR_MISSING_ARGS(\"name\");
    }

    const list = this.#searchParams;
    name = toUSVString(name);
    const { length } = list;
    let write = 0;

    if (value !== undefined) {
      value = toUSVString(value);
      for (let i = 0; i < length; i += 2) {
        if (list[i] === name && list[i + 1] === value) {
          continue;
        }
        if (write !== i) {
          list[write] = list[i];
          list[write + 1] = list[i + 1];
        }
        write += 2;
      }
    } else {
      for (let i = 0; i < length; i += 2) {
        if (list[i] === name) {
          continue;
        }
        if (write !== i) {
          list[write] = list[i];
          list[write + 1] = list[i + 1];
        }
        write += 2;
      }
    }

    if (write !== length) {
      list.length = write;
      this.#serialized = undefined;
    }

    if (this.#context) {
      setURLSearchParamsModified(this.#context);
    }
  }

  get(name) {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    if (arguments.length < 1) {
      throw new ERR_MISSING_ARGS(\"name\");
    }

    const list = this.#searchParams;
    name = toUSVString(name);
    const { length } = list;
    for (let i = 0; i < length; i += 2) {
      if (list[i] === name) {
        return list[i + 1];
      }
    }
    return null;
  }

  getAll(name) {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    if (arguments.length < 1) {
      throw new ERR_MISSING_ARGS(\"name\");
    }

    const list = this.#searchParams;
    const values = [];
    name = toUSVString(name);
    const { length } = list;
    for (let i = 0; i < length; i += 2) {
      if (list[i] === name) {
        values.push(list[i + 1]);
      }
    }
    return values;
  }

  has(name, value = undefined) {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    if (arguments.length < 1) {
      throw new ERR_MISSING_ARGS(\"name\");
    }

    const list = this.#searchParams;
    name = toUSVString(name);

    if (value !== undefined) {
      value = toUSVString(value);
    }

    const { length } = list;
    for (let i = 0; i < length; i += 2) {
      if (list[i] === name) {
        if (value === undefined || list[i + 1] === value) {
          return true;
        }
      }
    }

    return false;
  }

  set(name, value) {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    if (arguments.length < 2) {
      throw new ERR_MISSING_ARGS(\"name\", \"value\");
    }

    const list = this.#searchParams;
    name = toUSVString(name);
    value = toUSVString(value);
    const { length } = list;

    let found = false;
    let write = 0;
    for (let i = 0; i < length; i += 2) {
      const cur = list[i];
      let keep = true;
      if (cur === name) {
        if (!found) {
          list[write] = cur;
          list[write + 1] = value;
          found = true;
        } else {
          keep = false;
        }
      } else if (write !== i) {
        list[write] = cur;
        list[write + 1] = list[i + 1];
      }
      if (keep)
        write += 2;
    }

    if (found && write !== length) {
      list.length = write;
    }

    if (!found) {
      list.push(name, value);
    }

    this.#markUpdated();
  }

  sort() {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    const a = this.#searchParams;
    const len = a.length;

    if (len <= 2) {
    } else if (len < 100) {
      for (let i = 2; i < len; i += 2) {
        const curKey = a[i];
        const curVal = a[i + 1];
        let j;
        for (j = i - 2; j >= 0; j -= 2) {
          if (a[j] > curKey) {
            a[j + 2] = a[j];
            a[j + 3] = a[j + 1];
          } else {
            break;
          }
        }
        a[j + 2] = curKey;
        a[j + 3] = curVal;
      }
    } else {
      const lBuffer = new Array(len);
      const rBuffer = new Array(len);
      for (let step = 2; step < len; step *= 2) {
        for (let start = 0; start < len - 2; start += 2 * step) {
          const mid = start + step;
          let end = mid + step;
          end = end < len ? end : len;
          if (mid > end)
            continue;
          merge(a, start, mid, end, lBuffer, rBuffer);
        }
      }
    }

    this.#markUpdated();
  }

  entries() {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    return new URLSearchParamsIterator(this, \"key+value\");
  }

  forEach(callback, thisArg = undefined) {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    validateFunction(callback, \"callback\");

    let list = this.#searchParams;

    let i = 0;
    while (i < list.length) {
      const key = list[i];
      const value = list[i + 1];
      callback.call(thisArg, value, key, this);
      list = this.#searchParams;
      i += 2;
    }
  }

  keys() {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    return new URLSearchParamsIterator(this, \"key\");
  }

  values() {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    return new URLSearchParamsIterator(this, \"value\");
  }

  toString() {
    if (typeof this !== \"object\" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS(\"URLSearchParams\");

    if (this.#serialized !== undefined) {
      return this.#serialized;
    }
    const serialized = serializeParams(this.#searchParams);
    this.#serialized = serialized;
    return serialized;
  }
}

Object.defineProperties(URLSearchParams.prototype, {
  append: kEnumerableProperty,
  delete: kEnumerableProperty,
  get: kEnumerableProperty,
  getAll: kEnumerableProperty,
  has: kEnumerableProperty,
  set: kEnumerableProperty,
  size: kEnumerableProperty,
  sort: kEnumerableProperty,
  entries: kEnumerableProperty,
  forEach: kEnumerableProperty,
  keys: kEnumerableProperty,
  values: kEnumerableProperty,
  toString: kEnumerableProperty,
  [Symbol.toStringTag]: { __proto__: null, configurable: true, value: \"URLSearchParams\" },

  [Symbol.iterator]: {
    __proto__: null,
    configurable: true,
    writable: true,
    value: URLSearchParams.prototype.entries,
  },
});

function isURL(self) {
  return Boolean(self?.href && self.protocol && self.auth === undefined && self.path === undefined);
}

const kParseURLSymbol = Symbol(\"kParseURL\");
const kCreateURLFromPosixPathSymbol = Symbol(\"kCreateURLFromPosixPath\");
const kCreateURLFromWindowsPathSymbol = Symbol(\"kCreateURLFromWindowsPath\");

class URL {
  #context;
  #searchParams;
  #searchParamsModified;

  static {
    isURLInstance = (value) => typeof value === \"object\" && value !== null && #context in value;

    getURLHref = (value) => {
      value.#ensureSearchParamsUpdated();
      return value.#context.href;
    };

    setURLSearchParamsModified = (obj) => {
      obj.#searchParamsModified = true;

      if (obj.#context.hasSearch) {
        obj.#updateContext(bindingUrl.update(obj.#context.href, updateActions.kSearch, \"\"));
      }
    };
  }

  constructor(input, base = undefined, parseSymbol = undefined) {
    if (arguments.length === 0) {
      throw new ERR_MISSING_ARGS(\"url\");
    }

    if (typeof input !== \"string\") {
      input = `${input}`;
    }

    if (base !== undefined && typeof base !== \"string\") {
      base = `${base}`;
    }

    let href;
    if (arguments.length < 3) {
      href = bindingUrl.parse(input, base, true);
    } else {
      const raiseException = parseSymbol !== kParseURLSymbol;
      const interpretAsWindowsPath = parseSymbol === kCreateURLFromWindowsPathSymbol;
      const pathToFileURL = interpretAsWindowsPath || (parseSymbol === kCreateURLFromPosixPathSymbol);
      href = pathToFileURL ?
        bindingUrl.pathToFileURL(input, interpretAsWindowsPath, base) :
        bindingUrl.parse(input, base, raiseException);
    }

    this.#context = href ? new URLContext(href) : new URLContext();
    markTransferMode(this, false, false);
  }

  static parse(input, base = undefined) {
    if (arguments.length === 0) {
      throw new ERR_MISSING_ARGS(\"url\");
    }
    const parsedURLObject = new URL(input, base, kParseURLSymbol);
    return parsedURLObject.href ? parsedURLObject : null;
  }

  [inspect.custom](depth, opts) {
    if (typeof depth === \"number\" && depth < 0)
      return this;

    const constructor = getConstructorOf(this) || URL;
    const obj = { __proto__: { constructor } };

    obj.href = this.href;
    obj.origin = this.origin;
    obj.protocol = this.protocol;
    obj.username = this.username;
    obj.password = this.password;
    obj.host = this.host;
    obj.hostname = this.hostname;
    obj.port = this.port;
    obj.pathname = this.pathname;
    obj.search = this.search;
    obj.searchParams = this.searchParams;
    obj.hash = this.hash;

    if (opts.showHidden) {
      obj[contextForInspect] = this.#context;
    }

    return `${constructor.name} ${inspect(obj, opts)}`;
  }

  #getSearchFromContext() {
    if (!this.#context.hasSearch) return \"\";
    let endsAt = this.#context.href.length;
    if (this.#context.hasHash) endsAt = this.#context.hash_start;
    if (endsAt - this.#context.search_start <= 1) return \"\";
    return this.#context.href.slice(this.#context.search_start, endsAt);
  }

  #getSearchFromParams() {
    if (!this.#searchParams?.size) return \"\";
    return `?${this.#searchParams}`;
  }

  #ensureSearchParamsUpdated() {
    if (this.#searchParamsModified) {
      this.#searchParamsModified = false;
      this.#updateContext(bindingUrl.update(this.#context.href, updateActions.kSearch, this.#getSearchFromParams()));
    }
  }

    #updateContext(href, shouldUpdateSearchParams = false) {
    const previousSearch = shouldUpdateSearchParams && this.#searchParams &&
      (this.#searchParamsModified ? this.#getSearchFromParams() : this.#getSearchFromContext());

    setURLContextFromBinding(this.#context, href);

    if (this.#searchParams) {
      if (shouldUpdateSearchParams) {
        const currentSearch = this.#getSearchFromContext();
        if (previousSearch !== currentSearch) {
          setURLSearchParams(this.#searchParams, currentSearch);
          this.#searchParamsModified = false;
        }
      }

      this.#ensureSearchParamsUpdated();
    }
  }

  toString() {
    this.#ensureSearchParamsUpdated();
    return this.#context.href;
  }

  get href() {
    this.#ensureSearchParamsUpdated();
    return this.#context.href;
  }

  set href(value) {
    value = `${value}`;
    const href = bindingUrl.update(this.#context.href, updateActions.kHref, value);
    if (!href) { throw new ERR_INVALID_URL(value); }
    this.#updateContext(href, true);
  }

  get origin() {
    const protocol = this.#context.href.slice(0, this.#context.protocol_end);

    if (this.#context.scheme_type !== 1) {
      if (this.#context.scheme_type === 6) {
        return \"null\";
      }
      return `${protocol}//${this.host}`;
    }

    if (protocol === \"blob:\") {
      const path = this.pathname;
      if (path.length > 0) {
        try {
          const out = new URL(path);
          if (out.#context.scheme_type === 0 || out.#context.scheme_type === 2) {
            return `${out.protocol}//${out.host}`;
          }
        } catch {
        }
      }
    }

    return \"null\";
  }

  get protocol() {
    return this.#context.href.slice(0, this.#context.protocol_end);
  }

  set protocol(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kProtocol, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get username() {
    if (this.#context.protocol_end + 2 < this.#context.username_end) {
      return this.#context.href.slice(this.#context.protocol_end + 2, this.#context.username_end);
    }
    return \"\";
  }

  set username(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kUsername, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get password() {
    if (this.#context.host_start - this.#context.username_end > 0) {
      return this.#context.href.slice(this.#context.username_end + 1, this.#context.host_start);
    }
    return \"\";
  }

  set password(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kPassword, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get host() {
    let startsAt = this.#context.host_start;
    if (this.#context.href[startsAt] === \"@\") {
      startsAt++;
    }
    if (startsAt === this.#context.host_end) {
      return \"\";
    }
    return this.#context.href.slice(startsAt, this.#context.pathname_start);
  }

  set host(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kHost, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get hostname() {
    let startsAt = this.#context.host_start;
    if (this.#context.href[startsAt] === \"@\") {
      startsAt++;
    }
    return this.#context.href.slice(startsAt, this.#context.host_end);
  }

  set hostname(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kHostname, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get port() {
    if (this.#context.hasPort) {
      return `${this.#context.port}`;
    }
    return \"\";
  }

  set port(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kPort, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get pathname() {
    let endsAt;
    if (this.#context.hasSearch) {
      endsAt = this.#context.search_start;
    } else if (this.#context.hasHash) {
      endsAt = this.#context.hash_start;
    }
    return this.#context.href.slice(this.#context.pathname_start, endsAt);
  }

  set pathname(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kPathname, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get search() {
    this.#ensureSearchParamsUpdated();
    return this.#getSearchFromContext();
  }

  set search(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kSearch, `${value}`.toWellFormed());
    if (href) {
      this.#updateContext(href, true);
    }
  }

  get searchParams() {
    if (this.#searchParams == null) {
      this.#searchParams = new URLSearchParams(this.#getSearchFromContext());
      setURLSearchParamsContext(this.#searchParams, this);
      this.#searchParamsModified = false;
    }
    return this.#searchParams;
  }

  get hash() {
    if (!this.#context.hasHash || (this.#context.href.length - this.#context.hash_start <= 1)) {
      return \"\";
    }
    return this.#context.href.slice(this.#context.hash_start);
  }

  set hash(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kHash, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  toJSON() {
    this.#ensureSearchParamsUpdated();
    return this.#context.href;
  }

  static canParse(url, base = undefined) {
    if (arguments.length === 0) {
      throw new ERR_MISSING_ARGS(\"url\");
    }

    if (typeof url !== \"string\") {
      url = `${url}`;
    }

    if (base !== undefined) {
      return bindingUrl.canParse(url, typeof base === \"string\" ? base : `${base}`);
    }

    return bindingUrl.canParse(url);
  }
}

Object.defineProperties(URL.prototype, {
  [Symbol.toStringTag]: { __proto__: null, configurable: true, value: \"URL\" },
  toString: kEnumerableProperty,
  href: kEnumerableProperty,
  origin: kEnumerableProperty,
  protocol: kEnumerableProperty,
  username: kEnumerableProperty,
  password: kEnumerableProperty,
  host: kEnumerableProperty,
  hostname: kEnumerableProperty,
  port: kEnumerableProperty,
  pathname: kEnumerableProperty,
  search: kEnumerableProperty,
  searchParams: kEnumerableProperty,
  hash: kEnumerableProperty,
  toJSON: kEnumerableProperty,
});

Object.defineProperties(URL, {
  canParse: {
    __proto__: null,
    configurable: true,
    writable: true,
    enumerable: true,
  },
  parse: {
    __proto__: null,
    configurable: true,
    writable: true,
    enumerable: true,
  },
});

function toUSVString(value) {
  return typeof value === \"string\" ?
    value.toWellFormed() :
    `${value}`.toWellFormed();
}

function unescapeFormComponent(s) {
  try {
    return decodeURIComponent(s);
  } catch {
    return querystring.unescapeBuffer(s).toString();
  }
}

function hasPercentHex(s) {
  const end = s.length - 2;
  for (let i = 0; i < end; i++) {
    if (s.charCodeAt(i) === 37 && // \"%\"
        isHexTable[s.charCodeAt(i + 1)] === 1 &&
        isHexTable[s.charCodeAt(i + 2)] === 1) {
      return true;
    }
  }
  return false;
}

function decodeFormComponent(qs, start, end) {
  if (start >= end) {
    return \"\";
  }
  const s = qs.slice(start, end);
  const plus = s.indexOf(\"+\");
  const pct = s.indexOf(\"%\");
  if (plus === -1 && pct === -1) {
    return s;
  }
  const replaced = plus === -1 ? s : s.replaceAll(\"+\", \" \");
  if (pct === -1 || !hasPercentHex(replaced)) {
    return replaced;
  }
  return unescapeFormComponent(replaced);
}

function parseParams(qs) {
  const len = qs.length;
  let i = qs[0] === \"?\" ? 1 : 0;
  if (i >= len) {
    return [];
  }

  const out = [];
  const encoded = qs.indexOf(\"+\", i) !== -1 || qs.indexOf(\"%\", i) !== -1;
  while (i < len) {
    let amp = qs.indexOf(\"&\", i);
    if (amp === -1) {
      amp = len;
    }
    if (amp !== i) {
      const eq = qs.indexOf(\"=\", i);
      if (eq === -1 || eq > amp) {
        out.push(encoded ? decodeFormComponent(qs, i, amp) : qs.slice(i, amp), \"\");
      } else {
        out.push(
          encoded ? decodeFormComponent(qs, i, eq) : qs.slice(i, eq),
          encoded ? decodeFormComponent(qs, eq + 1, amp) : qs.slice(eq + 1, amp),
        );
      }
    }
    i = amp + 1;
  }
  return out;
}

const noEscape = new Int8Array([
/*
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
*/
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x00 - 0x0F
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x10 - 0x1F
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, // 0x20 - 0x2F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, // 0x30 - 0x3F
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x40 - 0x4F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, // 0x50 - 0x5F
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x60 - 0x6F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,  // 0x70 - 0x7F
]);

const paramHexTable = hexTable.slice();
paramHexTable[0x20] = \"+\";

function serializeParams(array) {
  const len = array.length;
  if (len === 0)
    return \"\";

  if (len === 2) {
    return encodeStr(array[0], noEscape, paramHexTable) + \"=\" +
           encodeStr(array[1], noEscape, paramHexTable);
  }

  const pairs = new Array(len / 2);
  for (let i = 0, j = 0; i < len; i += 2, ++j) {
    pairs[j] = encodeStr(array[i], noEscape, paramHexTable) + \"=\" +
               encodeStr(array[i + 1], noEscape, paramHexTable);
  }
  return pairs.join(\"&\");
}

function merge(out, start, mid, end, lBuffer, rBuffer) {
  const sizeLeft = mid - start;
  const sizeRight = end - mid;
  let l, r, o;

  for (l = 0; l < sizeLeft; l++)
    lBuffer[l] = out[start + l];
  for (r = 0; r < sizeRight; r++)
    rBuffer[r] = out[mid + r];

  l = 0;
  r = 0;
  o = start;
  while (l < sizeLeft && r < sizeRight) {
    if (lBuffer[l] <= rBuffer[r]) {
      out[o++] = lBuffer[l++];
      out[o++] = lBuffer[l++];
    } else {
      out[o++] = rBuffer[r++];
      out[o++] = rBuffer[r++];
    }
  }
  while (l < sizeLeft)
    out[o++] = lBuffer[l++];
  while (r < sizeRight)
    out[o++] = rBuffer[r++];
}

function domainToASCII(domain) {
  if (arguments.length < 1)
    throw new ERR_MISSING_ARGS(\"domain\");

  return bindingUrl.domainToASCII(`${domain}`);
}

function domainToUnicode(domain) {
  if (arguments.length < 1)
    throw new ERR_MISSING_ARGS(\"domain\");

  return bindingUrl.domainToUnicode(`${domain}`);
}

function urlToHttpOptions(url) {
  validateObject(url, \"url\", kValidateObjectAllowObjects);
  const { hostname, pathname, port, username, password, search } = url;
  const options = {
    __proto__: null,
    ...url, // In case the url object was extended by the user.
    protocol: url.protocol,
    hostname: hostname && hostname[0] === \"[\" ?
      hostname.slice(1, -1) :
      hostname,
    hash: url.hash,
    search: search,
    pathname: pathname,
    path: `${pathname || \"\"}${search || \"\"}`,
    href: url.href,
  };
  if (port !== \"\") {
    options.port = Number(port);
  }
  if (username || password) {
    options.auth = `${decodeURIComponent(username)}:${decodeURIComponent(password)}`;
  }
  return options;
}

function getPathFromURLWin32(url) {
  const hostname = url.hostname;
  let pathname = url.pathname;
  for (let n = 0; n < pathname.length; n++) {
    if (pathname[n] === \"%\") {
      const third = pathname.codePointAt(n + 2) | 0x20;
      if ((pathname[n + 1] === \"2\" && third === 102) || // 2f 2F /
          (pathname[n + 1] === \"5\" && third === 99)) {  // 5c 5C \\
        throw new ERR_INVALID_FILE_URL_PATH(
          \"must not include encoded \\\\ or / characters\", url,
        );
      }
    }
  }
  pathname = pathname.replace(FORWARD_SLASH, \"\\\\\");
  if (pathname.includes(\"%\")) {
    pathname = decodeURIComponent(pathname);
  }
  if (hostname !== \"\") {
    return `\\\\\\\\${domainToUnicode(hostname)}${pathname}`;
  }
  const letter = pathname.codePointAt(1) | 0x20;
  const sep = pathname.charAt(2);
  if (letter < CHAR_LOWERCASE_A || letter > CHAR_LOWERCASE_Z ||   // a..z A..Z
      (sep !== \":\")) {
    throw new ERR_INVALID_FILE_URL_PATH(\"must be absolute\", url);
  }
  return pathname.slice(1);
}

function getPathBufferFromURLWin32(url) {
  const hostname = url.hostname;
  let pathname = url.pathname;

  pathname = pathname.replace(FORWARD_SLASH, \"\\\\\");

  const decodedu8 = percentDecode(Buffer.from(pathname, \"utf8\"));
  const decodedPathname = Buffer.from(decodedu8.buffer,
                                      decodedu8.byteOffset,
                                      decodedu8.byteLength);
  if (hostname !== \"\") {

    const prefix = Buffer.from(\"\\\\\\\\\", \"ascii\");
    const domain = Buffer.from(domainToUnicode(hostname), \"utf8\");

    return Buffer.concat([prefix, domain, decodedPathname]);
  }

  const letter = decodedPathname[1] | 0x20;
  const sep = decodedPathname[2];

  if (letter < CHAR_LOWERCASE_A || letter > CHAR_LOWERCASE_Z ||   // a..z A..Z
      (sep !== CHAR_COLON)) {
    throw new ERR_INVALID_FILE_URL_PATH(\"must be absolute\", url);
  }

  return decodedPathname.subarray(1);
}

function getPathFromURLPosix(url) {
  if (url.hostname !== \"\") {
    throw new ERR_INVALID_FILE_URL_HOST(platform);
  }
  const pathname = url.pathname;
  for (let n = 0; n < pathname.length; n++) {
    if (pathname[n] === \"%\") {
      const third = pathname.codePointAt(n + 2) | 0x20;
      if (pathname[n + 1] === \"2\" && third === 102) {
        throw new ERR_INVALID_FILE_URL_PATH(
          \"must not include encoded / characters\",
          url,
        );
      }
    }
  }
  return pathname.includes(\"%\") ? decodeURIComponent(pathname) : pathname;
}

function getPathBufferFromURLPosix(url) {
  if (url.hostname !== \"\") {
    throw new ERR_INVALID_FILE_URL_HOST(platform);
  }
  const pathname = url.pathname;

  const u8 = percentDecode(Buffer.from(pathname, \"utf8\"));
  return Buffer.from(u8.buffer,
                     u8.byteOffset,
                     u8.byteLength);
}

function fileURLToPath(path, options = kEmptyObject) {
  const windows = options?.windows;
  if (typeof path === \"string\")
    path = new URL(path);
  else if (!isURL(path))
    throw new ERR_INVALID_ARG_TYPE(\"path\", [\"string\", \"URL\"], path);
  if (path.protocol !== \"file:\")
    throw new ERR_INVALID_URL_SCHEME(\"file\");
  return (windows ?? isWindows) ? getPathFromURLWin32(path) : getPathFromURLPosix(path);
}

function fileURLToPathBuffer(path, options = kEmptyObject) {
  const windows = options?.windows;
  if (typeof path === \"string\") {
    path = new URL(path);
  } else if (!isURL(path)) {
    throw new ERR_INVALID_ARG_TYPE(\"path\", [\"string\", \"URL\"], path);
  }
  if (path.protocol !== \"file:\") {
    throw new ERR_INVALID_URL_SCHEME(\"file\");
  }
  return (windows ?? isWindows) ? getPathBufferFromURLWin32(path) : getPathBufferFromURLPosix(path);
}

function _pathToFileURL(filepath, options = kEmptyObject) {
  const windows = options?.windows ?? isWindows;
  const isUNC = windows && filepath.startsWith(\"\\\\\\\\\");
  let resolved = isUNC ?
    filepath :
    (windows ? path.win32.resolve(filepath) : path.posix.resolve(filepath));
  if (isUNC || (windows && resolved.startsWith(\"\\\\\\\\\"))) {
    const isExtendedUNC = resolved.startsWith(\"\\\\\\\\?\\\\UNC\\\\\");
    const prefixLength = isExtendedUNC ? 8 : 2;
    const hostnameEndIndex = resolved.indexOf(\"\\\\\", prefixLength);
    if (hostnameEndIndex === -1) {
      throw new ERR_INVALID_ARG_VALUE(
        \"path\",
        resolved,
        \"Missing UNC resource path\",
      );
    }
    if (hostnameEndIndex === 2) {
      throw new ERR_INVALID_ARG_VALUE(
        \"path\",
        resolved,
        \"Empty UNC servername\",
      );
    }
    const hostname = resolved.slice(prefixLength, hostnameEndIndex);
    return new URL(resolved.slice(hostnameEndIndex), hostname, kCreateURLFromWindowsPathSymbol);
  }
  const filePathLast = filepath.charCodeAt(filepath.length - 1);
  if ((filePathLast === CHAR_FORWARD_SLASH ||
       ((windows ?? isWindows) && filePathLast === CHAR_BACKWARD_SLASH)) &&
      resolved[resolved.length - 1] !== path.sep)
    resolved += \"/\";

  return new URL(resolved, undefined, windows ? kCreateURLFromWindowsPathSymbol : kCreateURLFromPosixPathSymbol);
}

function toPathIfFileURL(fileURLOrPath) {
  if (!isURL(fileURLOrPath))
    return fileURLOrPath;
  return fileURLToPath(fileURLOrPath);
}

function getURLOrigin(url) {
  return bindingUrl.getOrigin(url);
}

function Url() {
  this.protocol = null;
  this.slashes = null;
  this.auth = null;
  this.host = null;
  this.port = null;
  this.hostname = null;
  this.hash = null;
  this.search = null;
  this.query = null;
  this.pathname = null;
  this.path = null;
  this.href = null;
}

const protocolPattern = /^[a-z0-9.+-]+:/i;
const portPattern = /:[0-9]*$/;
const hostPattern = /^\\/\\/[^@/]+@[^@/]+/;

const simplePathPattern = /^(\\/\\/?(?!\\/)[^?\\s]*)(\\?[^\\s]*)?$/;

const hostnameMaxLen = 255;
function urlParse(url, parseQueryString, slashesDenoteHost) {
  if (url instanceof Url) return url;

  const urlObject = new Url();
  urlObject.parse(url, parseQueryString, slashesDenoteHost);
  return urlObject;
}

function isIpv6Hostname(hostname) {
  return (
    hostname.charCodeAt(0) === CHAR_LEFT_SQUARE_BRACKET &&
    hostname.charCodeAt(hostname.length - 1) ===
    CHAR_RIGHT_SQUARE_BRACKET
  );
}

const forbiddenHostChars = /[\\0\\t\\n\\r #%/:<>?@[\\\\\\]^|]/;
const forbiddenHostCharsIpv6 = /[\\0\\t\\n\\r #%/<>?@\\\\^|]/;

Url.prototype.parse = function parse(url, parseQueryString, slashesDenoteHost) {
  validateString(url, \"url\");

  let hasHash = false;
  let hasAt = false;
  let start = -1;
  let end = -1;
  let rest = \"\";
  let lastPos = 0;
  for (let i = 0, inWs = false, split = false; i < url.length; ++i) {
    const code = url.charCodeAt(i);

    const isWs = code < 33 ||
                 code === CHAR_NO_BREAK_SPACE ||
                 code === CHAR_ZERO_WIDTH_NOBREAK_SPACE;
    if (start === -1) {
      if (isWs)
        continue;
      lastPos = start = i;
    } else if (inWs) {
      if (!isWs) {
        end = -1;
        inWs = false;
      }
    } else if (isWs) {
      end = i;
      inWs = true;
    }

    if (!split) {
      switch (code) {
        case CHAR_AT:
          hasAt = true;
          break;
        case CHAR_HASH:
          hasHash = true;
        case CHAR_QUESTION_MARK:
          split = true;
          break;
        case CHAR_BACKWARD_SLASH:
          if (i - lastPos > 0)
            rest += url.slice(lastPos, i);
          rest += \"/\";
          lastPos = i + 1;
          break;
      }
    } else if (!hasHash && code === CHAR_HASH) {
      hasHash = true;
    }
  }

  if (start !== -1) {
    if (lastPos === start) {

      if (end === -1) {
        if (start === 0)
          rest = url;
        else
          rest = url.slice(start);
      } else {
        rest = url.slice(start, end);
      }
    } else if (end === -1 && lastPos < url.length) {
      rest += url.slice(lastPos);
    } else if (end !== -1 && lastPos < end) {
      rest += url.slice(lastPos, end);
    }
  }

  if (!slashesDenoteHost && !hasHash && !hasAt) {
    const simplePath = simplePathPattern.exec(rest);
    if (simplePath) {
      this.path = rest;
      this.href = rest;
      this.pathname = simplePath[1];
      if (simplePath[2]) {
        this.search = simplePath[2];
        if (parseQueryString) {
          this.query = querystring.parse(this.search.slice(1));
        } else {
          this.query = this.search.slice(1);
        }
      } else if (parseQueryString) {
        this.search = null;
        this.query = { __proto__: null };
      }
      return this;
    }
  }

  let proto = protocolPattern.exec(rest);
  let lowerProto;
  if (proto) {
    proto = proto[0];
    lowerProto = proto.toLowerCase();
    this.protocol = lowerProto;
    rest = rest.slice(proto.length);
  }

  let slashes;
  if (slashesDenoteHost || proto || hostPattern.test(rest)) {
    slashes = rest.charCodeAt(0) === CHAR_FORWARD_SLASH &&
              rest.charCodeAt(1) === CHAR_FORWARD_SLASH;
    if (slashes && !(proto && hostlessProtocol.has(lowerProto))) {
      rest = rest.slice(2);
      this.slashes = true;
    }
  }

  if (!hostlessProtocol.has(lowerProto) &&
      (slashes || (proto && !slashedProtocol.has(proto)))) {

    let hostEnd = -1;
    let atSign = -1;
    let nonHost = -1;
    for (let i = 0; i < rest.length; ++i) {
      switch (rest.charCodeAt(i)) {
        case CHAR_TAB:
        case CHAR_LINE_FEED:
        case CHAR_CARRIAGE_RETURN:
          rest = rest.slice(0, i) + rest.slice(i + 1);
          i -= 1;
          break;
        case CHAR_SPACE:
        case CHAR_DOUBLE_QUOTE:
        case CHAR_PERCENT:
        case CHAR_SINGLE_QUOTE:
        case CHAR_SEMICOLON:
        case CHAR_LEFT_ANGLE_BRACKET:
        case CHAR_RIGHT_ANGLE_BRACKET:
        case CHAR_BACKWARD_SLASH:
        case CHAR_CIRCUMFLEX_ACCENT:
        case CHAR_GRAVE_ACCENT:
        case CHAR_LEFT_CURLY_BRACKET:
        case CHAR_VERTICAL_LINE:
        case CHAR_RIGHT_CURLY_BRACKET:
          if (nonHost === -1)
            nonHost = i;
          break;
        case CHAR_HASH:
        case CHAR_FORWARD_SLASH:
        case CHAR_QUESTION_MARK:
          if (nonHost === -1)
            nonHost = i;
          hostEnd = i;
          break;
        case CHAR_AT:
          atSign = i;
          nonHost = -1;
          break;
      }
      if (hostEnd !== -1)
        break;
    }
    start = 0;
    if (atSign !== -1) {
      this.auth = decodeURIComponent(rest.slice(0, atSign));
      start = atSign + 1;
    }
    if (nonHost === -1) {
      this.host = rest.slice(start);
      rest = \"\";
    } else {
      this.host = rest.slice(start, nonHost);
      rest = rest.slice(nonHost);
    }

    this.parseHost();

    if (typeof this.hostname !== \"string\")
      this.hostname = \"\";

    const hostname = this.hostname;

    const ipv6Hostname = isIpv6Hostname(hostname);

    if (!ipv6Hostname) {
      rest = getHostname(this, rest, hostname, url);
    }

    if (this.hostname.length > hostnameMaxLen) {
      this.hostname = \"\";
    } else {
      this.hostname = this.hostname.toLowerCase();
    }

    if (this.hostname !== \"\") {
      if (ipv6Hostname) {
        if (forbiddenHostCharsIpv6.test(this.hostname)) {
          throw new ERR_INVALID_URL(url);
        }
      } else {
        this.hostname = bindingUrl.toASCII(this.hostname);

        if (this.hostname === \"\" || forbiddenHostChars.test(this.hostname)) {
          throw new ERR_INVALID_URL(url);
        }
      }
    }

    const p = this.port ? \":\" + this.port : \"\";
    const h = this.hostname || \"\";
    this.host = h + p;

    if (ipv6Hostname) {
      this.hostname = this.hostname.slice(1, -1);
      if (rest[0] !== \"/\") {
        rest = \"/\" + rest;
      }
    }
  }

  if (!unsafeProtocol.has(lowerProto)) {
    rest = autoEscapeStr(rest);
  }

  let questionIdx = -1;
  let hashIdx = -1;
  for (let i = 0; i < rest.length; ++i) {
    const code = rest.charCodeAt(i);
    if (code === CHAR_HASH) {
      this.hash = rest.slice(i);
      hashIdx = i;
      break;
    } else if (code === CHAR_QUESTION_MARK && questionIdx === -1) {
      questionIdx = i;
    }
  }

  if (questionIdx !== -1) {
    if (hashIdx === -1) {
      this.search = rest.slice(questionIdx);
      this.query = rest.slice(questionIdx + 1);
    } else {
      this.search = rest.slice(questionIdx, hashIdx);
      this.query = rest.slice(questionIdx + 1, hashIdx);
    }
    if (parseQueryString) {
      this.query = querystring.parse(this.query);
    }
  } else if (parseQueryString) {
    this.search = null;
    this.query = { __proto__: null };
  }

  const useQuestionIdx =
    questionIdx !== -1 && (hashIdx === -1 || questionIdx < hashIdx);
  const firstIdx = useQuestionIdx ? questionIdx : hashIdx;
  if (firstIdx === -1) {
    if (rest.length > 0)
      this.pathname = rest;
  } else if (firstIdx > 0) {
    this.pathname = rest.slice(0, firstIdx);
  }
  if (slashedProtocol.has(lowerProto) &&
      this.hostname && !this.pathname) {
    this.pathname = \"/\";
  }

  if (this.pathname || this.search) {
    const p = this.pathname || \"\";
    const s = this.search || \"\";
    this.path = p + s;
  }

  this.href = this.format();
  return this;
};

function getHostname(self, rest, hostname, url) {
  for (let i = 0; i < hostname.length; ++i) {
    const code = hostname.charCodeAt(i);
    const isValid = (code !== CHAR_FORWARD_SLASH &&
                     code !== CHAR_BACKWARD_SLASH &&
                     code !== CHAR_HASH &&
                     code !== CHAR_QUESTION_MARK &&
                     code !== CHAR_COLON);

    if (!isValid) {
      if (code === CHAR_COLON) {
        throw new ERR_INVALID_ARG_VALUE(\"url\", \"Invalid port in url\", url);
      }
      self.hostname = hostname.slice(0, i);
      return `/${hostname.slice(i)}${rest}`;
    }
  }
  return rest;
}

const escapedCodes = [
  /* 0 - 9 */ \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"%09\",
  /* 10 - 19 */ \"%0A\", \"\", \"\", \"%0D\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 20 - 29 */ \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 30 - 39 */ \"\", \"\", \"%20\", \"\", \"%22\", \"\", \"\", \"\", \"\", \"%27\",
  /* 40 - 49 */ \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 50 - 59 */ \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 60 - 69 */ \"%3C\", \"\", \"%3E\", \"\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 70 - 79 */ \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 80 - 89 */ \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 90 - 99 */ \"\", \"\", \"%5C\", \"\", \"%5E\", \"\", \"%60\", \"\", \"\", \"\",
  /* 100 - 109 */ \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 110 - 119 */ \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\",
  /* 120 - 125 */ \"\", \"\", \"\", \"%7B\", \"%7C\", \"%7D\",
];

function autoEscapeStr(rest) {
  let escaped = \"\";
  let lastEscapedPos = 0;
  for (let i = 0; i < rest.length; ++i) {
    const escapedChar = escapedCodes[rest.charCodeAt(i)];
    if (escapedChar) {
      if (i > lastEscapedPos)
        escaped += rest.slice(lastEscapedPos, i);
      escaped += escapedChar;
      lastEscapedPos = i + 1;
    }
  }
  if (lastEscapedPos === 0)  // Nothing has been escaped.
    return rest;

  if (lastEscapedPos < rest.length)
    escaped += rest.slice(lastEscapedPos);

  return escaped;
}

function urlFormat(urlObject, options) {
  if (typeof urlObject === \"string\") {
    urlObject = urlParse(urlObject);
  } else if (typeof urlObject !== \"object\" || urlObject === null) {
    throw new ERR_INVALID_ARG_TYPE(\"urlObject\",
                                   [\"Object\", \"string\"], urlObject);
  } else if (urlObject instanceof URL) {
    let fragment = true;
    let unicode = false;
    let search = true;
    let auth = true;

    if (options) {
      validateObject(options, \"options\");

      if (options.fragment != null) {
        fragment = Boolean(options.fragment);
      }

      if (options.unicode != null) {
        unicode = Boolean(options.unicode);
      }

      if (options.search != null) {
        search = Boolean(options.search);
      }

      if (options.auth != null) {
        auth = Boolean(options.auth);
      }
    }

    return bindingUrl.format(urlObject.href, fragment, unicode, search, auth);
  }

  return Url.prototype.format.call(urlObject);
}

const noEscapeAuth = new Int8Array([
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x00 - 0x0F
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x10 - 0x1F
  0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, // 0x20 - 0x2F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, // 0x30 - 0x3F
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x40 - 0x4F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, // 0x50 - 0x5F
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x60 - 0x6F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, // 0x70 - 0x7F
]);

Url.prototype.format = function format() {
  let auth = this.auth || \"\";
  if (auth) {
    auth = encodeStr(auth, noEscapeAuth, hexTable);
    auth += \"@\";
  }

  let protocol = this.protocol || \"\";
  if (protocol && protocol.charCodeAt(protocol.length - 1) !== 58 /* : */) {
    protocol += \":\";
  }

  let pathname = this.pathname || \"\";
  let hash = this.hash || \"\";
  let host = \"\";
  let query = \"\";

  if (this.host) {
    host = auth + this.host;
  } else if (this.hostname) {
    host = auth + (
      this.hostname.indexOf(\":\") !== -1 && !isIpv6Hostname(this.hostname) ?
        \"[\" + this.hostname + \"]\" :
        this.hostname
    );
    if (this.port) {
      host += \":\" + this.port;
    }
  }

  if (this.query !== null && typeof this.query === \"object\") {
    query = querystring.stringify(this.query);
  }
  let search = this.search || (query && (\"?\" + query)) || \"\";

  if (pathname.indexOf(\"#\") !== -1 || pathname.indexOf(\"?\") !== -1) {
    let newPathname = \"\";
    let lastPos = 0;
    const len = pathname.length;
    for (let i = 0; i < len; i++) {
      const code = pathname.charCodeAt(i);
      if (code === CHAR_HASH || code === CHAR_QUESTION_MARK) {
        if (i > lastPos) {
          newPathname += pathname.slice(lastPos, i);
        }
        newPathname += (code === CHAR_HASH ? \"%23\" : \"%3F\");
        lastPos = i + 1;
      }
    }
    if (lastPos < len) {
      newPathname += pathname.slice(lastPos);
    }
    pathname = newPathname;
  }

  if (this.slashes || slashedProtocol.has(protocol)) {
    if (this.slashes || host) {
      if (pathname && pathname.charCodeAt(0) !== CHAR_FORWARD_SLASH)
        pathname = \"/\" + pathname;
      host = \"//\" + host;
    } else if (protocol.length >= 4 &&
               protocol.charCodeAt(0) === 102/* f */ &&
               protocol.charCodeAt(1) === 105/* i */ &&
               protocol.charCodeAt(2) === 108/* l */ &&
               protocol.charCodeAt(3) === 101/* e */) {
      host = \"//\";
    }
  }

  if (search.indexOf(\"#\") !== -1) {
    search = search.replaceAll(\"#\", \"%23\");
  }

  if (hash && hash.charCodeAt(0) !== CHAR_HASH) {
    hash = \"#\" + hash;
  }
  if (search && search.charCodeAt(0) !== CHAR_QUESTION_MARK) {
    search = \"?\" + search;
  }

  return protocol + host + pathname + search + hash;
};

function urlResolve(source, relative) {
  return urlParse(source, false, true).resolve(relative);
}

Url.prototype.resolve = function resolve(relative) {
  return this.resolveObject(urlParse(relative, false, true)).format();
};

function urlResolveObject(source, relative) {
  if (!source) return relative;
  return urlParse(source, false, true).resolveObject(relative);
}

Url.prototype.resolveObject = function resolveObject(relative) {
  if (typeof relative === \"string\") {
    const rel = new Url();
    rel.parse(relative, false, true);
    relative = rel;
  }

  const result = new Url();
  Object.assign(result, this);

  result.hash = relative.hash;

  if (relative.href === \"\") {
    result.href = result.format();
    return result;
  }

  if (relative.slashes && !relative.protocol) {
    const relativeWithoutProtocol = Object.keys(relative).reduce((acc, key) => {
      if (key !== \"protocol\") {
        acc[key] = relative[key];
      }
      return acc;
    }, {});
    Object.assign(result, relativeWithoutProtocol);

    if (slashedProtocol.has(result.protocol) &&
        result.hostname && !result.pathname) {
      result.path = result.pathname = \"/\";
    }

    result.href = result.format();
    return result;
  }

  if (relative.protocol && relative.protocol !== result.protocol) {
    if (!slashedProtocol.has(relative.protocol)) {
      Object.assign(result, relative);
      result.href = result.format();
      return result;
    }

    result.protocol = relative.protocol;
    if (!relative.host &&
        !/^file:?$/.test(relative.protocol) &&
        !hostlessProtocol.has(relative.protocol)) {
      const relPath = (relative.pathname || \"\").split(\"/\");
      while (relPath.length && !(relative.host = relPath.shift()));
      relative.host ||= \"\";
      relative.hostname ||= \"\";
      if (relPath[0] !== \"\") relPath.unshift(\"\");
      if (relPath.length < 2) relPath.unshift(\"\");
      result.pathname = relPath.join(\"/\");
    } else {
      result.pathname = relative.pathname;
    }
    result.search = relative.search;
    result.query = relative.query;
    result.host = relative.host || \"\";
    result.auth = relative.auth;
    result.hostname = relative.hostname || relative.host;
    result.port = relative.port;
    if (result.pathname || result.search) {
      const p = result.pathname || \"\";
      const s = result.search || \"\";
      result.path = p + s;
    }
    result.slashes ||= relative.slashes;
    result.href = result.format();
    return result;
  }

  const isSourceAbs = (result.pathname && result.pathname.charAt(0) === \"/\");
  const isRelAbs = (
    relative.host || (relative.pathname && relative.pathname.charAt(0) === \"/\")
  );
  let mustEndAbs = (isRelAbs || isSourceAbs ||
                    (result.host && relative.pathname));
  const removeAllDots = mustEndAbs;
  let srcPath = (result.pathname && result.pathname.split(\"/\")) || [];
  const relPath = (relative.pathname && relative.pathname.split(\"/\")) || [];
  const noLeadingSlashes = result.protocol &&
      !slashedProtocol.has(result.protocol);

  if (noLeadingSlashes) {
    result.hostname = \"\";
    result.port = null;
    if (result.host) {
      if (srcPath[0] === \"\") srcPath[0] = result.host;
      else srcPath.unshift(result.host);
    }
    result.host = \"\";
    if (relative.protocol) {
      relative.hostname = null;
      relative.port = null;
      result.auth = null;
      if (relative.host) {
        if (relPath[0] === \"\") relPath[0] = relative.host;
        else relPath.unshift(relative.host);
      }
      relative.host = null;
    }
    mustEndAbs &&= (relPath[0] === \"\" || srcPath[0] === \"\");
  }

  if (isRelAbs) {
    if (relative.host || relative.host === \"\") {
      if (result.host !== relative.host) result.auth = null;
      result.host = relative.host;
      result.port = relative.port;
    }
    if (relative.hostname || relative.hostname === \"\") {
      if (result.hostname !== relative.hostname) result.auth = null;
      result.hostname = relative.hostname;
    }
    result.search = relative.search;
    result.query = relative.query;
    srcPath = relPath;
  } else if (relPath.length) {
    srcPath ||= [];
    srcPath.pop();
    srcPath = srcPath.concat(relPath);
    result.search = relative.search;
    result.query = relative.query;
  } else if (relative.search !== null && relative.search !== undefined) {
    if (noLeadingSlashes) {
      result.hostname = result.host = srcPath.shift();
      const authInHost =
        result.host && result.host.indexOf(\"@\") > 0 && result.host.split(\"@\");
      if (authInHost) {
        result.auth = authInHost.shift();
        result.host = result.hostname = authInHost.shift();
      }
    }
    result.search = relative.search;
    result.query = relative.query;
    if (result.pathname !== null || result.search !== null) {
      result.path = (result.pathname ? result.pathname : \"\") +
                    (result.search ? result.search : \"\");
    }
    result.href = result.format();
    return result;
  }

  if (!srcPath.length) {
    result.pathname = null;
    if (result.search) {
      result.path = \"/\" + result.search;
    } else {
      result.path = null;
    }
    result.href = result.format();
    return result;
  }

  let last = srcPath[srcPath.length - 1];
  const hasTrailingSlash = (
    ((result.host || relative.host || srcPath.length > 1) &&
    (last === \".\" || last === \"..\")) || last === \"\");

  let up = 0;
  for (let i = srcPath.length - 1; i >= 0; i--) {
    last = srcPath[i];
    if (last === \".\") {
      spliceOne(srcPath, i);
    } else if (last === \"..\") {
      spliceOne(srcPath, i);
      up++;
    } else if (up) {
      spliceOne(srcPath, i);
      up--;
    }
  }

  if (!mustEndAbs && !removeAllDots) {
    while (up--) {
      srcPath.unshift(\"..\");
    }
  }

  if (mustEndAbs && srcPath[0] !== \"\" &&
      (!srcPath[0] || srcPath[0].charAt(0) !== \"/\")) {
    srcPath.unshift(\"\");
  }

  if (hasTrailingSlash && srcPath.join(\"/\").at(-1) !== \"/\") {
    srcPath.push(\"\");
  }

  const isAbsolute = srcPath[0] === \"\" ||
      (srcPath[0] && srcPath[0].charAt(0) === \"/\");

  if (noLeadingSlashes) {
    result.hostname =
      result.host = isAbsolute ? \"\" : srcPath.length ? srcPath.shift() : \"\";
    const authInHost = result.host && result.host.indexOf(\"@\") > 0 ?
      result.host.split(\"@\") : false;
    if (authInHost) {
      result.auth = authInHost.shift();
      result.host = result.hostname = authInHost.shift();
    }
  }

  mustEndAbs ||= (result.host && srcPath.length);

  if (mustEndAbs && !isAbsolute) {
    srcPath.unshift(\"\");
  }

  if (!srcPath.length) {
    result.pathname = null;
    result.path = null;
  } else {
    result.pathname = srcPath.join(\"/\");
  }

  if (result.pathname !== null || result.search !== null) {
    result.path = (result.pathname ? result.pathname : \"\") +
                  (result.search ? result.search : \"\");
  }
  result.auth = relative.auth || result.auth;
  result.slashes ||= relative.slashes;
  result.href = result.format();
  return result;
};

Url.prototype.parseHost = function parseHost() {
  let host = this.host;
  let port = portPattern.exec(host);
  if (port) {
    port = port[0];
    if (port !== \":\") {
      this.port = port.slice(1);
    }
    host = host.slice(0, host.length - port.length);
  }
  if (host) this.hostname = host;
};

function pathToFileURL(path, options) {
  validateString(path, \"path\");

  return _pathToFileURL(path, options);
}

module.exports = {
  Url,
  parse: urlParse,
  resolve: urlResolve,
  resolveObject: urlResolveObject,
  format: urlFormat,

  URL,
  URLSearchParams,
  domainToASCII,
  domainToUnicode,

  pathToFileURL,
  fileURLToPath,
  fileURLToPathBuffer,
  urlToHttpOptions,
};
"

pub const events = "\"use strict\";

// node's lib/events.js with the three internal files it pulls in. What it
// asks of the rest of node is answered by `shims` below

const errors = require(\"spark:errors\");
const loop = require(\"spark:loop\");

const AsyncIteratorPrototype = Object.getPrototypeOf(
  Object.getPrototypeOf(async function* () {}).prototype,
);

// a module the embedder may not have passed
function optional(specifier) {
  try {
    return require(specifier);
  } catch {
    return undefined;
  }
}

function spliceOne(list, index) {
  for (; index + 1 < list.length; index++) list[index] = list[index + 1];
  list.pop();
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

const shims = {
  // what the files see as `process`
  process: () => ({
    nextTick(callback, ...args) {
      loop.nextTick(() => callback(...args));
    },
    emitWarning(warning) {
      optional(\"node:process\")?.emitWarning?.(warning);
    },
  }),
  \"internal/errors\": () => errors,
  \"internal/validators\": () => errors,
  \"internal/util\": () => ({
    kEmptyObject: Object.freeze({ __proto__: null }),
    spliceOne,
  }),
  \"internal/util/inspect\": () => ({
    inspect: (value, options) => errors.inspect(value, options),
    identicalSequenceRange,
  }),
  // without the dom events module there are no event targets to meet
  \"internal/event_target\": () => {
    const found = optional(\"spark:dom_events\");
    if (found === undefined) {
      return {
        isEventTarget: () => false,
        kEvents: Symbol(\"events\"),
        kResistStopPropagation: Symbol(\"resistStopPropagation\"),
      };
    }
    return { Event: found.Event, ...found.internals };
  },
  \"internal/process/task_queues\": () => ({
    queueMicrotask: (callback) => {
      Promise.resolve().then(callback);
    },
  }),
  async_hooks: () => require(\"node:async_hooks\"),
};

const files = {
  \"events\": function (module, exports, require, process, primordials, internalBinding) {
    \"use strict\";

    const kRejection = Symbol.for(\"nodejs.rejection\");

    const { kEmptyObject, spliceOne } = require(\"internal/util\");

    const {
      inspect,
      identicalSequenceRange,
    } = require(\"internal/util/inspect\");

    let FixedQueue;
    let kFirstEventParam;
    let kResistStopPropagation;

    const {
      AbortError,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_UNHANDLED_ERROR,
      },
      genericNodeError,
      kEnhanceStackBeforeInspector,
    } = require(\"internal/errors\");

    const {
      validateInteger,
      validateAbortSignal,
      validateBoolean,
      validateFunction,
      validateNumber,
      validateObject,
      validateString,
    } = require(\"internal/validators\");
    const { addAbortListener } = require(\"internal/events/abort_listener\");

    const kCapture = Symbol(\"kCapture\");
    const kErrorMonitor = Symbol(\"events.errorMonitor\");
    const kShapeMode = Symbol(\"shapeMode\");
    const kEmitting = Symbol(\"events.emitting\");
    const kMaxEventTargetListeners = Symbol(\"events.maxEventTargetListeners\");
    const kMaxEventTargetListenersWarned =
      Symbol(\"events.maxEventTargetListenersWarned\");
    const kWatermarkData = Symbol.for(\"nodejs.watermarkData\");

    let EventEmitterAsyncResource;
    function lazyEventEmitterAsyncResource() {
      if (EventEmitterAsyncResource === undefined) {
        const {
          AsyncResource,
        } = require(\"async_hooks\");

        class EventEmitterReferencingAsyncResource extends AsyncResource {
          #eventEmitter;

                constructor(ee, type, options) {
            super(type, options);
            this.#eventEmitter = ee;
          }

                get eventEmitter() {
            return this.#eventEmitter;
          }
        }

        EventEmitterAsyncResource =
          class EventEmitterAsyncResource extends EventEmitter {
            #asyncResource;

                    constructor(options = undefined) {
              let name;
              if (typeof options === \"string\") {
                name = options;
                options = undefined;
              } else {
                if (new.target === EventEmitterAsyncResource) {
                  validateString(options?.name, \"options.name\");
                }
                name = options?.name || new.target.name;
              }
              super(options);

              this.#asyncResource = new EventEmitterReferencingAsyncResource(this, name, options);
            }

                    emit(event, ...args) {
              const asyncResource = this.#asyncResource;
              args.unshift(super.emit, this, event);
              return Reflect.apply(asyncResource.runInAsyncScope, asyncResource,
                                  args);
            }

                    emitDestroy() {
              this.#asyncResource.emitDestroy();
            }

                    get asyncId() {
              return this.#asyncResource.asyncId();
            }

                    get triggerAsyncId() {
              return this.#asyncResource.triggerAsyncId();
            }

                    get asyncResource() {
              return this.#asyncResource;
            }
          };
      }
      return EventEmitterAsyncResource;
    }

    function EventEmitter(opts) {
      EventEmitter.init.call(this, opts);
    }
    module.exports = EventEmitter;
    module.exports.addAbortListener = addAbortListener;
    module.exports.once = once;
    module.exports.on = on;
    module.exports.getEventListeners = getEventListeners;
    module.exports.getMaxListeners = getMaxListeners;
    module.exports.listenerCount = listenerCount;
    EventEmitter.EventEmitter = EventEmitter;

    EventEmitter.usingDomains = false;

    EventEmitter.captureRejectionSymbol = kRejection;
    Object.defineProperty(EventEmitter, \"captureRejections\", {
      __proto__: null,
      get() {
        return EventEmitter.prototype[kCapture];
      },
      set(value) {
        validateBoolean(value, \"EventEmitter.captureRejections\");

        EventEmitter.prototype[kCapture] = value;
      },
      enumerable: true,
    });

    Object.defineProperty(EventEmitter, \"EventEmitterAsyncResource\", {
      __proto__: null,
      enumerable: true,
      get: lazyEventEmitterAsyncResource,
      set: undefined,
      configurable: true,
    });

    EventEmitter.errorMonitor = kErrorMonitor;

    Object.defineProperty(EventEmitter.prototype, kCapture, {
      __proto__: null,
      value: false,
      writable: true,
      enumerable: false,
    });

    EventEmitter.prototype._events = undefined;
    EventEmitter.prototype._eventsCount = 0;
    EventEmitter.prototype._maxListeners = undefined;

    let defaultMaxListeners = 10;
    let isEventTarget;

    function checkListener(listener) {
      validateFunction(listener, \"listener\");
    }

    Object.defineProperty(EventEmitter, \"defaultMaxListeners\", {
      __proto__: null,
      enumerable: true,
      get: function() {
        return defaultMaxListeners;
      },
      set: function(arg) {
        validateNumber(arg, \"defaultMaxListeners\", 0);
        defaultMaxListeners = arg;
      },
    });

    Object.defineProperties(EventEmitter, {
      kMaxEventTargetListeners: {
        __proto__: null,
        value: kMaxEventTargetListeners,
        enumerable: false,
        configurable: false,
        writable: false,
      },
      kMaxEventTargetListenersWarned: {
        __proto__: null,
        value: kMaxEventTargetListenersWarned,
        enumerable: false,
        configurable: false,
        writable: false,
      },
    });

    EventEmitter.setMaxListeners =
      function(n = defaultMaxListeners, ...eventTargets) {
        validateNumber(n, \"setMaxListeners\", 0);
        if (eventTargets.length === 0) {
          defaultMaxListeners = n;
        } else {
          if (isEventTarget === undefined)
            isEventTarget = require(\"internal/event_target\").isEventTarget;

          for (let i = 0; i < eventTargets.length; i++) {
            const target = eventTargets[i];
            if (isEventTarget(target)) {
              target[kMaxEventTargetListeners] = n;
              target[kMaxEventTargetListenersWarned] = false;
            } else if (typeof target.setMaxListeners === \"function\") {
              target.setMaxListeners(n);
            } else {
              throw new ERR_INVALID_ARG_TYPE(
                \"eventTargets\",
                [\"EventEmitter\", \"EventTarget\"],
                target);
            }
          }
        }
      };

    EventEmitter.init = function(opts) {

      if (this._events === undefined ||
          this._events === Object.getPrototypeOf(this)._events) {
        this._events = { __proto__: null };
        this._eventsCount = 0;
        this[kShapeMode] = false;
      } else {
        this[kShapeMode] = true;
      }

      this._maxListeners ||= undefined;

      if (opts?.captureRejections) {
        validateBoolean(opts.captureRejections, \"options.captureRejections\");
        this[kCapture] = Boolean(opts.captureRejections);
      } else {
        this[kCapture] = EventEmitter.prototype[kCapture];
      }
    };

    function addCatch(that, promise, type, args) {
      if (!that[kCapture]) {
        return;
      }

      try {
        const then = promise.then;

        if (typeof then === \"function\") {
          then.call(promise, undefined, function(err) {
            process.nextTick(emitUnhandledRejectionOrErr, that, err, type, args);
          });
        }
      } catch (err) {
        that.emit(\"error\", err);
      }
    }

    function emitUnhandledRejectionOrErr(ee, err, type, args) {
      if (typeof ee[kRejection] === \"function\") {
        ee[kRejection](err, type, ...args);
      } else {
        const prev = ee[kCapture];

        try {
          ee[kCapture] = false;
          ee.emit(\"error\", err);
        } finally {
          ee[kCapture] = prev;
        }
      }
    }

    EventEmitter.prototype.setMaxListeners = function setMaxListeners(n) {
      validateNumber(n, \"setMaxListeners\", 0);
      this._maxListeners = n;
      return this;
    };

    function _getMaxListeners(that) {
      if (that._maxListeners === undefined)
        return EventEmitter.defaultMaxListeners;
      return that._maxListeners;
    }

    EventEmitter.prototype.getMaxListeners = function getMaxListeners() {
      return _getMaxListeners(this);
    };

    function enhanceStackTrace(err, own) {
      let ctorInfo = \"\";
      try {
        const { name } = this.constructor;
        if (name !== \"EventEmitter\")
          ctorInfo = ` on ${name} instance`;
      } catch {
      }
      const sep = `\\nEmitted \"error\" event${ctorInfo} at:\\n`;

      const errStack = err.stack.split(\"\\n\").slice(1);
      const ownStack = own.stack.split(\"\\n\").slice(1);

      const { len, offset } = identicalSequenceRange(ownStack, errStack);
      if (len > 0) {
        ownStack.splice(offset + 1, len - 2, \"    [... lines matching original stack trace ...]\");
      }

      return err.stack + sep + ownStack.join(\"\\n\");
    }

    function getUnhandledErrorException(ee, args) {
      let er;
      if (args.length > 0)
        er = args[0];
      if (er instanceof Error) {
        try {
          const capture = {};
          Error.captureStackTrace(capture, EventEmitter.prototype.emit);
          Object.defineProperty(er, kEnhanceStackBeforeInspector, {
            __proto__: null,
            value: enhanceStackTrace.bind(ee, er, capture),
            configurable: true,
          });
        } catch {
        }

        return er;
      }

      let stringifiedEr;
      try {
        stringifiedEr = inspect(er);
      } catch {
        stringifiedEr = er;
      }

      const err = new ERR_UNHANDLED_ERROR(stringifiedEr);
      err.context = er;
      return err;
    }

    EventEmitter.prototype.emit = function emit(type, ...args) {
      let doError = (type === \"error\");

      const events = this._events;
      if (events !== undefined) {
        if (doError && events[kErrorMonitor] !== undefined)
          this.emit(kErrorMonitor, ...args);
        doError &&= events.error === undefined;
      } else if (!doError)
        return false;

      if (doError) {
        const er = getUnhandledErrorException(this, args);
        throw er; // Unhandled \"error\" event
      }

      const handler = events[type];

      if (handler === undefined)
        return false;

      if (typeof handler === \"function\") {
        const result = Reflect.apply(handler, this, args);

        if (result !== undefined && result !== null) {
          addCatch(this, result, type, args);
        }
      } else {
        handler[kEmitting]++;
        try {
          for (let i = 0; i < handler.length; ++i) {
            const result = Reflect.apply(handler[i], this, args);

            if (result !== undefined && result !== null) {
              addCatch(this, result, type, args);
            }
          }
        } finally {
          handler[kEmitting]--;
        }
      }

      return true;
    };

    function _addListener(target, type, listener, prepend) {
      let m;
      let events;
      let existing;

      checkListener(listener);

      events = target._events;
      if (events === undefined) {
        events = target._events = { __proto__: null };
        target._eventsCount = 0;
      } else {
        if (events.newListener !== undefined) {
          target.emit(\"newListener\", type,
                      listener.listener ?? listener);

          events = target._events;
        }
        existing = events[type];
      }

      if (existing === undefined) {
        events[type] = listener;
        ++target._eventsCount;
      } else {
        if (typeof existing === \"function\") {
          existing = prepend ? [listener, existing] : [existing, listener];
          existing[kEmitting] = 0;
          events[type] = existing;
        } else {
          existing = ensureMutableListenerArray(events, type, existing);
          if (prepend) {
            existing.unshift(listener);
          } else {
            existing.push(listener);
          }
        }

        m = _getMaxListeners(target);
        if (m > 0 && existing.length > m && !existing.warned)
          warnMaxListenersExceeded(target, type, existing, m);
      }

      return target;
    }

    function warnMaxListenersExceeded(target, type, existing, m) {
      existing.warned = true;
      const w = genericNodeError(
        `Possible EventEmitter memory leak detected. ${existing.length} ${String(type)} listeners ` +
        `added to ${inspect(target, { depth: -1 })}. MaxListeners is ${m}. Use emitter.setMaxListeners() to increase limit`,
        { name: \"MaxListenersExceededWarning\", emitter: target, type: type, count: existing.length });
      process.emitWarning(w);
    }

    EventEmitter.prototype.addListener = function addListener(type, listener) {
      return _addListener(this, type, listener, false);
    };

    EventEmitter.prototype.on = EventEmitter.prototype.addListener;

    EventEmitter.prototype.prependListener =
        function prependListener(type, listener) {
          return _addListener(this, type, listener, true);
        };

    function _onceWrap(target, type, listener) {
      let fired = false;
      function wrapper(...args) {
        if (fired) return;
        fired = true;
        target.removeListener(type, wrapper);
        return Reflect.apply(listener, target, args);
      }
      wrapper.listener = listener;
      return wrapper;
    }

    EventEmitter.prototype.once = function once(type, listener) {
      checkListener(listener);

      this.on(type, _onceWrap(this, type, listener));
      return this;
    };

    EventEmitter.prototype.prependOnceListener =
        function prependOnceListener(type, listener) {
          checkListener(listener);

          this.prependListener(type, _onceWrap(this, type, listener));
          return this;
        };

    EventEmitter.prototype.removeListener =
        function removeListener(type, listener) {
          checkListener(listener);

          const events = this._events;
          if (events === undefined)
            return this;

          let list = events[type];
          if (list === undefined)
            return this;

          if (list === listener || list.listener === listener) {
            this._eventsCount -= 1;

            if (this[kShapeMode]) {
              events[type] = undefined;
            } else if (this._eventsCount === 0) {
              this._events = { __proto__: null };
            } else {
              delete events[type];
            }

            if (events.removeListener !== undefined)
              this.emit(\"removeListener\", type, list.listener || listener);
          } else if (typeof list !== \"function\") {
            list = ensureMutableListenerArray(events, type, list);
            let position = -1;

            for (let i = list.length - 1; i >= 0; i--) {
              if (list[i] === listener || list[i].listener === listener) {
                position = i;
                break;
              }
            }

            if (position < 0)
              return this;

            if (position === 0)
              list.shift();
            else {
              spliceOne(list, position);
            }

            if (list.length === 1)
              events[type] = list[0];

            if (events.removeListener !== undefined)
              this.emit(\"removeListener\", type, listener);
          }

          return this;
        };

    EventEmitter.prototype.off = EventEmitter.prototype.removeListener;

    EventEmitter.prototype.removeAllListeners =
        function removeAllListeners(type) {
          const events = this._events;
          if (events === undefined)
            return this;

          if (events.removeListener === undefined) {
            if (arguments.length === 0) {
              this._events = { __proto__: null };
              this._eventsCount = 0;
            } else if (events[type] !== undefined) {
              if (--this._eventsCount === 0)
                this._events = { __proto__: null };
              else
                delete events[type];
            }
            this[kShapeMode] = false;
            return this;
          }

          if (arguments.length === 0) {
            for (const key of Reflect.ownKeys(events)) {
              if (key === \"removeListener\") continue;
              this.removeAllListeners(key);
            }
            this.removeAllListeners(\"removeListener\");
            this._events = { __proto__: null };
            this._eventsCount = 0;
            this[kShapeMode] = false;
            return this;
          }

          const listeners = events[type];

          if (typeof listeners === \"function\") {
            this.removeListener(type, listeners);
          } else if (listeners !== undefined) {
            for (let i = listeners.length - 1; i >= 0; i--) {
              this.removeListener(type, listeners[i]);
            }
          }

          return this;
        };

    function _listeners(target, type, unwrap) {
      const events = target._events;

      if (events === undefined)
        return [];

      const evlistener = events[type];
      if (evlistener === undefined)
        return [];

      if (typeof evlistener === \"function\")
        return unwrap ? [evlistener.listener || evlistener] : [evlistener];

      return unwrap ?
        unwrapListeners(evlistener) : arrayClone(evlistener);
    }

    EventEmitter.prototype.listeners = function listeners(type) {
      return _listeners(this, type, true);
    };

    EventEmitter.prototype.rawListeners = function rawListeners(type) {
      return _listeners(this, type, false);
    };

    EventEmitter.prototype.listenerCount = function listenerCount(type, listener) {
      const events = this._events;

      if (events !== undefined) {
        const evlistener = events[type];

        if (typeof evlistener === \"function\") {
          if (listener != null) {
            return listener === evlistener || listener === evlistener.listener ? 1 : 0;
          }

          return 1;
        } else if (evlistener !== undefined) {
          if (listener != null) {
            let matching = 0;

            for (let i = 0, l = evlistener.length; i < l; i++) {
              if (evlistener[i] === listener || evlistener[i].listener === listener) {
                matching++;
              }
            }

            return matching;
          }

          return evlistener.length;
        }
      }

      return 0;
    };

    EventEmitter.prototype.eventNames = function eventNames() {
      if (this._eventsCount === 0)
        return [];
      const events = this._events;
      const names = [];
      for (const key of Reflect.ownKeys(events)) {
        if (events[key] !== undefined)
          names.push(key);
      }
      return names;
    };

    function arrayClone(arr) {
      switch (arr.length) {
        case 2: return [arr[0], arr[1]];
        case 3: return [arr[0], arr[1], arr[2]];
        case 4: return [arr[0], arr[1], arr[2], arr[3]];
        case 5: return [arr[0], arr[1], arr[2], arr[3], arr[4]];
        case 6: return [arr[0], arr[1], arr[2], arr[3], arr[4], arr[5]];
      }
      return arr.slice();
    }

    function cloneEventListenerArray(arr) {
      const copy = arrayClone(arr);
      copy[kEmitting] = 0;
      if (arr.warned) {
        copy.warned = true;
      }
      return copy;
    }

    function ensureMutableListenerArray(events, type, handler) {
      if (handler[kEmitting] > 0) {
        const copy = cloneEventListenerArray(handler);
        events[type] = copy;
        return copy;
      }
      return handler;
    }

    function unwrapListeners(arr) {
      const ret = arrayClone(arr);
      for (let i = 0; i < ret.length; ++i) {
        const orig = ret[i].listener;
        if (typeof orig === \"function\")
          ret[i] = orig;
      }
      return ret;
    }

    function getEventListeners(emitterOrTarget, type) {
      if (typeof emitterOrTarget.listeners === \"function\") {
        return emitterOrTarget.listeners(type);
      }
      const { isEventTarget, kEvents } = require(\"internal/event_target\");
      if (isEventTarget(emitterOrTarget)) {
        const root = emitterOrTarget[kEvents]?.get(type);
        const listeners = [];
        let handler = root?.next;
        while (handler?.listener !== undefined) {
          const listener = handler.listener?.deref ?
            handler.listener.deref() : handler.listener;
          listeners.push(listener);
          handler = handler.next;
        }
        return listeners;
      }
      throw new ERR_INVALID_ARG_TYPE(\"emitter\",
                                     [\"EventEmitter\", \"EventTarget\"],
                                     emitterOrTarget);
    }

    function getMaxListeners(emitterOrTarget) {
      if (typeof emitterOrTarget?.getMaxListeners === \"function\") {
        return _getMaxListeners(emitterOrTarget);
      } else if (typeof emitterOrTarget?.[kMaxEventTargetListeners] === \"number\") {
        return emitterOrTarget[kMaxEventTargetListeners];
      }

      throw new ERR_INVALID_ARG_TYPE(\"emitter\",
                                     [\"EventEmitter\", \"EventTarget\"],
                                     emitterOrTarget);
    }

    function listenerCount(emitterOrTarget, type) {
      if (typeof emitterOrTarget.listenerCount === \"function\") {
        return emitterOrTarget.listenerCount(type);
      }
      const { isEventTarget, kEvents } = require(\"internal/event_target\");
      if (isEventTarget(emitterOrTarget)) {
        return emitterOrTarget[kEvents]?.get(type)?.size ?? 0;
      }
      throw new ERR_INVALID_ARG_TYPE(\"emitter\",
                                     [\"EventEmitter\", \"EventTarget\"],
                                     emitterOrTarget);
    }

    async function once(emitter, name, options = kEmptyObject) {
      validateObject(options, \"options\");
      const { signal } = options;
      validateAbortSignal(signal, \"options.signal\");
      if (signal?.aborted)
        throw new AbortError(undefined, { cause: signal.reason });
      return new Promise((resolve, reject) => {
        const errorListener = (err) => {
          emitter.removeListener(name, resolver);
          if (signal != null) {
            eventTargetAgnosticRemoveListener(signal, \"abort\", abortListener);
          }
          reject(err);
        };
        const resolver = (...args) => {
          if (typeof emitter.removeListener === \"function\") {
            emitter.removeListener(\"error\", errorListener);
          }
          if (signal != null) {
            eventTargetAgnosticRemoveListener(signal, \"abort\", abortListener);
          }
          resolve(args);
        };

        kResistStopPropagation ??= require(\"internal/event_target\").kResistStopPropagation;
        const opts = { __proto__: null, once: true, [kResistStopPropagation]: true };
        eventTargetAgnosticAddListener(emitter, name, resolver, opts);
        if (name !== \"error\" && typeof emitter.once === \"function\") {
          emitter.once(\"error\", errorListener);
        }
        function abortListener() {
          eventTargetAgnosticRemoveListener(emitter, name, resolver);
          eventTargetAgnosticRemoveListener(emitter, \"error\", errorListener);
          reject(new AbortError(undefined, { cause: signal?.reason }));
        }
        if (signal != null) {
          eventTargetAgnosticAddListener(
            signal, \"abort\", abortListener, { __proto__: null, once: true, [kResistStopPropagation]: true });
        }
      });
    }

    function createIterResult(value, done) {
      return { done, value };
    }

    function eventTargetAgnosticRemoveListener(emitter, name, listener, flags) {
      if (typeof emitter.removeListener === \"function\") {
        emitter.removeListener(name, listener);
      } else if (typeof emitter.removeEventListener === \"function\") {
        emitter.removeEventListener(name, listener, flags);
      } else {
        throw new ERR_INVALID_ARG_TYPE(\"emitter\", \"EventEmitter\", emitter);
      }
    }

    function eventTargetAgnosticAddListener(emitter, name, listener, flags) {
      if (typeof emitter.on === \"function\") {
        if (flags?.once) {
          emitter.once(name, listener);
        } else {
          emitter.on(name, listener);
        }
      } else if (typeof emitter.addEventListener === \"function\") {
        emitter.addEventListener(name, listener, flags);
      } else {
        throw new ERR_INVALID_ARG_TYPE(\"emitter\", \"EventEmitter\", emitter);
      }
    }

    function on(emitter, event, options = kEmptyObject) {
      validateObject(options, \"options\");
      const signal = options.signal;
      validateAbortSignal(signal, \"options.signal\");
      if (signal?.aborted)
        throw new AbortError(undefined, { cause: signal.reason });
      const highWatermark = options.highWaterMark ?? options.highWatermark ?? Number.MAX_SAFE_INTEGER;
      validateInteger(highWatermark, \"options.highWaterMark\", 1);
      const lowWatermark = options.lowWaterMark ?? options.lowWatermark ?? 1;
      validateInteger(lowWatermark, \"options.lowWaterMark\", 1);

      FixedQueue ??= require(\"internal/fixed_queue\");
      const unconsumedEvents = new FixedQueue();
      const unconsumedPromises = new FixedQueue();
      let paused = false;
      let error = null;
      let finished = false;
      let size = 0;

      const iterator = Object.setPrototypeOf({
        next() {
          if (size) {
            const value = unconsumedEvents.shift();
            size--;
            if (paused && size < lowWatermark) {
              emitter.resume(); // Can not be finished yet
              paused = false;
            }
            return Promise.resolve(createIterResult(value, false));
          }

          if (error) {
            const p = Promise.reject(error);
            error = null;
            return p;
          }

          if (finished) return closeHandler();

          return new Promise(function(resolve, reject) {
            unconsumedPromises.push({ resolve, reject });
          });
        },

        return() {
          return closeHandler();
        },

        throw(err) {
          if (!err || !(err instanceof Error)) {
            throw new ERR_INVALID_ARG_TYPE(\"EventEmitter.AsyncIterator\",
                                           \"Error\", err);
          }
          errorHandler(err);
        },
        [Symbol.asyncIterator]() {
          return this;
        },
        [kWatermarkData]: {
                get size() {
            return size;
          },
                get low() {
            return lowWatermark;
          },
                get high() {
            return highWatermark;
          },
                get isPaused() {
            return paused;
          },
        },
      }, AsyncIteratorPrototype);

      const { addEventListener, removeAll } = listenersController();
      kFirstEventParam ??= require(\"internal/events/symbols\").kFirstEventParam;
      addEventListener(emitter, event, options[kFirstEventParam] ? eventHandler : function(...args) {
        return eventHandler(args);
      });
      if (event !== \"error\" && typeof emitter.on === \"function\") {
        addEventListener(emitter, \"error\", errorHandler);
      }
      const closeEvents = options?.close;
      if (closeEvents?.length) {
        for (let i = 0; i < closeEvents.length; i++) {
          addEventListener(emitter, closeEvents[i], closeHandler);
        }
      }

      const abortListenerDisposable = signal ? addAbortListener(signal, abortListener) : null;

      return iterator;

      function abortListener() {
        errorHandler(new AbortError(undefined, { cause: signal?.reason }));
      }

      function eventHandler(value) {
        if (unconsumedPromises.isEmpty()) {
          size++;
          if (!paused && size > highWatermark) {
            paused = true;
            emitter.pause();
          }
          unconsumedEvents.push(value);
        } else unconsumedPromises.shift().resolve(createIterResult(value, false));
      }

      function errorHandler(err) {
        if (unconsumedPromises.isEmpty()) error = err;
        else unconsumedPromises.shift().reject(err);

        closeHandler();
      }

      function closeHandler() {
        abortListenerDisposable?.[Symbol.dispose]();
        removeAll();
        finished = true;
        paused = false;
        const doneResult = createIterResult(undefined, true);
        while (!unconsumedPromises.isEmpty()) {
          unconsumedPromises.shift().resolve(doneResult);
        }

        return Promise.resolve(doneResult);
      }
    }

    function listenersController() {
      const listeners = [];

      return {
        addEventListener(emitter, event, handler, flags) {
          eventTargetAgnosticAddListener(emitter, event, handler, flags);
          listeners.push([emitter, event, handler, flags]);
        },
        removeAll() {
          while (listeners.length > 0) {
            Reflect.apply(eventTargetAgnosticRemoveListener, undefined, listeners.pop());
          }
        },
      };
    }
  },
  \"internal/events/abort_listener\": function (module, exports, require, process, primordials, internalBinding) {
    \"use strict\";

    const {
      validateAbortSignal,
      validateFunction,
    } = require(\"internal/validators\");
    const {
      codes: {
        ERR_INVALID_ARG_TYPE,
      },
    } = require(\"internal/errors\");

    let queueMicrotask;
    let kResistStopPropagation;
    let abortListenerOptions;
    let eventTarget;

    function addAbortListener(signal, listener) {
      if (signal === undefined) {
        throw new ERR_INVALID_ARG_TYPE(\"signal\", \"AbortSignal\", signal);
      }
      validateAbortSignal(signal, \"signal\");
      validateFunction(listener, \"listener\");

      let removeEventListener;
      if (signal.aborted) {
        queueMicrotask ??= require(\"internal/process/task_queues\").queueMicrotask;
        eventTarget ??= require(\"internal/event_target\");
        let disposed = false;
        queueMicrotask(() => {
          if (disposed) return;
          const { Event, kIsBeingDispatched, kTarget, kTrustEvent } = eventTarget;
          const event = new Event(\"abort\", { [kTrustEvent]: true });
          event[kTarget] = signal;
          event[kIsBeingDispatched] = true;
          listener(event);
          event[kIsBeingDispatched] = false;
        });
        removeEventListener = () => {
          disposed = true;
        };
      } else {
        kResistStopPropagation ??= require(\"internal/event_target\").kResistStopPropagation;
        abortListenerOptions ??= Object.freeze({ __proto__: null, once: true, [kResistStopPropagation]: true });
        signal.addEventListener(\"abort\", listener, abortListenerOptions);
        removeEventListener = () => {
          signal.removeEventListener(\"abort\", listener);
        };
      }
      return {
        __proto__: null,
        [Symbol.dispose]() {
          removeEventListener?.();
        },
      };
    }

    module.exports = {
      __proto__: null,
      addAbortListener,
    };
  },
  \"internal/events/symbols\": function (module, exports, require, process, primordials, internalBinding) {
    \"use strict\";

    const kFirstEventParam = Symbol(\"kFirstEventParam\");

    module.exports = {
      kFirstEventParam,
    };
  },
  \"internal/fixed_queue\": function (module, exports, require, process, primordials, internalBinding) {
    \"use strict\";

    const kSize = 2048;
    const kMask = kSize - 1;

    class FixedCircularBuffer {
      constructor() {
        this.bottom = 0;
        this.top = 0;
        this.list = (new Array(kSize)).fill(undefined);
        this.next = null;
      }

      isEmpty() {
        return this.top === this.bottom;
      }

      isFull() {
        return ((this.top + 1) & kMask) === this.bottom;
      }

      push(data) {
        this.list[this.top] = data;
        this.top = (this.top + 1) & kMask;
      }

      shift() {
        const nextItem = this.list[this.bottom];
        if (nextItem === undefined)
          return null;
        this.list[this.bottom] = undefined;
        this.bottom = (this.bottom + 1) & kMask;
        return nextItem;
      }
    }

    module.exports = class FixedQueue {
      static #pool = [];

      constructor() {
        this.head = this.tail = FixedQueue.#pool.pop() ?? new FixedCircularBuffer();
      }

      isEmpty() {
        return this.head.isEmpty();
      }

      push(data) {
        if (this.head.isFull()) {
          this.head = this.head.next = FixedQueue.#pool.pop() ?? new FixedCircularBuffer();
        }
        this.head.push(data);
      }

      shift() {
        const tail = this.tail;
        const next = tail.shift();
        if (tail.isEmpty() && tail.next !== null) {
          this.tail = tail.next;
          tail.next = null;
          tail.bottom = 0;
          tail.top = 0;

          if (FixedQueue.#pool.length < 64) {
            FixedQueue.#pool.push(tail); // Recycle old tail
          }
        }
        return next;
      }
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
    throw new Error(\"no module \" + name + \" in this port\");
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

module.exports = load(\"events\");
"

pub const string_decoder = "\"use strict\";

// node's lib/string_decoder.js. The decoding it leaves to c++ is written out
// below, after src/string_decoder.cc

const errors = require(\"spark:errors\");
const { Buffer } = require(\"node:buffer\");

function normalizeEncoding(enc) {
  if (enc == null || enc === \"utf8\" || enc === \"utf-8\") return \"utf8\";
  return slowCases(enc);
}

function slowCases(enc) {
  switch (enc.length) {
    case 4:
      if (enc === \"UTF8\") return \"utf8\";
      if (enc === \"ucs2\" || enc === \"UCS2\") return \"utf16le\";
      enc = enc.toLowerCase();
      if (enc === \"utf8\") return \"utf8\";
      if (enc === \"ucs2\") return \"utf16le\";
      break;
    case 3:
      if (enc === \"hex\" || enc === \"HEX\" ||
      enc.toLowerCase() === \"hex\")
        return \"hex\";
      break;
    case 5:
      if (enc === \"ascii\") return \"ascii\";
      if (enc === \"ucs-2\") return \"utf16le\";
      if (enc === \"UTF-8\") return \"utf8\";
      if (enc === \"ASCII\") return \"ascii\";
      if (enc === \"UCS-2\") return \"utf16le\";
      enc = enc.toLowerCase();
      if (enc === \"utf-8\") return \"utf8\";
      if (enc === \"ascii\") return \"ascii\";
      if (enc === \"ucs-2\") return \"utf16le\";
      break;
    case 6:
      if (enc === \"base64\") return \"base64\";
      if (enc === \"latin1\" || enc === \"binary\") return \"latin1\";
      if (enc === \"BASE64\") return \"base64\";
      if (enc === \"LATIN1\" || enc === \"BINARY\") return \"latin1\";
      enc = enc.toLowerCase();
      if (enc === \"base64\") return \"base64\";
      if (enc === \"latin1\" || enc === \"binary\") return \"latin1\";
      break;
    case 7:
      if (enc === \"utf16le\" || enc === \"UTF16LE\" ||
      enc.toLowerCase() === \"utf16le\")
        return \"utf16le\";
      break;
    case 8:
      if (enc === \"utf-16le\" || enc === \"UTF-16LE\" ||
      enc.toLowerCase() === \"utf-16le\")
        return \"utf16le\";
      break;
    case 9:
      if (enc === \"base64url\" || enc === \"BASE64URL\" ||
      enc.toLowerCase() === \"base64url\")
        return \"base64url\";
      break;
    default:
      if (enc === \"\") return \"utf8\";
  }
}

// the decoder's state is seven bytes: up to four of a character cut short,
// how many more it needs, how many are held, and the encoding
const kIncompleteCharactersStart = 0;
const kIncompleteCharactersEnd = 4;
const kMissingBytes = 4;
const kBufferedBytes = 5;
const kEncodingField = 6;
const kSize = 7;

const encodings = [\"ascii\", \"utf8\", \"base64\", \"utf16le\", \"latin1\", \"hex\", \"base64url\"];
const encodingsMap = { __proto__: null };
for (let i = 0; i < encodings.length; ++i) encodingsMap[encodings[i]] = i;

function makeString(bytes, encoding) {
  return Buffer.from(bytes.buffer, bytes.byteOffset, bytes.byteLength).toString(
    encoding,
  );
}

function hold(state, data, from, count) {
  const at = state[kBufferedBytes];
  for (let i = 0; i < count; i++) state[at + i] = data[from + i];
}

function decode(state, view) {
  const data = new Uint8Array(view.buffer, view.byteOffset, view.byteLength);
  const encoding = encodings[state[kEncodingField]];
  if (encoding === \"ascii\" || encoding === \"hex\" || encoding === \"latin1\") {
    return makeString(data, encoding);
  }

  let start = 0;
  let nread = data.length;
  let prepend;

  // bytes owed to a character the last chunk cut short
  if (state[kMissingBytes] > 0) {
    if (encoding === \"utf8\") {
      for (let i = 0; i < nread && i < state[kMissingBytes]; ++i) {
        if ((data[start + i] & 0xc0) !== 0x80) {
          // not a continuation byte: the cut character ends here, unfinished
          state[kMissingBytes] = 0;
          hold(state, data, start, i);
          state[kBufferedBytes] += i;
          start += i;
          nread -= i;
          break;
        }
      }
    }
    const found = Math.min(nread, state[kMissingBytes]);
    hold(state, data, start, found);
    start += found;
    nread -= found;
    state[kMissingBytes] -= found;
    state[kBufferedBytes] += found;
    if (state[kMissingBytes] === 0) {
      prepend = makeString(state.subarray(0, state[kBufferedBytes]), encoding);
      state[kBufferedBytes] = 0;
    }
  }

  if (nread === 0) return prepend ?? \"\";

  // a character this chunk cuts short is held back for the next one
  if (encoding === \"utf8\" && data[start + nread - 1] & 0x80) {
    for (let i = nread - 1; ; --i) {
      state[kBufferedBytes]++;
      const byte = data[start + i];
      if ((byte & 0xc0) === 0x80) {
        if (state[kBufferedBytes] >= 4 || i === 0) {
          state[kBufferedBytes] = 0;
          break;
        }
      } else {
        if ((byte & 0xe0) === 0xc0) {
          state[kMissingBytes] = 2;
        } else if ((byte & 0xf0) === 0xe0) {
          state[kMissingBytes] = 3;
        } else if ((byte & 0xf8) === 0xf0) {
          state[kMissingBytes] = 4;
        } else {
          state[kBufferedBytes] = 0;
          break;
        }
        if (state[kBufferedBytes] >= state[kMissingBytes]) {
          state[kMissingBytes] = 0;
          state[kBufferedBytes] = 0;
        }
        state[kMissingBytes] -= state[kBufferedBytes];
        break;
      }
    }
  } else if (encoding === \"utf16le\") {
    if (nread % 2 === 1) {
      state[kBufferedBytes] = 1;
      state[kMissingBytes] = 1;
    } else if ((data[start + nread - 1] & 0xfc) === 0xd8) {
      state[kBufferedBytes] = 2;
      state[kMissingBytes] = 2;
    }
  } else if (encoding === \"base64\" || encoding === \"base64url\") {
    state[kBufferedBytes] = nread % 3;
    if (state[kBufferedBytes] > 0) {
      state[kMissingBytes] = 3 - state[kBufferedBytes];
    }
  }

  const held = state[kBufferedBytes];
  if (held > 0) {
    nread -= held;
    for (let i = 0; i < held; i++) state[i] = data[start + nread + i];
  }
  const body =
    nread > 0 ? makeString(data.subarray(start, start + nread), encoding) : \"\";
  return prepend === undefined ? body : prepend + body;
}

function flush(state) {
  const encoding = encodings[state[kEncodingField]];
  if (encoding === \"utf16le\" && state[kBufferedBytes] % 2 === 1) {
    state[kMissingBytes]--;
    state[kBufferedBytes]--;
  }
  if (state[kBufferedBytes] === 0) return \"\";
  const text = makeString(state.subarray(0, state[kBufferedBytes]), encoding);
  state[kMissingBytes] = 0;
  state[kBufferedBytes] = 0;
  return text;
}

const shims = {
  process: () => undefined,
  buffer: () => ({ Buffer }),
  \"internal/errors\": () => errors,
  \"internal/util\": () => ({ encodingsMap, normalizeEncoding }),
  \"binding:string_decoder\": () => ({
    kIncompleteCharactersStart,
    kIncompleteCharactersEnd,
    kMissingBytes,
    kBufferedBytes,
    kEncodingField,
    kSize,
    decode,
    flush,
  }),
};

const files = {
  \"string_decoder\": function (module, exports, require, process, primordials, internalBinding) {
    \"use strict\";

    const { Buffer } = require(\"buffer\");
    const {
      kIncompleteCharactersStart,
      kIncompleteCharactersEnd,
      kMissingBytes,
      kBufferedBytes,
      kEncodingField,
      kSize,
      decode,
      flush,
    } = require(\"binding:string_decoder\");
    const {
      encodingsMap,
      normalizeEncoding,
    } = require(\"internal/util\");
    const {
      ERR_INVALID_ARG_TYPE,
      ERR_INVALID_THIS,
      ERR_UNKNOWN_ENCODING,
    } = require(\"internal/errors\").codes;

    const kNativeDecoder = Symbol(\"kNativeDecoder\");

    function StringDecoder(encoding) {
      this.encoding = normalizeEncoding(encoding);
      if (this.encoding === undefined) {
        throw new ERR_UNKNOWN_ENCODING(encoding);
      }
      this[kNativeDecoder] = Buffer.alloc(kSize);
      this[kNativeDecoder][kEncodingField] = encodingsMap[this.encoding];
    }

    StringDecoder.prototype.write = function write(buf) {
      if (typeof buf === \"string\")
        return buf;
      if (!ArrayBuffer.isView(buf))
        throw new ERR_INVALID_ARG_TYPE(\"buf\",
                                       [\"Buffer\", \"TypedArray\", \"DataView\"],
                                       buf);
      if (!this[kNativeDecoder]) {
        throw new ERR_INVALID_THIS(\"StringDecoder\");
      }
      return decode(this[kNativeDecoder], buf);
    };

    StringDecoder.prototype.end = function end(buf) {
      const ret = buf === undefined ? \"\" : this.write(buf);
      if (this[kNativeDecoder][kBufferedBytes] > 0)
        return ret + flush(this[kNativeDecoder]);
      return ret;
    };

    /* Everything below this line is undocumented legacy stuff. */
    StringDecoder.prototype.text = function text(buf, offset) {
      this[kNativeDecoder][kMissingBytes] = 0;
      this[kNativeDecoder][kBufferedBytes] = 0;
      return this.write(buf.slice(offset));
    };

    Object.defineProperties(StringDecoder.prototype, {
      lastChar: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() {
          return this[kNativeDecoder].subarray(kIncompleteCharactersStart, kIncompleteCharactersEnd);
        },
      },
      lastNeed: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() {
          return this[kNativeDecoder][kMissingBytes];
        },
      },
      lastTotal: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() {
          return this[kNativeDecoder][kBufferedBytes] +
                 this[kNativeDecoder][kMissingBytes];
        },
      },
    });

    exports.StringDecoder = StringDecoder;
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
    throw new Error(\"no module \" + name + \" in this port\");
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

module.exports = load(\"string_decoder\");
"

pub const util = "\"use strict\";

// node's lib/util.js and the files only it uses: deep equality, parseArgs,
// MIMEType, debounce, throttle and diff. inspect, format, the type checks,
// promisify and deprecate come from the shared internals

const internal = require(\"spark:internal\");

function optional(specifier) {
  try {
    return require(specifier);
  } catch {
    return undefined;
  }
}

internal.addLoader((name) => load(name));

function missing(what) {
  return () => {
    throw new internal.load(\"internal/errors\").codes.ERR_METHOD_NOT_IMPLEMENTED(what);
  };
}

const shims = {
  process: internal.process,
  primordials: internal.primordials,
  internalBinding: internal.internalBinding,
  fallback: internal.load,
  // until the stream module is here nothing is a stream
  \"internal/streams/utils\": () => ({
    isReadableStream: () => false,
    isWritableStream: () => false,
    isNodeStream: () => false,
  }),
  \"internal/abort_controller\": () => ({
    transferableAbortSignal: missing(\"transferableAbortSignal\"),
    transferableAbortController: missing(\"transferableAbortController\"),
    aborted: missing(\"aborted\"),
  }),
  \"internal/encoding\": () => optional(\"spark:encoding\") ?? {},
};

const files = {
  \"util\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayIsArray,
      ArrayPrototypePop,
      ArrayPrototypePush,
      Error,
      ErrorCaptureStackTrace,
      FunctionPrototypeBind,
      NumberIsSafeInteger,
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectGetOwnPropertyDescriptors,
      ObjectGetOwnPropertyNames,
      ObjectKeys,
      ObjectSetPrototypeOf,
      ObjectValues,
      ReflectApply,
      RegExpPrototypeExec,
      SafeMap,
      StringPrototypeSlice,
      StringPrototypeToWellFormed,
    } = primordials;

    const {
      ErrnoException,
      ExceptionWithHostPort,
      codes: {
        ERR_FALSY_VALUE_REJECTION,
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_ARG_VALUE,
        ERR_OUT_OF_RANGE,
      },
      isErrorStackTraceLimitWritable,
    } = require('internal/errors');
    const { Buffer } = require('buffer');
    const {
      format,
      formatWithOptions,
      inspect,
      stripVTControlCharacters,
    } = require('internal/util/inspect');
    const { debuglog } = require('internal/util/debuglog');
    const {
      validateBoolean,
      validateFunction,
      validateNumber,
      validateString,
      validateOneOf,
      validateObject,
      validateInteger,
    } = require('internal/validators');
    const {
      isReadableStream,
      isWritableStream,
      isNodeStream,
    } = require('internal/streams/utils');
    const types = require('internal/util/types');

    let utilColors;
    function lazyUtilColors() {
      utilColors ??= require('internal/util/colors');
      return utilColors;
    }
    const { getOptionValue } = require('internal/options');

    const binding = internalBinding('util');

    const {
      convertProcessSignalToExitCode,
      deprecate: internalDeprecate,
      getLazy,
      getSystemErrorMap,
      getSystemErrorName: internalErrorName,
      getSystemErrorMessage: internalErrorMessage,
      promisify,
      defineLazyProperties,
      kEmptyObject,
    } = require('internal/util');

    let abortController;

    function lazyAbortController() {
      abortController ??= require('internal/abort_controller');
      return abortController;
    }

    let internalDeepEqual;
    let internalPartialDeepEqual;

    const kEscape = '\\u001b[';
    const kEscapeEnd = 'm';

    const kDimCode = 2;
    const kBoldCode = 1;

    const kHexCloseSeq = kEscape + '39' + kEscapeEnd;

    let styleCache;

    const kHexStyleCacheMax = 256;

    let hexStyleCache;

    function getHexStyleCache() {
      hexStyleCache ??= new SafeMap();
      return hexStyleCache;
    }

    function codesToStyle(codes) {
      const openNum = codes[0];
      return {
        __proto__: null,
        openSeq: kEscape + openNum + kEscapeEnd,
        closeSeq: kEscape + codes[1] + kEscapeEnd,
        keepClose: openNum === kDimCode || openNum === kBoldCode,
      };
    }

    function getStyleCache() {
      if (styleCache === undefined) {
        styleCache = { __proto__: null };
        const colors = inspect.colors;
        for (const key of Object.getOwnPropertyNames(colors)) {
          const codes = colors[key];
          if (codes) {
            styleCache[key] = codesToStyle(codes);
          }
        }
      }
      return styleCache;
    }

    function getHexStyle(hex) {
      const cache = getHexStyleCache();
      const cached = cache.get(hex);
      if (cached !== undefined) return cached;
      const { 0: r, 1: g, 2: b } = hexToRgb(hex);
      const style = {
        __proto__: null,
        openSeq: kEscape + rgbToAnsi24Bit(r, g, b) + kEscapeEnd,
        closeSeq: kHexCloseSeq,
      };
      if (cache.size >= kHexStyleCacheMax)
        cache.delete(cache.keys().next().value);
      cache.set(hex, style);
      return style;
    }

    function replaceCloseCode(str, closeSeq, openSeq, keepClose) {
      const closeLen = closeSeq.length;
      let index = str.indexOf(closeSeq);
      if (index === -1) return str;

      let result = '';
      let lastIndex = 0;
      const replacement = keepClose ? closeSeq + openSeq : openSeq;

      do {
        const afterClose = index + closeLen;
        if (afterClose < str.length) {
          result += str.slice(lastIndex, index) + replacement;
          lastIndex = afterClose;
        } else {
          break;
        }
        index = str.indexOf(closeSeq, lastIndex);
      } while (index !== -1);

      return result + str.slice(lastIndex);
    }

    const hexColorRegExp = /^#(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{6})$/;

    function hexToRgb(hex) {
      let hexStr;
      if (hex.length === 4) {
        hexStr = hex[1] + hex[1] + hex[2] + hex[2] + hex[3] + hex[3];
      } else if (hex.length === 7) {
        hexStr = hex.slice(1);
      } else {
        throw new ERR_OUT_OF_RANGE('hex', '#RGB or #RRGGBB', hex);
      }

      return Buffer.from(hexStr, 'hex');
    }

    function rgbToAnsi24Bit(r, g, b) {
      return `38;2;${r};${g};${b}`;
    }

    function styleText(format, text, options) {
      const validateStream = options?.validateStream ?? true;

      if (!validateStream && typeof format === 'string' && typeof text === 'string') {
        const cache = getStyleCache();
        if (format === 'none') return text;
        const style = cache[format];
        if (style !== undefined) {
          const processed = replaceCloseCode(text, style.closeSeq, style.openSeq, style.keepClose);
          return style.openSeq + processed + style.closeSeq;
        }

        if (format[0] === '#') {
          let hexStyle = getHexStyleCache().get(format);
          if (hexStyle === undefined && hexColorRegExp.exec(format) !== null) {
            hexStyle = getHexStyle(format);
          }
          if (hexStyle !== undefined) {
            const processed = replaceCloseCode(text, hexStyle.closeSeq, hexStyle.openSeq, false);
            return hexStyle.openSeq + processed + hexStyle.closeSeq;
          }
        }
      }

      validateString(text, 'text');
      if (options !== undefined) {
        validateObject(options, 'options');
      }
      validateBoolean(validateStream, 'options.validateStream');

      let skipColorize;
      if (validateStream) {
        const stream = options?.stream ?? process.stdout;
        if (
          !isReadableStream(stream) &&
          !isWritableStream(stream) &&
          !isNodeStream(stream)
        ) {
          throw new ERR_INVALID_ARG_TYPE('stream', ['ReadableStream', 'WritableStream', 'Stream'], stream);
        }
        skipColorize = !lazyUtilColors().shouldColorize(stream);
      }

      const formatArray = Array.isArray(format) ? format : [format];
      const colors = inspect.colors;

      let openCodes = '';
      let closeCodes = '';
      let processedText = text;

      for (const key of formatArray) {
        if (key === 'none') continue;

        if (typeof key === 'string' && key[0] === '#') {
          if (hexColorRegExp.exec(key) === null) {
            throw new ERR_INVALID_ARG_VALUE('format', key,
                                            'must be a valid hex color (#RGB or #RRGGBB)');
          }
          if (skipColorize) continue;
          const { 0: r, 1: g, 2: b } = hexToRgb(key);
          const hexOpenSeq = kEscape + rgbToAnsi24Bit(r, g, b) + kEscapeEnd;
          openCodes += hexOpenSeq;
          closeCodes = kHexCloseSeq + closeCodes;
          processedText = replaceCloseCode(processedText, kHexCloseSeq, hexOpenSeq, false);
          continue;
        }

        const codes = colors[key];
        if (!codes) {
          validateOneOf(key, 'format', Object.getOwnPropertyNames(inspect.colors));
        }
        const { openSeq, closeSeq, keepClose } = codesToStyle(codes);
        openCodes += openSeq;
        closeCodes = closeSeq + closeCodes;
        processedText = replaceCloseCode(processedText, closeSeq, openSeq, keepClose);
      }

      if (skipColorize) return text;

      return `${openCodes}${processedText}${closeCodes}`;
    }

    function inherits(ctor, superCtor) {

      if (ctor === undefined || ctor === null)
        throw new ERR_INVALID_ARG_TYPE('ctor', 'Function', ctor);

      if (superCtor === undefined || superCtor === null)
        throw new ERR_INVALID_ARG_TYPE('superCtor', 'Function', superCtor);

      if (superCtor.prototype === undefined) {
        throw new ERR_INVALID_ARG_TYPE('superCtor.prototype',
                                       'Object', superCtor.prototype);
      }
      Object.defineProperty(ctor, 'super_', {
        __proto__: null,
        value: superCtor,
        writable: true,
        configurable: true,
      });
      Object.setPrototypeOf(ctor.prototype, superCtor.prototype);
    }

    function _extend(target, source) {
      if (source === null || typeof source !== 'object') return target;

      const keys = Object.keys(source);
      let i = keys.length;
      while (i--) {
        target[keys[i]] = source[keys[i]];
      }
      return target;
    }

    const callbackifyOnRejected = (reason, cb) => {
      if (!reason) {
        reason = new ERR_FALSY_VALUE_REJECTION.HideStackFramesError(reason);
        Error.captureStackTrace(reason, callbackifyOnRejected);
      }
      return cb(reason);
    };

    function callbackify(original) {
      validateFunction(original, 'original');

      function callbackified(...args) {
        const maybeCb = args.pop();
        validateFunction(maybeCb, 'last argument');
        const cb = FunctionPrototypeBind(maybeCb, this);
        Reflect.apply(original, this, args)
          .then((ret) => process.nextTick(cb, null, ret),
                (rej) => process.nextTick(callbackifyOnRejected, rej, cb));
      }

      const descriptors = ObjectGetOwnPropertyDescriptors(original);
      if (typeof descriptors.length.value === 'number') {
        descriptors.length.value++;
      }
      if (typeof descriptors.name.value === 'string') {
        descriptors.name.value += 'Callbackified';
      }
      const propertiesValues = Object.values(descriptors);
      for (let i = 0; i < propertiesValues.length; i++) {
        Object.setPrototypeOf(propertiesValues[i], null);
      }
      Object.defineProperties(callbackified, descriptors);
      return callbackified;
    }

    function getSystemErrorMessage(err) {
      validateNumber(err, 'err');
      if (err >= 0 || !Number.isSafeInteger(err)) {
        throw new ERR_OUT_OF_RANGE('err', 'a negative integer', err);
      }
      return internalErrorMessage(err);
    }

    function getSystemErrorName(err) {
      validateNumber(err, 'err');
      if (err >= 0 || !Number.isSafeInteger(err)) {
        throw new ERR_OUT_OF_RANGE('err', 'a negative integer', err);
      }
      return internalErrorName(err);
    }

    function _errnoException(...args) {
      if (isErrorStackTraceLimitWritable()) {
        const limit = Error.stackTraceLimit;
        Error.stackTraceLimit = 0;
        const e = new ErrnoException(...args);
        Error.stackTraceLimit = limit;
        Error.captureStackTrace(e, _errnoException);
        return e;
      }
      return new ErrnoException(...args);
    }

    function _exceptionWithHostPort(...args) {
      if (isErrorStackTraceLimitWritable()) {
        const limit = Error.stackTraceLimit;
        Error.stackTraceLimit = 0;
        const e = new ExceptionWithHostPort(...args);
        Error.stackTraceLimit = limit;
        Error.captureStackTrace(e, _exceptionWithHostPort);
        return e;
      }
      return new ExceptionWithHostPort(...args);
    }

    function parseEnv(content) {
      validateString(content, 'content');
      return binding.parseEnv(content);
    }

    const lazySourceMap = getLazy(() => require('internal/source_map/source_map_cache'));

    function reconstructCallSite(callSite) {
      const { scriptName, lineNumber, columnNumber } = callSite;
      const sourceMap = lazySourceMap().findSourceMap(scriptName);
      if (!sourceMap) return;
      const entry = sourceMap.findEntry(lineNumber - 1, columnNumber - 1);
      if (!entry?.originalSource) return;
      return {
        __proto__: null,
        functionName: entry.name || callSite.functionName,
        scriptName: entry.originalSource,
        lineNumber: entry.originalLine + 1,
        column: entry.originalColumn + 1,
        columnNumber: entry.originalColumn + 1,
      };
    }

    function mapCallSite(callSites) {
      const result = [];
      for (let i = 0; i < callSites.length; ++i) {
        const callSite = callSites[i];
        const found = reconstructCallSite(callSite);
        result.push(found ?? callSite);
      }
      return result;
    }

    function getCallSites(frameCount = 10, options) {
      if (options === undefined) {
        if (typeof frameCount === 'object') {
          options = frameCount;
          validateObject(options, 'options');
          if (options.sourceMap !== undefined) {
            validateBoolean(options.sourceMap, 'options.sourceMap');
          }
          frameCount = 10;
        } else {
          options = {};
        };
      } else {
        validateObject(options, 'options');
        if (options.sourceMap !== undefined) {
          validateBoolean(options.sourceMap, 'options.sourceMap');
        }
      }

      validateInteger(frameCount, 'frameCount', 1, 200);
      if (options.sourceMap === true || (getOptionValue('--enable-source-maps') && options.sourceMap !== false)) {
        return mapCallSite(binding.getCallSites(frameCount));
      }
      return binding.getCallSites(frameCount);
    };

    function deprecate(fn, msg, code, { modifyPrototype } = kEmptyObject) {
      return internalDeprecate(fn, msg, code, undefined, modifyPrototype);
    }

    function markPromiseAsHandled(promise) {
      if (!types.isPromise(promise)) {
        throw new ERR_INVALID_ARG_TYPE('promise', 'Promise', promise);
      }
      binding.markPromiseAsHandled(promise);
    }

    module.exports = {
      _errnoException,
      _exceptionWithHostPort,
      _extend: internalDeprecate(_extend,
                                 'The `util._extend` API is deprecated. Please use Object.assign() instead.',
                                 'DEP0060'),
      callbackify,
      convertProcessSignalToExitCode,
      debug: debuglog,
      debuglog,
      deprecate,
      format,
      styleText,
      formatWithOptions,
      getCallSites,
      getSystemErrorMap,
      getSystemErrorName,
      getSystemErrorMessage,
      inherits,
      inspect,
      isArray: internalDeprecate(ArrayIsArray,
                                 'The `util.isArray` API is deprecated. Please use `Array.isArray()` instead.',
                                 'DEP0044'),
      isDeepStrictEqual(a, b, skipPrototype) {
        if (internalDeepEqual === undefined) {
          internalDeepEqual = require('internal/util/comparisons').isDeepStrictEqual;
        }
        return internalDeepEqual(a, b, skipPrototype);
      },
      isPartialDeepStrictEqual(a, b) {
        if (internalPartialDeepEqual === undefined) {
          internalPartialDeepEqual = require('internal/util/comparisons').isPartialStrictEqual;
        }
        return internalPartialDeepEqual(a, b);
      },
      markPromiseAsHandled,
      promisify,
      stripVTControlCharacters,
      toUSVString(input) {
        return `${input}`.toWellFormed();
      },
      get transferableAbortSignal() {
        return lazyAbortController().transferableAbortSignal;
      },
      get transferableAbortController() {
        return lazyAbortController().transferableAbortController;
      },
      get aborted() {
        return lazyAbortController().aborted;
      },
      types,
      parseEnv,
    };

    defineLazyProperties(
      module.exports,
      'internal/util/parse_args/parse_args',
      ['parseArgs'],
    );

    defineLazyProperties(
      module.exports,
      'internal/encoding',
      ['TextDecoder', 'TextEncoder'],
    );

    defineLazyProperties(
      module.exports,
      'internal/mime',
      ['MIMEType', 'MIMEParams'],
    );

    defineLazyProperties(
      module.exports,
      'internal/util/diff',
      ['diff'],
    );

    Object.defineProperties(module.exports, {
      debounce: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() { return require('internal/util/debounce'); },
      },
      throttle: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() { return require('internal/util/throttle'); },
      },
    });

    defineLazyProperties(
      module.exports,
      'internal/util/trace_sigint',
      ['setTraceSigInt'],
    );
  },
  \"internal/util/comparisons\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      Array,
      ArrayBuffer,
      ArrayIsArray,
      ArrayPrototypePush,
      BigInt,
      BigInt64Array,
      BigIntPrototypeValueOf,
      BigUint64Array,
      Boolean,
      BooleanPrototypeValueOf,
      DataView,
      Date,
      DatePrototypeGetTime,
      Error,
      Float16Array,
      Float32Array,
      Float64Array,
      Function,
      Int16Array,
      Int32Array,
      Int8Array,
      Map,
      Number,
      NumberPrototypeValueOf,
      Object,
      ObjectGetOwnPropertyDescriptor,
      ObjectGetOwnPropertySymbols: getOwnSymbols,
      ObjectGetPrototypeOf,
      ObjectIs,
      ObjectKeys,
      ObjectPrototypeHasOwnProperty: hasOwn,
      ObjectPrototypePropertyIsEnumerable: hasEnumerable,
      ObjectPrototypeToString,
      Promise,
      RegExp,
      SafeSet,
      Set,
      String,
      StringPrototypeValueOf,
      Symbol,
      SymbolPrototypeValueOf,
      SymbolToStringTag,
      TypedArrayPrototypeGetByteLength: getByteLength,
      TypedArrayPrototypeGetSymbolToStringTag,
      Uint16Array,
      Uint32Array,
      Uint8Array,
      Uint8ClampedArray,
      WeakMap,
      WeakSet,
    } = primordials;

    const { compare } = internalBinding('buffer');
    const assert = require('internal/assert');
    const { isURL } = require('internal/url');
    const { isError } = require('internal/util');
    const { Buffer } = require('buffer');

    const wellKnownConstructors = new SafeSet()
      .add(Array)
      .add(ArrayBuffer)
      .add(BigInt)
      .add(BigInt64Array)
      .add(BigUint64Array)
      .add(Boolean)
      .add(Buffer)
      .add(DataView)
      .add(Date)
      .add(Error)
      .add(Float16Array)
      .add(Float32Array)
      .add(Float64Array)
      .add(Function)
      .add(Int16Array)
      .add(Int32Array)
      .add(Int8Array)
      .add(Map)
      .add(Number)
      .add(Object)
      .add(Promise)
      .add(RegExp)
      .add(Set)
      .add(String)
      .add(Symbol)
      .add(Uint16Array)
      .add(Uint32Array)
      .add(Uint8Array)
      .add(Uint8ClampedArray)
      .add(WeakMap)
      .add(WeakSet);

    const types = require('internal/util/types');
    const {
      isAnyArrayBuffer,
      isArrayBufferView,
      isBigIntObject,
      isBooleanObject,
      isBoxedPrimitive,
      isCryptoKey,
      isDate,
      isFloat16Array,
      isFloat32Array,
      isFloat64Array,
      isKeyObject,
      isMap,
      isNativeError,
      isNumberObject,
      isPromise,
      isRegExp,
      isSet,
      isStringObject,
      isSymbolObject,
      isWeakMap,
      isWeakSet,
    } = types;
    const {
      constants: {
        ONLY_ENUMERABLE,
        SKIP_SYMBOLS,
      },
      getOwnNonIndexProperties,
    } = internalBinding('util');

    let getCryptoKeyHandle;
    let getCryptoKeyType;
    let getCryptoKeyExtractable;
    let getCryptoKeyAlgorithm;
    let getCryptoKeyUsagesMask;
    let getCryptoKeySecondaryHandle;
    let getCryptoKeySeedData;
    let getKeyObjectHandle;
    let getKeyObjectType;

    const kStrict = 2;
    const kStrictWithoutPrototypes = 3;
    const kLoose = 0;
    const kPartial = 1;

    const kNoIterator = 0;
    const kIsArray = 1;
    const kIsSet = 2;
    const kIsMap = 3;

    function areSimilarRegExps(a, b) {
      return a.source === b.source &&
             a.flags === b.flags &&
             a.lastIndex === b.lastIndex;
    }

    function isPartialUint8Array(a, b) {
      const lenA = getByteLength(a);
      const lenB = getByteLength(b);
      if (lenA < lenB) {
        return false;
      }
      let offsetA = 0;
      for (let offsetB = 0; offsetB < lenB; offsetB++) {
        while (a[offsetA] !== b[offsetB]) {
          offsetA++;
          if (offsetA > lenA - lenB + offsetB) {
            return false;
          }
        }
        offsetA++;
      }
      return true;
    }

    function isPartialArrayBufferView(a, b) {
      if (a.byteLength < b.byteLength) {
        return false;
      }
      return isPartialUint8Array(
        new Uint8Array(a.buffer, a.byteOffset, a.byteLength),
        new Uint8Array(b.buffer, b.byteOffset, b.byteLength),
      );
    }

    function areSimilarFloatArrays(a, b) {
      const len = getByteLength(a);
      if (len !== getByteLength(b)) {
        return false;
      }
      for (let offset = 0; offset < len; offset++) {
        if (a[offset] !== b[offset]) {
          return false;
        }
      }
      return true;
    }

    function areSimilarTypedArrays(a, b) {
      return a.byteLength === b.byteLength && compare(a, b) === 0;
    }

    function areEqualArrayBuffers(buf1, buf2) {
      return buf1.byteLength === buf2.byteLength &&
        compare(new Uint8Array(buf1), new Uint8Array(buf2)) === 0;
    }

    function isEqualBoxedPrimitive(val1, val2) {
      if (isNumberObject(val1)) {
        return isNumberObject(val2) &&
               Object.is(val1.valueOf(),
                        val2.valueOf());
      }
      if (isStringObject(val1)) {
        return isStringObject(val2) &&
               val1.valueOf() === val2.valueOf();
      }
      if (isBooleanObject(val1)) {
        return isBooleanObject(val2) &&
               BooleanPrototypeValueOf(val1) === BooleanPrototypeValueOf(val2);
      }
      if (isBigIntObject(val1)) {
        return isBigIntObject(val2) &&
               BigIntPrototypeValueOf(val1) === BigIntPrototypeValueOf(val2);
      }
      if (isSymbolObject(val1)) {
        return isSymbolObject(val2) &&
              SymbolPrototypeValueOf(val1) === SymbolPrototypeValueOf(val2);
      }
      /* c8 ignore next */
      assert.fail(`Unknown boxed type ${val1}`);
    }

    function isEnumerableOrIdentical(val1, val2, prop, mode, memos) {
      return hasEnumerable(val2, prop) || // This is handled by Object.keys()
          (mode === kPartial && (val2[prop] === undefined || (prop === 'message' && val2[prop] === ''))) ||
          innerDeepEqual(val1[prop], val2[prop], mode, memos);
    }

    function innerDeepEqual(val1, val2, mode, memos) {
      if (val1 === val2) {
        return val1 !== 0 || Object.is(val1, val2) || mode === kLoose;
      }

      if (mode !== kLoose) {
        if (typeof val1 === 'number') {
          return val1 !== val1 && val2 !== val2;
        }
        if (typeof val2 !== 'object' ||
            typeof val1 !== 'object' ||
            val1 === null ||
            val2 === null) {
          return false;
        }
      } else {
        if (val1 === null || typeof val1 !== 'object') {
          return (val2 === null || typeof val2 !== 'object') &&
                 (val1 == val2 || (val1 !== val1 && val2 !== val2));
        }
        if (val2 === null || typeof val2 !== 'object') {
          return false;
        }
      }
      return objectComparisonStart(val1, val2, mode, memos);
    }

    function hasUnequalTag(val1, val2) {
      return val1[SymbolToStringTag] !== val2[SymbolToStringTag];
    }

    function slowHasUnequalTag(val1Tag, val1, val2) {
      if (val1[SymbolToStringTag] !== undefined && val2[SymbolToStringTag] !== undefined) {
        return val1[SymbolToStringTag] !== val2[SymbolToStringTag];
      }
      return val1Tag !== ObjectPrototypeToString(val2);
    }

    function objectComparisonStart(val1, val2, mode, memos) {
      if (mode === kStrict) {
        if (wellKnownConstructors.has(val1.constructor) ||
            (val1.constructor !== undefined && !hasOwn(val1, 'constructor'))) {
          if (val1.constructor !== val2.constructor) {
            return false;
          }
        } else if (Object.getPrototypeOf(val1) !== Object.getPrototypeOf(val2)) {
          return false;
        }
      }

      if (Array.isArray(val1)) {
        if (!Array.isArray(val2) ||
            (val1.length !== val2.length && (mode !== kPartial || val1.length < val2.length)) ||
            hasUnequalTag(val1, val2)) {
          return false;
        }

        const filter = mode !== kLoose ? ONLY_ENUMERABLE : ONLY_ENUMERABLE | SKIP_SYMBOLS;
        const keys2 = getOwnNonIndexProperties(val2, filter);
        if (mode !== kPartial &&
            keys2.length !== getOwnNonIndexProperties(val1, filter).length) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kIsArray, keys2);
      }

      let val1Tag;
      if (val1[SymbolToStringTag] === undefined &&
          (val1Tag = ObjectPrototypeToString(val1)) === '[object Object]') {
        if (slowHasUnequalTag(val1Tag, val1, val2)) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kNoIterator);
      } else if (isSet(val1)) {
        if (!isSet(val2) ||
            (val1.size !== val2.size && (mode !== kPartial || val1.size < val2.size)) ||
            hasUnequalTag(val1, val2)) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kIsSet);
      } else if (isMap(val1)) {
        if (!isMap(val2) ||
            (val1.size !== val2.size && (mode !== kPartial || val1.size < val2.size)) ||
            hasUnequalTag(val1, val2)) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kIsMap);
      } else if (isArrayBufferView(val1)) {
        if (TypedArrayPrototypeGetSymbolToStringTag(val1) !==
            TypedArrayPrototypeGetSymbolToStringTag(val2)) {
          return false;
        }
        if (mode === kPartial && val1.byteLength !== val2.byteLength) {
          if (!isPartialArrayBufferView(val1, val2)) {
            return false;
          }
        } else if (mode === kLoose &&
                   (isFloat32Array(val1) || isFloat64Array(val1) || isFloat16Array(val1))) {
          if (!areSimilarFloatArrays(val1, val2)) {
            return false;
          }
        } else if (!areSimilarTypedArrays(val1, val2)) {
          return false;
        }
        const filter = mode !== kLoose ? ONLY_ENUMERABLE : ONLY_ENUMERABLE | SKIP_SYMBOLS;
        const keys2 = getOwnNonIndexProperties(val2, filter);
        if (mode !== kPartial &&
            keys2.length !== getOwnNonIndexProperties(val1, filter).length) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kNoIterator, keys2);
      } else if (isDate(val1)) {
        if (!isDate(val2) || hasUnequalTag(val1, val2)) {
          return false;
        }
        const time1 = DatePrototypeGetTime(val1);
        const time2 = DatePrototypeGetTime(val2);
        if (time1 !== time2 && (time1 === time1 || time2 === time2)) {
          return false;
        }
      } else if (isRegExp(val1)) {
        if (!isRegExp(val2) || !areSimilarRegExps(val1, val2) || hasUnequalTag(val1, val2)) {
          return false;
        }
      } else if (isAnyArrayBuffer(val1)) {
        if (!isAnyArrayBuffer(val2) || hasUnequalTag(val1, val2)) {
          return false;
        }
        if (mode !== kPartial || val1.byteLength === val2.byteLength) {
          if (!areEqualArrayBuffers(val1, val2)) {
            return false;
          }
        } else if (!isPartialUint8Array(new Uint8Array(val1), new Uint8Array(val2))) {
          return false;
        }
      } else if (slowHasUnequalTag(val1Tag ?? ObjectPrototypeToString(val1), val1, val2) ||
                Array.isArray(val2) ||
                isArrayBufferView(val2) ||
                isSet(val2) ||
                isMap(val2) ||
                isDate(val2) ||
                isRegExp(val2) ||
                isAnyArrayBuffer(val2)) {
        return false;
      } else if (isError(val1)) {
        if (!isError(val2) ||
            !isEnumerableOrIdentical(val1, val2, 'message', mode, memos) ||
            !isEnumerableOrIdentical(val1, val2, 'name', mode, memos) ||
            !isEnumerableOrIdentical(val1, val2, 'cause', mode, memos) ||
            !isEnumerableOrIdentical(val1, val2, 'errors', mode, memos)) {
          return false;
        }
        const hasOwnVal2Cause = hasOwn(val2, 'cause');
        if ((hasOwnVal2Cause !== hasOwn(val1, 'cause') && (mode !== kPartial || hasOwnVal2Cause))) {
          return false;
        }
      } else if (isBoxedPrimitive(val1)) {
        if (!isEqualBoxedPrimitive(val1, val2)) {
          return false;
        }
      } else if (isURL(val1)) {
        if (!isURL(val2) || val1.href !== val2.href) {
          return false;
        }
      } else if (isKeyObject(val1)) {
        if (getKeyObjectHandle === undefined) {
          ({
            getKeyObjectHandle,
            getKeyObjectType,
          } = require('internal/crypto/keys'));
        }
        if (!isKeyObject(val2) ||
          getKeyObjectType(val1) !== getKeyObjectType(val2) ||
          !getKeyObjectHandle(val1).equals(getKeyObjectHandle(val2))
        ) {
          return false;
        }
      } else if (isCryptoKey(val1)) {
        if (getCryptoKeyHandle === undefined) {
          ({
            getCryptoKeyHandle,
            getCryptoKeyType,
            getCryptoKeyExtractable,
            getCryptoKeyAlgorithm,
            getCryptoKeyUsagesMask,
            getCryptoKeySecondaryHandle,
            getCryptoKeySeedData,
          } = require('internal/crypto/keys'));
        }
        const val1SecondaryHandle = getCryptoKeySecondaryHandle(val1);
        const val2SecondaryHandle = isCryptoKey(val2) ?
          getCryptoKeySecondaryHandle(val2) : undefined;
        const val1SeedData = getCryptoKeySeedData(val1);
        const val2SeedData = isCryptoKey(val2) ?
          getCryptoKeySeedData(val2) : undefined;
        if (!isCryptoKey(val2) ||
          getCryptoKeyType(val1) !== getCryptoKeyType(val2) ||
          getCryptoKeyExtractable(val1) !== getCryptoKeyExtractable(val2) ||
          !innerDeepEqual(getCryptoKeyAlgorithm(val1), getCryptoKeyAlgorithm(val2), mode, memos) ||
          getCryptoKeyUsagesMask(val1) !== getCryptoKeyUsagesMask(val2) ||
          !getCryptoKeyHandle(val1).equals(getCryptoKeyHandle(val2)) ||
          (val1SecondaryHandle === undefined) !== (val2SecondaryHandle === undefined) ||
          (val1SecondaryHandle !== undefined &&
            !val1SecondaryHandle.equals(val2SecondaryHandle)) ||
          (val1SeedData === undefined) !== (val2SeedData === undefined) ||
          (val1SeedData !== undefined &&
            !innerDeepEqual(val1SeedData, val2SeedData, mode, memos))
        ) {
          return false;
        }
      } else if (isBoxedPrimitive(val2) ||
          isNativeError(val2) ||
          val2 instanceof Error ||
          isWeakMap(val1) ||
          isWeakSet(val1) ||
          isPromise(val1)) {
        return false;
      }

      return keyCheck(val1, val2, mode, memos, kNoIterator);
    }

    function partialSymbolEquiv(val1, val2, keys2) {
      const symbolKeys = getOwnSymbols(val2);
      if (symbolKeys.length !== 0) {
        for (const key of symbolKeys) {
          if (hasEnumerable(val2, key)) {
            keys2.push(key);
          }
        }
      }
      return true;
    }

    function keyCheck(val1, val2, mode, memos, iterationType, keys2) {
      const isArrayLikeObject = keys2 !== undefined;

      if (keys2 === undefined) {
        keys2 = Object.keys(val2);
      }
      let keys1;

      if (!isArrayLikeObject) {
        if (mode === kPartial) {
          if (!partialSymbolEquiv(val1, val2, keys2)) {
            return false;
          }
        } else if (keys2.length !== (keys1 = Object.keys(val1)).length) {
          return false;
        } else if (mode === kStrict || mode === kStrictWithoutPrototypes) {
          for (const key of getOwnSymbols(val1)) {
            if (hasEnumerable(val1, key)) {
              keys1.push(key);
            }
          }
          for (const key of getOwnSymbols(val2)) {
            if (hasEnumerable(val2, key)) {
              keys2.push(key);
            }
          }
          if (keys1.length !== keys2.length) {
            return false;
          }
        }
      }

      if (keys2.length === 0 &&
          (iterationType === kNoIterator ||
            (iterationType === kIsArray && val2.length === 0) ||
            val2.size === 0)) {
        return true;
      }

      if (memos === null) {
        return objEquiv(val1, val2, mode, keys1, keys2, memos, iterationType);
      }
      return handleCycles(val1, val2, mode, keys1, keys2, memos, iterationType);
    }

    function handleCycles(val1, val2, mode, keys1, keys2, memos, iterationType) {
      if (memos === undefined) {
        memos = {
          set: undefined,
          a: val1,
          b: val2,
          c: undefined,
          d: undefined,
          deep: false,
        };
        return objEquiv(val1, val2, mode, keys1, keys2, memos, iterationType);
      }

      if (memos.set === undefined) {
        if (memos.deep === false) {
          if (memos.a === val1) {
            return memos.b === val2;
          }
          if (memos.b === val2) {
            return false;
          }
          memos.c = val1;
          memos.d = val2;
          memos.deep = true;
          const result = objEquiv(val1, val2, mode, keys1, keys2, memos, iterationType);
          memos.deep = false;
          if (memos.set !== undefined) {
            memos.set.delete(memos.c);
            memos.set.delete(memos.d);
          }
          return result;
        }
        memos.set = new SafeSet();
        memos.set.add(memos.a);
        memos.set.add(memos.b);
        memos.set.add(memos.c);
        memos.set.add(memos.d);
      }

      const { set } = memos;

      const originalSize = set.size;
      set.add(val1);
      set.add(val2);
      if (originalSize !== set.size - 2) {
        return originalSize === set.size;
      }

      const areEq = objEquiv(val1, val2, mode, keys1, keys2, memos, iterationType);

      set.delete(val1);
      set.delete(val2);

      return areEq;
    }

    function findLooseMatchingPrimitives(prim) {
      switch (typeof prim) {
        case 'undefined':
          return null;
        case 'object': // Only pass in null as object!
          return undefined;
        case 'symbol':
          return false;
        case 'string':
          prim = +prim;
        case 'number':
          if (prim !== prim) {
            return false;
          }
      }
      return true;
    }

    function setMightHaveLoosePrim(a, b, prim) {
      const altValue = findLooseMatchingPrimitives(prim);
      if (altValue != null)
        return altValue;

      return !b.has(altValue) && a.has(altValue);
    }

    function mapMightHaveLoosePrim(a, b, prim, item2, memo) {
      const altValue = findLooseMatchingPrimitives(prim);
      if (altValue != null) {
        return altValue;
      }
      const item1 = a.get(altValue);
      if ((item1 === undefined && !a.has(altValue)) ||
          !innerDeepEqual(item1, item2, kLoose, memo)) {
        return false;
      }
      return !b.has(altValue) && innerDeepEqual(item1, item2, kLoose, memo);
    }

    function partialObjectSetEquiv(array, a, b, mode, memo) {
      let aPos = 0;
      let direction = 1;
      let start = 0;
      let end = array.length - 1;
      for (const val1 of a) {
        aPos++;
        if (!b.has(val1)) {
          let innerStart = start;
          if (direction === 1) {
            if (innerDeepEqual(val1, array[start], mode, memo)) {
              if (start === end) {
                return true;
              }
              start += 1;
              continue;
            }
            if (start === end) {
              continue;
            }
            direction = -1;
            innerStart += 1;
          }
          let matched = true;
          if (!innerDeepEqual(val1, array[end], mode, memo)) {
            direction = 1;
            matched = arrayHasEqualElement(array, val1, mode, memo, innerDeepEqual, innerStart, end);
          }
          if (matched) {
            if (start === end) {
              return true;
            }
            end -= 1;
          }
        }
        if (a.size - aPos <= end - start) {
          return false;
        }
      }
      return false;
    }

    function arrayHasEqualElement(array, val1, mode, memo, comparator, start, end) {
      for (let i = end - 1; i >= start; i--) {
        if (comparator(val1, array[i], mode, memo)) {
          array[i] = array[end];
          return true;
        }
      }
      return false;
    }

    function setObjectEquiv(array, a, b, mode, memo) {
      let direction = 1;
      let start = 0;
      let end = array.length - 1;
      const comparator = mode !== kLoose ? objectComparisonStart : innerDeepEqual;
      const extraChecks = mode === kLoose || array.length !== a.size;
      for (const val1 of a) {
        if (typeof val1 !== 'object' || val1 === null) {
          if (b.has(val1)) {
            continue;
          }
          if (mode !== kLoose) {
            return false;
          }
        } else if (extraChecks && b.has(val1)) {
          continue;
        }

        let innerStart = start;
        if (direction === 1) {
          if (comparator(val1, array[start], mode, memo)) {
            start += 1;
            continue;
          }
          if (start === end) {
            return false;
          }
          direction = -1;
          innerStart += 1;
        }
        if (!comparator(val1, array[end], mode, memo)) {
          direction = 1;
          if (!arrayHasEqualElement(array, val1, mode, memo, comparator, innerStart, end)) {
            return false;
          }
        }
        end -= 1;
      }
      return true;
    }

    function compareSmallSets(a, b, val, iteratorB, mode, memo) {
      const iteratorA = a.values();
      const firstA = iteratorA.next().value;
      const first = innerDeepEqual(firstA, val, mode, memo);
      if (first) {
        if (b.size === 1) { // Partial mode && a.size === 1 || b.size === 1
          return true;
        }
        const secondA = iteratorA.next().value;
        return b.has(secondA) || innerDeepEqual(secondA, iteratorB.next().value, mode, memo);
      }
      return a.size !== 1 && innerDeepEqual(iteratorA.next().value, val, mode, memo) && (
        b.size === 1 || // Partial mode
        b.has(firstA) || // Primitive or reference equal
        innerDeepEqual(firstA, iteratorB.next().value, mode, memo)
      );
    }

    function setEquiv(a, b, mode, memo) {
      let array;

      const iteratorB = b.values();
      for (const val of iteratorB) {
        if (!a.has(val)) {
          if ((typeof val !== 'object' || val === null) &&
              (mode !== kLoose || !setMightHaveLoosePrim(a, b, val))) {
            return false;
          }

          if (array === undefined) {
            if (a.size < 3) {
              return compareSmallSets(a, b, val, iteratorB, mode, memo);
            }
            array = [];
          }
          array.push(val);
        }
      }

      if (array === undefined) {
        return true;
      }
      if (mode === kPartial) {
        return partialObjectSetEquiv(array, a, b, mode, memo);
      }
      return setObjectEquiv(array, a, b, mode, memo);
    }

    function partialObjectMapEquiv(array, a, b, mode, memo) {
      let aPos = 0;
      let direction = 1;
      let start = 0;
      let end = array.length - 1;
      for (const { 0: key1, 1: item1 } of a) {
        aPos++;
        if (typeof key1 === 'object' && key1 !== null) {
          let innerStart = start;
          if (direction === 1) {
            const key2 = array[start];
            if (objectComparisonStart(key1, key2, mode, memo) && innerDeepEqual(item1, b.get(key2), mode, memo)) {
              if (start === end) {
                return true;
              }
              start += 1;
              continue;
            }
            if (start === end) {
              continue;
            }
            direction = -1;
            innerStart += 1;
          }
          let matched = true;
          const key2 = array[end];
          if (!objectComparisonStart(key1, key2, mode, memo) || !innerDeepEqual(item1, b.get(key2), mode, memo)) {
            direction = 1;
            matched = arrayHasEqualMapElement(array, key1, item1, b, mode, memo, objectComparisonStart, innerStart, end);
          }
          if (matched) {
            if (start === end) {
              return true;
            }
            end -= 1;
          }
        }
        if (a.size - aPos <= end - start) {
          return false;
        }
      }
      return false;
    }

    function arrayHasEqualMapElement(array, key1, item1, b, mode, memo, comparator, start, end) {
      for (let i = end - 1; i >= start; i--) {
        const key2 = array[i];
        if (comparator(key1, key2, mode, memo) &&
            innerDeepEqual(item1, b.get(key2), mode, memo)) {
          array[i] = array[end];
          return true;
        }
      }
      return false;
    }

    function mapObjectEquiv(array, a, b, mode, memo) {
      let direction = 1;
      let start = 0;
      let end = array.length - 1;
      const comparator = mode !== kLoose ? objectComparisonStart : innerDeepEqual;

      for (const { 0: key1, 1: item1 } of a) {
        if (typeof key1 !== 'object' || key1 === null) {
          if (b.has(key1)) {
            if (mode !== kLoose || innerDeepEqual(item1, b.get(key1), mode, memo)) {
              continue;
            }
          } else if (mode !== kLoose) {
            return false;
          }
        }

        let innerStart = start;
        if (direction === 1) {
          const key2 = array[start];
          if (comparator(key1, key2, mode, memo) && innerDeepEqual(item1, b.get(key2), mode, memo)) {
            start += 1;
            continue;
          }
          if (start === end) {
            return false;
          }
          direction = -1;
          innerStart += 1;
        }
        const key2 = array[end];
        if ((!comparator(key1, key2, mode, memo) || !innerDeepEqual(item1, b.get(key2), mode, memo))) {
          direction = 1;
          if (!arrayHasEqualMapElement(array, key1, item1, b, mode, memo, comparator, innerStart, end)) {
            return false;
          }
        }
        end -= 1;
      }
      return true;
    }

    function mapEquiv(a, b, mode, memo) {
      let array;

      for (const { 0: key2, 1: item2 } of b) {
        if (typeof key2 === 'object' && key2 !== null) {
          if (array === undefined) {
            if (a.size === 1) {
              const { 0: key1, 1: item1 } = a.entries().next().value;
              return innerDeepEqual(key1, key2, mode, memo) &&
                      innerDeepEqual(item1, item2, mode, memo);
            }
            array = [];
          }
          array.push(key2);
        } else {
          const item1 = a.get(key2);
          if (((item1 === undefined && !a.has(key2)) ||
              !innerDeepEqual(item1, item2, mode, memo))) {
            if (mode !== kLoose)
              return false;
            if (!mapMightHaveLoosePrim(a, b, key2, item2, memo))
              return false;
            if (array === undefined) {
              array = [];
            }
            array.push(key2);
          }
        }
      }

      if (array === undefined) {
        return true;
      }

      if (mode === kPartial) {
        return partialObjectMapEquiv(array, a, b, mode, memo);
      }

      return mapObjectEquiv(array, a, b, mode, memo);
    }

    function partialSparseArrayEquiv(a, b, mode, memos, startA, startB) {
      let aPos = startA;
      const keysA = Object.keys(a);
      const keysB = Object.keys(b);
      const lenA = keysA.length - startA;
      const lenB = keysB.length - startB;
      if (lenA < lenB) {
        return false;
      }
      for (let i = 0; i < lenB; i++) {
        const keyB = keysB[startB + i];
        while (!innerDeepEqual(a[keysA[aPos]], b[keyB], mode, memos)) {
          aPos++;
          if (aPos > keysA.length - lenB + i) {
            return false;
          }
        }
        aPos++;
      }
      return true;
    }

    function partialArrayEquiv(a, b, mode, memos) {
      let aPos = 0;
      for (let i = 0; i < b.length; i++) {
        let isSparse = b[i] === undefined && !hasOwn(b, i);
        if (isSparse) {
          return partialSparseArrayEquiv(a, b, mode, memos, aPos, i);
        }
        while (!(isSparse = a[aPos] === undefined && !hasOwn(a, aPos)) &&
               !innerDeepEqual(a[aPos], b[i], mode, memos)) {
          aPos++;
          if (aPos > a.length - b.length + i) {
            return false;
          }
        }
        if (isSparse) {
          return partialSparseArrayEquiv(a, b, mode, memos, aPos, i);
        }
        aPos++;
      }
      return true;
    }

    function sparseArrayEquiv(a, b, mode, memos, i) {
      const keysA = Object.keys(a);
      const keysB = Object.keys(b);
      if (keysA.length !== keysB.length) {
        return false;
      }
      for (; i < keysB.length; i++) {
        const key = keysB[i];
        if ((a[key] === undefined && !hasOwn(a, key)) || !innerDeepEqual(a[key], b[key], mode, memos)) {
          return false;
        }
      }
      return true;
    }

    function objEquiv(a, b, mode, keys1, keys2, memos, iterationType) {
      if (keys2.length > 0) {
        let i = 0;
        if (keys1 !== undefined) {
          for (; i < keys2.length; i++) {
            const key = keys2[i];
            if (keys1[i] !== key) {
              break;
            }
            if (!innerDeepEqual(a[key], b[key], mode, memos)) {
              return false;
            }
          }
        }
        for (; i < keys2.length; i++) {
          const key = keys2[i];
          const descriptor = Object.getOwnPropertyDescriptor(a, key);
          if (descriptor === undefined || descriptor.enumerable !== true) {
            return false;
          }
          const value = descriptor.writable !== undefined ? descriptor.value : a[key];
          if (!innerDeepEqual(value, b[key], mode, memos)) {
            return false;
          }
        }
      }

      if (iterationType === kIsArray) {
        if (mode === kPartial) {
          return partialArrayEquiv(a, b, mode, memos);
        }
        for (let i = 0; i < a.length; i++) {
          if (b[i] === undefined) {
            if (!hasOwn(b, i))
              return sparseArrayEquiv(a, b, mode, memos, i);
            if ((a[i] !== undefined || !hasOwn(a, i)) && (mode !== kLoose || a[i] !== null))
              return false;
          } else if ((a[i] === undefined || !innerDeepEqual(a[i], b[i], mode, memos)) &&
                     (mode !== kLoose || b[i] !== null)) {
            return false;
          }
        }
      } else if (iterationType === kIsSet) {
        if (!setEquiv(a, b, mode, memos)) {
          return false;
        }
      } else if (iterationType === kIsMap) {
        if (!mapEquiv(a, b, mode, memos)) {
          return false;
        }
      }

      return true;
    }

    let detectCycles = function(val1, val2, mode) {
      try {
        return innerDeepEqual(val1, val2, mode, null);
      } catch {
        detectCycles = innerDeepEqual;
        return innerDeepEqual(val1, val2, mode, undefined);
      }
    };

    module.exports = {
      isDeepEqual(val1, val2) {
        return detectCycles(val1, val2, kLoose);
      },
      isDeepStrictEqual(val1, val2, skipPrototype) {
        return detectCycles(val1, val2, skipPrototype ? kStrictWithoutPrototypes : kStrict);
      },
      isPartialStrictEqual(val1, val2) {
        return detectCycles(val1, val2, kPartial);
      },
    };
  },
  \"internal/util/parse_args/parse_args\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypeForEach,
      ArrayPrototypeMap,
      ArrayPrototypePush,
      ArrayPrototypePushApply,
      ArrayPrototypeShift,
      ArrayPrototypeSlice,
      ArrayPrototypeUnshiftApply,
      ObjectEntries,
      ObjectPrototypeHasOwnProperty: ObjectHasOwn,
      StringPrototypeCharAt,
      StringPrototypeIndexOf,
      StringPrototypeSlice,
      StringPrototypeStartsWith,
    } = primordials;

    const {
      validateArray,
      validateBoolean,
      validateBooleanArray,
      validateObject,
      validateString,
      validateStringArray,
      validateUnion,
    } = require('internal/validators');

    const {
      findLongOptionForShort,
      isLoneLongOption,
      isLoneShortOption,
      isLongOptionAndValue,
      isOptionValue,
      isOptionLikeValue,
      isShortOptionAndValue,
      isShortOptionGroup,
      useDefaultValueOption,
      objectGetOwn,
      optionsGetOwn,
    } = require('internal/util/parse_args/utils');

    const {
      codes: {
        ERR_INVALID_ARG_VALUE,
        ERR_PARSE_ARGS_INVALID_OPTION_VALUE,
        ERR_PARSE_ARGS_UNEXPECTED_POSITIONAL,
        ERR_PARSE_ARGS_UNKNOWN_OPTION,
      },
    } = require('internal/errors');

    const {
      kEmptyObject,
    } = require('internal/util');

    const { getOptionValue } = require('internal/options');

    function getMainArgs() {
      const evalValue = getOptionValue('--eval');

      if (evalValue.length !== 0) {
        return process.argv.slice(1);
      }

      return process.argv.slice(2);
    }

    function checkOptionLikeValue(token) {
      if (!token.inlineValue && isOptionLikeValue(token.value)) {
        const example = token.rawName.startsWith('--') ?
          `'${token.rawName}=-XYZ'` :
          `'--${token.name}=-XYZ' or '${token.rawName}-XYZ'`;
        const errorMessage = `Option '${token.rawName}' argument is ambiguous.
    Did you forget to specify the option argument for '${token.rawName}'?
    To specify an option argument starting with a dash use ${example}.`;
        throw new ERR_PARSE_ARGS_INVALID_OPTION_VALUE(errorMessage);
      }
    }

    function checkOptionUsage(config, token) {
      let tokenName = token.name;
      if (!ObjectHasOwn(config.options, tokenName)) {
        if (config.allowNegative && tokenName.startsWith('no-')) {
          tokenName = tokenName.slice(3);
          if (!ObjectHasOwn(config.options, tokenName) || optionsGetOwn(config.options, tokenName, 'type') !== 'boolean') {
            throw new ERR_PARSE_ARGS_UNKNOWN_OPTION(
              token.rawName, config.allowPositionals);
          }
        } else {
          throw new ERR_PARSE_ARGS_UNKNOWN_OPTION(
            token.rawName, config.allowPositionals);
        }
      }

      const short = optionsGetOwn(config.options, tokenName, 'short');
      const shortAndLong = `${short ? `-${short}, ` : ''}--${tokenName}`;
      const type = optionsGetOwn(config.options, tokenName, 'type');
      if (type === 'string' && typeof token.value !== 'string') {
        throw new ERR_PARSE_ARGS_INVALID_OPTION_VALUE(`Option '${shortAndLong} <value>' argument missing`);
      }
      if (type === 'boolean' && token.value != null) {
        throw new ERR_PARSE_ARGS_INVALID_OPTION_VALUE(`Option '${shortAndLong}' does not take an argument`);
      }
    }

    function storeOption(token, options, values, allowNegative) {
      let longOption = token.name;
      let optionValue = token.value;
      if (longOption === '__proto__') {
        return; // No. Just no.
      }

      if (allowNegative && longOption.startsWith('no-') && optionValue === undefined) {
        longOption = longOption.slice(3);
        token.name = longOption;
        optionValue = false;
      }

      const newValue = optionValue ?? true;
      if (optionsGetOwn(options, longOption, 'multiple')) {
        if (values[longOption]) {
          values[longOption].push(newValue);
        } else {
          values[longOption] = [newValue];
        }
      } else {
        values[longOption] = newValue;
      }
    }

    function storeDefaultOption(longOption, optionValue, values) {
      if (longOption === '__proto__') {
        return; // No. Just no.
      }

      values[longOption] = optionValue;
    }

    function argsToTokens(args, options) {
      const tokens = [];
      let index = -1;
      let groupCount = 0;

      const remainingArgs = args.slice();
      while (remainingArgs.length > 0) {
        const arg = remainingArgs.shift();
        const nextArg = remainingArgs[0];
        if (groupCount > 0)
          groupCount--;
        else
          index++;

        if (arg === '--') {
          tokens.push({ kind: 'option-terminator', index });
          ArrayPrototypePushApply(
            tokens, remainingArgs.map((arg) => {
              return { kind: 'positional', index: ++index, value: arg };
            }),
          );
          break; // Finished processing args, leave while loop.
        }

        if (isLoneShortOption(arg)) {
          const shortOption = arg.charAt(1);
          const longOption = findLongOptionForShort(shortOption, options);
          let value;
          let inlineValue;
          if (optionsGetOwn(options, longOption, 'type') === 'string' &&
              isOptionValue(nextArg)) {
            value = remainingArgs.shift();
            inlineValue = false;
          }
          tokens.push({ kind: 'option', name: longOption, rawName: arg,
              index, value, inlineValue });
          if (value != null) ++index;
          continue;
        }

        if (isShortOptionGroup(arg, options)) {
          const expanded = [];
          for (let index = 1; index < arg.length; index++) {
            const shortOption = arg.charAt(index);
            const longOption = findLongOptionForShort(shortOption, options);
            if (optionsGetOwn(options, longOption, 'type') !== 'string' ||
              index === arg.length - 1) {
              expanded.push(`-${shortOption}`);
            } else {
              expanded.push(`-${arg.slice(index)}`);
              break; // finished short group
            }
          }
          ArrayPrototypeUnshiftApply(remainingArgs, expanded);
          groupCount = expanded.length;
          continue;
        }

        if (isShortOptionAndValue(arg, options)) {
          const shortOption = arg.charAt(1);
          const longOption = findLongOptionForShort(shortOption, options);
          const value = arg.slice(2);
          tokens.push({ kind: 'option', name: longOption, rawName: `-${shortOption}`,
              index, value, inlineValue: true });
          continue;
        }

        if (isLoneLongOption(arg)) {
          const longOption = arg.slice(2);
          let value;
          let inlineValue;
          if (optionsGetOwn(options, longOption, 'type') === 'string' &&
              isOptionValue(nextArg)) {
            value = remainingArgs.shift();
            inlineValue = false;
          }
          tokens.push({ kind: 'option', name: longOption, rawName: arg,
              index, value, inlineValue });
          if (value != null) ++index;
          continue;
        }

        if (isLongOptionAndValue(arg)) {
          const equalIndex = arg.indexOf('=');
          const longOption = arg.slice(2, equalIndex);
          const value = arg.slice(equalIndex + 1);
          tokens.push({ kind: 'option', name: longOption, rawName: `--${longOption}`,
              index, value, inlineValue: true });
          continue;
        }

        tokens.push({ kind: 'positional', index, value: arg });
      }
      return tokens;
    }

    const parseArgs = (config = kEmptyObject) => {
      const args = objectGetOwn(config, 'args') ?? getMainArgs();
      const strict = objectGetOwn(config, 'strict') ?? true;
      const allowPositionals = objectGetOwn(config, 'allowPositionals') ?? !strict;
      const returnTokens = objectGetOwn(config, 'tokens') ?? false;
      const allowNegative = objectGetOwn(config, 'allowNegative') ?? false;
      const options = objectGetOwn(config, 'options') ?? { __proto__: null };
      const parseConfig = { args, strict, options, allowPositionals, allowNegative };

      validateArray(args, 'args');
      validateBoolean(strict, 'strict');
      validateBoolean(allowPositionals, 'allowPositionals');
      validateBoolean(returnTokens, 'tokens');
      validateBoolean(allowNegative, 'allowNegative');
      validateObject(options, 'options');
      Object.entries(options).forEach(({ 0: longOption, 1: optionConfig }) => {
          validateObject(optionConfig, `options.${longOption}`);

          const optionType = objectGetOwn(optionConfig, 'type');
          validateUnion(optionType, `options.${longOption}.type`, ['string', 'boolean']);

          if (ObjectHasOwn(optionConfig, 'short')) {
            const shortOption = optionConfig.short;
            validateString(shortOption, `options.${longOption}.short`);
            if (shortOption.length !== 1) {
              throw new ERR_INVALID_ARG_VALUE(
                `options.${longOption}.short`,
                shortOption,
                'must be a single character',
              );
            }
          }

          const multipleOption = objectGetOwn(optionConfig, 'multiple');
          if (ObjectHasOwn(optionConfig, 'multiple')) {
            validateBoolean(multipleOption, `options.${longOption}.multiple`);
          }

          const defaultValue = objectGetOwn(optionConfig, 'default');
          if (defaultValue !== undefined) {
            let validator;
            switch (optionType) {
              case 'string':
                validator = multipleOption ? validateStringArray : validateString;
                break;

              case 'boolean':
                validator = multipleOption ? validateBooleanArray : validateBoolean;
                break;
            }
            validator(defaultValue, `options.${longOption}.default`);
          }
        });

      const tokens = argsToTokens(args, options);

      const result = {
        values: { __proto__: null },
        positionals: [],
      };
      if (returnTokens) {
        result.tokens = tokens;
      }
      tokens.forEach((token) => {
        if (token.kind === 'option') {
          if (strict) {
            checkOptionUsage(parseConfig, token);
            checkOptionLikeValue(token);
          }
          storeOption(token, options, result.values, parseConfig.allowNegative);
        } else if (token.kind === 'positional') {
          if (!allowPositionals) {
            throw new ERR_PARSE_ARGS_UNEXPECTED_POSITIONAL(token.value);
          }
          result.positionals.push(token.value);
        }
      });

      Object.entries(options).forEach(({ 0: longOption,
                                                       1: optionConfig }) => {
        const mustSetDefault = useDefaultValueOption(longOption,
                                                     optionConfig,
                                                     result.values);
        if (mustSetDefault) {
          storeDefaultOption(longOption,
                             objectGetOwn(optionConfig, 'default'),
                             result.values);
        }
      });

      return result;
    };

    module.exports = {
      parseArgs,
    };
  },
  \"internal/util/parse_args/utils\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypeFind,
      ObjectEntries,
      ObjectPrototypeHasOwnProperty: ObjectHasOwn,
      StringPrototypeCharAt,
      StringPrototypeIncludes,
      StringPrototypeStartsWith,
    } = primordials;

    const {
      validateObject,
    } = require('internal/validators');

    function objectGetOwn(obj, prop) {
      if (ObjectHasOwn(obj, prop))
        return obj[prop];
    }

    function optionsGetOwn(options, longOption, prop) {
      if (ObjectHasOwn(options, longOption))
        return objectGetOwn(options[longOption], prop);
    }

    function isOptionValue(value) {
      if (value == null) return false;

      return true; // greedy!
    }

    function isOptionLikeValue(value) {
      if (value == null) return false;

      return value.length > 1 && value.charAt(0) === '-';
    }

    function isLoneShortOption(arg) {
      return arg.length === 2 &&
        arg.charAt(0) === '-' &&
        arg.charAt(1) !== '-';
    }

    function isLoneLongOption(arg) {
      return arg.length > 2 &&
        arg.startsWith('--') &&
        !arg.includes('=', 3);
    }

    function isLongOptionAndValue(arg) {
      return arg.length > 2 &&
        arg.startsWith('--') &&
        arg.includes('=', 3);
    }

    function isShortOptionGroup(arg, options) {
      if (arg.length <= 2) return false;
      if (arg.charAt(0) !== '-') return false;
      if (arg.charAt(1) === '-') return false;

      const firstShort = arg.charAt(1);
      const longOption = findLongOptionForShort(firstShort, options);
      return optionsGetOwn(options, longOption, 'type') !== 'string';
    }

    function isShortOptionAndValue(arg, options) {
      validateObject(options, 'options');

      if (arg.length <= 2) return false;
      if (arg.charAt(0) !== '-') return false;
      if (arg.charAt(1) === '-') return false;

      const shortOption = arg.charAt(1);
      const longOption = findLongOptionForShort(shortOption, options);
      return optionsGetOwn(options, longOption, 'type') === 'string';
    }

    function findLongOptionForShort(shortOption, options) {
      validateObject(options, 'options');
      const longOptionEntry = Object.entries(options).find(({ 1: optionConfig }) => objectGetOwn(optionConfig, 'short') === shortOption);
      return longOptionEntry?.[0] ?? shortOption;
    }

    function useDefaultValueOption(longOption, optionConfig, values) {
      return objectGetOwn(optionConfig, 'default') !== undefined &&
        values[longOption] === undefined;
    }

    module.exports = {
      findLongOptionForShort,
      isLoneLongOption,
      isLoneShortOption,
      isLongOptionAndValue,
      isOptionValue,
      isOptionLikeValue,
      isShortOptionAndValue,
      isShortOptionGroup,
      useDefaultValueOption,
      objectGetOwn,
      optionsGetOwn,
    };
  },
  \"internal/mime\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      FunctionPrototypeCall,
      ObjectDefineProperty,
      SafeMap,
      StringPrototypeCharCodeAt,
      StringPrototypeSlice,
      StringPrototypeToLowerCase,
      Symbol,
      SymbolIterator,
      Uint8Array,
    } = primordials;
    const {
      ERR_ILLEGAL_CONSTRUCTOR,
      ERR_INVALID_MIME_SYNTAX,
    } = require('internal/errors').codes;

    const kHTTPToken = 1;
    const kHTTPQuotedStringToken = 2;
    const codePointClass = new Uint8Array(256);
    {
      codePointClass[0x09] = kHTTPQuotedStringToken;
      for (let c = 0x20; c <= 0x7E; c++) codePointClass[c] = kHTTPQuotedStringToken;
      for (let c = 0x80; c <= 0xFF; c++) codePointClass[c] = kHTTPQuotedStringToken;
      const tokens = \"!#$%&'*+-.^_`|~0123456789\" +
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
      for (let i = 0; i < tokens.length; i++) {
        codePointClass[tokens.charCodeAt(i)] |= kHTTPToken;
      }
    }

    const kNoThrow = Symbol('kNoThrow');

    function isHTTPWhitespace(c) {
      return c === 0x20 || c === 0x09 || c === 0x0A || c === 0x0D;
    }

    function findInvalid(str, start, end, cls) {
      for (let i = start; i < end; i++) {
        const c = str.charCodeAt(i);
        if (c > 0xFF || (codePointClass[c] & cls) === 0) return i - start;
      }
      return -1;
    }

    function toASCIILower(str) {
      let hasUpper = false;
      for (let i = 0; i < str.length; i++) {
        const c = str.charCodeAt(i);
        if (c > 0x7F) {
          let result = '';
          for (let j = 0; j < str.length; j++) {
            const char = str[j];
            result += char >= 'A' && char <= 'Z' ?
              char.toLowerCase() :
              char;
          }
          return result;
        }
        if (c <= 0x5A && c >= 0x41) hasUpper = true;
      }
      return hasUpper ? str.toLowerCase() : str;
    }

    let parsedType = '';
    let parsedSubtype = '';
    let parsedParamsStart = 0;
    let parsedParamsEnd = 0;
    let failedProduction = '';
    let failedIndex = -1;

    function parseTypeAndSubtype(str) {
      const length = str.length;

      let position = 0;
      while (position < length &&
             isHTTPWhitespace(str.charCodeAt(position))) {
        position++;
      }

      const typeStart = position;
      let invalidTypeIndex = -1;
      let typeHasUpper = false;
      for (; position < length; position++) {
        const c = str.charCodeAt(position);
        if (c === 0x2F /* / */) break;
        if (c > 0xFF || (codePointClass[c] & kHTTPToken) === 0) {
          if (invalidTypeIndex === -1) invalidTypeIndex = position - typeStart;
        } else if (c <= 0x5A && c >= 0x41) {
          typeHasUpper = true;
        }
      }
      if (position === typeStart || invalidTypeIndex !== -1 || position >= length) {
        failedProduction = 'type';
        failedIndex = invalidTypeIndex;
        return false;
      }
      const typeEnd = position;

      position++;
      const subtypeStart = position;
      let subtypeEnd = position;
      let firstNonToken = -1;
      let subtypeHasUpper = false;
      for (; position < length; position++) {
        const c = str.charCodeAt(position);
        if (c === 0x3B /* ; */) break;
        if (c <= 0xFF && (codePointClass[c] & kHTTPToken) !== 0) {
          if (c <= 0x5A && c >= 0x41) subtypeHasUpper = true;
        } else if (firstNonToken === -1) {
          firstNonToken = position;
        }
        if (!isHTTPWhitespace(c)) subtypeEnd = position + 1;
      }
      const subtypeRawEnd = position;
      const invalidSubtypeIndex = firstNonToken !== -1 && firstNonToken < subtypeEnd ?
        firstNonToken - subtypeStart : -1;
      if (subtypeEnd === subtypeStart || invalidSubtypeIndex !== -1) {
        failedProduction = 'subtype';
        failedIndex = invalidSubtypeIndex;
        return false;
      }

      let paramsEnd = length;
      while (paramsEnd > subtypeRawEnd &&
             isHTTPWhitespace(str.charCodeAt(paramsEnd - 1))) {
        paramsEnd--;
      }

      const type = str.slice(typeStart, typeEnd);
      const subtype = str.slice(subtypeStart, subtypeEnd);
      parsedType = typeHasUpper ? type.toLowerCase() : type;
      parsedSubtype = subtypeHasUpper ? subtype.toLowerCase() : subtype;
      parsedParamsStart = subtypeRawEnd + 1;
      parsedParamsEnd = paramsEnd;
      return true;
    }

    function parseParameters(str, position, end, params) {
      while (position < end) {
        while (position < end &&
               isHTTPWhitespace(str.charCodeAt(position))) {
          position++;
        }

        const nameStart = position;
        let nameIsToken = true;
        let nameHasUpper = false;
        for (; position < end; position++) {
          const c = str.charCodeAt(position);
          if (c === 0x3B /* ; */ || c === 0x3D /* = */) break;
          if (c > 0xFF || (codePointClass[c] & kHTTPToken) === 0) {
            nameIsToken = false;
          } else if (c <= 0x5A && c >= 0x41) {
            nameHasUpper = true;
          }
        }
        const nameEnd = position;

        if (position < end) {
          if (str.charCodeAt(position) === 0x3B /* ; */) {
            position++;
            continue;
          }
          position++;
        }
        if (position >= end) break;

        let value;
        if (str.charCodeAt(position) === 0x22 /* \" */) {
          position++;
          value = '';
          let chunkStart = position;
          while (true) {
            while (position < end) {
              const c = str.charCodeAt(position);
              if (c === 0x22 /* \" */ || c === 0x5C /* \\ */) break;
              position++;
            }
            if (position >= end) {
              value += str.slice(chunkStart, position);
              break;
            }
            const quoteOrBackslash = str.charCodeAt(position);
            value += str.slice(chunkStart, position);
            position++;
            if (quoteOrBackslash === 0x5C /* \\ */) {
              if (position >= end) {
                value += '\\\\';
                break;
              }
              chunkStart = position;
              position++;
            } else {
              break;
            }
          }
          while (position < end &&
                 str.charCodeAt(position) !== 0x3B /* ; */) {
            position++;
          }
          if (findInvalid(value, 0, value.length, kHTTPQuotedStringToken) !== -1) {
            position++;
            continue;
          }
        } else {
          const valueStart = position;
          let valueEnd = position;
          let firstInvalid = -1;
          for (; position < end; position++) {
            const c = str.charCodeAt(position);
            if (c === 0x3B /* ; */) break;
            if (firstInvalid === -1 &&
                (c > 0xFF || (codePointClass[c] & kHTTPQuotedStringToken) === 0)) {
              firstInvalid = position;
            }
            if (!isHTTPWhitespace(c)) valueEnd = position + 1;
          }
          if (valueEnd === valueStart ||
              (firstInvalid !== -1 && firstInvalid < valueEnd)) {
            position++;
            continue;
          }
          value = str.slice(valueStart, valueEnd);
        }

        if (nameEnd !== nameStart && nameIsToken) {
          const name = nameHasUpper ?
            str.slice(nameStart, nameEnd).toLowerCase() :
            str.slice(nameStart, nameEnd);
          if (!params.has(name)) params.set(name, value);
        }
        position++;
      }
    }

    function escapeQuoteOrSolidus(str) {
      let result = '';
      for (let i = 0; i < str.length; i++) {
        const char = str[i];
        result += (char === '\"' || char === '\\\\') ? `\\\\${char}` : char;
      }
      return result;
    }

    const encode = (value) => {
      if (value.length === 0) return '\"\"';
      if (findInvalid(value, 0, value.length, kHTTPToken) === -1) return value;
      return `\"${escapeQuoteOrSolidus(value)}\"`;
    };

    class MIMEParams {
      #data = null;
      #string = null;
      #start = 0;
      #end = 0;

        static instantiateMimeParams(str, start, end) {
        const instance = new MIMEParams();
        instance.#string = str;
        instance.#start = start;
        instance.#end = end;
        return instance;
      }

        delete(name) {
        this.#parse().delete(toASCIILower(`${name}`));
      }

      get(name) {
        const value = this.#parse().get(toASCIILower(`${name}`));
        return value === undefined ? null : value;
      }

      has(name) {
        return this.#parse().has(toASCIILower(`${name}`));
      }

      set(name, value) {
        const data = this.#parse();
        name = toASCIILower(`${name}`);
        value = `${value}`;
        const invalidNameIndex = findInvalid(name, 0, name.length, kHTTPToken);
        if (name.length === 0 || invalidNameIndex !== -1) {
          throw new ERR_INVALID_MIME_SYNTAX(
            'parameter name',
            name,
            invalidNameIndex,
          );
        }
        const invalidValueIndex = findInvalid(value, 0, value.length,
                                              kHTTPQuotedStringToken);
        if (invalidValueIndex !== -1) {
          throw new ERR_INVALID_MIME_SYNTAX(
            'parameter value',
            value,
            invalidValueIndex,
          );
        }
        data.set(name, value);
      }

      *entries() {
        yield* this.#parse().entries();
      }

      *keys() {
        yield* this.#parse().keys();
      }

      *values() {
        yield* this.#parse().values();
      }

      toString() {
        let ret = '';
        for (const { 0: key, 1: value } of this.#parse()) {
          const encoded = encode(value);
          if (ret.length) ret += ';';
          ret += `${key}=${encoded}`;
        }
        return ret;
      }

        #parse() {
        let data = this.#data;
        if (data !== null) return data;
        data = this.#data = new SafeMap();
        const str = this.#string;
        if (str !== null) {
          this.#string = null;
          parseParameters(str, this.#start, this.#end, data);
        }
        return data;
      }
    }
    const MIMEParamsStringify = MIMEParams.prototype.toString;
    Object.defineProperty(MIMEParams.prototype, SymbolIterator, {
      __proto__: null,
      configurable: true,
      value: MIMEParams.prototype.entries,
      writable: true,
    });
    Object.defineProperty(MIMEParams.prototype, 'toJSON', {
      __proto__: null,
      configurable: true,
      value: MIMEParamsStringify,
      writable: true,
    });

    const { instantiateMimeParams } = MIMEParams;
    delete MIMEParams.instantiateMimeParams;

    class MIMEType {
      #type;
      #subtype;
      #parameters;
      constructor(string, noThrowSymbol = null) {
        if (noThrowSymbol === kNoThrow) {
          this.#init(string);
          return;
        }
        string = `${string}`;
        if (noThrowSymbol != null) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
        if (!parseTypeAndSubtype(string)) {
          throw new ERR_INVALID_MIME_SYNTAX(failedProduction, string, failedIndex);
        }
        this.#init(string);
      }

      #init(string) {
        this.#type = parsedType;
        this.#subtype = parsedSubtype;
        this.#parameters = instantiateMimeParams(string, parsedParamsStart,
                                                 parsedParamsEnd);
      }

      static parse(string) {
        string = `${string}`;
        if (!parseTypeAndSubtype(string)) return null;
        return new MIMEType(string, kNoThrow);
      }

      get type() {
        return this.#type;
      }

      set type(v) {
        v = `${v}`;
        const invalidTypeIndex = findInvalid(v, 0, v.length, kHTTPToken);
        if (v.length === 0 || invalidTypeIndex !== -1) {
          throw new ERR_INVALID_MIME_SYNTAX('type', v, invalidTypeIndex);
        }
        this.#type = toASCIILower(v);
      }

      get subtype() {
        return this.#subtype;
      }

      set subtype(v) {
        v = `${v}`;
        const invalidSubtypeIndex = findInvalid(v, 0, v.length, kHTTPToken);
        if (v.length === 0 || invalidSubtypeIndex !== -1) {
          throw new ERR_INVALID_MIME_SYNTAX('subtype', v, invalidSubtypeIndex);
        }
        this.#subtype = toASCIILower(v);
      }

      get essence() {
        return `${this.#type}/${this.#subtype}`;
      }

      get params() {
        return this.#parameters;
      }

      toString() {
        let ret = `${this.#type}/${this.#subtype}`;
        const paramStr = FunctionPrototypeCall(MIMEParamsStringify, this.#parameters);
        if (paramStr.length) ret += `;${paramStr}`;
        return ret;
      }
    }
    Object.defineProperty(MIMEType.prototype, 'toJSON', {
      __proto__: null,
      configurable: true,
      value: MIMEType.prototype.toString,
      writable: true,
    });

    module.exports = {
      MIMEParams,
      MIMEType,
    };
  },
  \"internal/util/debounce\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypePush,
      ObjectDefineProperties,
      PromiseReject,
      PromiseWithResolvers,
      ReflectApply,
    } = primordials;

    const {
      AbortError,
    } = require('internal/errors');
    const {
      validateAbortSignal,
      validateBoolean,
      validateFunction,
      validateInteger,
      validateObject,
    } = require('internal/validators');
    const { addAbortListener } = require('internal/events/abort_listener');
    const { kEmptyObject } = require('internal/util');
    const { TIMEOUT_MAX } = require('internal/timers');
    const { clearTimeout, setTimeout } = require('timers');
    const { markPromiseAsHandled } = internalBinding('util');

    function debounce(fn, wait, options = kEmptyObject) {
      validateFunction(fn, 'fn');
      validateInteger(wait, 'wait', 0, TIMEOUT_MAX);
      validateObject(options, 'options');

      const {
        leading = false,
        rejectOnCancel = false,
        signal,
      } = options;

      validateBoolean(leading, 'options.leading');
      validateBoolean(rejectOnCancel, 'options.rejectOnCancel');
      validateAbortSignal(signal, 'options.signal');

      if (signal?.aborted) {
        throw new AbortError(undefined, { __proto__: null, cause: signal.reason });
      }

      let args;
      let abortError;
      let pendingCalls = [];
      let refed = true;
      let timeout;

      function rejectPending(error) {
        const calls = pendingCalls;
        pendingCalls = [];
        for (let i = 0; i < calls.length; i++) {
          calls[i].reject(error);
        }
      }

      function cancelWithError(error) {
        if (timeout === undefined) return;
        clearTimeout(timeout);
        timeout = undefined;
        args = undefined;
        rejectPending(error);
      }

      function cancel(reason) {
        if (timeout === undefined) return;
        const error = reason === undefined ?
          new AbortError() :
          new AbortError(undefined, { __proto__: null, cause: reason });
        cancelWithError(error);
      }

      function abort() {
        if (abortError === undefined) {
          abortError = signal.reason === undefined ?
            new AbortError() :
            new AbortError(undefined, { __proto__: null, cause: signal.reason });
        }
        cancelWithError(abortError);
      }

      function ref() {
        refed = true;
        timeout?.ref();
        return debounced;
      }

      function unref() {
        refed = false;
        timeout?.unref();
        return debounced;
      }

      function invoke(callArgs, calls) {
        if (signal?.aborted) {
          abort();
          for (let i = 0; i < calls.length; i++) {
            calls[i].reject(abortError);
          }
          return;
        }

        let result;
        try {
          result = Reflect.apply(fn, debounced, callArgs);
        } catch (error) {
          for (let i = 0; i < calls.length; i++) {
            calls[i].reject(error);
          }
          return;
        }

        for (let i = 0; i < calls.length; i++) {
          calls[i].resolve(result);
        }
      }

      function invokePending() {
        const callArgs = args;
        args = undefined;
        const calls = pendingCalls;
        pendingCalls = [];
        if (calls.length !== 0) invoke(callArgs, calls);
      }

      function onTimeout() {
        timeout = undefined;
        invokePending();
      }

      function flush() {
        if (pendingCalls.length === 0) return;

        clearTimeout(timeout);
        timeout = undefined;
        invokePending();
      }

      function debounced(...callArgs) {
        if (signal?.aborted) abort();
        if (abortError !== undefined) return Promise.reject(abortError);

        const invokeNow = leading && timeout === undefined;
        if (timeout !== undefined) {
          if (pendingCalls.length !== 0) {
            markPromiseAsHandled(pendingCalls[pendingCalls.length - 1].promise);
            if (rejectOnCancel) {
              rejectPending(new AbortError('The debounced call was superseded'));
            }
          }
          timeout.refresh();
        } else {
          timeout = setTimeout(onTimeout, wait);
          if (!refed) timeout.unref();
        }

        if (signal?.aborted) {
          abort();
          return Promise.reject(abortError);
        }

        const call = PromiseWithResolvers();
        if (invokeNow) {
          invoke(callArgs, [call]);
        } else {
          pendingCalls.push(call);
          args = callArgs;
        }
        return call.promise;
      }

      function createFn(value) {
        return {
          __proto__: null,
          configurable: true,
          enumerable: true,
          writable: true,
          value,
        };
      }

      Object.defineProperties(debounced, {
        __proto__: null,
        cancel: createFn(cancel),
        flush: createFn(flush),
        ref: createFn(ref),
        unref: createFn(unref),
        pending: {
          __proto__: null,
          enumerable: false,
          get() {
            return pendingCalls.length === 0 ? null : pendingCalls[pendingCalls.length - 1].promise;
          },
        },
        pendingCount: {
          __proto__: null,
          enumerable: false,
          get() { return pendingCalls.length; },
        },
        length: {
          __proto__: null,
          configurable: true,
          value: fn.length,
        },
        name: {
          __proto__: null,
          configurable: true,
          value: fn.name,
        },
      });

      if (signal !== undefined) {
        addAbortListener(signal, abort);
      }

      return debounced;
    }

    module.exports = debounce;
  },
  \"internal/util/throttle\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypePush,
      ArrayPrototypeSlice,
      ObjectDefineProperties,
      PromisePrototypeThen,
      PromiseReject,
      PromiseResolve,
      PromiseWithResolvers,
      ReflectApply,
    } = primordials;

    const {
      AbortError,
      codes: {
        ERR_THROTTLED,
      },
    } = require('internal/errors');
    const {
      validateAbortSignal,
      validateBoolean,
      validateFunction,
      validateInteger,
      validateObject,
      validateOneOf,
    } = require('internal/validators');
    const { addAbortListener } = require('internal/events/abort_listener');
    const { kEmptyObject } = require('internal/util');
    const { TIMEOUT_MAX } = require('internal/timers');
    const { clearTimeout, setTimeout } = require('timers');

    const timersBinding = internalBinding('timers');
    const { markPromiseAsHandled } = internalBinding('util');

    function throttle(fn, limit, interval, options = kEmptyObject) {
      validateFunction(fn, 'fn');
      validateInteger(limit, 'limit', 1);
      validateInteger(interval, 'interval', 0, TIMEOUT_MAX);
      validateObject(options, 'options');

      const {
        concurrency = Infinity,
        maxPending = Infinity,
        overflow = 'queue',
        signal,
        strict = false,
      } = options;

      if (concurrency !== Infinity) {
        validateInteger(concurrency, 'options.concurrency', 1);
      }
      if (maxPending !== Infinity) {
        validateInteger(maxPending, 'options.maxPending', 0);
      }
      validateOneOf(overflow, 'options.overflow', ['queue', 'drop']);
      validateAbortSignal(signal, 'options.signal');
      validateBoolean(strict, 'options.strict');

      if (signal?.aborted) {
        throw new AbortError(undefined, { __proto__: null, cause: signal.reason });
      }

      let abortError;
      let activeCount = 0;
      let pendingCalls = [];
      let pendingIndex = 0;
      let refed = true;
      let strictIndex = 0;
      let strictTicks = [];
      let timeout;
      let windowCount = 0;
      let windowStart;

      function pendingCount() {
        return pendingCalls.length - pendingIndex;
      }

      function dequeue() {
        const call = pendingCalls[pendingIndex];
        pendingCalls[pendingIndex++] = undefined;

        if (pendingIndex === pendingCalls.length) {
          pendingCalls = [];
          pendingIndex = 0;
        } else if (pendingIndex > 1024 && pendingIndex * 2 >= pendingCalls.length) {
          pendingCalls = pendingCalls.slice(pendingIndex);
          pendingIndex = 0;
        }

        return call;
      }

      function rejectPending(error) {
        const calls = pendingCalls;
        const start = pendingIndex;
        pendingCalls = [];
        pendingIndex = 0;
        for (let i = start; i < calls.length; i++) {
          calls[i].reject(error);
        }
      }

      function resetLimiter() {
        strictIndex = 0;
        strictTicks = [];
        windowCount = 0;
        windowStart = undefined;
      }

      function cancelWithError(error) {
        if (timeout !== undefined) {
          clearTimeout(timeout);
          timeout = undefined;
        }
        resetLimiter();
        rejectPending(error);
      }

      function cancel(reason) {
        const error = reason === undefined ?
          new AbortError() :
          new AbortError(undefined, { __proto__: null, cause: reason });
        cancelWithError(error);
      }

      function abort() {
        if (abortError === undefined) {
          abortError = signal.reason === undefined ?
            new AbortError() :
            new AbortError(undefined, { __proto__: null, cause: signal.reason });
        }
        cancelWithError(abortError);
      }

      function ref() {
        refed = true;
        timeout?.ref();
        return throttled;
      }

      function unref() {
        refed = false;
        timeout?.unref();
        return throttled;
      }

      function hasImmediateCapacity() {
        if (signal?.aborted ||
            pendingCount() !== 0 ||
            activeCount >= concurrency) {
          return false;
        }

        const now = timersBinding.getLibuvNow();
        if (strict) {
          let index = strictIndex;
          while (index < strictTicks.length &&
                 now - strictTicks[index] >= interval) {
            index++;
          }
          return strictTicks.length - index < limit;
        }

        return windowStart === undefined ||
          now - windowStart >= interval ||
          windowCount < limit;
      }

      function pruneStrictTicks(now) {
        while (strictIndex < strictTicks.length &&
               now - strictTicks[strictIndex] >= interval) {
          strictIndex++;
        }

        if (strictIndex === strictTicks.length) {
          strictIndex = 0;
          strictTicks = [];
        } else if (strictIndex > 1024 && strictIndex * 2 >= strictTicks.length) {
          strictTicks = strictTicks.slice(strictIndex);
          strictIndex = 0;
        }
      }

      function hasCapacity(now) {
        if (strict) {
          pruneStrictTicks(now);
          return strictTicks.length - strictIndex < limit;
        }

        if (windowStart === undefined || now - windowStart >= interval) {
          windowCount = 0;
          windowStart = now;
        }
        return windowCount < limit;
      }

      function reserve(now) {
        if (strict) {
          strictTicks.push(now);
        } else {
          windowCount++;
        }
      }

      function onSettled() {
        activeCount--;
        drain();
      }

      function invoke(call) {
        activeCount++;
        let resultPromise;
        try {
          resultPromise = Promise.resolve(Reflect.apply(fn, throttled, call.args));
        } catch (error) {
          resultPromise = Promise.reject(error);
        }
        call.resolve(resultPromise);
        PromisePrototypeThen(resultPromise, onSettled, onSettled);
      }

      function drop() {
        const promise = Promise.reject(new ERR_THROTTLED());
        markPromiseAsHandled(promise);
        return promise;
      }

      function schedule() {
        if (timeout !== undefined ||
            pendingCount() === 0 ||
            activeCount >= concurrency) {
          return;
        }

        const now = timersBinding.getLibuvNow();
        if (hasCapacity(now)) {
          drain();
          return;
        }

        const delay = strict ?
          strictTicks[strictIndex] + interval - now :
          windowStart + interval - now;
        timeout = setTimeout(onTimeout, delay);
        if (!refed) timeout.unref();
      }

      function drain() {
        if (signal?.aborted) {
          abort();
          return;
        }

        while (pendingCount() !== 0 && activeCount < concurrency) {
          const now = timersBinding.getLibuvNow();
          if (!hasCapacity(now)) break;
          const call = dequeue();
          reserve(now);
          invoke(call);
        }

        schedule();
      }

      function onTimeout() {
        timeout = undefined;
        drain();
      }

      function throttled(...callArgs) {
        if (signal?.aborted) abort();
        if (abortError !== undefined) return Promise.reject(abortError);

        const now = timersBinding.getLibuvNow();
        const canInvoke = pendingCount() === 0 &&
          activeCount < concurrency &&
          hasCapacity(now);
        if (!canInvoke &&
            (overflow === 'drop' || pendingCount() >= maxPending)) {
          return drop();
        }

        const call = PromiseWithResolvers();
        call.args = callArgs;

        if (canInvoke) {
          reserve(now);
          invoke(call);
        } else {
          pendingCalls.push(call);
          schedule();
        }

        return call.promise;
      }

      function createFn(value) {
        return {
          __proto__: null,
          configurable: true,
          enumerable: true,
          writable: true,
          value,
        };
      }

      Object.defineProperties(throttled, {
        __proto__: null,
        cancel: createFn(cancel),
        hasImmediateCapacity: createFn(hasImmediateCapacity),
        ref: createFn(ref),
        unref: createFn(unref),
        pending: {
          __proto__: null,
          enumerable: false,
          get() {
            return pendingCount() === 0 ? null :
              pendingCalls[pendingCalls.length - 1].promise;
          },
        },
        pendingCount: {
          __proto__: null,
          enumerable: false,
          get: pendingCount,
        },
        activeCount: {
          __proto__: null,
          enumerable: false,
          get() {
            return activeCount;
          },
        },
        length: {
          __proto__: null,
          configurable: true,
          value: fn.length,
        },
        name: {
          __proto__: null,
          configurable: true,
          value: fn.name,
        },
      });

      if (signal !== undefined) {
        addAbortListener(signal, abort);
      }

      return throttled;
    }

    module.exports = throttle;
  },
  \"internal/util/diff\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayIsArray,
      ArrayPrototypeReverse,
    } = primordials;

    const { validateStringArray, validateString } = require('internal/validators');
    const { myersDiff } = require('internal/assert/myers_diff');

    function validateInput(value, name) {
      if (!Array.isArray(value)) {
        validateString(value, name);
        return;
      }

      validateStringArray(value, name);
    }

    function diff(actual, expected) {
      if (actual === expected) {
        return [];
      }

      validateInput(actual, 'actual');
      validateInput(expected, 'expected');

      return myersDiff(actual, expected).reverse();
    }

    module.exports = {
      diff,
    };
  },
  \"internal/assert/myers_diff\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypePush,
      Int32Array,
      StringPrototypeEndsWith,
    } = primordials;

    const {
      codes: {
        ERR_OUT_OF_RANGE,
      },
    } = require('internal/errors');

    const colors = require('internal/util/colors');

    const kNopLinesToCollapse = 5;
    const kOperations = {
      DELETE: -1,
      NOP: 0,
      INSERT: 1,
    };

    function areLinesEqual(actual, expected, checkCommaDisparity) {
      if (actual === expected) {
        return true;
      }
      if (checkCommaDisparity) {
        return (actual + ',') === expected || actual === (expected + ',');
      }
      return false;
    }

    function myersDiff(actual, expected, checkCommaDisparity = false) {
      const actualLength = actual.length;
      const expectedLength = expected.length;
      const max = actualLength + expectedLength;

      if (max > 2 ** 31 - 1) {
        throw new ERR_OUT_OF_RANGE(
          'myersDiff input size',
          '< 2^31',
          max,
        );
      }

      const v = new Int32Array(2 * max + 1);
      const trace = [];

      for (let diffLevel = 0; diffLevel <= max; diffLevel++) {
        trace.push(new Int32Array(v)); // Clone the current state of `v`

        for (let diagonalIndex = -diffLevel; diagonalIndex <= diffLevel; diagonalIndex += 2) {
          const offset = diagonalIndex + max;
          const previousOffset = v[offset - 1];
          const nextOffset = v[offset + 1];
          let x = diagonalIndex === -diffLevel || (diagonalIndex !== diffLevel && previousOffset < nextOffset) ?
            nextOffset :
            previousOffset + 1;
          let y = x - diagonalIndex;

          while (
            x < actualLength &&
            y < expectedLength &&
            areLinesEqual(actual[x], expected[y], checkCommaDisparity)
          ) {
            x++;
            y++;
          }

          v[offset] = x;

          if (x >= actualLength && y >= expectedLength) {
            return backtrack(trace, actual, expected, checkCommaDisparity);
          }
        }
      }
    }

    function backtrack(trace, actual, expected, checkCommaDisparity) {
      const actualLength = actual.length;
      const expectedLength = expected.length;
      const max = actualLength + expectedLength;

      let x = actualLength;
      let y = expectedLength;
      const result = [];

      for (let diffLevel = trace.length - 1; diffLevel >= 0; diffLevel--) {
        const v = trace[diffLevel];
        const diagonalIndex = x - y;
        const offset = diagonalIndex + max;

        let prevDiagonalIndex;
        if (
          diagonalIndex === -diffLevel ||
          (diagonalIndex !== diffLevel && v[offset - 1] < v[offset + 1])
        ) {
          prevDiagonalIndex = diagonalIndex + 1;
        } else {
          prevDiagonalIndex = diagonalIndex - 1;
        }

        const prevX = v[prevDiagonalIndex + max];
        const prevY = prevX - prevDiagonalIndex;

        while (x > prevX && y > prevY) {
          const actualItem = actual[x - 1];
          const value = checkCommaDisparity && !actualItem.endsWith(',') ? expected[y - 1] : actualItem;
          result.push([ kOperations.NOP, value ]);
          x--;
          y--;
        }

        if (diffLevel > 0) {
          if (x > prevX) {
            result.push([ kOperations.INSERT, actual[--x] ]);
          } else {
            result.push([ kOperations.DELETE, expected[--y] ]);
          }
        }
      }

      return result;
    }

    function printSimpleMyersDiff(diff) {
      let message = '';

      for (let diffIdx = diff.length - 1; diffIdx >= 0; diffIdx--) {
        const { 0: operation, 1: value } = diff[diffIdx];
        let color = colors.white;

        if (operation === kOperations.INSERT) {
          color = colors.green;
        } else if (operation === kOperations.DELETE) {
          color = colors.red;
        }

        message += `${color}${value}${colors.white}`;
      }

      return `\\n${message}`;
    }

    function printMyersDiff(diff, operator) {
      let message = '';
      let skipped = false;
      let nopCount = 0;

      for (let diffIdx = diff.length - 1; diffIdx >= 0; diffIdx--) {
        const { 0: operation, 1: value } = diff[diffIdx];
        const previousOperation = diffIdx < diff.length - 1 ? diff[diffIdx + 1][0] : null;

        if (previousOperation === kOperations.NOP && operation !== previousOperation) {
          if (nopCount === kNopLinesToCollapse + 1) {
            message += `${colors.white}  ${diff[diffIdx + 1][1]}\\n`;
          } else if (nopCount === kNopLinesToCollapse + 2) {
            message += `${colors.white}  ${diff[diffIdx + 2][1]}\\n`;
            message += `${colors.white}  ${diff[diffIdx + 1][1]}\\n`;
          } else if (nopCount >= kNopLinesToCollapse + 3) {
            message += `${colors.blue}...${colors.white}\\n`;
            message += `${colors.white}  ${diff[diffIdx + 1][1]}\\n`;
            skipped = true;
          }
          nopCount = 0;
        }

        if (operation === kOperations.INSERT) {
          if (operator === 'partialDeepStrictEqual') {
            message += `${colors.gray}${colors.hasColors ? ' ' : '+'} ${value}${colors.white}\\n`;
          } else {
            message += `${colors.green}+${colors.white} ${value}\\n`;
          }
        } else if (operation === kOperations.DELETE) {
          message += `${colors.red}-${colors.white} ${value}\\n`;
        } else if (operation === kOperations.NOP) {
          if (nopCount < kNopLinesToCollapse) {
            message += `${colors.white}  ${value}\\n`;
          }
          nopCount++;
        }
      }

      message = message.trimEnd();

      return { message: `\\n${message}`, skipped };
    }

    module.exports = { myersDiff, printMyersDiff, printSimpleMyersDiff };
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
    throw new Error(\"no module \" + name + \" in this port\");
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

module.exports = load(\"util\");
"

pub const stream = "\"use strict\";

// node's lib/stream.js and lib/internal/streams/*: Readable, Writable,
// Duplex, Transform, PassThrough, pipeline, finished, compose and the
// promise and consumer helpers

const internal = require(\"spark:internal\");

function optional(specifier) {
  try {
    return require(specifier);
  } catch {
    return undefined;
  }
}

internal.addLoader((name) => load(name));

class Bytes {
  #parts;

  constructor(parts) {
    this.#parts = parts;
  }

  async arrayBuffer() {
    const { Buffer } = require(\"node:buffer\");
    const joined = Buffer.concat(
      this.#parts.map((part) =>
        typeof part === \"string\"
          ? Buffer.from(part)
          : ArrayBuffer.isView(part)
            ? Buffer.from(part.buffer, part.byteOffset, part.byteLength)
            : Buffer.from(part),
      ),
    );
    return joined.buffer.slice(
      joined.byteOffset,
      joined.byteOffset + joined.byteLength,
    );
  }
}

const shims = {
  process: internal.process,
  primordials: internal.primordials,
  internalBinding: internal.internalBinding,
  fallback: internal.load,
  events: () => require(\"node:events\"),
  buffer: () => require(\"node:buffer\"),
  string_decoder: () => require(\"node:string_decoder\"),
  async_hooks: () => require(\"node:async_hooks\"),
  \"internal/events/abort_listener\": () => ({
    addAbortListener: require(\"node:events\").addAbortListener,
  }),
  \"internal/buffer\": () => {
    const { Buffer } = require(\"node:buffer\");
    return {
      FastBuffer: function FastBuffer(buffer, byteOffset, length) {
        return Buffer.from(buffer, byteOffset, length);
      },
    };
  },
  \"internal/abort_controller\": () => optional(\"spark:dom_events\") ?? {},
  \"internal/event_target\": () => optional(\"spark:dom_events\")?.internals ?? {},
  \"internal/encoding\": () => optional(\"spark:encoding\") ?? {},
  // there is no Blob yet; this holds bytes for the consumers that go through one
  \"internal/blob\": () => ({ Blob: Bytes, isBlob: () => false }),
  // no hooks, and no context to carry into a stream's callbacks
  \"internal/async_hooks\": () => ({ enabledHooksExist: () => false }),
  \"internal/async_context_frame\": () => ({ current: () => undefined }),
};

const files = {
  \"stream\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectDefineProperty,
      ObjectKeys,
      ReflectApply,
    } = primordials;

    const {
      promisify: { custom: customPromisify },
    } = require('internal/util');

    const {
      streamReturningOperators,
      promiseReturningOperators,
    } = require('internal/streams/operators');

    const {
      codes: {
        ERR_ILLEGAL_CONSTRUCTOR,
      },
    } = require('internal/errors');
    const compose = require('internal/streams/compose');
    const { setDefaultHighWaterMark, getDefaultHighWaterMark } = require('internal/streams/state');
    const { pipeline } = require('internal/streams/pipeline');
    const { destroyer } = require('internal/streams/destroy');
    const { eos } = require('internal/streams/end-of-stream');
    const internalBuffer = require('internal/buffer');

    const promises = require('stream/promises');
    const utils = require('internal/streams/utils');
    const { isArrayBufferView, isUint8Array } = require('internal/util/types');

    const Stream = module.exports = require('internal/streams/legacy').Stream;

    Stream.isDestroyed = utils.isDestroyed;
    Stream.isDisturbed = utils.isDisturbed;
    Stream.isErrored = utils.isErrored;
    Stream.isReadable = utils.isReadable;
    Stream.isWritable = utils.isWritable;

    Stream.Readable = require('internal/streams/readable');
    const streamKeys = Object.keys(streamReturningOperators);
    for (let i = 0; i < streamKeys.length; i++) {
      const key = streamKeys[i];
      const op = streamReturningOperators[key];
      function fn(...args) {
        if (new.target) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
        return Stream.Readable.from(Reflect.apply(op, this, args));
      }
      Object.defineProperty(fn, 'name', { __proto__: null, value: op.name });
      Object.defineProperty(fn, 'length', { __proto__: null, value: op.length });
      Object.defineProperty(Stream.Readable.prototype, key, {
        __proto__: null,
        value: fn,
        enumerable: false,
        configurable: true,
        writable: true,
      });
    }
    const promiseKeys = Object.keys(promiseReturningOperators);
    for (let i = 0; i < promiseKeys.length; i++) {
      const key = promiseKeys[i];
      const op = promiseReturningOperators[key];
      function fn(...args) {
        if (new.target) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
        return Reflect.apply(op, this, args);
      }
      Object.defineProperty(fn, 'name', { __proto__: null, value: op.name });
      Object.defineProperty(fn, 'length', { __proto__: null, value: op.length });
      Object.defineProperty(Stream.Readable.prototype, key, {
        __proto__: null,
        value: fn,
        enumerable: false,
        configurable: true,
        writable: true,
      });
    }
    Stream.Writable = require('internal/streams/writable');
    Stream.Duplex = require('internal/streams/duplex');
    Stream.Transform = require('internal/streams/transform');
    Stream.PassThrough = require('internal/streams/passthrough');
    Stream.duplexPair = require('internal/streams/duplexpair');
    Stream.pipeline = pipeline;
    const { addAbortSignal } = require('internal/streams/add-abort-signal');
    Stream.addAbortSignal = addAbortSignal;
    Stream.finished = eos;
    Stream.destroy = destroyer;
    Stream.compose = compose;
    Stream.setDefaultHighWaterMark = setDefaultHighWaterMark;
    Stream.getDefaultHighWaterMark = getDefaultHighWaterMark;

    Object.defineProperty(Stream, 'promises', {
      __proto__: null,
      configurable: true,
      enumerable: true,
      get() {
        return promises;
      },
    });

    Object.defineProperty(pipeline, customPromisify, {
      __proto__: null,
      enumerable: true,
      get() {
        return promises.pipeline;
      },
    });

    Object.defineProperty(eos, customPromisify, {
      __proto__: null,
      enumerable: true,
      get() {
        return promises.finished;
      },
    });

    Stream.Stream = Stream;

    Stream._isArrayBufferView = isArrayBufferView;
    Stream._isUint8Array = isUint8Array;
    Stream._uint8ArrayToBuffer = function _uint8ArrayToBuffer(chunk) {
      return new internalBuffer.FastBuffer(chunk.buffer,
                                           chunk.byteOffset,
                                           chunk.byteLength);
    };
  },
  \"stream/promises\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayIsArray,
      ArrayPrototypePop,
      Promise,
    } = primordials;

    const {
      isIterable,
      isNodeStream,
      isWebStream,
    } = require('internal/streams/utils');

    const { pipelineImpl: pl } = require('internal/streams/pipeline');
    const { finished } = require('internal/streams/end-of-stream');

    require('stream');

    function pipeline(...streams) {
      return new Promise((resolve, reject) => {
        let signal;
        let end;
        const lastArg = streams[streams.length - 1];
        if (lastArg && typeof lastArg === 'object' &&
            !isNodeStream(lastArg) && !isIterable(lastArg) && !isWebStream(lastArg)) {
          const options = streams.pop();
          signal = options.signal;
          end = options.end;
        }

        let lastStream = streams[streams.length - 1];
        if (streams.length === 1 && Array.isArray(streams[0])) {
          lastStream = streams[0][streams[0].length - 1];
        }

        const stream = pl(streams, (err, value) => {
          if (err) {
            reject(err);
          } else {
            resolve(value);
          }
        }, { signal, end });

        if (typeof lastStream === 'function' && stream.readable) {
          stream.resume();
        }
      });
    }

    module.exports = {
      finished,
      pipeline,
    };
  },
  \"stream/consumers\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      JSONParse,
      Uint8Array,
    } = primordials;

    const {
      TextDecoder,
    } = require('internal/encoding');

    const {
      Blob,
    } = require('internal/blob');

    const {
      Buffer,
    } = require('buffer');

    async function blob(stream) {
      const chunks = [];
      for await (const chunk of stream)
        chunks.push(chunk);
      return new Blob(chunks);
    }

    async function arrayBuffer(stream) {
      const ret = await blob(stream);
      return ret.arrayBuffer();
    }

    async function buffer(stream) {
      return Buffer.from(await arrayBuffer(stream));
    }

    async function bytes(stream) {
      return new Uint8Array(await arrayBuffer(stream));
    }

    async function text(stream) {
      const dec = new TextDecoder();
      let str = '';
      for await (const chunk of stream) {
        if (typeof chunk === 'string')
          str += chunk;
        else
          str += dec.decode(chunk, { stream: true });
      }
      str += dec.decode(undefined, { stream: false });
      return str;
    }

    async function json(stream) {
      const str = await text(stream);
      return JSON.parse(str);
    }

    module.exports = {
      arrayBuffer,
      blob,
      buffer,
      bytes,
      text,
      json,
    };
  },
  \"internal/fixed_queue\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      Array,
      ArrayPrototypeFill,
    } = primordials;

    const kSize = 2048;
    const kMask = kSize - 1;

    class FixedCircularBuffer {
      constructor() {
        this.bottom = 0;
        this.top = 0;
        this.list = (new Array(kSize)).fill(undefined);
        this.next = null;
      }

      isEmpty() {
        return this.top === this.bottom;
      }

      isFull() {
        return ((this.top + 1) & kMask) === this.bottom;
      }

      push(data) {
        this.list[this.top] = data;
        this.top = (this.top + 1) & kMask;
      }

      shift() {
        const nextItem = this.list[this.bottom];
        if (nextItem === undefined)
          return null;
        this.list[this.bottom] = undefined;
        this.bottom = (this.bottom + 1) & kMask;
        return nextItem;
      }
    }

    module.exports = class FixedQueue {
      static #pool = [];

      constructor() {
        this.head = this.tail = FixedQueue.#pool.pop() ?? new FixedCircularBuffer();
      }

      isEmpty() {
        return this.head.isEmpty();
      }

      push(data) {
        if (this.head.isFull()) {
          this.head = this.head.next = FixedQueue.#pool.pop() ?? new FixedCircularBuffer();
        }
        this.head.push(data);
      }

      shift() {
        const tail = this.tail;
        const next = tail.shift();
        if (tail.isEmpty() && tail.next !== null) {
          this.tail = tail.next;
          tail.next = null;
          tail.bottom = 0;
          tail.top = 0;

          if (FixedQueue.#pool.length < 64) {
            FixedQueue.#pool.push(tail); // Recycle old tail
          }
        }
        return next;
      }
    };
  },
  \"internal/streams/add-abort-signal\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      SymbolDispose,
    } = primordials;

    const {
      AbortError,
      codes: {
        ERR_INVALID_ARG_TYPE,
      },
    } = require('internal/errors');

    const {
      isNodeStream,
      isWebStream,
      kControllerErrorFunction,
    } = require('internal/streams/utils');

    const { eos } = require('internal/streams/end-of-stream');
    let addAbortListener;

    const validateAbortSignal = (signal, name) => {
      if (typeof signal !== 'object' ||
           !('aborted' in signal)) {
        throw new ERR_INVALID_ARG_TYPE(name, 'AbortSignal', signal);
      }
    };

    module.exports.addAbortSignal = function addAbortSignal(signal, stream) {
      validateAbortSignal(signal, 'signal');
      if (!isNodeStream(stream) && !isWebStream(stream)) {
        throw new ERR_INVALID_ARG_TYPE('stream', ['ReadableStream', 'WritableStream', 'Stream'], stream);
      }
      return module.exports.addAbortSignalNoValidate(signal, stream);
    };

    module.exports.addAbortSignalNoValidate = function(signal, stream) {
      if (typeof signal !== 'object' || !('aborted' in signal)) {
        return stream;
      }
      const onAbort = isNodeStream(stream) ?
        () => {
          stream.destroy(new AbortError(undefined, { cause: signal.reason }));
        } :
        () => {
          stream[kControllerErrorFunction](new AbortError(undefined, { cause: signal.reason }));
        };
      if (signal.aborted) {
        onAbort();
      } else {
        addAbortListener ??= require('internal/events/abort_listener').addAbortListener;
        const disposable = addAbortListener(signal, onAbort);
        eos(stream, disposable[SymbolDispose]);
      }
      return stream;
    };
  },
  \"internal/streams/compose\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypeSlice,
    } = primordials;

    const { pipeline } = require('internal/streams/pipeline');
    const Duplex = require('internal/streams/duplex');
    const { destroyer } = require('internal/streams/destroy');
    const {
      isNodeStream,
      isReadable,
      isWritable,
      isWebStream,
      isTransformStream,
      isWritableStream,
      isReadableStream,
    } = require('internal/streams/utils');
    const {
      AbortError,
      codes: {
        ERR_INVALID_ARG_VALUE,
        ERR_MISSING_ARGS,
      },
    } = require('internal/errors');
    const { eos } = require('internal/streams/end-of-stream');

    module.exports = function compose(...streams) {
      if (streams.length === 0) {
        throw new ERR_MISSING_ARGS('streams');
      }

      if (streams.length === 1) {
        return Duplex.from(streams[0]);
      }

      const orgStreams = streams.slice();

      if (typeof streams[0] === 'function') {
        streams[0] = Duplex.from(streams[0]);
      }

      if (typeof streams[streams.length - 1] === 'function') {
        const idx = streams.length - 1;
        streams[idx] = Duplex.from(streams[idx]);
      }

      for (let n = 0; n < streams.length; ++n) {
        if (!isNodeStream(streams[n]) && !isWebStream(streams[n])) {
          continue;
        }
        if (
          n < streams.length - 1 &&
          !(
            isReadable(streams[n]) ||
            isReadableStream(streams[n]) ||
            isTransformStream(streams[n])
          )
        ) {
          throw new ERR_INVALID_ARG_VALUE(
            `streams[${n}]`,
            orgStreams[n],
            'must be readable',
          );
        }
        if (
          n > 0 &&
          !(
            isWritable(streams[n]) ||
            isWritableStream(streams[n]) ||
            isTransformStream(streams[n])
          )
        ) {
          throw new ERR_INVALID_ARG_VALUE(
            `streams[${n}]`,
            orgStreams[n],
            'must be writable',
          );
        }
      }

      let ondrain;
      let onfinish;
      let onclose;
      let d;

      function onfinished(err) {
        const cb = onclose;
        onclose = null;

        if (cb) {
          cb(err);
        } else if (err) {
          d.destroy(err);
        } else if (!readable && !writable) {
          d.destroy();
        }
      }

      const head = streams[0];
      const tail = pipeline(streams, onfinished);

      const writable = !!(
        isWritable(head) ||
        isWritableStream(head) ||
        isTransformStream(head)
      );
      const readable = !!(
        isReadable(tail) ||
        isReadableStream(tail) ||
        isTransformStream(tail)
      );

      d = new Duplex({
        writableObjectMode: !!head?.writableObjectMode,
        readableObjectMode: !!tail?.readableObjectMode,
        writable,
        readable,
      });

      if (writable) {
        if (isNodeStream(head)) {
          d._write = function(chunk, encoding, callback) {
            if (head.write(chunk, encoding)) {
              callback();
            } else {
              ondrain = callback;
            }
          };

          d._final = function(callback) {
            head.end();
            onfinish = callback;
          };

          head.on('drain', function() {
            if (ondrain) {
              const cb = ondrain;
              ondrain = null;
              cb();
            }
          });
        } else if (isWebStream(head)) {
          const writable = isTransformStream(head) ? head.writable : head;
          const writer = writable.getWriter();

          d._write = async function(chunk, encoding, callback) {
            try {
              await writer.ready;
              writer.write(chunk).catch(() => {});
              callback();
            } catch (err) {
              callback(err);
            }
          };

          d._final = async function(callback) {
            try {
              await writer.ready;
              writer.close().catch(() => {});
              onfinish = callback;
            } catch (err) {
              callback(err);
            }
          };
        }

        const toRead = isTransformStream(tail) ? tail.readable : tail;

        eos(toRead, () => {
          if (onfinish) {
            const cb = onfinish;
            onfinish = null;
            cb();
          }
        });
      }

      if (readable) {
        if (isNodeStream(tail)) {
          d._read = function() {
            tail.resume();
          };

          tail.on('data', function(chunk) {
            if (!d.push(chunk)) {
              tail.pause();
            }
          });

          tail.on('end', function() {
            d.push(null);
          });
        } else if (isWebStream(tail)) {
          const readable = isTransformStream(tail) ? tail.readable : tail;
          const reader = readable.getReader();
          d._read = async function() {
            while (true) {
              try {
                const { value, done } = await reader.read();

                if (done) {
                  d.push(null);
                  return;
                }

                if (!d.push(value)) {
                  return;
                }
              } catch {
                return;
              }
            }
          };
        }
      }

      d._destroy = function(err, callback) {
        if (!err && onclose !== null) {
          err = new AbortError();
        }

        ondrain = null;
        onfinish = null;

        if (isNodeStream(tail)) {
          destroyer(tail, err);
        }

        if (onclose === null) {
          callback(err);
        } else {
          onclose = callback;
        }
      };

      return d;
    };
  },
  \"internal/streams/destroy\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      Symbol,
    } = primordials;

    const {
      AbortError,
      aggregateTwoErrors,
      codes: {
        ERR_MULTIPLE_CALLBACK,
      },
    } = require('internal/errors');
    const {
      kIsDestroyed,
      isDestroyed,
      isFinished,
      isServerRequest,
      kState,
      kErrorEmitted,
      kEmitClose,
      kClosed,
      kCloseEmitted,
      kConstructed,
      kDestroyed,
      kAutoDestroy,
      kErrored,
    } = require('internal/streams/utils');

    const kDestroy = Symbol('kDestroy');
    const kConstruct = Symbol('kConstruct');

    function checkError(err, w, r) {
      if (err) {
        err.stack; // eslint-disable-line no-unused-expressions

        if (w && !w.errored) {
          w.errored = err;
        }
        if (r && !r.errored) {
          r.errored = err;
        }
      }
    }

    function destroy(err, cb) {
      const r = this._readableState;
      const w = this._writableState;
      const s = w || r;

      if (
        (w && (w[kState] & kDestroyed) !== 0) ||
        (r && (r[kState] & kDestroyed) !== 0)
      ) {
        if (typeof cb === 'function') {
          cb();
        }

        return this;
      }

      checkError(err, w, r);

      if (w) {
        w[kState] |= kDestroyed;
      }
      if (r) {
        r[kState] |= kDestroyed;
      }

      if ((s[kState] & kConstructed) === 0) {
        this.once(kDestroy, function(er) {
          _destroy(this, aggregateTwoErrors(er, err), cb);
        });
      } else {
        _destroy(this, err, cb);
      }

      return this;
    }

    function _destroy(self, err, cb) {
      let called = false;

      function onDestroy(err) {
        if (called) {
          return;
        }
        called = true;

        const r = self._readableState;
        const w = self._writableState;

        checkError(err, w, r);

        if (w) {
          w[kState] |= kClosed;
        }
        if (r) {
          r[kState] |= kClosed;
        }

        if (typeof cb === 'function') {
          cb(err);
        }

        if (err) {
          process.nextTick(emitErrorCloseNT, self, err);
        } else {
          process.nextTick(emitCloseNT, self);
        }
      }
      try {
        self._destroy(err || null, onDestroy);
      } catch (err) {
        onDestroy(err);
      }
    }

    function emitErrorCloseNT(self, err) {
      emitErrorNT(self, err);
      emitCloseNT(self);
    }

    function emitCloseNT(self) {
      const r = self._readableState;
      const w = self._writableState;

      if (w) {
        w[kState] |= kCloseEmitted;
      }
      if (r) {
        r[kState] |= kCloseEmitted;
      }

      if (
        (w && (w[kState] & kEmitClose) !== 0) ||
        (r && (r[kState] & kEmitClose) !== 0)
      ) {
        self.emit('close');
      }
    }

    function emitErrorNT(self, err) {
      const r = self._readableState;
      const w = self._writableState;

      if (
        (w && (w[kState] & kErrorEmitted) !== 0) ||
        (r && (r[kState] & kErrorEmitted) !== 0)
      ) {
        return;
      }

      if (w) {
        w[kState] |= kErrorEmitted;
      }
      if (r) {
        r[kState] |= kErrorEmitted;
      }

      self.emit('error', err);
    }

    function undestroy() {
      const r = this._readableState;
      const w = this._writableState;

      if (r) {
        r.constructed = true;
        r.closed = false;
        r.closeEmitted = false;
        r.destroyed = false;
        r.errored = null;
        r.errorEmitted = false;
        r.reading = false;
        r.fastChunk = null;
        r.ended = r.readable === false;
        r.endEmitted = r.readable === false;
      }

      if (w) {
        w.constructed = true;
        w.destroyed = false;
        w.closed = false;
        w.closeEmitted = false;
        w.errored = null;
        w.errorEmitted = false;
        w.finalCalled = false;
        w.prefinished = false;
        w.ended = w.writable === false;
        w.ending = w.writable === false;
        w.finished = w.writable === false;
      }
    }

    function errorOrDestroy(stream, err, sync) {

      const r = stream._readableState;
      const w = stream._writableState;

      if (
        (w && (w[kState] ? (w[kState] & kDestroyed) !== 0 : w.destroyed)) ||
        (r && (r[kState] ? (r[kState] & kDestroyed) !== 0 : r.destroyed))
      ) {
        return this;
      }

      if (
        (r && (r[kState] & kAutoDestroy) !== 0) ||
        (w && (w[kState] & kAutoDestroy) !== 0)
      ) {
        stream.destroy(err);
      } else if (err) {
        err.stack; // eslint-disable-line no-unused-expressions

        if (w && (w[kState] & kErrored) === 0) {
          w.errored = err;
        }
        if (r && (r[kState] & kErrored) === 0) {
          r.errored = err;
        }
        if (sync) {
          process.nextTick(emitErrorNT, stream, err);
        } else {
          emitErrorNT(stream, err);
        }
      }
    }

    function construct(stream, cb) {
      if (typeof stream._construct !== 'function') {
        return;
      }

      const r = stream._readableState;
      const w = stream._writableState;

      if (r) {
        r[kState] &= ~kConstructed;
      }
      if (w) {
        w[kState] &= ~kConstructed;
      }

      stream.once(kConstruct, cb);

      if (stream.listenerCount(kConstruct) > 1) {
        return;
      }

      process.nextTick(constructNT, stream);
    }

    function constructNT(stream) {
      let called = false;

      function onConstruct(err) {
        if (called) {
          errorOrDestroy(stream, err ?? new ERR_MULTIPLE_CALLBACK());
          return;
        }
        called = true;

        const r = stream._readableState;
        const w = stream._writableState;
        const s = w || r;

        if (r) {
          r[kState] |= kConstructed;
        }
        if (w) {
          w[kState] |= kConstructed;
        }

        if (s.destroyed) {
          stream.emit(kDestroy, err);
        } else if (err) {
          errorOrDestroy(stream, err, true);
        } else {
          stream.emit(kConstruct);
        }
      }

      try {
        stream._construct((err) => {
          process.nextTick(onConstruct, err);
        });
      } catch (err) {
        process.nextTick(onConstruct, err);
      }
    }

    function isRequest(stream) {
      return stream?.setHeader && typeof stream.abort === 'function';
    }

    function emitCloseLegacy(stream) {
      stream.emit('close');
    }

    function emitErrorCloseLegacy(stream, err) {
      stream.emit('error', err);
      process.nextTick(emitCloseLegacy, stream);
    }

    function destroyer(stream, err) {
      if (!stream || isDestroyed(stream)) {
        return;
      }

      if (!err && !isFinished(stream)) {
        err = new AbortError();
      }

      if (isServerRequest(stream)) {
        stream.socket = null;
        stream.destroy(err);
      } else if (isRequest(stream)) {
        stream.abort();
      } else if (isRequest(stream.req)) {
        stream.req.abort();
      } else if (typeof stream.destroy === 'function') {
        stream.destroy(err);
      } else if (typeof stream.close === 'function') {
        stream.close();
      } else if (err) {
        process.nextTick(emitErrorCloseLegacy, stream, err);
      } else {
        process.nextTick(emitCloseLegacy, stream);
      }

      if (!stream.destroyed) {
        stream[kIsDestroyed] = true;
      }
    }

    module.exports = {
      construct,
      destroyer,
      destroy,
      undestroy,
      errorOrDestroy,
    };
  },
  \"internal/streams/duplex\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectDefineProperties,
      ObjectGetOwnPropertyDescriptor,
      ObjectKeys,
      ObjectSetPrototypeOf,
    } = primordials;

    module.exports = Duplex;

    const Stream = require('internal/streams/legacy').Stream;
    const Readable = require('internal/streams/readable');
    const Writable = require('internal/streams/writable');

    const {
      addAbortSignal,
    } = require('internal/streams/add-abort-signal');

    const destroyImpl = require('internal/streams/destroy');
    const { kOnConstructed } = require('internal/streams/utils');

    Object.setPrototypeOf(Duplex.prototype, Readable.prototype);
    Object.setPrototypeOf(Duplex, Readable);

    {
      const keys = Object.keys(Writable.prototype);
      for (let i = 0; i < keys.length; i++) {
        const method = keys[i];
        Duplex.prototype[method] ||= Writable.prototype[method];
      }
    }

    Duplex.prototype.destroy = Writable.prototype.destroy;

    function Duplex(options) {
      if (!(this instanceof Duplex))
        return new Duplex(options);

      this._events ??= {
        close: undefined,
        error: undefined,
        prefinish: undefined,
        finish: undefined,
        drain: undefined,
        data: undefined,
        end: undefined,
        readable: undefined,
      };

      this._readableState = new Readable.ReadableState(options, this, true);
      this._writableState = new Writable.WritableState(options, this, true);

      if (options) {
        this.allowHalfOpen = options.allowHalfOpen !== false;

        if (options.readable === false) {
          this._readableState.readable = false;
          this._readableState.ended = true;
          this._readableState.endEmitted = true;
        }

        if (options.writable === false) {
          this._writableState.writable = false;
          this._writableState.ending = true;
          this._writableState.ended = true;
          this._writableState.finished = true;
        }

        if (typeof options.read === 'function')
          this._read = options.read;

        if (typeof options.write === 'function')
          this._write = options.write;

        if (typeof options.writev === 'function')
          this._writev = options.writev;

        if (typeof options.destroy === 'function')
          this._destroy = options.destroy;

        if (typeof options.final === 'function')
          this._final = options.final;

        if (typeof options.construct === 'function')
          this._construct = options.construct;

        if (options.signal)
          addAbortSignal(options.signal, this);
      } else {
        this.allowHalfOpen = true;
      }

      Stream.call(this, options);

      if (this._construct != null) {
        destroyImpl.construct(this, () => {
          this._readableState[kOnConstructed](this);
          this._writableState[kOnConstructed](this);
        });
      }
    }

    Object.defineProperties(Duplex.prototype, {
      writable:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writable') },
      writableHighWaterMark:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writableHighWaterMark') },
      writableObjectMode:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writableObjectMode') },
      writableBuffer:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writableBuffer') },
      writableLength:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writableLength') },
      writableFinished:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writableFinished') },
      writableCorked:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writableCorked') },
      writableEnded:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writableEnded') },
      writableNeedDrain:
        { __proto__: null, ...Object.getOwnPropertyDescriptor(Writable.prototype, 'writableNeedDrain') },

      destroyed: {
        __proto__: null,
        get() {
          if (this._readableState === undefined ||
            this._writableState === undefined) {
            return false;
          }
          return this._readableState.destroyed && this._writableState.destroyed;
        },
        set(value) {
          if (this._readableState && this._writableState) {
            this._readableState.destroyed = value;
            this._writableState.destroyed = value;
          }
        },
      },
    });

    let webStreamsAdapters;

    function lazyWebStreams() {
      if (webStreamsAdapters === undefined)
        webStreamsAdapters = require('internal/webstreams/adapters');
      return webStreamsAdapters;
    }

    Duplex.fromWeb = function(pair, options) {
      return lazyWebStreams().newStreamDuplexFromReadableWritablePair(
        pair,
        options);
    };

    Duplex.toWeb = function(duplex, options) {
      return lazyWebStreams().newReadableWritablePairFromDuplex(duplex, options);
    };

    let duplexify;

    Duplex.from = function(body) {
      duplexify ??= require('internal/streams/duplexify');
      return duplexify(body, 'body');
    };
  },
  \"internal/streams/duplexify\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      FunctionPrototypeCall,
      PromiseWithResolvers,
    } = primordials;

    const {
      isReadable,
      isWritable,
      isIterable,
      isNodeStream,
      isReadableNodeStream,
      isWritableNodeStream,
      isDuplexNodeStream,
      isReadableStream,
      isWritableStream,
    } = require('internal/streams/utils');
    const { eos } = require('internal/streams/end-of-stream');
    const {
      AbortError,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_RETURN_VALUE,
      },
    } = require('internal/errors');
    const { destroyer } = require('internal/streams/destroy');
    const Duplex = require('internal/streams/duplex');
    const Readable = require('internal/streams/readable');
    const Writable = require('internal/streams/writable');
    const from = require('internal/streams/from');

    const {
      isBlob,
    } = require('internal/blob');
    const { AbortController } = require('internal/abort_controller');

    class Duplexify extends Duplex {
      constructor(options) {
        super(options);

        if (options?.readable === false) {
          this._readableState.readable = false;
          this._readableState.ended = true;
          this._readableState.endEmitted = true;
        }

        if (options?.writable === false) {
          this._writableState.writable = false;
          this._writableState.ending = true;
          this._writableState.ended = true;
          this._writableState.finished = true;
        }
      }
    }

    module.exports = function duplexify(body, name) {
      if (isDuplexNodeStream(body)) {
        return body;
      }

      if (isReadableNodeStream(body)) {
        return _duplexify({ __proto__: null, readable: body });
      }

      if (isWritableNodeStream(body)) {
        return _duplexify({ __proto__: null, writable: body });
      }

      if (isNodeStream(body)) {
        return _duplexify({ __proto__: null, writable: false, readable: false });
      }

      if (isReadableStream(body)) {
        return _duplexify({ __proto__: null, readable: Readable.fromWeb(body) });
      }

      if (isWritableStream(body)) {
        return _duplexify({ __proto__: null, writable: Writable.fromWeb(body) });
      }

      if (typeof body === 'function') {
        const { value, write, final, destroy } = fromAsyncGen(body);

        if (isDuplexNodeStream(value)) {
          return value;
        }

        if (isIterable(value)) {
          return from(Duplexify, value, {
            objectMode: true,
            write,
            final,
            destroy,
          });
        }

        const then = value?.then;
        if (typeof then === 'function') {
          let d;
          let finalized = false;

          const promise = FunctionPrototypeCall(
            then,
            value,
            (val) => {
              if (val != null) {
                throw new ERR_INVALID_RETURN_VALUE('nully', 'body', val);
              }
              if (!finalized) {
                destroyer(d);
              }
            },
            (err) => {
              destroyer(d, err);
            },
          );

          return d = new Duplexify({
            objectMode: true,
            readable: false,
            write,
            final(cb) {
              finalized = true;
              final(async () => {
                try {
                  await promise;
                  process.nextTick(cb, null);
                } catch (err) {
                  process.nextTick(cb, err);
                }
              });
            },
            destroy,
          });
        }

        throw new ERR_INVALID_RETURN_VALUE(
          'Iterable, AsyncIterable or AsyncFunction', name, value);
      }

      if (isBlob(body)) {
        return duplexify(body.arrayBuffer());
      }

      if (isIterable(body)) {
        return from(Duplexify, body, {
          objectMode: true,
          writable: false,
        });
      }

      if (
        isReadableStream(body?.readable) &&
        isWritableStream(body?.writable)
      ) {
        return Duplexify.fromWeb(body);
      }

      if (
        typeof body?.writable === 'object' ||
        typeof body?.readable === 'object'
      ) {
        const readable = body?.readable ?
          isReadableNodeStream(body?.readable) ? body?.readable :
            duplexify(body.readable) :
          undefined;

        const writable = body?.writable ?
          isWritableNodeStream(body?.writable) ? body?.writable :
            duplexify(body.writable) :
          undefined;

        return _duplexify({ __proto__: null, readable, writable });
      }

      const then = body?.then;
      if (typeof then === 'function') {
        let d;

        FunctionPrototypeCall(
          then,
          body,
          (val) => {
            if (val != null) {
              d.push(val);
            }
            d.push(null);
          },
          (err) => {
            destroyer(d, err);
          },
        );

        return d = new Duplexify({
          objectMode: true,
          writable: false,
          read() {},
        });
      }

      throw new ERR_INVALID_ARG_TYPE(
        name,
        ['Blob', 'ReadableStream', 'WritableStream', 'Stream', 'Iterable',
         'AsyncIterable', 'Function', '{ readable, writable } pair', 'Promise'],
        body);
    };

    function fromAsyncGen(fn) {
      let { promise, resolve } = PromiseWithResolvers();
      const ac = new AbortController();
      const signal = ac.signal;
      const value = fn(async function*() {
        while (true) {
          const _promise = promise;
          promise = null;
          const { chunk, done, cb } = await _promise;
          process.nextTick(cb);
          if (done) return;
          if (signal.aborted)
            throw new AbortError(undefined, { cause: signal.reason });
          ({ promise, resolve } = PromiseWithResolvers());
          yield chunk;
        }
      }(), { signal });

      return {
        value,
        write(chunk, encoding, cb) {
          const _resolve = resolve;
          resolve = null;
          _resolve({ __proto__: null, chunk, done: false, cb });
        },
        final(cb) {
          const _resolve = resolve;
          resolve = null;
          _resolve({ __proto__: null, done: true, cb });
        },
        destroy(err, cb) {
          ac.abort(err);

          if (resolve !== null) {
            const _resolve = resolve;
            resolve = null;
            _resolve({ __proto__: null, done: true, cb() {} });
          }

          cb(err);
        },
      };
    }

    function _duplexify(pair) {
      const r = pair.readable && typeof pair.readable.read !== 'function' ?
        Readable.wrap(pair.readable) : pair.readable;
      const w = pair.writable;

      let readable = !!isReadable(r);
      let writable = !!isWritable(w);

      let ondrain;
      let onfinish;
      let onreadable;
      let onclose;
      let d;

      function onfinished(err) {
        const cb = onclose;
        onclose = null;

        if (cb) {
          cb(err);
        } else if (err) {
          d.destroy(err);
        }
      }

      d = new Duplexify({
        readableObjectMode: !!r?.readableObjectMode,
        writableObjectMode: !!w?.writableObjectMode,
        readable,
        writable,
      });

      if (writable) {
        eos(w, (err) => {
          writable = false;
          if (err) {
            destroyer(r, err);
          }
          onfinished(err);
        });

        d._write = function(chunk, encoding, callback) {
          if (w.write(chunk, encoding)) {
            callback();
          } else {
            ondrain = callback;
          }
        };

        d._final = function(callback) {
          w.end();
          onfinish = callback;
        };

        w.on('drain', function() {
          if (ondrain) {
            const cb = ondrain;
            ondrain = null;
            cb();
          }
        });

        w.on('finish', function() {
          if (onfinish) {
            const cb = onfinish;
            onfinish = null;
            cb();
          }
        });
      }

      if (readable) {
        eos(r, (err) => {
          readable = false;
          if (err) {
            destroyer(w, err);
          }
          onfinished(err);
        });

        r.on('readable', function() {
          if (onreadable) {
            const cb = onreadable;
            onreadable = null;
            cb();
          }
        });

        r.on('end', function() {
          d.push(null);
        });

        d._read = function() {
          while (true) {
            const buf = r.read();

            if (buf === null) {
              onreadable = d._read;
              return;
            }

            if (!d.push(buf)) {
              return;
            }
          }
        };
      }

      d._destroy = function(err, callback) {
        if (!err && onclose !== null) {
          err = new AbortError();
        }

        onreadable = null;
        ondrain = null;
        onfinish = null;

        if (onclose === null) {
          callback(err);
        } else {
          onclose = callback;
          destroyer(w, err);
          destroyer(r, err);
        }
      };

      return d;
    }
  },
  \"internal/streams/duplexpair\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';
    const {
      Symbol,
    } = primordials;

    const { Duplex } = require('stream');
    const assert = require('internal/assert');

    const kCallback = Symbol('Callback');
    const kInitOtherSide = Symbol('InitOtherSide');

    class DuplexSide extends Duplex {
      #otherSide = null;

      constructor(options) {
        super(options);
        this[kCallback] = null;
        this.#otherSide = null;
      }

      [kInitOtherSide](otherSide) {
        if (this.#otherSide === null) {
          this.#otherSide = otherSide;
        } else {
          assert(this.#otherSide === null);
        }
      }

      _read() {
        const callback = this[kCallback];
        if (callback) {
          this[kCallback] = null;
          callback();
        }
      }

      _write(chunk, encoding, callback) {
        assert(this.#otherSide !== null);
        assert(this.#otherSide[kCallback] === null);
        if (chunk.length === 0) {
          process.nextTick(callback);
        } else {
          this.#otherSide.push(chunk);
          this.#otherSide[kCallback] = callback;
        }
      }

      _final(callback) {
        this.#otherSide.on('end', callback);
        this.#otherSide.push(null);
      }

      _destroy(err, callback) {
        const otherSide = this.#otherSide;

        if (otherSide !== null && !otherSide.destroyed) {
          process.nextTick(() => {
            if (otherSide.destroyed) return;

            if (err) {
              otherSide.destroy();
            } else {
              otherSide.push(null);
            }
          });
        }

        callback(err);
      }
    }

    function duplexPair(options) {
      const side0 = new DuplexSide(options);
      const side1 = new DuplexSide(options);
      side0[kInitOtherSide](side1);
      side1[kInitOtherSide](side0);
      return [side0, side1];
    }
    module.exports = duplexPair;
  },
  \"internal/streams/end-of-stream\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      Promise,
      PromisePrototypeThen,
      ReflectApply,
      Symbol,
      SymbolDispose,
    } = primordials;

    const {
      AbortError,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_STREAM_PREMATURE_CLOSE,
      },
    } = require('internal/errors');
    const {
      kEmptyObject,
      once,
    } = require('internal/util');
    const {
      validateAbortSignal,
      validateFunction,
      validateObject,
      validateBoolean,
    } = require('internal/validators');

    const {
      isClosed,
      isReadable,
      isReadableNodeStream,
      isReadableStream,
      isReadableFinished,
      isReadableErrored,
      isWritable,
      isWritableNodeStream,
      isWritableStream,
      isWritableFinished,
      isWritableErrored,
      isNodeStream,
      willEmitClose: _willEmitClose,
      kIsClosedPromise,
    } = require('internal/streams/utils');

    const { enabledHooksExist } = require('internal/async_hooks');
    const AsyncContextFrame = require('internal/async_context_frame');

    let AsyncResource;
    let addAbortListener;

    function isRequest(stream) {
      return stream.setHeader && typeof stream.abort === 'function';
    }

    const nop = () => {};

    function bindAsyncResource(fn, type) {
      AsyncResource ??= require('async_hooks').AsyncResource;
      const resource = new AsyncResource(type);
      return function(...args) {
        return resource.runInAsyncScope(fn, this, ...args);
      };
    }

    function getEosErrored(stream) {
      const errored = isWritableErrored(stream) || isReadableErrored(stream);
      return typeof errored !== 'boolean' && errored || null;
    }

    function getEosOnCloseError(stream, readable, readableFinished, writable, writableFinished) {
      const errored = getEosErrored(stream);
      if (errored) {
        return errored;
      }

      if (readable && !readableFinished && isReadableNodeStream(stream, true)) {
        if (!isReadableFinished(stream, false)) {
          return new ERR_STREAM_PREMATURE_CLOSE();
        }
      }
      if (writable && !writableFinished) {
        if (!isWritableFinished(stream, false)) {
          return new ERR_STREAM_PREMATURE_CLOSE();
        }
      }

      return null;
    }

    const kEosNodeSynchronousCallback = Symbol('kEosNodeSynchronousCallback');

    function eos(stream, options, callback) {
      if (arguments.length === 2) {
        callback = options;
        options = kEmptyObject;
      } else if (options == null) {
        options = kEmptyObject;
      } else {
        validateObject(options, 'options');
      }
      validateFunction(callback, 'callback');
      validateAbortSignal(options.signal, 'options.signal');

      if (isReadableStream(stream) || isWritableStream(stream)) {
        return eosWeb(stream, options, callback);
      }

      if (!isNodeStream(stream)) {
        throw new ERR_INVALID_ARG_TYPE('stream', ['ReadableStream', 'WritableStream', 'Stream'], stream);
      }

      const readable = options.readable ?? isReadableNodeStream(stream);
      const writable = options.writable ?? isWritableNodeStream(stream);

      let willEmitClose = (
        _willEmitClose(stream) &&
        isReadableNodeStream(stream) === readable &&
        isWritableNodeStream(stream) === writable
      );
      let writableFinished = isWritableFinished(stream, false);
      let readableFinished = isReadableFinished(stream, false);

      const wState = stream._writableState;
      const rState = stream._readableState;

        let immediateResult;
      if (isClosed(stream)) {
        immediateResult = getEosOnCloseError(
          stream,
          readable,
          readableFinished,
          writable,
          writableFinished,
        );
      } else if (wState?.errorEmitted || rState?.errorEmitted) {
        if (!willEmitClose) {
          immediateResult = getEosErrored(stream);
        }
      } else if (
        !readable &&
        (!willEmitClose || isReadable(stream)) &&
        (writableFinished || isWritable(stream) === false) &&
        (wState == null || wState.pendingcb === undefined || wState.pendingcb === 0)
      ) {
        immediateResult = getEosErrored(stream);
      } else if (
        !writable &&
        (!willEmitClose || isWritable(stream)) &&
        (readableFinished || isReadable(stream) === false)
      ) {
        immediateResult = getEosErrored(stream);
      } else if ((rState && stream.req && stream.aborted)) {
        immediateResult = getEosErrored(stream);
      }
      let cleanup = () => {
        callback = nop;
      };
      if (immediateResult !== undefined) {
        if (options.error !== false) {
          stream.on('error', nop);
          cleanup = () => {
            callback = nop;
            stream.removeListener('error', nop);
          };
        }
      } else if (options.signal?.aborted) {
        immediateResult = new AbortError(undefined, { cause: options.signal.reason });
      }
      if (immediateResult !== undefined && options[kEosNodeSynchronousCallback]) {
        Reflect.apply(callback, stream, immediateResult === null ? [] : [immediateResult]);
        return cleanup;
      }

      if (AsyncContextFrame.current() || enabledHooksExist()) {
        callback = bindAsyncResource(callback, 'STREAM_END_OF_STREAM');
      }

      if (immediateResult !== undefined) {
        process.nextTick(() => Reflect.apply(callback, stream, immediateResult === null ? [] : [immediateResult]));
        return cleanup;
      }

      callback = once(callback);

      const onlegacyfinish = () => {
        if (!stream.writable) {
          onfinish();
        }
      };

      const onfinish = () => {
        writableFinished = true;
        if (stream.destroyed) {
          willEmitClose = false;
        }

        if (willEmitClose && (!stream.readable || readable)) {
          return;
        }

        if (!readable || readableFinished) {
          callback.call(stream);
        }
      };

      const onend = () => {
        readableFinished = true;
        if (stream.destroyed) {
          willEmitClose = false;
        }

        if (willEmitClose && (!stream.writable || writable)) {
          return;
        }

        if (!writable || writableFinished) {
          callback.call(stream);
        }
      };

      const onerror = (err) => {
        callback.call(stream, err);
      };

      const onclose = () => {
        const error = getEosOnCloseError(stream, readable, readableFinished, writable, writableFinished);
        if (error === null) {
          callback.call(stream);
        } else {
          callback.call(stream, error);
        }
      };

      const onrequest = () => {
        stream.req.on('finish', onfinish);
      };

      if (isRequest(stream)) {
        stream.on('complete', onfinish);
        if (!willEmitClose) {
          stream.on('abort', onclose);
        }
        if (stream.req) {
          onrequest();
        } else {
          stream.on('request', onrequest);
        }
      } else if (writable && !wState) { // legacy streams
        stream.on('end', onlegacyfinish);
        stream.on('close', onlegacyfinish);
      }

      if (!willEmitClose && typeof stream.aborted === 'boolean') {
        stream.on('aborted', onclose);
      }

      stream.on('end', onend);
      stream.on('finish', onfinish);
      if (options.error !== false) {
        stream.on('error', onerror);
      }
      stream.on('close', onclose);

      cleanup = () => {
        callback = nop;
        stream.removeListener('aborted', onclose);
        stream.removeListener('complete', onfinish);
        stream.removeListener('abort', onclose);
        stream.removeListener('request', onrequest);
        if (stream.req) stream.req.removeListener('finish', onfinish);
        stream.removeListener('end', onlegacyfinish);
        stream.removeListener('close', onlegacyfinish);
        stream.removeListener('finish', onfinish);
        stream.removeListener('end', onend);
        stream.removeListener('error', onerror);
        stream.removeListener('close', onclose);
      };

      if (options.signal) {
        const abort = () => {
          const endCallback = callback;
          cleanup();
          endCallback.call(
            stream,
            new AbortError(undefined, { cause: options.signal.reason }));
        };
        addAbortListener ??= require('internal/events/abort_listener').addAbortListener;
        const disposable = addAbortListener(options.signal, abort);
        const originalCallback = callback;
        callback = once((...args) => {
          disposable[SymbolDispose]();
          Reflect.apply(originalCallback, stream, args);
        });
      }

      return cleanup;
    }

    function eosWeb(stream, options, callback) {
      if (AsyncContextFrame.current() || enabledHooksExist()) {
        callback = once(bindAsyncResource(callback, 'STREAM_END_OF_STREAM'));
      } else {
        callback = once(callback);
      }

      let isAborted = false;
      let abort = nop;
      if (options.signal) {
        abort = () => {
          isAborted = true;
          callback.call(stream, new AbortError(undefined, { cause: options.signal.reason }));
        };
        if (options.signal.aborted) {
          process.nextTick(abort);
        } else {
          addAbortListener ??= require('internal/events/abort_listener').addAbortListener;
          const disposable = addAbortListener(options.signal, abort);
          const originalCallback = callback;
          callback = once((...args) => {
            disposable[SymbolDispose]();
            Reflect.apply(originalCallback, stream, args);
          });
        }
      }
      const resolverFn = (...args) => {
        if (!isAborted) {
          process.nextTick(() => Reflect.apply(callback, stream, args));
        }
      };
      PromisePrototypeThen(
        stream[kIsClosedPromise].promise,
        resolverFn,
        resolverFn,
      );
      return nop;
    }

    function finished(stream, opts) {
      let autoCleanup = false;
      if (opts === null) {
        opts = kEmptyObject;
      }
      if (opts?.cleanup) {
        validateBoolean(opts.cleanup, 'cleanup');
        autoCleanup = opts.cleanup;
      }
      return new Promise((resolve, reject) => {
        const cleanup = eos(stream, opts, (err) => {
          if (autoCleanup) {
            cleanup();
          }
          if (err) {
            reject(err);
          } else {
            resolve();
          }
        });
      });
    }

    module.exports = {
      eos,
      finished,
      kEosNodeSynchronousCallback,
    };
  },
  \"internal/streams/from\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      PromisePrototypeThen,
      SymbolAsyncIterator,
      SymbolIterator,
    } = primordials;
    const { Buffer } = require('buffer');

    const {
      aggregateTwoErrors,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_STREAM_NULL_VALUES,
      },
    } = require('internal/errors');

    function from(Readable, iterable, opts) {
      let iterator;
      if (typeof iterable === 'string' || iterable instanceof Buffer) {
        return new Readable({
          objectMode: true,
          ...opts,
          read() {
            this.push(iterable);
            this.push(null);
          },
        });
      }

      let isAsync;
      if (iterable?.[SymbolAsyncIterator]) {
        isAsync = true;
        iterator = iterable[SymbolAsyncIterator]();
      } else if (iterable?.[SymbolIterator]) {
        isAsync = false;
        iterator = iterable[SymbolIterator]();
      } else {
        throw new ERR_INVALID_ARG_TYPE('iterable', ['Iterable'], iterable);
      }

      const readable = new Readable({
        objectMode: true,
        highWaterMark: 1,
        ...opts,
      });
      const originalDestroy = readable._destroy;

      let reading = false;
      let isAsyncValues = false;

      readable._read = function() {
        if (!reading) {
          reading = true;

          if (isAsync) {
            nextAsync();
          } else if (isAsyncValues) {
            nextSyncWithAsyncValues();
          } else {
            nextSyncWithSyncValues();
          }
        }
      };

      readable._destroy = function(error, cb) {
        originalDestroy.call(this, error, (destroyError) => {
          const combinedError = destroyError || error;
          PromisePrototypeThen(
            close(combinedError),
            () => process.nextTick(cb, combinedError),
            (closeError) => process.nextTick(
              cb,
              aggregateTwoErrors(combinedError, closeError),
            ),
          );
        });
      };

      async function close(error) {
        const hadError = (error !== undefined) && (error !== null);
        const hasThrow = typeof iterator.throw === 'function';
        if (hadError && hasThrow) {
          const { value, done } = await iterator.throw(error);
          await value;
          if (done) {
            return;
          }
        }
        if (typeof iterator.return === 'function') {
          const { value } = await iterator.return();
          await value;
        }
      }

      function nextSyncWithSyncValues() {
        for (;;) {
          try {
            const { value, done } = iterator.next();

            if (done) {
              readable.push(null);
              return;
            }

            if (value &&
              typeof value.then === 'function') {
              return changeToAsyncValues(value);
            }

            if (value === null) {
              reading = false;
              throw new ERR_STREAM_NULL_VALUES();
            }

            if (readable.push(value)) {
              continue;
            }

            reading = false;
          } catch (err) {
            readable.destroy(err);
          }
          break;
        }
      }

      async function changeToAsyncValues(value) {
        isAsyncValues = true;

        try {
          const res = await value;

          if (res === null) {
            reading = false;
            throw new ERR_STREAM_NULL_VALUES();
          }

          if (readable.push(res)) {
            nextSyncWithAsyncValues();
            return;
          }

          reading = false;
        } catch (err) {
          readable.destroy(err);
        }
      }

      async function nextSyncWithAsyncValues() {
        for (;;) {
          try {
            const { value, done } = iterator.next();

            if (done) {
              readable.push(null);
              return;
            }

            const res = (value &&
              typeof value.then === 'function') ?
              await value :
              value;

            if (res === null) {
              reading = false;
              throw new ERR_STREAM_NULL_VALUES();
            }

            if (readable.push(res)) {
              continue;
            }

            reading = false;
          } catch (err) {
            readable.destroy(err);
          }
          break;
        }
      }

      async function nextAsync() {
        for (;;) {
          try {
            const { value, done } = await iterator.next();

            if (done) {
              readable.push(null);
              return;
            }

            if (value === null) {
              reading = false;
              throw new ERR_STREAM_NULL_VALUES();
            }

            if (readable.push(value)) {
              continue;
            }

            reading = false;
          } catch (err) {
            readable.destroy(err);
          }
          break;
        }
      }
      return readable;
    }

    module.exports = from;
  },
  \"internal/streams/lazy_transform\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectSetPrototypeOf,
    } = primordials;

    const stream = require('stream');

    module.exports = LazyTransform;

    function LazyTransform(options) {
      this._options = options;
    }
    Object.setPrototypeOf(LazyTransform.prototype, stream.Transform.prototype);
    Object.setPrototypeOf(LazyTransform, stream.Transform);

    function makeGetter(name) {
      return function() {
        stream.Transform.call(this, this._options);
        this._writableState.decodeStrings = false;
        return this[name];
      };
    }

    function makeSetter(name) {
      return function(val) {
        Object.defineProperty(this, name, {
          __proto__: null,
          value: val,
          enumerable: true,
          configurable: true,
          writable: true,
        });
      };
    }

    Object.defineProperties(LazyTransform.prototype, {
      _readableState: {
        __proto__: null,
        get: makeGetter('_readableState'),
        set: makeSetter('_readableState'),
        configurable: true,
        enumerable: true,
      },
      _writableState: {
        __proto__: null,
        get: makeGetter('_writableState'),
        set: makeSetter('_writableState'),
        configurable: true,
        enumerable: true,
      },
    });
  },
  \"internal/streams/legacy\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayIsArray,
      ObjectSetPrototypeOf,
      ReflectOwnKeys,
    } = primordials;

    const EE = require('events');

    function Stream(opts) {
      EE.call(this, opts);
    }
    Object.setPrototypeOf(Stream.prototype, EE.prototype);
    Object.setPrototypeOf(Stream, EE);

    Stream.prototype.pipe = function(dest, options) {
      const source = this;

      function ondata(chunk) {
        if (dest.writable && dest.write(chunk) === false && source.pause) {
          source.pause();
        }
      }

      source.on('data', ondata);

      function ondrain() {
        if (source.readable && source.resume) {
          source.resume();
        }
      }

      dest.on('drain', ondrain);

      if (!dest._isStdio && (!options || options.end !== false)) {
        source.on('end', onend);
        source.on('close', onclose);
      }

      let didOnEnd = false;
      function onend() {
        if (didOnEnd) return;
        didOnEnd = true;

        dest.end();
      }

      function onclose() {
        if (didOnEnd) return;
        didOnEnd = true;

        if (typeof dest.destroy === 'function') dest.destroy();
      }

      function onerror(er) {
        cleanup();
        if (this.listenerCount?.('error') === 0) {
          this.emit('error', er);
        }
      }

      prependListener(source, 'error', onerror);
      prependListener(dest, 'error', onerror);

      function cleanup() {
        source.removeListener('data', ondata);
        dest.removeListener('drain', ondrain);

        source.removeListener('end', onend);
        source.removeListener('close', onclose);

        source.removeListener('error', onerror);
        dest.removeListener('error', onerror);

        source.removeListener('end', cleanup);
        source.removeListener('close', cleanup);

        dest.removeListener('close', cleanup);
      }

      source.on('end', cleanup);
      source.on('close', cleanup);

      dest.on('close', cleanup);
      dest.emit('pipe', source);

      return dest;
    };

    Stream.prototype.eventNames = function eventNames() {
      const names = [];
      for (const key of Reflect.ownKeys(this._events)) {
        if (typeof this._events[key] === 'function' || (Array.isArray(this._events[key]) && this._events[key].length > 0)) {
          names.push(key);
        }
      }
      return names;
    };

    function prependListener(emitter, event, fn) {
      if (typeof emitter.prependListener === 'function')
        return emitter.prependListener(event, fn);

      if (!emitter._events || !emitter._events[event])
        emitter.on(event, fn);
      else if (Array.isArray(emitter._events[event]))
        emitter._events[event].unshift(fn);
      else
        emitter._events[event] = [fn, emitter._events[event]];
    }

    module.exports = { Stream, prependListener };
  },
  \"internal/streams/operators\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypePush,
      Boolean,
      MathFloor,
      Number,
      NumberIsNaN,
      Promise,
      PromisePrototypeThen,
      PromiseReject,
      PromiseResolve,
      Symbol,
    } = primordials;

    const { AbortController, AbortSignal } = require('internal/abort_controller');

    const {
      AbortError,
      codes: {
        ERR_MISSING_ARGS,
        ERR_OUT_OF_RANGE,
      },
    } = require('internal/errors');
    const {
      validateAbortSignal,
      validateInteger,
      validateObject,
      validateFunction,
    } = require('internal/validators');
    const { kWeakHandler, kResistStopPropagation } = require('internal/event_target');
    const { finished } = require('internal/streams/end-of-stream');

    const kEmpty = Symbol('kEmpty');
    const kEof = Symbol('kEof');

    function map(fn, options) {
      validateFunction(fn, 'fn');
      if (options != null) {
        validateObject(options, 'options');
      }
      if (options?.signal != null) {
        validateAbortSignal(options.signal, 'options.signal');
      }

      let concurrency = 1;
      if (options?.concurrency != null) {
        concurrency = Math.floor(options.concurrency);
      }

      let highWaterMark = concurrency - 1;
      if (options?.highWaterMark != null) {
        highWaterMark = Math.floor(options.highWaterMark);
      }

      validateInteger(concurrency, 'options.concurrency', 1);
      validateInteger(highWaterMark, 'options.highWaterMark', 0);

      highWaterMark += concurrency;

      return async function* map() {
        const signal = AbortSignal.any([options?.signal].filter(Boolean));
        const stream = this;
        const queue = [];
        const signalOpt = { signal };

        let next;
        let resume;
        let done = false;
        let cnt = 0;

        function onCatch() {
          done = true;
          afterItemProcessed();
        }

        function afterItemProcessed() {
          cnt -= 1;
          maybeResume();
        }

        function maybeResume() {
          if (
            resume &&
            !done &&
            cnt < concurrency &&
            queue.length < highWaterMark
          ) {
            resume();
            resume = null;
          }
        }

        async function pump() {
          try {
            for await (let val of stream) {
              if (done) {
                return;
              }

              if (signal.aborted) {
                throw new AbortError();
              }

              try {
                val = fn(val, signalOpt);

                if (val === kEmpty) {
                  continue;
                }

                val = Promise.resolve(val);
              } catch (err) {
                val = Promise.reject(err);
              }

              cnt += 1;

              PromisePrototypeThen(val, afterItemProcessed, onCatch);

              queue.push(val);
              if (next) {
                next();
                next = null;
              }

              if (!done && (queue.length >= highWaterMark || cnt >= concurrency)) {
                await new Promise((resolve) => {
                  resume = resolve;
                });
              }
            }
            queue.push(kEof);
          } catch (err) {
            const val = Promise.reject(err);
            PromisePrototypeThen(val, afterItemProcessed, onCatch);
            queue.push(val);
          } finally {
            done = true;
            if (next) {
              next();
              next = null;
            }
          }
        }

        pump();

        try {
          while (true) {
            while (queue.length > 0) {
              const val = await queue[0];

              if (val === kEof) {
                return;
              }

              if (signal.aborted) {
                throw new AbortError();
              }

              if (val !== kEmpty) {
                yield val;
              }

              queue.shift();
              maybeResume();
            }

            await new Promise((resolve) => {
              next = resolve;
            });
          }
        } finally {
          done = true;
          if (resume) {
            resume();
            resume = null;
          }
        }
      }.call(this);
    }

    async function some(fn, options = undefined) {
      for await (const unused of filter.call(this, fn, options)) {
        return true;
      }
      return false;
    }

    async function every(fn, options = undefined) {
      validateFunction(fn, 'fn');
      return !(await some.call(this, async (...args) => {
        return !(await fn(...args));
      }, options));
    }

    async function find(fn, options) {
      for await (const result of filter.call(this, fn, options)) {
        return result;
      }
      return undefined;
    }

    async function forEach(fn, options) {
      validateFunction(fn, 'fn');
      async function forEachFn(value, options) {
        await fn(value, options);
        return kEmpty;
      }
      for await (const unused of map.call(this, forEachFn, options));
    }

    function filter(fn, options) {
      validateFunction(fn, 'fn');
      async function filterFn(value, options) {
        if (await fn(value, options)) {
          return value;
        }
        return kEmpty;
      }
      return map.call(this, filterFn, options);
    }

    class ReduceAwareErrMissingArgs extends ERR_MISSING_ARGS {
      constructor() {
        super('reduce');
        this.message = 'Reduce of an empty stream requires an initial value';
      }
    }

    async function reduce(reducer, initialValue, options) {
      validateFunction(reducer, 'reducer');
      if (options != null) {
        validateObject(options, 'options');
      }
      if (options?.signal != null) {
        validateAbortSignal(options.signal, 'options.signal');
      }

      let hasInitialValue = arguments.length > 1;
      if (options?.signal?.aborted) {
        const err = new AbortError(undefined, { cause: options.signal.reason });
        this.once('error', () => {}); // The error is already propagated
        await finished(this.destroy(err));
        throw err;
      }
      const ac = new AbortController();
      const signal = ac.signal;
      if (options?.signal) {
        const opts = { once: true, [kWeakHandler]: this, [kResistStopPropagation]: true };
        options.signal.addEventListener('abort', () => ac.abort(), opts);
      }
      let gotAnyItemFromStream = false;
      try {
        for await (const value of this) {
          gotAnyItemFromStream = true;
          if (options?.signal?.aborted) {
            throw new AbortError();
          }
          if (!hasInitialValue) {
            initialValue = value;
            hasInitialValue = true;
          } else {
            initialValue = await reducer(initialValue, value, { signal });
          }
        }
        if (!gotAnyItemFromStream && !hasInitialValue) {
          throw new ReduceAwareErrMissingArgs();
        }
      } finally {
        ac.abort();
      }
      return initialValue;
    }

    async function toArray(options) {
      if (options != null) {
        validateObject(options, 'options');
      }
      if (options?.signal != null) {
        validateAbortSignal(options.signal, 'options.signal');
      }

      const result = [];
      for await (const val of this) {
        if (options?.signal?.aborted) {
          throw new AbortError(undefined, { cause: options.signal.reason });
        }
        result.push(val);
      }
      return result;
    }

    function flatMap(fn, options) {
      const values = map.call(this, fn, options);
      return async function* flatMap() {
        for await (const val of values) {
          yield* val;
        }
      }.call(this);
    }

    function toIntegerOrInfinity(number) {
      number = Number(number);
      if (Number.isNaN(number)) {
        return 0;
      }
      if (number < 0) {
        throw new ERR_OUT_OF_RANGE('number', '>= 0', number);
      }
      return number;
    }

    function drop(number, options = undefined) {
      if (options != null) {
        validateObject(options, 'options');
      }
      if (options?.signal != null) {
        validateAbortSignal(options.signal, 'options.signal');
      }

      number = toIntegerOrInfinity(number);
      return async function* drop() {
        if (options?.signal?.aborted) {
          throw new AbortError();
        }
        for await (const val of this) {
          if (options?.signal?.aborted) {
            throw new AbortError();
          }
          if (number-- <= 0) {
            yield val;
          }
        }
      }.call(this);
    }

    function take(number, options = undefined) {
      if (options != null) {
        validateObject(options, 'options');
      }
      if (options?.signal != null) {
        validateAbortSignal(options.signal, 'options.signal');
      }

      number = toIntegerOrInfinity(number);
      return async function* take() {
        if (options?.signal?.aborted) {
          throw new AbortError();
        }
        for await (const val of this) {
          if (options?.signal?.aborted) {
            throw new AbortError();
          }
          if (number-- > 0) {
            yield val;
          }

          if (number <= 0) {
            return;
          }
        }
      }.call(this);
    }

    module.exports.streamReturningOperators = {
      drop,
      filter,
      flatMap,
      map,
      take,
    };

    module.exports.promiseReturningOperators = {
      every,
      forEach,
      reduce,
      toArray,
      some,
      find,
    };
  },
  \"internal/streams/passthrough\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectSetPrototypeOf,
    } = primordials;

    module.exports = PassThrough;

    const Transform = require('internal/streams/transform');
    Object.setPrototypeOf(PassThrough.prototype, Transform.prototype);
    Object.setPrototypeOf(PassThrough, Transform);

    function PassThrough(options) {
      if (!(this instanceof PassThrough))
        return new PassThrough(options);

      Transform.call(this, options);
    }

    PassThrough.prototype._transform = function(chunk, encoding, cb) {
      cb(null, chunk);
    };
  },
  \"internal/streams/pipeline\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayIsArray,
      Promise,
      SymbolAsyncIterator,
      SymbolDispose,
    } = primordials;

    const { eos } = require('internal/streams/end-of-stream');
    const { once } = require('internal/util');
    const destroyImpl = require('internal/streams/destroy');
    const Duplex = require('internal/streams/duplex');
    const {
      AbortError,
      aggregateTwoErrors,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_RETURN_VALUE,
        ERR_MISSING_ARGS,
        ERR_STREAM_DESTROYED,
        ERR_STREAM_PREMATURE_CLOSE,
        ERR_STREAM_UNABLE_TO_PIPE,
      },
    } = require('internal/errors');

    const {
      validateFunction,
      validateAbortSignal,
    } = require('internal/validators');

    const {
      isIterable,
      isReadable,
      isReadableNodeStream,
      isNodeStream,
      isTransformStream,
      isWebStream,
      isReadableStream,
      isReadableFinished,
    } = require('internal/streams/utils');
    const { AbortController } = require('internal/abort_controller');

    let PassThrough;
    let Readable;
    let addAbortListener;

    function destroyer(stream, reading, writing) {
      let finished = false;
      stream.on('close', () => {
        finished = true;
      });

      const cleanup = eos(stream, { readable: reading, writable: writing }, (err) => {
        finished = !err;
      });

      return {
        destroy: (err) => {
          if (finished) return;
          finished = true;
          destroyImpl.destroyer(stream, err || new ERR_STREAM_DESTROYED('pipe'));
        },
        cleanup,
      };
    }

    function popCallback(streams) {
      validateFunction(streams[streams.length - 1], 'streams[stream.length - 1]');
      return streams.pop();
    }

    function makeAsyncIterable(val) {
      if (isIterable(val)) {
        return val;
      } else if (isReadableNodeStream(val)) {
        return fromReadable(val);
      }
      throw new ERR_INVALID_ARG_TYPE(
        'val', ['Readable', 'Iterable', 'AsyncIterable'], val);
    }

    async function* fromReadable(val) {
      Readable ??= require('internal/streams/readable');
      yield* Readable.prototype[SymbolAsyncIterator].call(val);
    }

    async function pumpToNode(iterable, writable, finish, { end }) {
      let error;
      let onresolve = null;

      const resume = (err) => {
        if (err) {
          error = err;
        }

        if (onresolve) {
          const callback = onresolve;
          onresolve = null;
          callback();
        }
      };

      const wait = () => new Promise((resolve, reject) => {
        if (error) {
          reject(error);
        } else {
          onresolve = () => {
            if (error) {
              reject(error);
            } else {
              resolve();
            }
          };
        }
      });

      writable.on('drain', resume);
      const cleanup = eos(writable, { readable: false }, resume);

      try {
        if (writable.writableNeedDrain) {
          await wait();
        }

        for await (const chunk of iterable) {
          if (!writable.write(chunk)) {
            await wait();
          }
        }

        if (end) {
          writable.end();
          await wait();
        }

        finish();
      } catch (err) {
        finish(error !== err ? aggregateTwoErrors(error, err) : err);
      } finally {
        cleanup();
        writable.off('drain', resume);
      }
    }

    async function pumpToWeb(readable, writable, finish, { end }) {
      if (isTransformStream(writable)) {
        writable = writable.writable;
      }
      const writer = writable.getWriter();
      try {
        for await (const chunk of readable) {
          await writer.ready;
          writer.write(chunk).catch(() => {});
        }

        await writer.ready;

        if (end) {
          await writer.close();
        }

        finish();
      } catch (err) {
        try {
          await writer.abort(err);
          finish(err);
        } catch (err) {
          finish(err);
        }
      }
    }

    function pipeline(...streams) {
      return pipelineImpl(streams, once(popCallback(streams)));
    }

    function pipelineImpl(streams, callback, opts) {
      if (streams.length === 1 && Array.isArray(streams[0])) {
        streams = streams[0];
      }

      if (streams.length < 2) {
        throw new ERR_MISSING_ARGS('streams');
      }

      const ac = new AbortController();
      const signal = ac.signal;
      const outerSignal = opts?.signal;

      const lastStreamCleanup = [];

      validateAbortSignal(outerSignal, 'options.signal');

      function abort() {
        finishImpl(new AbortError(undefined, { cause: outerSignal?.reason }));
      }

      addAbortListener ??= require('internal/events/abort_listener').addAbortListener;
      let disposable;
      if (outerSignal) {
        disposable = addAbortListener(outerSignal, abort);
      }

      let error;
      let value;
      const destroys = [];

      let finishCount = 0;

      function finish(err) {
        finishImpl(err, --finishCount === 0);
      }

      function finishOnlyHandleError(err) {
        finishImpl(err, false);
      }

      function finishImpl(err, final) {
        if (err && (!error || error.code === 'ERR_STREAM_PREMATURE_CLOSE' || error.name === 'AbortError')) {
          error = err;
        }

        if (!error && !final) {
          return;
        }

        while (destroys.length) {
          destroys.shift()(error);
        }

        disposable?.[SymbolDispose]();
        ac.abort();

        if (final) {
          if (!error) {
            lastStreamCleanup.forEach((fn) => fn());
          }
          process.nextTick(callback, error, value);
        }
      }

      let ret;
      for (let i = 0; i < streams.length; i++) {
        const stream = streams[i];
        const reading = i < streams.length - 1;
        const writing = i > 0;
        const next = i + 1 < streams.length ? streams[i + 1] : null;
        const end = reading || opts?.end !== false;
        const isLastStream = i === streams.length - 1;

        if (isNodeStream(stream)) {
          if (next !== null && (next?.closed || next?.destroyed)) {
            throw new ERR_STREAM_UNABLE_TO_PIPE();
          }

          if (end) {
            const { destroy, cleanup } = destroyer(stream, reading, writing);
            destroys.push(destroy);

            if (isReadable(stream) && isLastStream) {
              lastStreamCleanup.push(cleanup);
            }
          }

          function onError(err) {
            if (
              err &&
              err.name !== 'AbortError' &&
              err.code !== 'ERR_STREAM_PREMATURE_CLOSE'
            ) {
              finishOnlyHandleError(err);
            }
          }
          stream.on('error', onError);
          if (isReadable(stream) && isLastStream) {
            lastStreamCleanup.push(() => {
              stream.removeListener('error', onError);
            });
          }
        }

        if (i === 0) {
          if (typeof stream === 'function') {
            ret = stream({ signal });
            if (!isIterable(ret)) {
              throw new ERR_INVALID_RETURN_VALUE(
                'Iterable, AsyncIterable or Stream', 'source', ret);
            }
          } else if (isIterable(stream) || isReadableNodeStream(stream) || isTransformStream(stream)) {
            ret = stream;
          } else {
            ret = Duplex.from(stream);
          }
        } else if (typeof stream === 'function') {
          if (isTransformStream(ret)) {
            ret = makeAsyncIterable(ret?.readable);
          } else {
            ret = makeAsyncIterable(ret);
          }
          ret = stream(ret, { signal });

          if (reading) {
            if (!isIterable(ret, true)) {
              throw new ERR_INVALID_RETURN_VALUE(
                'AsyncIterable', `transform[${i - 1}]`, ret);
            }
          } else {
            PassThrough ??= require('internal/streams/passthrough');

            const pt = new PassThrough({
              objectMode: true,
            });

            const then = ret?.then;
            if (typeof then === 'function') {
              finishCount++;
              then.call(ret,
                        (val) => {
                          value = val;
                          if (val != null) {
                            pt.write(val);
                          }
                          if (end) {
                            pt.end();
                          }
                          process.nextTick(finish);
                        }, (err) => {
                          pt.destroy(err);
                          process.nextTick(finish, err);
                        },
              );
            } else if (isIterable(ret, true)) {
              finishCount++;
              pumpToNode(ret, pt, finish, { end });
            } else if (isReadableStream(ret) || isTransformStream(ret)) {
              const toRead = ret.readable || ret;
              finishCount++;
              pumpToNode(toRead, pt, finish, { end });
            } else {
              throw new ERR_INVALID_RETURN_VALUE(
                'AsyncIterable or Promise', 'destination', ret);
            }

            ret = pt;

            const { destroy, cleanup } = destroyer(ret, false, true);
            destroys.push(destroy);
            if (isLastStream) {
              lastStreamCleanup.push(cleanup);
            }
          }
        } else if (isNodeStream(stream)) {
          if (isReadableNodeStream(ret)) {
            finishCount += 2;
            const cleanup = pipe(ret, stream, finish, finishOnlyHandleError, { end });
            if (isReadable(stream) && isLastStream) {
              lastStreamCleanup.push(cleanup);
            }
          } else if (isTransformStream(ret) || isReadableStream(ret)) {
            const toRead = ret.readable || ret;
            finishCount++;
            pumpToNode(toRead, stream, finish, { end });
          } else if (isIterable(ret)) {
            finishCount++;
            pumpToNode(ret, stream, finish, { end });
          } else {
            throw new ERR_INVALID_ARG_TYPE(
              'val', ['Readable', 'Iterable', 'AsyncIterable', 'ReadableStream', 'TransformStream'], ret);
          }
          ret = stream;
        } else if (isWebStream(stream)) {
          if (isReadableNodeStream(ret)) {
            finishCount++;
            pumpToWeb(makeAsyncIterable(ret), stream, finish, { end });
          } else if (isReadableStream(ret) || isIterable(ret)) {
            finishCount++;
            pumpToWeb(ret, stream, finish, { end });
          } else if (isTransformStream(ret)) {
            finishCount++;
            pumpToWeb(ret.readable, stream, finish, { end });
          } else {
            throw new ERR_INVALID_ARG_TYPE(
              'val', ['Readable', 'Iterable', 'AsyncIterable', 'ReadableStream', 'TransformStream'], ret);
          }
          ret = stream;
        } else {
          ret = Duplex.from(stream);
        }
      }

      if (signal?.aborted || outerSignal?.aborted) {
        process.nextTick(abort);
      }

      return ret;
    }

    function pipe(src, dst, finish, finishOnlyHandleError, { end }) {
      let ended = false;
      dst.on('close', () => {
        if (!ended) {
          finishOnlyHandleError(new ERR_STREAM_PREMATURE_CLOSE());
        }
      });

      src.pipe(dst, { end: false }); // If end is true we already will have a listener to end dst.

      if (end) {

        function endFn() {
          ended = true;
          dst.end();
        }

        if (isReadableFinished(src)) { // End the destination if the source has already ended.
          process.nextTick(endFn);
        } else {
          src.once('end', endFn);
        }
      } else {
        finish();
      }

      eos(src, { readable: true, writable: false }, (err) => {
        const rState = src._readableState;
        if (
          err &&
          err.code === 'ERR_STREAM_PREMATURE_CLOSE' &&
          (rState?.ended && !rState.errored && !rState.errorEmitted)
        ) {
          src
            .once('end', finish)
            .once('error', finish);
        } else {
          finish(err);
        }
      });
      return eos(dst, { readable: false, writable: true }, finish);
    }

    module.exports = { pipelineImpl, pipeline };
  },
  \"internal/streams/readable\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypeIndexOf,
      AsyncIteratorPrototype,
      FunctionPrototypeCall,
      NumberIsInteger,
      NumberIsNaN,
      NumberParseInt,
      ObjectDefineProperties,
      ObjectKeys,
      ObjectSetPrototypeOf,
      Promise,
      PromisePrototypeThen,
      PromiseReject,
      PromiseResolve,
      ReflectApply,
      SafeSet,
      Symbol,
      SymbolAsyncDispose,
      SymbolAsyncIterator,
      SymbolFor,
      SymbolSpecies,
      TypedArrayPrototypeSet,
    } = primordials;

    module.exports = Readable;
    Readable.ReadableState = ReadableState;

    const EE = require('events');
    const { Stream, prependListener } = require('internal/streams/legacy');
    const { Buffer } = require('buffer');

    const {
      addAbortSignal,
      addAbortSignalNoValidate,
    } = require('internal/streams/add-abort-signal');
    const { eos } = require('internal/streams/end-of-stream');

    const { getOptionValue } = require('internal/options');

    let debug = require('internal/util/debuglog').debuglog('stream', (fn) => {
      debug = fn;
    });
    const destroyImpl = require('internal/streams/destroy');
    const {
      getHighWaterMark,
      getDefaultHighWaterMark,
    } = require('internal/streams/state');
    const {
      kState,
      kObjectMode,
      kErrorEmitted,
      kAutoDestroy,
      kEmitClose,
      kDestroyed,
      kClosed,
      kCloseEmitted,
      kErrored,
      kConstructed,
      kOnConstructed,
    } = require('internal/streams/utils');

    const {
      AbortError,
      aggregateTwoErrors,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_METHOD_NOT_IMPLEMENTED,
        ERR_OUT_OF_RANGE,
        ERR_STREAM_ITER_MISSING_FLAG,
        ERR_STREAM_PUSH_AFTER_EOF,
        ERR_STREAM_UNSHIFT_AFTER_END_EVENT,
        ERR_UNKNOWN_ENCODING,
      },
    } = require('internal/errors');
    const {
      validateAbortSignal,
      validateObject,
    } = require('internal/validators');

    const FastBuffer = Buffer[SymbolSpecies];

    const { StringDecoder } = require('string_decoder');
    const from = require('internal/streams/from');
    const FixedQueue = require('internal/fixed_queue');

    Object.setPrototypeOf(Readable.prototype, Stream.prototype);
    Object.setPrototypeOf(Readable, Stream);
    const nop = () => {};

    const { errorOrDestroy } = destroyImpl;

    const kErroredValue = Symbol('kErroredValue');
    const kDefaultEncodingValue = Symbol('kDefaultEncodingValue');
    const kDecoderValue = Symbol('kDecoderValue');
    const kEncodingValue = Symbol('kEncodingValue');

    const kEnded = 1 << 9;
    const kEndEmitted = 1 << 10;
    const kReading = 1 << 11;
    const kSync = 1 << 12;
    const kNeedReadable = 1 << 13;
    const kEmittedReadable = 1 << 14;
    const kReadableListening = 1 << 15;
    const kResumeScheduled = 1 << 16;
    const kMultiAwaitDrain = 1 << 17;
    const kReadingMore = 1 << 18;
    const kDataEmitted = 1 << 19;
    const kDefaultUTF8Encoding = 1 << 20;
    const kDecoder = 1 << 21;
    const kEncoding = 1 << 22;
    const kHasFlowing = 1 << 23;
    const kFlowing = 1 << 24;
    const kHasPaused = 1 << 25;
    const kPaused = 1 << 26;
    const kDataListening = 1 << 27;
    const kEndScheduled = 1 << 28;
    const kEofReadablePending = 1 << 29;
    const kFastPush = 1 << 30;

    function makeBitMapDescriptor(bit) {
      return {
        enumerable: false,
        get() { return (this[kState] & bit) !== 0; },
        set(value) {
          if (value) this[kState] |= bit;
          else this[kState] &= ~bit;
        },
      };
    }
    Object.defineProperties(ReadableState.prototype, {
      objectMode: makeBitMapDescriptor(kObjectMode),
      ended: makeBitMapDescriptor(kEnded),
      endEmitted: makeBitMapDescriptor(kEndEmitted),
      reading: makeBitMapDescriptor(kReading),
      constructed: makeBitMapDescriptor(kConstructed),
      sync: makeBitMapDescriptor(kSync),
      needReadable: makeBitMapDescriptor(kNeedReadable),
      emittedReadable: makeBitMapDescriptor(kEmittedReadable),
      readableListening: makeBitMapDescriptor(kReadableListening),
      resumeScheduled: makeBitMapDescriptor(kResumeScheduled),
      errorEmitted: makeBitMapDescriptor(kErrorEmitted),
      emitClose: makeBitMapDescriptor(kEmitClose),
      autoDestroy: makeBitMapDescriptor(kAutoDestroy),
      destroyed: makeBitMapDescriptor(kDestroyed),
      closed: makeBitMapDescriptor(kClosed),
      closeEmitted: makeBitMapDescriptor(kCloseEmitted),
      multiAwaitDrain: makeBitMapDescriptor(kMultiAwaitDrain),
      readingMore: makeBitMapDescriptor(kReadingMore),
      dataEmitted: makeBitMapDescriptor(kDataEmitted),

      errored: {
        __proto__: null,
        enumerable: false,
        get() {
          return (this[kState] & kErrored) !== 0 ? this[kErroredValue] : null;
        },
        set(value) {
          if (value) {
            this[kErroredValue] = value;
            this[kState] |= kErrored;
          } else {
            this[kState] &= ~kErrored;
          }
        },
      },

      defaultEncoding: {
        __proto__: null,
        enumerable: false,
        get() { return (this[kState] & kDefaultUTF8Encoding) !== 0 ? 'utf8' : this[kDefaultEncodingValue]; },
        set(value) {
          if (value === 'utf8' || value === 'utf-8') {
            this[kState] |= kDefaultUTF8Encoding;
          } else {
            this[kState] &= ~kDefaultUTF8Encoding;
            this[kDefaultEncodingValue] = value;
          }
        },
      },

      decoder: {
        __proto__: null,
        enumerable: false,
        get() {
          return (this[kState] & kDecoder) !== 0 ? this[kDecoderValue] : null;
        },
        set(value) {
          if (value) {
            this[kDecoderValue] = value;
            this[kState] |= kDecoder;
          } else {
            this[kState] &= ~kDecoder;
          }
        },
      },

      encoding: {
        __proto__: null,
        enumerable: false,
        get() {
          return (this[kState] & kEncoding) !== 0 ? this[kEncodingValue] : null;
        },
        set(value) {
          if (value) {
            this[kEncodingValue] = value;
            this[kState] |= kEncoding;
          } else {
            this[kState] &= ~kEncoding;
          }
        },
      },

      flowing: {
        __proto__: null,
        enumerable: false,
        get() {
          return (this[kState] & kHasFlowing) !== 0 ? (this[kState] & kFlowing) !== 0 : null;
        },
        set(value) {
          if (value == null) {
            this[kState] &= ~(kHasFlowing | kFlowing);
          } else if (value) {
            this[kState] |= (kHasFlowing | kFlowing);
          } else {
            this[kState] |= kHasFlowing;
            this[kState] &= ~kFlowing;
          }
        },
      },
    });

    function ReadableState(options, stream, isDuplex) {
      this[kState] = kEmitClose | kAutoDestroy | kConstructed | kSync;

      if (options?.objectMode)
        this[kState] |= kObjectMode;

      if (isDuplex && options?.readableObjectMode)
        this[kState] |= kObjectMode;

      this.highWaterMark = options ?
        getHighWaterMark(this, options, 'readableHighWaterMark', isDuplex) :
        getDefaultHighWaterMark(false);

      this.buffer = [];
      this.bufferIndex = 0;
      this.length = 0;
      this.fastChunk = null;
      this.pipeOnData = null;
      this.pipePause = null;
      this.pipes = [];

      if (options && options.emitClose === false) this[kState] &= ~kEmitClose;

      if (options && options.autoDestroy === false) this[kState] &= ~kAutoDestroy;

      const defaultEncoding = options?.defaultEncoding;
      if (defaultEncoding == null || defaultEncoding === 'utf8' || defaultEncoding === 'utf-8') {
        this[kState] |= kDefaultUTF8Encoding;
      } else if (Buffer.isEncoding(defaultEncoding)) {
        this.defaultEncoding = defaultEncoding;
      } else {
        throw new ERR_UNKNOWN_ENCODING(defaultEncoding);
      }

      this.awaitDrainWriters = null;

      if (options?.encoding) {
        this.decoder = new StringDecoder(options.encoding);
        this.encoding = options.encoding;
      }
    }

    ReadableState.prototype[kOnConstructed] = function onConstructed(stream) {
      if ((this[kState] & kNeedReadable) !== 0) {
        maybeReadMore(stream, this);
      }
    };

    function Readable(options) {
      if (!(this instanceof Readable))
        return new Readable(options);

      this._events ??= {
        close: undefined,
        error: undefined,
        data: undefined,
        end: undefined,
        readable: undefined,
      };

      this._readableState = new ReadableState(options, this, false);

      if (options) {
        if (typeof options.read === 'function')
          this._read = options.read;

        if (typeof options.destroy === 'function')
          this._destroy = options.destroy;

        if (typeof options.construct === 'function')
          this._construct = options.construct;

        if (options.signal)
          addAbortSignal(options.signal, this);
      }

      Stream.call(this, options);

      if (this._construct != null) {
        destroyImpl.construct(this, () => {
          this._readableState[kOnConstructed](this);
        });
      }
    }

    Readable.prototype.destroy = destroyImpl.destroy;
    Readable.prototype._undestroy = destroyImpl.undestroy;
    Readable.prototype._destroy = function(err, cb) {
      cb(err);
    };

    Readable.prototype[EE.captureRejectionSymbol] = function(err) {
      this.destroy(err);
    };

    Readable.prototype[SymbolAsyncDispose] = async function() {
      let error;
      if (!this.destroyed) {
        error = this.readableEnded ? null : new AbortError();
        this.destroy(error);
      }
      await new Promise((resolve, reject) => eos(this, (err) => (err && err !== error ? reject(err) : resolve(null))));
    };

    Readable.prototype.push = function(chunk, encoding) {
      const state = this._readableState;

      if ((state[kState] & kFastPush) !== 0 && encoding == null &&
          state.fastChunk == null && chunk instanceof Buffer && chunk.length > 0) {
        state.fastChunk = chunk;
        state.length = chunk.length;
        state[kState] &= ~kReading;
        return chunk.length < state.highWaterMark;
      }
      if ((state[kState] & kFastPush) !== 0) {
        state[kState] &= ~kFastPush;
        if (state.fastChunk != null) {
          const first = state.fastChunk;
          state.fastChunk = null;
          state.length = 0;
          readableAddChunkPushByteMode(this, state, first);
        }
      }

      debug('push', chunk);

      return (state[kState] & kObjectMode) === 0 ?
        readableAddChunkPushByteMode(this, state, chunk, encoding) :
        readableAddChunkPushObjectMode(this, state, chunk, encoding);
    };

    Readable.prototype.unshift = function(chunk, encoding) {
      debug('unshift', chunk);
      const state = this._readableState;
      return (state[kState] & kObjectMode) === 0 ?
        readableAddChunkUnshiftByteMode(this, state, chunk, encoding) :
        readableAddChunkUnshiftObjectMode(this, state, chunk);
    };

    function readableAddChunkUnshiftByteMode(stream, state, chunk, encoding) {
      if (chunk === null) {
        state[kState] &= ~kReading;
        onEofChunk(stream, state);

        return false;
      }

      if (typeof chunk === 'string') {
        encoding ||= state.defaultEncoding;
        if (state.encoding !== encoding) {
          if (state.encoding) {
            chunk = Buffer.from(chunk, encoding).toString(state.encoding);
          } else {
            chunk = Buffer.from(chunk, encoding);
          }
        }
      } else if (Stream._isArrayBufferView(chunk)) {
        chunk = Stream._uint8ArrayToBuffer(chunk);
      } else if (chunk !== undefined && !(chunk instanceof Buffer)) {
        errorOrDestroy(stream, new ERR_INVALID_ARG_TYPE(
          'chunk', ['string', 'Buffer', 'TypedArray', 'DataView'], chunk));
        return false;
      }

      if (!(chunk && chunk.length > 0)) {
        return canPushMore(state);
      }

      return readableAddChunkUnshiftValue(stream, state, chunk);
    }

    function readableAddChunkUnshiftObjectMode(stream, state, chunk) {
      if (chunk === null) {
        state[kState] &= ~kReading;
        onEofChunk(stream, state);

        return false;
      }

      return readableAddChunkUnshiftValue(stream, state, chunk);
    }

    function readableAddChunkUnshiftValue(stream, state, chunk) {
      if ((state[kState] & kEndEmitted) !== 0)
        errorOrDestroy(stream, new ERR_STREAM_UNSHIFT_AFTER_END_EVENT());
      else if ((state[kState] & (kDestroyed | kErrored)) !== 0)
        return false;
      else
        addChunk(stream, state, chunk, true);

      return canPushMore(state);
    }

    function readableAddChunkPushByteMode(stream, state, chunk, encoding) {
      if (chunk === null) {
        state[kState] &= ~kReading;
        onEofChunk(stream, state);
        return false;
      }

      if (typeof chunk === 'string') {
        encoding ||= state.defaultEncoding;
        if (state.encoding !== encoding) {
          chunk = Buffer.from(chunk, encoding);
          encoding = '';
        }
      } else if (chunk instanceof Buffer) {
        encoding = '';
      } else if (Stream._isArrayBufferView(chunk)) {
        chunk = Stream._uint8ArrayToBuffer(chunk);
        encoding = '';
      } else if (chunk !== undefined) {
        errorOrDestroy(stream, new ERR_INVALID_ARG_TYPE(
          'chunk', ['string', 'Buffer', 'TypedArray', 'DataView'], chunk));
        return false;
      }

      if (!chunk || chunk.length <= 0) {
        state[kState] &= ~kReading;
        maybeReadMore(stream, state);

        return canPushMore(state);
      }

      if ((state[kState] & kEnded) !== 0) {
        errorOrDestroy(stream, new ERR_STREAM_PUSH_AFTER_EOF());
        return false;
      }

      if ((state[kState] & (kDestroyed | kErrored)) !== 0) {
        return false;
      }

      state[kState] &= ~kReading;
      if ((state[kState] & kDecoder) !== 0 && !encoding) {
        chunk = state[kDecoderValue].write(chunk);
        if (chunk.length === 0) {
          maybeReadMore(stream, state);
          return canPushMore(state);
        }
      }

      addChunk(stream, state, chunk, false);
      return canPushMore(state);
    }

    function readableAddChunkPushObjectMode(stream, state, chunk, encoding) {
      if (chunk === null) {
        state[kState] &= ~kReading;
        onEofChunk(stream, state);
        return false;
      }

      if ((state[kState] & kEnded) !== 0) {
        errorOrDestroy(stream, new ERR_STREAM_PUSH_AFTER_EOF());
        return false;
      }

      if ((state[kState] & (kDestroyed | kErrored)) !== 0) {
        return false;
      }

      state[kState] &= ~kReading;

      if ((state[kState] & kDecoder) !== 0 && !encoding) {
        chunk = state[kDecoderValue].write(chunk);
      }

      addChunk(stream, state, chunk, false);
      return canPushMore(state);
    }

    function canPushMore(state) {
      return (state[kState] & kEnded) === 0 &&
        (state.length < state.highWaterMark || state.length === 0);
    }

    function addChunk(stream, state, chunk, addToFront) {
      if ((state[kState] & (kFlowing | kSync | kDataListening)) === (kFlowing | kDataListening) && state.length === 0) {
        if ((state[kState] & kMultiAwaitDrain) !== 0) {
          state.awaitDrainWriters.clear();
        } else {
          state.awaitDrainWriters = null;
        }

        state[kState] |= kDataEmitted;
        stream.emit('data', chunk);
      } else {
        state.length += (state[kState] & kObjectMode) !== 0 ? 1 : chunk.length;
        if (addToFront) {
          if (state.bufferIndex > 0) {
            state.buffer[--state.bufferIndex] = chunk;
          } else {
            state.buffer.unshift(chunk); // Slow path
          }
        } else {
          state.buffer.push(chunk);
        }

        if ((state[kState] & kNeedReadable) !== 0)
          emitReadable(stream);
      }
      maybeReadMore(stream, state);
    }

    Readable.prototype.isPaused = function() {
      const state = this._readableState;
      return (state[kState] & kPaused) !== 0 || (state[kState] & (kHasFlowing | kFlowing)) === kHasFlowing;
    };

    Readable.prototype.setEncoding = function(enc) {
      const state = this._readableState;
      if (state.fastChunk != null)
        materializeFastChunk(state);

      const decoder = new StringDecoder(enc);
      state.decoder = decoder;
      state.encoding = state.decoder.encoding;

      let content = '';
      for (const data of state.buffer.slice(state.bufferIndex)) {
        content += decoder.write(data);
      }
      if ((state[kState] & kEnded) !== 0)
        content += decoder.end();
      state.buffer.length = 0;
      state.bufferIndex = 0;

      if (content !== '')
        state.buffer.push(content);
      state.length = content.length;
      return this;
    };

    const MAX_HWM = 0x40000000;
    function computeNewHighWaterMark(n) {
      if (n > MAX_HWM) {
        throw new ERR_OUT_OF_RANGE('size', '<= 1GiB', n);
      } else {
        n--;
        n |= n >>> 1;
        n |= n >>> 2;
        n |= n >>> 4;
        n |= n >>> 8;
        n |= n >>> 16;
        n++;
      }
      return n;
    }

    function howMuchToRead(n, state) {
      if (n <= 0 || (state.length === 0 && (state[kState] & kEnded) !== 0))
        return 0;
      if ((state[kState] & kObjectMode) !== 0)
        return 1;
      if (Number.isNaN(n)) {
        if ((state[kState] & kDecoder) === 0 && state.length)
          return state.buffer[state.bufferIndex].length;

        if ((state[kState] & kFlowing) !== 0 && state.length)
          return state.buffer[state.bufferIndex].length;

        return state.length;
      }
      if (n <= state.length)
        return n;
      return (state[kState] & kEnded) !== 0 ? state.length : 0;
    }

    Readable.prototype.read = function(n) {
      if (this._readableState.fastChunk != null)
        materializeFastChunk(this._readableState);

      debug('read', n);
      if (n === undefined) {
        n = NaN;
      } else if (!Number.isInteger(n)) {
        n = Number.parseInt(n, 10);
      }
      const state = this._readableState;
      const nOrig = n;

      if (n > state.highWaterMark)
        state.highWaterMark = computeNewHighWaterMark(n);

      if (n !== 0)
        state[kState] &= ~(kEmittedReadable | kEofReadablePending);

      const stateLength = state.length;
      if (n === 0 &&
          (state[kState] & kNeedReadable) !== 0 &&
          ((state.highWaterMark !== 0 ?
            stateLength >= state.highWaterMark :
            stateLength > 0) ||
           (state[kState] & kEnded) !== 0)) {
        debug('read: emitReadable');
        if (stateLength === 0 && (state[kState] & kEnded) !== 0)
          endReadable(this);
        else
          emitReadable(this);
        return null;
      }

      n = howMuchToRead(n, state);

      if (n === 0 && (state[kState] & kEnded) !== 0) {
        if (state.length === 0)
          endReadable(this);
        return null;
      }

      let doRead = (state[kState] & kNeedReadable) !== 0;
      debug('need readable', doRead);

      if (state.length === 0 || state.length - n < state.highWaterMark) {
        doRead = true;
        debug('length less than watermark', doRead);
      }

      if ((state[kState] & (kReading | kEnded | kDestroyed | kErrored | kConstructed)) !== kConstructed) {
        doRead = false;
        debug('reading, ended or constructing', doRead);
      } else if (doRead) {
        debug('do read');
        state[kState] |= kReading | kSync;
        if (state.length === 0)
          state[kState] |= kNeedReadable;

        try {
          this._read(state.highWaterMark);
        } catch (err) {
          errorOrDestroy(this, err);
        }
        state[kState] &= ~kSync;

        if ((state[kState] & kReading) === 0)
          n = howMuchToRead(nOrig, state);
      }

      let ret;
      if (n > 0)
        ret = fromList(n, state);
      else
        ret = null;

      if (ret === null) {
        state[kState] |= state.length <= state.highWaterMark ? kNeedReadable : 0;
        n = 0;
      } else {
        state.length -= n;
        if ((state[kState] & kMultiAwaitDrain) !== 0) {
          state.awaitDrainWriters.clear();
        } else {
          state.awaitDrainWriters = null;
        }
      }

      if (state.length === 0) {
        if ((state[kState] & kEnded) === 0)
          state[kState] |= kNeedReadable;

        if (nOrig !== n && (state[kState] & kEnded) !== 0)
          endReadable(this);
      }

      if (ret !== null && (state[kState] & (kErrorEmitted | kCloseEmitted)) === 0) {
        state[kState] |= kDataEmitted;
        this.emit('data', ret);
      }

      return ret;
    };

    function onEofChunk(stream, state) {
      debug('onEofChunk');
      if ((state[kState] & kEnded) !== 0) return;
      const decoder = (state[kState] & kDecoder) !== 0 ? state[kDecoderValue] : null;
      if (decoder) {
        const chunk = decoder.end();
        if (chunk?.length) {
          state.buffer.push(chunk);
          state.length += (state[kState] & kObjectMode) !== 0 ? 1 : chunk.length;
        }
      }
      state[kState] |= kEnded;

      if ((state[kState] & kSync) !== 0) {
        if ((state[kState] & (kFlowing | kNeedReadable)) !== 0 ||
            stream.listenerCount('readable') > 0) {
          emitReadable(stream);
        } else {
          state[kState] |= kEofReadablePending;
        }
      } else {
        state[kState] &= ~kNeedReadable;
        state[kState] |= kEmittedReadable;
        emitReadable_(stream);
      }
    }

    function emitReadable(stream) {
      const state = stream._readableState;
      debug('emitReadable');
      state[kState] &= ~kNeedReadable;
      if ((state[kState] & kEmittedReadable) === 0) {
        debug('emitReadable', (state[kState] & kFlowing) !== 0);
        state[kState] |= kEmittedReadable;
        process.nextTick(emitReadable_, stream);
      }
    }

    function emitReadable_(stream) {
      const state = stream._readableState;
      debug('emitReadable_');
      if ((state[kState] & (kDestroyed | kErrored)) === 0 && (state.length || (state[kState] & kEnded) !== 0)) {
        stream.emit('readable');
        state[kState] &= ~kEmittedReadable;
      }

      state[kState] |=
        (state[kState] & (kFlowing | kEnded)) === 0 &&
        state.length <= state.highWaterMark ? kNeedReadable : 0;
      flow(stream);
    }

    function maybeReadMore(stream, state) {
      if ((state[kState] & (kReadingMore | kReading | kConstructed)) === kConstructed) {
        state[kState] |= kReadingMore;
        process.nextTick(maybeReadMore_, stream, state);
      }
    }

    function maybeReadMore_(stream, state) {
      while ((state[kState] & (kReading | kEnded)) === 0 &&
             (state.length < state.highWaterMark ||
              ((state[kState] & kFlowing) !== 0 && state.length === 0))) {
        const len = state.length;
        debug('maybeReadMore read 0');
        stream.read(0);
        if (len === state.length)
          break;
      }
      state[kState] &= ~kReadingMore;
    }

    Readable.prototype._read = function(n) {
      throw new ERR_METHOD_NOT_IMPLEMENTED('_read()');
    };

    Readable.prototype.pipe = function(dest, pipeOpts) {
      const src = this;
      const state = this._readableState;

      if (state.pipes.length === 1) {
        if ((state[kState] & kMultiAwaitDrain) === 0) {
          state[kState] |= kMultiAwaitDrain;
          state.awaitDrainWriters = new SafeSet(
            state.awaitDrainWriters ? [state.awaitDrainWriters] : [],
          );
        }
      }

      state.pipes.push(dest);
      if (state.pipes.length === 1) {
        state.pipeOnData = ondata;
        state.pipePause = pause;
      } else {
        state.pipeOnData = null;
        state.pipePause = null;
      }
      debug('pipe count=%d opts=%j', state.pipes.length, pipeOpts);

      const doEnd = (!pipeOpts || pipeOpts.end !== false) &&
                  dest !== process.stdout &&
                  dest !== process.stderr;

      const endFn = doEnd ? onend : unpipe;
      if ((state[kState] & kEndEmitted) !== 0)
        process.nextTick(endFn);
      else
        src.once('end', endFn);

      dest.on('unpipe', onunpipe);
      function onunpipe(readable, unpipeInfo) {
        debug('onunpipe');
        if (readable === src) {
          if (unpipeInfo && unpipeInfo.hasUnpiped === false) {
            unpipeInfo.hasUnpiped = true;
            cleanup();
          }
        }
      }

      function onend() {
        debug('onend');
        dest.end();
      }

      let ondrain;

      let cleanedUp = false;
      function cleanup() {
        debug('cleanup');
        dest.removeListener('close', onclose);
        dest.removeListener('finish', onfinish);
        if (ondrain) {
          dest.removeListener('drain', ondrain);
        }
        dest.removeListener('error', onerror);
        dest.removeListener('unpipe', onunpipe);
        src.removeListener('end', onend);
        src.removeListener('end', unpipe);
        src.removeListener('data', ondata);

        cleanedUp = true;

        if (ondrain && state.awaitDrainWriters)
          ondrain();
      }

      function pause() {
        if (!cleanedUp) {
          if (state.pipes.length === 1 && state.pipes[0] === dest) {
            debug('false write response, pause', 0);
            state.awaitDrainWriters = dest;
            state[kState] &= ~kMultiAwaitDrain;
          } else if (state.pipes.length > 1 && state.pipes.includes(dest)) {
            debug('false write response, pause', state.awaitDrainWriters.size);
            state.awaitDrainWriters.add(dest);
          }
          src.pause();
        }
        if (!ondrain) {
          ondrain = pipeOnDrain(src, dest);
          dest.on('drain', ondrain);
        }
      }

      src.on('data', ondata);
      function ondata(chunk) {
        debug('ondata');
        try {
          const ret = dest.write(chunk);
          debug('dest.write', ret);

          if (ret === false) {
            pause();
          }
        } catch (error) {
          dest.destroy(error);
        }
      }

      function onerror(er) {
        debug('onerror', er);
        unpipe();
        dest.removeListener('error', onerror);
        if (dest.listenerCount('error') === 0) {
          const s = dest._writableState || dest._readableState;
          if (s && !s.errorEmitted) {
            errorOrDestroy(dest, er);
          } else {
            dest.emit('error', er);
          }
        }
      }

      prependListener(dest, 'error', onerror);

      function onclose() {
        dest.removeListener('finish', onfinish);
        unpipe();
      }
      dest.once('close', onclose);
      function onfinish() {
        debug('onfinish');
        dest.removeListener('close', onclose);
        unpipe();
      }
      dest.once('finish', onfinish);

      function unpipe() {
        debug('unpipe');
        src.unpipe(dest);
      }

      dest.emit('pipe', src);

      if (dest.writableNeedDrain === true) {
        pause();
      } else if ((state[kState] & kFlowing) === 0) {
        debug('pipe resume');
        src.resume();
      }

      return dest;
    };

    function pipeOnDrain(src, dest) {
      return function pipeOnDrainFunctionResult() {
        const state = src._readableState;

        if (state.awaitDrainWriters === dest) {
          debug('pipeOnDrain', 1);
          state.awaitDrainWriters = null;
        } else if ((state[kState] & kMultiAwaitDrain) !== 0) {
          debug('pipeOnDrain', state.awaitDrainWriters.size);
          state.awaitDrainWriters.delete(dest);
        }

        if ((!state.awaitDrainWriters || state.awaitDrainWriters.size === 0) &&
          (state[kState] & kDataListening) !== 0) {
          src.resume();
        }
      };
    }

    Readable.prototype.unpipe = function(dest) {
      const state = this._readableState;
      const unpipeInfo = { hasUnpiped: false };

      if (state.pipes.length === 0)
        return this;

      if (!dest) {
        const dests = state.pipes;
        state.pipes = [];
        state.pipeOnData = null;
        state.pipePause = null;
        this.pause();

        for (let i = 0; i < dests.length; i++)
          dests[i].emit('unpipe', this, { hasUnpiped: false });
        return this;
      }

      const index = state.pipes.indexOf(dest);
      if (index === -1)
        return this;

      state.pipes.splice(index, 1);
      state.pipeOnData = null;
      state.pipePause = null;
      if (state.pipes.length === 0)
        this.pause();

      dest.emit('unpipe', this, unpipeInfo);

      return this;
    };

    Readable.prototype.on = function(ev, fn) {
      const res = Stream.prototype.on.call(this, ev, fn);
      const state = this._readableState;

      if (ev === 'data') {
        state[kState] |= kDataListening;

        state[kState] |= this.listenerCount('readable') > 0 ? kReadableListening : 0;

        if ((state[kState] & (kHasFlowing | kFlowing)) !== kHasFlowing) {
          this.resume();
        }
      } else if (ev === 'readable') {
        if ((state[kState] & (kEndEmitted | kReadableListening)) === 0) {
          state[kState] |= kReadableListening | kNeedReadable | kHasFlowing;
          state[kState] &= ~(kFlowing | kEmittedReadable);
          debug('on readable');
          if (state.length) {
            emitReadable(this);
          } else {
            if ((state[kState] & kEofReadablePending) !== 0) {
              state[kState] &= ~kEofReadablePending;
              emitReadable(this);
            }
            if ((state[kState] & kReading) === 0) {
              process.nextTick(nReadingNextTick, this);
            }
          }
        }
      }

      return res;
    };
    Readable.prototype.addListener = Readable.prototype.on;

    Readable.prototype.removeListener = function(ev, fn) {
      const state = this._readableState;

      const res = Stream.prototype.removeListener.call(this,
                                                       ev, fn);

      if (ev === 'readable') {
        process.nextTick(updateReadableListening, this);
      } else if (ev === 'data' && this.listenerCount('data') === 0) {
        state[kState] &= ~kDataListening;
      }

      return res;
    };
    Readable.prototype.off = Readable.prototype.removeListener;

    Readable.prototype.removeAllListeners = function(ev) {
      const res = Reflect.apply(Stream.prototype.removeAllListeners, this, arguments);

      if (ev === 'readable' || ev === undefined) {
        process.nextTick(updateReadableListening, this);
      }

      return res;
    };

    function updateReadableListening(self) {
      const state = self._readableState;

      if (self.listenerCount('readable') > 0) {
        state[kState] |= kReadableListening;
      } else {
        state[kState] &= ~kReadableListening;
      }

      if ((state[kState] & (kHasPaused | kPaused | kResumeScheduled)) === (kHasPaused | kResumeScheduled)) {
        state[kState] |= kHasFlowing | kFlowing;

      } else if ((state[kState] & kDataListening) !== 0) {
        self.resume();
      } else if ((state[kState] & kReadableListening) === 0) {
        state[kState] &= ~(kHasFlowing | kFlowing);
      }
    }

    function nReadingNextTick(self) {
      debug('readable nexttick read 0');
      self.read(0);
    }

    Readable.prototype.resume = function() {
      const state = this._readableState;
      if ((state[kState] & kDestroyed) !== 0) {
        return this;
      }
      if ((state[kState] & kFlowing) === 0) {
        debug('resume');
        state[kState] |= kHasFlowing;
        if ((state[kState] & kReadableListening) === 0) {
          state[kState] |= kFlowing;
        } else {
          state[kState] &= ~kFlowing;
        }
        resume(this, state);
      }
      state[kState] |= kHasPaused;
      state[kState] &= ~kPaused;
      return this;
    };

    function resume(stream, state) {
      if ((state[kState] & kResumeScheduled) === 0) {
        state[kState] |= kResumeScheduled;
        process.nextTick(resume_, stream, state);
      }
    }

    function resume_(stream, state) {
      debug('resume', (state[kState] & kReading) !== 0);
      if ((state[kState] & kReading) === 0) {
        stream.read(0);
      }

      state[kState] &= ~kResumeScheduled;
      stream.emit('resume');
      flow(stream);
      if ((state[kState] & (kFlowing | kReading)) === kFlowing)
        stream.read(0);
    }

    Readable.prototype.pause = function() {
      const state = this._readableState;
      if ((state[kState] & kDestroyed) !== 0) {
        return this;
      }
      debug('call pause');
      if ((state[kState] & (kHasFlowing | kFlowing)) !== kHasFlowing) {
        debug('pause');
        state[kState] |= kHasFlowing;
        state[kState] &= ~kFlowing;
        this.emit('pause');
      }
      state[kState] |= kHasPaused | kPaused;
      return this;
    };

    function flow(stream) {
      const state = stream._readableState;
      debug('flow');
      if (flowSync(stream, state))
        return;
      while ((state[kState] & kFlowing) !== 0 && stream.read() !== null);
    }

    const kFastFlowNeed = kConstructed | kFlowing | kDataListening;
    const kFastFlowBlock = kObjectMode | kDecoder | kEnded | kDestroyed |
      kErrored | kPaused | kReading | kSync;

    let writeKnownBuffer;

    function deliverFlowChunk(stream, state, chunk) {
      const pipeOnData = state.pipeOnData;
      const events = stream._events;
      if (pipeOnData !== null && events !== undefined && events.data === pipeOnData) {
        writeKnownBuffer ??= require('internal/streams/writable').writeKnownBuffer;
        const dest = state.pipes[0];
        let ret;
        try {
          ret = writeKnownBuffer(dest, chunk);
        } catch (error) {
          dest.destroy(error);
          return;
        }
        if (ret === undefined)
          stream.emit('data', chunk);
        else if (ret === false && state.pipePause !== null)
          state.pipePause();
        return;
      }
      stream.emit('data', chunk);
    }

    function flowSync(stream, state) {
      const bits = state[kState];
      if ((bits & kFastFlowNeed) !== kFastFlowNeed ||
          (bits & kFastFlowBlock) !== 0 ||
          !(state.highWaterMark > 0) ||
          state.fastChunk != null) {
        return false;
      }

      if (state.length !== 0) {
        const buf = state.buffer;
        const idx = state.bufferIndex;
        const chunk = buf[idx];
        if (buf.length !== idx + 1 || chunk == null || chunk.length !== state.length)
          return false;
        buf.length = 0;
        state.bufferIndex = 0;
        state.fastChunk = chunk;
      }

      while ((state[kState] & kFlowing) !== 0) {
        const current = state.fastChunk;
        if (current == null)
          break;
        if ((state[kState] & kFastFlowBlock) !== 0)
          break;

        state.fastChunk = null;
        state.length = 0;
        state[kState] |= kReading | kSync | kFastPush;
        try {
          stream._read(state.highWaterMark);
        } catch (err) {
          state[kState] &= ~(kSync | kFastPush);
          errorOrDestroy(stream, err);
          break;
        }
        state[kState] &= ~(kSync | kFastPush);

        if ((state[kState] & (kErrorEmitted | kCloseEmitted)) === 0) {
          state[kState] |= kDataEmitted;
          deliverFlowChunk(stream, state, current);
        }

        if (state.fastChunk == null && state.length !== 0)
          break;
      }

      if (state.fastChunk != null)
        materializeFastChunk(state);

      while ((state[kState] & kFlowing) !== 0 && stream.read() !== null);
      return true;
    }

    function materializeFastChunk(state) {
      const chunk = state.fastChunk;
      if (chunk == null)
        return;
      state.fastChunk = null;
      if (state.bufferIndex > 0) {
        state.buffer[--state.bufferIndex] = chunk;
      } else {
        state.buffer.unshift(chunk);
      }
    }

    Readable.prototype.wrap = function(stream) {
      let paused = false;

      stream.on('data', (chunk) => {
        if (!this.push(chunk) && stream.pause) {
          paused = true;
          stream.pause();
        }
      });

      stream.on('end', () => {
        this.push(null);
      });

      stream.on('error', (err) => {
        errorOrDestroy(this, err);
      });

      stream.on('close', () => {
        this.destroy();
      });

      stream.on('destroy', () => {
        this.destroy();
      });

      this._read = () => {
        if (paused && stream.resume) {
          paused = false;
          stream.resume();
        }
      };

      const streamKeys = Object.keys(stream);
      for (let j = 0; j < streamKeys.length; j++) {
        const i = streamKeys[j];
        if (this[i] === undefined && typeof stream[i] === 'function') {
          this[i] = stream[i].bind(stream);
        }
      }

      return this;
    };

    Readable.prototype[SymbolAsyncIterator] = function() {
      return streamToAsyncIterator(this);
    };

    Readable.prototype.iterator = function(options) {
      if (options !== undefined) {
        validateObject(options, 'options');
      }
      return streamToAsyncIterator(this, options);
    };

    function streamToAsyncIterator(stream, options) {
      if (typeof stream.read !== 'function') {
        stream = Readable.wrap(stream, { objectMode: true });
      }

      const iter = createAsyncIterator(stream, options);
      iter.stream = stream;
      return iter;
    }

    function createAsyncIterator(stream, options) {
      let callback = nop;
      let error;            // undefined: active, null: ended cleanly, else: Error
      let started = false;
      let completed = false;
      let inFlight = false; // An asynchronous request is outstanding
      let queue = null;     // Requests received while inFlight
      let draining = false;
      let cleanup;

      function wakeup(resolve) {
        if (this === stream) {
          callback();
          callback = nop;
        } else {
          callback = resolve;
        }
      }

      function start() {
        started = true;

        stream.on('readable', wakeup);

        cleanup = eos(stream, { writable: false }, (err) => {
          error = err ? aggregateTwoErrors(error, err) : null;
          callback();
          callback = nop;
        });
      }

      function finalize() {
        completed = true;

        if (
          (error || options?.destroyOnReturn !== false) &&
          (error === undefined || stream._readableState.autoDestroy)
        ) {
          destroyImpl.destroyer(stream, null);
        } else {
          stream.off('readable', wakeup);
          cleanup();
        }
      }

      function settleError(err, reject) {
        error = aggregateTwoErrors(error, err);
        finalize();
        reject(error);
      }

      function drain() {
        if (draining) {
          return;
        }
        draining = true;
        try {
          while (!inFlight && !queue.isEmpty()) {
            const req = queue.shift();
            if (req.type === 'next') {
              processNext(req.resolve, req.reject);
            } else if (req.type === 'return') {
              processReturn(req.value, req.resolve);
            } else {
              processThrow(req.value, req.reject);
            }
          }
        } finally {
          draining = false;
        }
      }

      function onChunkFulfilled(value) {
        inFlight = false;
        if (queue !== null) drain();
        return { done: false, value };
      }

      function onChunkRejected(err) {
        inFlight = false;
        error = aggregateTwoErrors(error, err);
        finalize();
        if (queue !== null) drain();
        throw error;
      }

      function pump(resolve, reject) {
        const chunk = stream.destroyed ? null : stream.read();
        if (chunk !== null) {
          const then = chunk?.then;
          if (typeof then === 'function') {
            FunctionPrototypeCall(then, chunk, (value) => {
              inFlight = false;
              resolve({ done: false, value });
              if (queue !== null) drain();
            }, (err) => {
              inFlight = false;
              settleError(err, reject);
              if (queue !== null) drain();
            });
            return;
          }
          inFlight = false;
          resolve({ done: false, value: chunk });
          if (queue !== null) drain();
        } else if (error) {
          inFlight = false;
          settleError(error, reject);
          if (queue !== null) drain();
        } else if (error === null) {
          inFlight = false;
          finalize();
          resolve({ done: true, value: undefined });
          if (queue !== null) drain();
        } else {
          PromisePrototypeThen(new Promise(wakeup), () => pump(resolve, reject));
        }
      }

      function processNext(resolve, reject) {
        if (completed) {
          resolve({ done: true, value: undefined });
          return;
        }
        if (!started) start();
        inFlight = true;
        pump(resolve, reject);
      }

      function processReturn(value, resolve) {
        if (!completed) {
          if (started) {
            finalize();
          } else {
            completed = true;
          }
        }
        resolve({ done: true, value });
      }

      function processThrow(err, reject) {
        if (completed || !started) {
          completed = true;
          reject(err);
          return;
        }
        settleError(err, reject);
      }

      return {
        __proto__: AsyncIteratorPrototype,
        next() {
          if (!inFlight && !completed) {
            if (!started) start();
            const chunk = stream.destroyed ? null : stream.read();
            if (chunk !== null) {
              const then = chunk?.then;
              if (typeof then === 'function') {
                inFlight = true;
                return FunctionPrototypeCall(
                  then, chunk, onChunkFulfilled, onChunkRejected);
              }
              return Promise.resolve({ done: false, value: chunk });
            }
            if (error) {
              finalize();
              return Promise.reject(error);
            }
            if (error === null) {
              finalize();
              return Promise.resolve({ done: true, value: undefined });
            }
            inFlight = true;
            return new Promise((resolve, reject) => {
              PromisePrototypeThen(new Promise(wakeup), () => pump(resolve, reject));
            });
          }
          return new Promise((resolve, reject) => {
            if (inFlight) {
              queue ??= new FixedQueue();
              queue.push({ __proto__: null, type: 'next', value: undefined, resolve, reject });
            } else {
              resolve({ done: true, value: undefined });
            }
          });
        },
        return(value) {
          return new Promise((resolve, reject) => {
            if (inFlight) {
              queue ??= new FixedQueue();
              queue.push({ __proto__: null, type: 'return', value, resolve, reject });
            } else {
              processReturn(value, resolve);
            }
          });
        },
        throw(err) {
          return new Promise((resolve, reject) => {
            if (inFlight) {
              queue ??= new FixedQueue();
              queue.push({ __proto__: null, type: 'throw', value: err, resolve, reject });
            } else {
              processThrow(err, reject);
            }
          });
        },
      };
    }

    let composeImpl;

    Readable.prototype.compose = function compose(stream, options) {
      if (options != null) {
        validateObject(options, 'options');
      }
      if (options?.signal != null) {
        validateAbortSignal(options.signal, 'options.signal');
      }

      composeImpl ??= require('internal/streams/compose');
      const composedStream = composeImpl(this, stream);

      if (options?.signal) {
        addAbortSignalNoValidate(
          options.signal,
          composedStream,
        );
      }

      return composedStream;
    };

    Object.defineProperties(Readable.prototype, {
      readable: {
        __proto__: null,
        get() {
          const r = this._readableState;
          return !!r && r.readable !== false && !r.destroyed && !r.errorEmitted &&
            !r.endEmitted;
        },
        set(val) {
          if (this._readableState) {
            this._readableState.readable = !!val;
          }
        },
      },

      readableDidRead: {
        __proto__: null,
        enumerable: false,
        get: function() {
          return this._readableState.dataEmitted;
        },
      },

      readableAborted: {
        __proto__: null,
        enumerable: false,
        get: function() {
          return !!(
            this._readableState.readable !== false &&
            (this._readableState.destroyed || this._readableState.errored) &&
            !this._readableState.endEmitted
          );
        },
      },

      readableHighWaterMark: {
        __proto__: null,
        enumerable: false,
        get: function() {
          return this._readableState.highWaterMark;
        },
      },

      readableBuffer: {
        __proto__: null,
        enumerable: false,
        get: function() {
          const state = this._readableState;
          if (state == null)
            return undefined;
          if (state.fastChunk == null)
            return state.buffer;
          if (state.bufferIndex === state.buffer.length)
            return [state.fastChunk];
          const out = state.buffer.slice(state.bufferIndex);
          out.push(state.fastChunk);
          return out;
        },
      },

      readableFlowing: {
        __proto__: null,
        enumerable: false,
        get: function() {
          return this._readableState.flowing;
        },
        set: function(state) {
          if (this._readableState) {
            this._readableState.flowing = state;
          }
        },
      },

      readableLength: {
        __proto__: null,
        enumerable: false,
        get() {
          return this._readableState.length;
        },
      },

      readableObjectMode: {
        __proto__: null,
        enumerable: false,
        get() {
          return this._readableState ? this._readableState.objectMode : false;
        },
      },

      readableEncoding: {
        __proto__: null,
        enumerable: false,
        get() {
          return this._readableState ? this._readableState.encoding : null;
        },
      },

      errored: {
        __proto__: null,
        enumerable: false,
        get() {
          return this._readableState ? this._readableState.errored : null;
        },
      },

      closed: {
        __proto__: null,
        get() {
          return this._readableState ? this._readableState.closed : false;
        },
      },

      destroyed: {
        __proto__: null,
        enumerable: false,
        get() {
          return this._readableState ? this._readableState.destroyed : false;
        },
        set(value) {
          if (!this._readableState) {
            return;
          }

          this._readableState.destroyed = value;
        },
      },

      readableEnded: {
        __proto__: null,
        enumerable: false,
        get() {
          return this._readableState ? this._readableState.endEmitted : false;
        },
      },

    });

    Object.defineProperties(ReadableState.prototype, {
      pipesCount: {
        __proto__: null,
        get() {
          return this.pipes.length;
        },
      },

      paused: {
        __proto__: null,
        get() {
          return (this[kState] & kPaused) !== 0;
        },
        set(value) {
          this[kState] |= kHasPaused;
          if (value) {
            this[kState] |= kPaused;
          } else {
            this[kState] &= ~kPaused;
          }
        },
      },
    });

    Readable._fromList = fromList;

    function fromList(n, state) {
      const stateLength = state.length;

      if (stateLength === 0)
        return null;

      let idx = state.bufferIndex;
      let ret;

      const buf = state.buffer;
      const len = buf.length;

      if ((state[kState] & kObjectMode) !== 0) {
        ret = buf[idx];
        buf[idx++] = null;
      } else if (!n || n >= stateLength) {
        if ((state[kState] & kDecoder) !== 0) {
          ret = '';
          while (idx < len) {
            ret += buf[idx];
            buf[idx++] = null;
          }
        } else if (len - idx === 0) {
          ret = new FastBuffer();
        } else if (len - idx === 1) {
          ret = buf[idx];
          buf[idx++] = null;
        } else {
          ret = Buffer.allocUnsafe(stateLength);

          let i = 0;
          while (idx < len) {
            const data = buf[idx];
            ret.set(data, i);
            i += data.length;
            buf[idx++] = null;
          }
        }
      } else {
        const first = buf[idx];
        const firstLength = first.length;
        if (n < firstLength) {
          ret = first.slice(0, n);
          buf[idx] = first.slice(n);
        } else if (n === firstLength) {
          ret = first;
          buf[idx++] = null;
        } else if ((state[kState] & kDecoder) !== 0) {
          ret = '';
          while (idx < len) {
            const str = buf[idx];
            const strLength = str.length;
            if (n > strLength) {
              ret += str;
              n -= strLength;
              buf[idx++] = null;
            } else {
              if (n === strLength) {
                ret += str;
                buf[idx++] = null;
              } else {
                ret += str.slice(0, n);
                buf[idx] = str.slice(n);
              }
              break;
            }
          }
        } else {
          ret = Buffer.allocUnsafe(n);

          const retLen = n;
          while (idx < len) {
            const data = buf[idx];
            const dataLength = data.length;
            if (n > dataLength) {
              ret.set(data, retLen - n);
              n -= dataLength;
              buf[idx++] = null;
            } else {
              if (n === dataLength) {
                ret.set(data, retLen - n);
                buf[idx++] = null;
              } else {
                ret.set(new FastBuffer(data.buffer, data.byteOffset, n), retLen - n);
                buf[idx] = new FastBuffer(data.buffer, data.byteOffset + n, dataLength - n);
              }
              break;
            }
          }
        }
      }

      if (idx === len) {
        state.buffer.length = 0;
        state.bufferIndex = 0;
      } else if (idx > 1024) {
        state.buffer.splice(0, idx);
        state.bufferIndex = 0;
      } else {
        state.bufferIndex = idx;
      }

      return ret;
    }

    function endReadable(stream) {
      const state = stream._readableState;

      debug('endReadable');
      if ((state[kState] & (kEndEmitted | kEndScheduled)) === 0) {
        state[kState] |= kEnded | kEndScheduled;
        process.nextTick(endReadableNT, state, stream);
      }
    }

    function endReadableNT(state, stream) {
      debug('endReadableNT');

      state[kState] &= ~kEndScheduled;

      if ((state[kState] & (kErrored | kCloseEmitted | kEndEmitted)) === 0 && state.length === 0) {
        state[kState] |= kEndEmitted;
        stream.emit('end');

        if (stream.writable && stream.allowHalfOpen === false) {
          process.nextTick(endWritableNT, stream);
        } else if (state.autoDestroy) {
          const wState = stream._writableState;
          const autoDestroy = !wState || (
            wState.autoDestroy &&
            (wState.finished || wState.writable === false)
          );

          if (autoDestroy) {
            stream.destroy();
          }
        }
      }
    }

    function endWritableNT(stream) {
      const writable = stream.writable && !stream.writableEnded &&
        !stream.destroyed;
      if (writable) {
        stream.end();
      }
    }

    Readable.from = function(iterable, opts) {
      return from(Readable, iterable, opts);
    };

    let webStreamsAdapters;

    function lazyWebStreams() {
      if (webStreamsAdapters === undefined)
        webStreamsAdapters = require('internal/webstreams/adapters');
      return webStreamsAdapters;
    }

    Readable.fromWeb = function(readableStream, options) {
      return lazyWebStreams().newStreamReadableFromReadableStream(
        readableStream,
        options);
    };

    Readable.toWeb = function(streamReadable, options) {
      return lazyWebStreams().newReadableStreamFromStreamReadable(
        streamReadable,
        options);
    };

    Readable.wrap = function(src, options) {
      return new Readable({
        objectMode: src.readableObjectMode ?? src.objectMode ?? true,
        ...options,
        destroy(err, callback) {
          destroyImpl.destroyer(src, err);
          callback(err);
        },
      }).wrap(src);
    };

    {
      const toAsyncStreamable = Symbol.for('Stream.toAsyncStreamable');
      let createBatchedAsyncIterator;
      let normalizeBatch;
      let kValidatedSource;

      Readable.prototype[toAsyncStreamable] = function() {
        if (createBatchedAsyncIterator === undefined) {
          if (!getOptionValue('--experimental-stream-iter')) {
            throw new ERR_STREAM_ITER_MISSING_FLAG();
          }
          ({
            createBatchedAsyncIterator,
            normalizeBatch,
          } = require('internal/streams/iter/classic'));
          ({ kValidatedSource } = require('internal/streams/iter/types'));
        }
        const state = this._readableState;
        const normalize = (state.objectMode || state.encoding) ?
          normalizeBatch : null;
        const iter = createBatchedAsyncIterator(this, normalize);
        iter[kValidatedSource] = true;
        iter.stream = this;
        return iter;
      };
    }
  },
  \"internal/streams/state\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      MathFloor,
      NumberIsInteger,
    } = primordials;
    const { validateInteger } = require('internal/validators');

    const { ERR_INVALID_ARG_VALUE } = require('internal/errors').codes;

    let defaultHighWaterMarkBytes = process.platform === 'win32' ? 16 * 1024 : 64 * 1024;
    let defaultHighWaterMarkObjectMode = 16;

    function highWaterMarkFrom(options, isDuplex, duplexKey) {
      return options.highWaterMark != null ? options.highWaterMark :
        isDuplex ? options[duplexKey] : null;
    }

    function getDefaultHighWaterMark(objectMode) {
      return objectMode ? defaultHighWaterMarkObjectMode : defaultHighWaterMarkBytes;
    }

    function setDefaultHighWaterMark(objectMode, value) {
      validateInteger(value, 'value', 0);
      if (objectMode) {
        defaultHighWaterMarkObjectMode = value;
      } else {
        defaultHighWaterMarkBytes = value;
      }
    }

    function getHighWaterMark(state, options, duplexKey, isDuplex) {
      const hwm = highWaterMarkFrom(options, isDuplex, duplexKey);
      if (hwm != null) {
        if (!Number.isInteger(hwm) || hwm < 0) {
          const name = isDuplex ? `options.${duplexKey}` : 'options.highWaterMark';
          throw new ERR_INVALID_ARG_VALUE(name, hwm);
        }
        return Math.floor(hwm);
      }

      return getDefaultHighWaterMark(state.objectMode);
    }

    module.exports = {
      getHighWaterMark,
      getDefaultHighWaterMark,
      setDefaultHighWaterMark,
    };
  },
  \"internal/streams/transform\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectSetPrototypeOf,
      Symbol,
    } = primordials;

    module.exports = Transform;
    const {
      ERR_METHOD_NOT_IMPLEMENTED,
    } = require('internal/errors').codes;
    const Duplex = require('internal/streams/duplex');
    const { getHighWaterMark } = require('internal/streams/state');
    Object.setPrototypeOf(Transform.prototype, Duplex.prototype);
    Object.setPrototypeOf(Transform, Duplex);

    const kCallback = Symbol('kCallback');

    function Transform(options) {
      if (!(this instanceof Transform))
        return new Transform(options);

      const readableHighWaterMark = options ? getHighWaterMark(this, options, 'readableHighWaterMark', true) : null;
      if (readableHighWaterMark === 0) {
        options = {
          ...options,
          highWaterMark: null,
          readableHighWaterMark,
          writableHighWaterMark: options.writableHighWaterMark || 0,
        };
      }

      Duplex.call(this, options);

      this._readableState.sync = false;

      this[kCallback] = null;

      if (options) {
        if (typeof options.transform === 'function')
          this._transform = options.transform;

        if (typeof options.flush === 'function')
          this._flush = options.flush;
      }

      this.on('prefinish', prefinish);
    }

    function final(cb) {
      if (typeof this._flush === 'function' && !this.destroyed) {
        this._flush((er, data) => {
          if (er) {
            if (cb) {
              cb(er);
            } else {
              this.destroy(er);
            }
            return;
          }

          if (data != null) {
            this.push(data);
          }
          this.push(null);
          if (cb) {
            cb();
          }
        });
      } else {
        this.push(null);
        if (cb) {
          cb();
        }
      }
    }

    function prefinish() {
      if (this._final !== final) {
        final.call(this);
      }
    }

    Transform.prototype._final = final;

    Transform.prototype._transform = function(chunk, encoding, callback) {
      throw new ERR_METHOD_NOT_IMPLEMENTED('_transform()');
    };

    Transform.prototype._write = function(chunk, encoding, callback) {
      const rState = this._readableState;
      const wState = this._writableState;
      const length = rState.length;

      this._transform(chunk, encoding, (err, val) => {
        if (err) {
          callback(err);
          return;
        }

        if (val != null) {
          this.push(val);
        }

        if (rState.ended) {
          process.nextTick(callback);
        } else if (
          wState.ended || // Backwards compat.
          length === rState.length || // Backwards compat.
          rState.length < rState.highWaterMark
        ) {
          callback();
        } else {
          this[kCallback] = callback;
        }
      });
    };

    Transform.prototype._read = function() {
      if (this[kCallback]) {
        const callback = this[kCallback];
        this[kCallback] = null;
        callback();
      }
    };
  },
  \"internal/streams/utils\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      Symbol,
      SymbolAsyncIterator,
      SymbolFor,
      SymbolIterator,
    } = primordials;

    const kIsDestroyed = Symbol.for('nodejs.stream.destroyed');
    const kIsErrored = Symbol.for('nodejs.stream.errored');
    const kIsReadable = Symbol.for('nodejs.stream.readable');
    const kIsWritable = Symbol.for('nodejs.stream.writable');
    const kIsDisturbed = Symbol.for('nodejs.stream.disturbed');

    const kOnConstructed = Symbol('kOnConstructed');

    const kIsClosedPromise = Symbol.for('nodejs.webstream.isClosedPromise');
    const kControllerErrorFunction = Symbol.for('nodejs.webstream.controllerErrorFunction');

    const kState = Symbol('kState');
    const kObjectMode = 1 << 0;
    const kErrorEmitted = 1 << 1;
    const kAutoDestroy = 1 << 2;
    const kEmitClose = 1 << 3;
    const kDestroyed = 1 << 4;
    const kClosed = 1 << 5;
    const kCloseEmitted = 1 << 6;
    const kErrored = 1 << 7;
    const kConstructed = 1 << 8;

    function isReadableNodeStream(obj, strict = false) {
      return !!(
        obj &&
        typeof obj.pipe === 'function' &&
        typeof obj.on === 'function' &&
        (
          !strict ||
          (typeof obj.pause === 'function' && typeof obj.resume === 'function')
        ) &&
        (!obj._writableState || obj._readableState?.readable !== false) && // Duplex
        (!obj._writableState || obj._readableState) // Writable has .pipe.
      );
    }

    function isWritableNodeStream(obj) {
      return !!(
        obj &&
        typeof obj.write === 'function' &&
        typeof obj.on === 'function' &&
        (!obj._readableState || obj._writableState?.writable !== false) // Duplex
      );
    }

    function isDuplexNodeStream(obj) {
      return !!(
        obj &&
        (typeof obj.pipe === 'function' && obj._readableState) &&
        typeof obj.on === 'function' &&
        typeof obj.write === 'function'
      );
    }

    function isNodeStream(obj) {
      return (
        obj &&
        (
          obj._readableState ||
          obj._writableState ||
          (typeof obj.write === 'function' && typeof obj.on === 'function') ||
          (typeof obj.pipe === 'function' && typeof obj.on === 'function')
        )
      );
    }

    function isReadableStream(obj) {
      return !!(
        obj &&
        !isNodeStream(obj) &&
        typeof obj.pipeThrough === 'function' &&
        typeof obj.getReader === 'function' &&
        typeof obj.cancel === 'function'
      );
    }

    function isWritableStream(obj) {
      return !!(
        obj &&
        !isNodeStream(obj) &&
        typeof obj.getWriter === 'function' &&
        typeof obj.abort === 'function'
      );
    }

    function isTransformStream(obj) {
      return !!(
        obj &&
        !isNodeStream(obj) &&
        typeof obj.readable === 'object' &&
        typeof obj.writable === 'object'
      );
    }

    function isWebStream(obj) {
      return isReadableStream(obj) || isWritableStream(obj) || isTransformStream(obj);
    }

    function isIterable(obj, isAsync) {
      if (obj == null) return false;
      if (isAsync === true) return typeof obj[SymbolAsyncIterator] === 'function';
      if (isAsync === false) return typeof obj[SymbolIterator] === 'function';
      return typeof obj[SymbolAsyncIterator] === 'function' ||
        typeof obj[SymbolIterator] === 'function';
    }

    function isDestroyed(stream) {
      if (!isNodeStream(stream)) return null;
      const wState = stream._writableState;
      const rState = stream._readableState;
      const state = wState || rState;
      return !!(stream.destroyed || stream[kIsDestroyed] || state?.destroyed);
    }

    function isWritableEnded(stream) {
      if (!isWritableNodeStream(stream)) return null;
      if (stream.writableEnded === true) return true;
      const wState = stream._writableState;
      if (wState?.errored) return false;
      if (typeof wState?.ended !== 'boolean') return null;
      return wState.ended;
    }

    function isWritableFinished(stream, strict) {
      if (!isWritableNodeStream(stream)) return null;
      if (stream.writableFinished === true) return true;
      const wState = stream._writableState;
      if (wState?.errored) return false;
      if (typeof wState?.finished !== 'boolean') return null;
      return !!(
        wState.finished ||
        (strict === false && wState.ended === true && wState.length === 0)
      );
    }

    function isReadableEnded(stream) {
      if (!isReadableNodeStream(stream)) return null;
      if (stream.readableEnded === true) return true;
      const rState = stream._readableState;
      if (!rState || rState.errored) return false;
      if (typeof rState?.ended !== 'boolean') return null;
      return rState.ended;
    }

    function isReadableFinished(stream, strict) {
      if (!isReadableNodeStream(stream)) return null;
      const rState = stream._readableState;
      if (rState?.errored) return false;
      if (typeof rState?.endEmitted !== 'boolean') return null;
      return !!(
        rState.endEmitted ||
        (strict === false && rState.ended === true && rState.length === 0)
      );
    }

    function isReadable(stream) {
      if (stream && stream[kIsReadable] != null) return stream[kIsReadable];
      if (typeof stream?.readable !== 'boolean') return null;
      if (isDestroyed(stream)) return false;
      return isReadableNodeStream(stream) &&
        stream.readable &&
        !isReadableFinished(stream);
    }

    function isWritable(stream) {
      if (stream && stream[kIsWritable] != null) return stream[kIsWritable];
      if (typeof stream?.writable !== 'boolean') return null;
      if (isDestroyed(stream)) return false;
      return isWritableNodeStream(stream) &&
        stream.writable &&
        !isWritableEnded(stream);
    }

    function isFinished(stream, opts) {
      if (!isNodeStream(stream)) {
        return null;
      }

      if (isDestroyed(stream)) {
        return true;
      }

      if (opts?.readable !== false && isReadable(stream)) {
        return false;
      }

      if (opts?.writable !== false && isWritable(stream)) {
        return false;
      }

      return true;
    }

    function isWritableErrored(stream) {
      if (!isNodeStream(stream)) {
        return null;
      }

      if (stream.writableErrored) {
        return stream.writableErrored;
      }

      return stream._writableState?.errored ?? null;
    }

    function isReadableErrored(stream) {
      if (!isNodeStream(stream)) {
        return null;
      }

      if (stream.readableErrored) {
        return stream.readableErrored;
      }

      return stream._readableState?.errored ?? null;
    }

    function isClosed(stream) {
      if (!isNodeStream(stream)) {
        return null;
      }

      if (typeof stream.closed === 'boolean') {
        return stream.closed;
      }

      const wState = stream._writableState;
      const rState = stream._readableState;

      if (
        typeof wState?.closed === 'boolean' ||
        typeof rState?.closed === 'boolean'
      ) {
        return wState?.closed || rState?.closed;
      }

      if (typeof stream._closed === 'boolean' && isOutgoingMessage(stream)) {
        return stream._closed;
      }

      return null;
    }

    function isOutgoingMessage(stream) {
      return (
        typeof stream._closed === 'boolean' &&
        typeof stream._defaultKeepAlive === 'boolean' &&
        typeof stream._removedConnection === 'boolean' &&
        typeof stream._removedContLen === 'boolean'
      );
    }

    function isServerResponse(stream) {
      return (
        typeof stream._sent100 === 'boolean' &&
        isOutgoingMessage(stream)
      );
    }

    function isServerRequest(stream) {
      return (
        typeof stream._consuming === 'boolean' &&
        typeof stream._dumped === 'boolean' &&
        stream.req?.upgradeOrConnect === undefined
      );
    }

    function willEmitClose(stream) {
      if (!isNodeStream(stream)) return null;

      const wState = stream._writableState;
      const rState = stream._readableState;
      const state = wState || rState;

      return (!state && isServerResponse(stream)) || !!(
        state?.autoDestroy &&
        state.emitClose &&
        state.closed === false
      );
    }

    function isDisturbed(stream) {
      return !!(stream && (
        stream[kIsDisturbed] ??
        (stream.readableDidRead || stream.readableAborted)
      ));
    }

    function isErrored(stream) {
      return !!(stream && (
        stream[kIsErrored] ??
        stream.readableErrored ??
        stream.writableErrored ??
        stream._readableState?.errorEmitted ??
        stream._writableState?.errorEmitted ??
        stream._readableState?.errored ??
        stream._writableState?.errored
      ));
    }

    module.exports = {
      kOnConstructed,
      isDestroyed,
      kIsDestroyed,
      isDisturbed,
      kIsDisturbed,
      isErrored,
      kIsErrored,
      isReadable,
      kIsReadable,
      kIsClosedPromise,
      kControllerErrorFunction,
      kIsWritable,
      isClosed,
      isDuplexNodeStream,
      isFinished,
      isIterable,
      isReadableNodeStream,
      isReadableStream,
      isReadableEnded,
      isReadableFinished,
      isReadableErrored,
      isNodeStream,
      isWebStream,
      isWritable,
      isWritableNodeStream,
      isWritableStream,
      isWritableEnded,
      isWritableFinished,
      isWritableErrored,
      isServerRequest,
      isServerResponse,
      willEmitClose,
      isTransformStream,
      kState,
      kObjectMode,
      kErrorEmitted,
      kAutoDestroy,
      kEmitClose,
      kDestroyed,
      kClosed,
      kCloseEmitted,
      kErrored,
      kConstructed,
    };
  },
  \"internal/streams/writable\": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypeSlice,
      Error,
      FunctionPrototypeSymbolHasInstance,
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectSetPrototypeOf,
      Promise,
      StringPrototypeToLowerCase,
      Symbol,
      SymbolAsyncDispose,
      SymbolHasInstance,
    } = primordials;

    module.exports = Writable;
    Writable.WritableState = WritableState;

    const EE = require('events');
    const Stream = require('internal/streams/legacy').Stream;
    const { Buffer } = require('buffer');
    const destroyImpl = require('internal/streams/destroy');
    const { eos } = require('internal/streams/end-of-stream');
    const {
      addAbortSignal,
    } = require('internal/streams/add-abort-signal');

    const {
      getHighWaterMark,
      getDefaultHighWaterMark,
    } = require('internal/streams/state');
    const {
      AbortError,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_METHOD_NOT_IMPLEMENTED,
        ERR_MULTIPLE_CALLBACK,
        ERR_STREAM_ALREADY_FINISHED,
        ERR_STREAM_CANNOT_PIPE,
        ERR_STREAM_DESTROYED,
        ERR_STREAM_NULL_VALUES,
        ERR_STREAM_WRITE_AFTER_END,
        ERR_UNKNOWN_ENCODING,
      },
    } = require('internal/errors');
    const {
      kState,
      kObjectMode,
      kErrorEmitted,
      kAutoDestroy,
      kEmitClose,
      kDestroyed,
      kClosed,
      kCloseEmitted,
      kErrored,
      kConstructed,
      kOnConstructed,
    } = require('internal/streams/utils');

    const { assignFunctionName } = require('internal/util');

    const { errorOrDestroy } = destroyImpl;

    Object.setPrototypeOf(Writable.prototype, Stream.prototype);
    Object.setPrototypeOf(Writable, Stream);

    function nop() {}

    const kOnFinishedValue = Symbol('kOnFinishedValue');
    const kErroredValue = Symbol('kErroredValue');
    const kDefaultEncodingValue = Symbol('kDefaultEncodingValue');
    const kWriteCbValue = Symbol('kWriteCbValue');
    const kAfterWriteTickInfoValue = Symbol('kAfterWriteTickInfoValue');
    const kBufferedValue = Symbol('kBufferedValue');

    const kSync = 1 << 9;
    const kFinalCalled = 1 << 10;
    const kNeedDrain = 1 << 11;
    const kEnding = 1 << 12;
    const kFinished = 1 << 13;
    const kDecodeStrings = 1 << 14;
    const kWriting = 1 << 15;
    const kBufferProcessing = 1 << 16;
    const kPrefinished = 1 << 17;
    const kAllBuffers = 1 << 18;
    const kAllNoop = 1 << 19;
    const kOnFinished = 1 << 20;
    const kHasWritable = 1 << 21;
    const kWritable = 1 << 22;
    const kCorked = 1 << 23;
    const kDefaultUTF8Encoding = 1 << 24;
    const kWriteCb = 1 << 25;
    const kExpectWriteCb = 1 << 26;
    const kAfterWriteTickInfo = 1 << 27;
    const kAfterWritePending = 1 << 28;
    const kBuffered = 1 << 29;
    const kEnded = 1 << 30;

    function makeBitMapDescriptor(bit) {
      return {
        enumerable: false,
        get() { return (this[kState] & bit) !== 0; },
        set(value) {
          if (value) this[kState] |= bit;
          else this[kState] &= ~bit;
        },
      };
    }
    Object.defineProperties(WritableState.prototype, {
      objectMode: makeBitMapDescriptor(kObjectMode),

      finalCalled: makeBitMapDescriptor(kFinalCalled),

      needDrain: makeBitMapDescriptor(kNeedDrain),

      ending: makeBitMapDescriptor(kEnding),

      ended: makeBitMapDescriptor(kEnded),

      finished: makeBitMapDescriptor(kFinished),

      destroyed: makeBitMapDescriptor(kDestroyed),

      decodeStrings: makeBitMapDescriptor(kDecodeStrings),

      writing: makeBitMapDescriptor(kWriting),

      sync: makeBitMapDescriptor(kSync),

      bufferProcessing: makeBitMapDescriptor(kBufferProcessing),

      constructed: makeBitMapDescriptor(kConstructed),

      prefinished: makeBitMapDescriptor(kPrefinished),

      errorEmitted: makeBitMapDescriptor(kErrorEmitted),

      emitClose: makeBitMapDescriptor(kEmitClose),

      autoDestroy: makeBitMapDescriptor(kAutoDestroy),

      closed: makeBitMapDescriptor(kClosed),

      closeEmitted: makeBitMapDescriptor(kCloseEmitted),

      allBuffers: makeBitMapDescriptor(kAllBuffers),
      allNoop: makeBitMapDescriptor(kAllNoop),

      errored: {
        __proto__: null,
        enumerable: false,
        get() { return (this[kState] & kErrored) !== 0 ? this[kErroredValue] : null; },
        set(value) {
          if (value) {
            this[kErroredValue] = value;
            this[kState] |= kErrored;
          } else {
            this[kState] &= ~kErrored;
          }
        },
      },

      writable: {
        __proto__: null,
        enumerable: false,
        get() { return (this[kState] & kHasWritable) !== 0 ? (this[kState] & kWritable) !== 0 : undefined; },
        set(value) {
          if (value == null) {
            this[kState] &= ~(kHasWritable | kWritable);
          } else if (value) {
            this[kState] |= (kHasWritable | kWritable);
          } else {
            this[kState] |= kHasWritable;
            this[kState] &= ~kWritable;
          }
        },
      },

      defaultEncoding: {
        __proto__: null,
        enumerable: false,
        get() { return (this[kState] & kDefaultUTF8Encoding) !== 0 ? 'utf8' : this[kDefaultEncodingValue]; },
        set(value) {
          if (value === 'utf8' || value === 'utf-8') {
            this[kState] |= kDefaultUTF8Encoding;
          } else {
            this[kState] &= ~kDefaultUTF8Encoding;
            this[kDefaultEncodingValue] = value;
          }
        },
      },

      writecb: {
        __proto__: null,
        enumerable: false,
        get() { return (this[kState] & kWriteCb) !== 0 ? this[kWriteCbValue] : nop; },
        set(value) {
          this[kWriteCbValue] = value;
          if (value) {
            this[kState] |= kWriteCb;
          } else {
            this[kState] &= ~kWriteCb;
          }
        },
      },

      afterWriteTickInfo: {
        __proto__: null,
        enumerable: false,
        get() { return (this[kState] & kAfterWriteTickInfo) !== 0 ? this[kAfterWriteTickInfoValue] : null; },
        set(value) {
          this[kAfterWriteTickInfoValue] = value;
          if (value) {
            this[kState] |= kAfterWriteTickInfo;
          } else {
            this[kState] &= ~kAfterWriteTickInfo;
          }
        },
      },

      buffered: {
        __proto__: null,
        enumerable: false,
        get() { return (this[kState] & kBuffered) !== 0 ? this[kBufferedValue] : []; },
        set(value) {
          this[kBufferedValue] = value;
          if (value) {
            this[kState] |= kBuffered;
          } else {
            this[kState] &= ~kBuffered;
          }
        },
      },

    });

    function WritableState(options, stream, isDuplex) {
      this[kState] = kSync | kConstructed | kEmitClose | kAutoDestroy;

      if (options?.objectMode)
        this[kState] |= kObjectMode;

      if (isDuplex && options?.writableObjectMode)
        this[kState] |= kObjectMode;

      this.highWaterMark = options ?
        getHighWaterMark(this, options, 'writableHighWaterMark', isDuplex) :
        getDefaultHighWaterMark(false);

      if (!options || options.decodeStrings !== false) this[kState] |= kDecodeStrings;

      if (options && options.emitClose === false) this[kState] &= ~kEmitClose;

      if (options && options.autoDestroy === false) this[kState] &= ~kAutoDestroy;

      const defaultEncoding = options ? options.defaultEncoding : null;
      if (defaultEncoding == null || defaultEncoding === 'utf8' || defaultEncoding === 'utf-8') {
        this[kState] |= kDefaultUTF8Encoding;
      } else if (Buffer.isEncoding(defaultEncoding)) {
        this[kState] &= ~kDefaultUTF8Encoding;
        this[kDefaultEncodingValue] = defaultEncoding;
      } else {
        throw new ERR_UNKNOWN_ENCODING(defaultEncoding);
      }

      this.length = 0;

      this.corked = 0;

      this.onwrite = (er) => onwrite(stream, er);

      this.writelen = 0;

      resetBuffer(this);

      this.pendingcb = 0;
    }

    function resetBuffer(state) {
      state[kBufferedValue] = null;
      state.bufferedIndex = 0;
      state[kState] |= kAllBuffers | kAllNoop;
      state[kState] &= ~kBuffered;
    }

    WritableState.prototype.getBuffer = function getBuffer() {
      return (this[kState] & kBuffered) === 0 ? [] : this.buffered.slice(this.bufferedIndex);
    };

    Object.defineProperty(WritableState.prototype, 'bufferedRequestCount', {
      __proto__: null,
      get() {
        return (this[kState] & kBuffered) === 0 ? 0 : this[kBufferedValue].length - this.bufferedIndex;
      },
    });

    WritableState.prototype[kOnConstructed] = function onConstructed(stream) {
      if ((this[kState] & kWriting) === 0) {
        clearBuffer(stream, this);
      }

      if ((this[kState] & kEnding) !== 0) {
        finishMaybe(stream, this);
      }
    };

    function Writable(options) {
      if (!(this instanceof Writable))
        return new Writable(options);

      this._events ??= {
        close: undefined,
        error: undefined,
        prefinish: undefined,
        finish: undefined,
        drain: undefined,
      };

      this._writableState = new WritableState(options, this, false);

      if (options) {
        if (typeof options.write === 'function')
          this._write = options.write;

        if (typeof options.writev === 'function')
          this._writev = options.writev;

        if (typeof options.destroy === 'function')
          this._destroy = options.destroy;

        if (typeof options.final === 'function')
          this._final = options.final;

        if (typeof options.construct === 'function')
          this._construct = options.construct;

        if (options.signal)
          addAbortSignal(options.signal, this);
      }

      Stream.call(this, options);

      if (this._construct != null) {
        destroyImpl.construct(this, () => {
          this._writableState[kOnConstructed](this);
        });
      }
    }

    Object.defineProperty(Writable, SymbolHasInstance, {
      __proto__: null,
      value: assignFunctionName(SymbolHasInstance, function(instance) {
        if (FunctionPrototypeSymbolHasInstance(this, instance)) return true;
        if (this !== Writable) return false;

        return instance && instance._writableState instanceof WritableState;
      }),
    });

    Writable.prototype.pipe = function() {
      errorOrDestroy(this, new ERR_STREAM_CANNOT_PIPE());
    };

    function _write(stream, chunk, encoding, cb) {
      const state = stream._writableState;

      if (cb == null || typeof cb !== 'function') {
        cb = nop;
      }

      if (chunk === null) {
        throw new ERR_STREAM_NULL_VALUES();
      }

      if ((state[kState] & kObjectMode) === 0) {
        if (!encoding) {
          encoding = (state[kState] & kDefaultUTF8Encoding) !== 0 ? 'utf8' : state.defaultEncoding;
        } else if (encoding !== 'buffer' && !Buffer.isEncoding(encoding)) {
          throw new ERR_UNKNOWN_ENCODING(encoding);
        }

        if (typeof chunk === 'string') {
          if (encoding === 'buffer') {
            throw new ERR_UNKNOWN_ENCODING(encoding);
          }
          if ((state[kState] & kDecodeStrings) !== 0) {
            chunk = Buffer.from(chunk, encoding);
            encoding = 'buffer';
          }
        } else if (chunk instanceof Buffer) {
          encoding = 'buffer';
        } else if (Stream._isArrayBufferView(chunk)) {
          chunk = Stream._uint8ArrayToBuffer(chunk);
          encoding = 'buffer';
        } else {
          throw new ERR_INVALID_ARG_TYPE(
            'chunk', ['string', 'Buffer', 'TypedArray', 'DataView'], chunk);
        }
      }

      let err;
      if ((state[kState] & kEnding) !== 0) {
        err = new ERR_STREAM_WRITE_AFTER_END();
      } else if ((state[kState] & kDestroyed) !== 0) {
        err = new ERR_STREAM_DESTROYED('write');
      }

      if (err) {
        process.nextTick(cb, err);
        errorOrDestroy(stream, err, true);
        return err;
      }

      state.pendingcb++;
      return writeOrBuffer(stream, state, chunk, encoding, cb);
    }

    Writable.prototype.write = function(chunk, encoding, cb) {
      if (encoding != null && typeof encoding === 'function') {
        cb = encoding;
        encoding = null;
      }

      return _write(this, chunk, encoding, cb) === true;
    };

    Writable.prototype.cork = function() {
      const state = this._writableState;

      state[kState] |= kCorked;
      state.corked++;
    };

    Writable.prototype.uncork = function() {
      const state = this._writableState;

      if (state.corked) {
        state.corked--;

        if (!state.corked) {
          state[kState] &= ~kCorked;
        }

        if ((state[kState] & kWriting) === 0)
          clearBuffer(this, state);
      }
    };

    Writable.prototype.setDefaultEncoding = function setDefaultEncoding(encoding) {
      if (typeof encoding === 'string')
        encoding = encoding.toLowerCase();
      if (!Buffer.isEncoding(encoding))
        throw new ERR_UNKNOWN_ENCODING(encoding);
      this._writableState.defaultEncoding = encoding;
      return this;
    };

    function writeOrBuffer(stream, state, chunk, encoding, callback) {
      const len = (state[kState] & kObjectMode) !== 0 ? 1 : chunk.length;

      state.length += len;

      if ((state[kState] & (kWriting | kErrored | kCorked | kConstructed)) !== kConstructed) {
        if ((state[kState] & kBuffered) === 0) {
          state[kState] |= kBuffered;
          state[kBufferedValue] = [];
        }

        state[kBufferedValue].push({ chunk, encoding, callback });
        if ((state[kState] & kAllBuffers) !== 0 && encoding !== 'buffer') {
          state[kState] &= ~kAllBuffers;
        }
        if ((state[kState] & kAllNoop) !== 0 && callback !== nop) {
          state[kState] &= ~kAllNoop;
        }
      } else {
        state.writelen = len;
        if (callback !== nop) {
          state.writecb = callback;
        }
        state[kState] |= kWriting | kSync | kExpectWriteCb;
        stream._write(chunk, encoding, state.onwrite);
        state[kState] &= ~kSync;
      }

      const ret = state.length < state.highWaterMark || state.length === 0;

      if (!ret) {
        state[kState] |= kNeedDrain;
      }

      return ret && (state[kState] & (kDestroyed | kErrored)) === 0;
    }

    const kWriteFlowBlock = kObjectMode | kDestroyed | kErrored | kSync |
      kEnding | kFinished | kWriting | kCorked | kBuffered | kEnded |
      kNeedDrain | kWriteCb | kExpectWriteCb | kBufferProcessing |
      kFinalCalled | kPrefinished | kOnFinished | kErrorEmitted;

    function writeKnownBuffer(stream, chunk) {
      const state = stream._writableState;
      if (state == null || state.length !== 0 || !(chunk instanceof Buffer))
        return undefined;

      const bits = state[kState];
      if ((bits & kConstructed) === 0 || (bits & kWriteFlowBlock) !== 0)
        return undefined;

      const len = chunk.length;
      state.pendingcb++;
      state.length = len;
      state.writelen = len;
      state[kState] = bits | kWriting | kSync | kExpectWriteCb;
      stream._write(chunk, 'buffer', state.onwrite);
      state[kState] &= ~kSync;

      const ret = state.length < state.highWaterMark || state.length === 0;
      if (!ret)
        state[kState] |= kNeedDrain;
      return ret && (state[kState] & (kDestroyed | kErrored)) === 0;
    }

    Object.defineProperty(Writable, 'writeKnownBuffer', {
      __proto__: null,
      value: writeKnownBuffer,
      enumerable: false,
    });

    function doWrite(stream, state, writev, len, chunk, encoding, cb) {
      state.writelen = len;
      if (cb !== nop) {
        state.writecb = cb;
      }
      state[kState] |= kWriting | kSync | kExpectWriteCb;
      if ((state[kState] & kDestroyed) !== 0)
        state.onwrite(new ERR_STREAM_DESTROYED('write'));
      else if (writev)
        stream._writev(chunk, state.onwrite);
      else
        stream._write(chunk, encoding, state.onwrite);
      state[kState] &= ~kSync;
    }

    function onwriteError(stream, state, er, cb) {
      --state.pendingcb;

      cb(er);
      errorBuffer(state);
      errorOrDestroy(stream, er);
    }

    function onwrite(stream, er) {
      const state = stream._writableState;

      if ((state[kState] & kExpectWriteCb) === 0) {
        errorOrDestroy(stream, new ERR_MULTIPLE_CALLBACK());
        return;
      }

      const sync = (state[kState] & kSync) !== 0;
      const cb = (state[kState] & kWriteCb) !== 0 ? state[kWriteCbValue] : nop;

      state.writecb = null;
      state[kState] &= ~(kWriting | kExpectWriteCb);
      state.length -= state.writelen;
      state.writelen = 0;

      if (er) {
        er.stack; // eslint-disable-line no-unused-expressions

        if ((state[kState] & kErrored) === 0) {
          state[kErroredValue] = er;
          state[kState] |= kErrored;
        }

        if (stream._readableState && !stream._readableState.errored) {
          stream._readableState.errored = er;
        }

        if (sync) {
          process.nextTick(onwriteError, stream, state, er, cb);
        } else {
          onwriteError(stream, state, er, cb);
        }
      } else {
        if ((state[kState] & kBuffered) !== 0) {
          clearBuffer(stream, state);
        }

        if (sync) {
          const needDrain = (state[kState] & kNeedDrain) !== 0 && state.length === 0;
          const needTick = needDrain || (state[kState] & kDestroyed !== 0) || cb !== nop;

          if (cb === nop) {
            if ((state[kState] & kAfterWritePending) === 0 && needTick) {
              process.nextTick(afterWrite, stream, state, 1, cb);
              state[kState] |= kAfterWritePending;
            } else {
              state.pendingcb--;
              if ((state[kState] & kEnding) !== 0) {
                finishMaybe(stream, state, true);
              }
            }
          } else if ((state[kState] & kAfterWriteTickInfo) !== 0 &&
                     state[kAfterWriteTickInfoValue].cb === cb) {
            state[kAfterWriteTickInfoValue].count++;
          } else if (needTick) {
            state[kAfterWriteTickInfoValue] = { count: 1, cb, stream, state };
            process.nextTick(afterWriteTick, state[kAfterWriteTickInfoValue]);
            state[kState] |= (kAfterWritePending | kAfterWriteTickInfo);
          } else {
            state.pendingcb--;
            if ((state[kState] & kEnding) !== 0) {
              finishMaybe(stream, state, true);
            }
          }
        } else {
          afterWrite(stream, state, 1, cb);
        }
      }
    }

    function afterWriteTick({ stream, state, count, cb }) {
      state[kState] &= ~kAfterWriteTickInfo;
      state[kAfterWriteTickInfoValue] = null;
      return afterWrite(stream, state, count, cb);
    }

    function afterWrite(stream, state, count, cb) {
      state[kState] &= ~kAfterWritePending;

      const needDrain = (state[kState] & (kEnding | kNeedDrain | kDestroyed)) === kNeedDrain && state.length === 0;
      if (needDrain) {
        state[kState] &= ~kNeedDrain;
        stream.emit('drain');
      }

      while (count-- > 0) {
        state.pendingcb--;
        cb(null);
      }

      if ((state[kState] & kDestroyed) !== 0) {
        errorBuffer(state);
      }

      if ((state[kState] & kEnding) !== 0) {
        finishMaybe(stream, state, true);
      }
    }

    function errorBuffer(state) {
      if ((state[kState] & kWriting) !== 0) {
        return;
      }

      if ((state[kState] & kBuffered) !== 0) {
        for (let n = state.bufferedIndex; n < state.buffered.length; ++n) {
          const { chunk, callback } = state[kBufferedValue][n];
          const len = (state[kState] & kObjectMode) !== 0 ? 1 : chunk.length;
          state.length -= len;
          callback(state.errored ?? new ERR_STREAM_DESTROYED('write'));
        }
      }

      callFinishedCallbacks(state, state.errored ?? new ERR_STREAM_DESTROYED('end'));

      resetBuffer(state);
    }

    function clearBuffer(stream, state) {
      if ((state[kState] & (kDestroyed | kBufferProcessing | kCorked | kBuffered | kConstructed)) !==
          (kBuffered | kConstructed)) {
        return;
      }

      const objectMode = (state[kState] & kObjectMode) !== 0;
      const { [kBufferedValue]: buffered, bufferedIndex } = state;
      const bufferedLength = buffered.length - bufferedIndex;

      if (!bufferedLength) {
        return;
      }

      let i = bufferedIndex;

      state[kState] |= kBufferProcessing;
      if (bufferedLength > 1 && stream._writev) {
        state.pendingcb -= bufferedLength - 1;

        const callback = (state[kState] & kAllNoop) !== 0 ? nop : (err) => {
          for (let n = i; n < buffered.length; ++n) {
            buffered[n].callback(err);
          }
        };
        const chunks = (state[kState] & kAllNoop) !== 0 && i === 0 ?
          buffered : buffered.slice(i);
        chunks.allBuffers = (state[kState] & kAllBuffers) !== 0;

        doWrite(stream, state, true, state.length, chunks, '', callback);

        resetBuffer(state);
      } else {
        do {
          const { chunk, encoding, callback } = buffered[i];
          buffered[i++] = null;
          const len = objectMode ? 1 : chunk.length;
          doWrite(stream, state, false, len, chunk, encoding, callback);
        } while (i < buffered.length && (state[kState] & kWriting) === 0);

        if (i === buffered.length) {
          resetBuffer(state);
        } else if (i > 256 && i * 2 >= buffered.length) {
          buffered.splice(0, i);
          state.bufferedIndex = 0;
        } else {
          state.bufferedIndex = i;
        }
      }
      state[kState] &= ~kBufferProcessing;
    }

    Writable.prototype._write = function(chunk, encoding, cb) {
      if (this._writev) {
        this._writev([{ chunk, encoding }], cb);
      } else {
        throw new ERR_METHOD_NOT_IMPLEMENTED('_write()');
      }
    };

    Writable.prototype._writev = null;

    Writable.prototype.end = function(chunk, encoding, cb) {
      const state = this._writableState;

      if (typeof chunk === 'function') {
        cb = chunk;
        chunk = null;
        encoding = null;
      } else if (typeof encoding === 'function') {
        cb = encoding;
        encoding = null;
      }

      let err;

      if (chunk != null) {
        const ret = _write(this, chunk, encoding);
        if (ret instanceof Error) {
          err = ret;
        }
      }

      if ((state[kState] & kCorked) !== 0) {
        state.corked = 1;
        this.uncork();
      }

      if (err) {
      } else if ((state[kState] & (kEnding | kErrored)) === 0) {

        state[kState] |= kEnding;
        finishMaybe(this, state, true);
        state[kState] |= kEnded;
      } else if ((state[kState] & kFinished) !== 0) {
        err = new ERR_STREAM_ALREADY_FINISHED('end');
      } else if ((state[kState] & kDestroyed) !== 0) {
        err = new ERR_STREAM_DESTROYED('end');
      }

      if (typeof cb === 'function') {
        if (err) {
          process.nextTick(cb, err);
        } else if ((state[kState] & kErrored) !== 0) {
          process.nextTick(cb, state[kErroredValue]);
        } else if ((state[kState] & kFinished) !== 0) {
          process.nextTick(cb, null);
        } else {
          state[kState] |= kOnFinished;
          state[kOnFinishedValue] ??= [];
          state[kOnFinishedValue].push(cb);
        }
      }

      return this;
    };

    function needFinish(state) {
      return (
        (state[kState] & (
          kEnding |
              kDestroyed |
              kConstructed |
              kFinished |
              kWriting |
              kErrorEmitted |
              kCloseEmitted |
              kErrored |
              kBuffered
        )) === (kEnding | kConstructed) && state.length === 0);
    }

    function onFinish(stream, state, err) {
      if ((state[kState] & kPrefinished) !== 0) {
        errorOrDestroy(stream, err ?? new ERR_MULTIPLE_CALLBACK());
        return;
      }
      state.pendingcb--;
      if (err) {
        callFinishedCallbacks(state, err);
        errorOrDestroy(stream, err, (state[kState] & kSync) !== 0);
      } else if (needFinish(state)) {
        state[kState] |= kPrefinished;
        stream.emit('prefinish');
        state.pendingcb++;
        process.nextTick(finish, stream, state);
      }
    }

    function prefinish(stream, state) {
      if ((state[kState] & (kPrefinished | kFinalCalled)) !== 0) {
        return;
      }

      if (typeof stream._final === 'function' && (state[kState] & kDestroyed) === 0) {
        state[kState] |= kFinalCalled | kSync;
        state.pendingcb++;

        try {
          stream._final((err) => onFinish(stream, state, err));
        } catch (err) {
          onFinish(stream, state, err);
        }

        state[kState] &= ~kSync;
      } else {
        state[kState] |= kFinalCalled | kPrefinished;
        stream.emit('prefinish');
      }
    }

    function finishMaybe(stream, state, sync) {
      if (needFinish(state)) {
        prefinish(stream, state);
        if (state.pendingcb === 0) {
          if (sync) {
            state.pendingcb++;
            process.nextTick((stream, state) => {
              if (needFinish(state)) {
                finish(stream, state);
              } else {
                state.pendingcb--;
              }
            }, stream, state);
          } else if (needFinish(state)) {
            state.pendingcb++;
            finish(stream, state);
          }
        }
      }
    }

    function finish(stream, state) {
      state.pendingcb--;
      state[kState] |= kFinished;

      callFinishedCallbacks(state, null);

      stream.emit('finish');

      if ((state[kState] & kAutoDestroy) !== 0) {
        const rState = stream._readableState;
        const autoDestroy = !rState || (
          rState.autoDestroy &&
          (rState.endEmitted || rState.readable === false)
        );
        if (autoDestroy) {
          stream.destroy();
        }
      }
    }

    function callFinishedCallbacks(state, err) {
      if ((state[kState] & kOnFinished) === 0) {
        return;
      }

      const onfinishCallbacks = state[kOnFinishedValue];
      state[kOnFinishedValue] = null;
      state[kState] &= ~kOnFinished;
      for (let i = 0; i < onfinishCallbacks.length; i++) {
        onfinishCallbacks[i](err);
      }
    }

    Object.defineProperties(Writable.prototype, {
      closed: {
        __proto__: null,
        get() {
          return this._writableState ? (this._writableState[kState] & kClosed) !== 0 : false;
        },
      },

      destroyed: {
        __proto__: null,
        get() {
          return this._writableState ? (this._writableState[kState] & kDestroyed) !== 0 : false;
        },
        set(value) {
          if (!this._writableState) return;

          if (value) this._writableState[kState] |= kDestroyed;
          else this._writableState[kState] &= ~kDestroyed;
        },
      },

      writable: {
        __proto__: null,
        get() {
          const w = this._writableState;
          return !!w && w.writable !== false &&
            (w[kState] & (kEnding | kEnded | kDestroyed | kErrored)) === 0;
        },
        set(val) {
          if (this._writableState) {
            this._writableState.writable = !!val;
          }
        },
      },

      writableFinished: {
        __proto__: null,
        get() {
          const state = this._writableState;
          return state ? (state[kState] & kFinished) !== 0 : false;
        },
      },

      writableObjectMode: {
        __proto__: null,
        get() {
          const state = this._writableState;
          return state ? (state[kState] & kObjectMode) !== 0 : false;
        },
      },

      writableBuffer: {
        __proto__: null,
        get() {
          const state = this._writableState;
          return state && state.getBuffer();
        },
      },

      writableEnded: {
        __proto__: null,
        get() {
          const state = this._writableState;
          return state ? (state[kState] & kEnding) !== 0 : false;
        },
      },

      writableNeedDrain: {
        __proto__: null,
        get() {
          const state = this._writableState;
          return state ? (state[kState] & (kDestroyed | kEnding | kNeedDrain)) === kNeedDrain : false;
        },
      },

      writableHighWaterMark: {
        __proto__: null,
        get() {
          const state = this._writableState;
          return state?.highWaterMark;
        },
      },

      writableCorked: {
        __proto__: null,
        get() {
          const state = this._writableState;
          return state ? state.corked : 0;
        },
      },

      writableLength: {
        __proto__: null,
        get() {
          const state = this._writableState;
          return state?.length;
        },
      },

      errored: {
        __proto__: null,
        enumerable: false,
        get() {
          const state = this._writableState;
          return state ? state.errored : null;
        },
      },

      writableAborted: {
        __proto__: null,
        get: function() {
          const state = this._writableState;
          return (
            (state[kState] & (kHasWritable | kWritable)) !== kHasWritable &&
            (state[kState] & (kDestroyed | kErrored)) !== 0 &&
            (state[kState] & kFinished) === 0
          );
        },
      },
    });

    const destroy = destroyImpl.destroy;
    Writable.prototype.destroy = function(err, cb) {
      const state = this._writableState;

      if ((state[kState] & (kBuffered | kOnFinished)) !== 0 && (state[kState] & kDestroyed) === 0) {
        process.nextTick(errorBuffer, state);
      }

      destroy.call(this, err, cb);
      return this;
    };

    Writable.prototype._undestroy = destroyImpl.undestroy;
    Writable.prototype._destroy = function(err, cb) {
      cb(err);
    };

    Writable.prototype[EE.captureRejectionSymbol] = function(err) {
      this.destroy(err);
    };

    let webStreamsAdapters;

    function lazyWebStreams() {
      if (webStreamsAdapters === undefined)
        webStreamsAdapters = require('internal/webstreams/adapters');
      return webStreamsAdapters;
    }

    Writable.fromWeb = function(writableStream, options) {
      return lazyWebStreams().newStreamWritableFromWritableStream(
        writableStream,
        options);
    };

    Writable.toWeb = function(streamWritable) {
      return lazyWebStreams().newWritableStreamFromStreamWritable(streamWritable);
    };

    Writable.prototype[SymbolAsyncDispose] = async function() {
      let error;
      if (!this.destroyed) {
        error = this.writableFinished ? null : new AbortError();
        this.destroy(error);
      }
      await new Promise((resolve, reject) =>
        eos(this, (err) => (err && err.name !== 'AbortError' ? reject(err) : resolve(null))),
      );
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
    throw new Error(\"no module \" + name + \" in this port\");
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

module.exports = load(\"stream\");
"

pub const stream_promises = "\"use strict\";

module.exports = require(\"node:stream\").promises;
"

pub const stream_consumers = "\"use strict\";

// the stream module holds the file; loading it makes it findable
require(\"node:stream\");
const consumers = require(\"spark:internal\").find(\"stream/consumers\");

module.exports = {
  ...consumers,
  // needs a real Blob
  blob() {
    const { codes } = require(\"spark:internal\").load(\"internal/errors\");
    return Promise.reject(new codes.ERR_METHOD_NOT_IMPLEMENTED(\"blob\"));
  },
};
"

pub const structured_clone = "\"use strict\";

// the html structured clone algorithm, as node's global structuredClone
// behaves: a deep copy that keeps shared and circular references, and
// refuses what cannot be copied with a DataCloneError

const { kind, tracksLength } = native;
const { typeError, isUncloneable } = require(\"spark:errors\");
const { DOMException } = require(\"spark:domexception\");

const TypedArray = Object.getPrototypeOf(Uint8Array);
const typedArrayName = Object.getOwnPropertyDescriptor(
  TypedArray.prototype,
  Symbol.toStringTag,
).get;
const typedArrays = {
  __proto__: null,
  Int8Array,
  Uint8Array,
  Uint8ClampedArray,
  Int16Array,
  Uint16Array,
  Int32Array,
  Uint32Array,
  Float16Array: globalThis.Float16Array,
  Float32Array,
  Float64Array,
  BigInt64Array,
  BigUint64Array,
};
// the only names an error keeps; any other becomes a plain Error
const errors = {
  __proto__: null,
  Error,
  EvalError,
  RangeError,
  ReferenceError,
  SyntaxError,
  TypeError,
  URIError,
};
const getter = (prototype, key) =>
  Object.getOwnPropertyDescriptor(prototype, key).get;
const bufferOf = getter(TypedArray.prototype, \"buffer\");
const offsetOf = getter(TypedArray.prototype, \"byteOffset\");
const lengthOf = getter(TypedArray.prototype, \"length\");
const viewBufferOf = getter(DataView.prototype, \"buffer\");
const viewOffsetOf = getter(DataView.prototype, \"byteOffset\");
const viewLengthOf = getter(DataView.prototype, \"byteLength\");
const objectToString = Object.prototype.toString;

function refused(message) {
  return new DOMException(message, \"DataCloneError\");
}

// how v8 names a value it will not clone
function described(value, itsKind) {
  if (typeof value === \"symbol\") return String(value);
  if (typeof value === \"function\") {
    return Function.prototype.toString.call(value);
  }
  switch (itsKind) {
    case \"symbol_object\":
    case \"generator_object\":
    case \"array_iterator\":
    case \"string_iterator\":
    case \"map_iterator\":
    case \"set_iterator\":
    case \"iterator_helper\":
      return objectToString.call(value);
  }
  let name;
  try {
    name = value.constructor.name;
  } catch {
    name = undefined;
  }
  return `#<${typeof name === \"string\" && name !== \"\" ? name : \"Object\"}>`;
}

function clonedBuffer(buffer) {
  if (buffer.detached) {
    throw refused(\"An ArrayBuffer is detached and could not be cloned.\");
  }
  if (!buffer.resizable) return buffer.slice(0);
  const copy = new ArrayBuffer(buffer.byteLength, {
    maxByteLength: buffer.maxByteLength,
  });
  new Uint8Array(copy).set(new Uint8Array(buffer));
  return copy;
}

function clonedError(value, memory) {
  const stack = value.stack;
  let copy;
  if (value instanceof DOMException) {
    copy = new DOMException(value.message, value.name);
    memory.set(value, copy);
  } else {
    const name = value.name;
    const Kind = (typeof name === \"string\" && errors[name]) || Error;
    const message = Object.getOwnPropertyDescriptor(value, \"message\");
    copy =
      message !== undefined && \"value\" in message
        ? new Kind(`${message.value}`)
        : new Kind();
    memory.set(value, copy);
    if (Object.hasOwn(value, \"cause\")) {
      Object.defineProperty(copy, \"cause\", {
        value: cloned(value.cause, memory),
        writable: true,
        enumerable: false,
        configurable: true,
      });
    }
  }
  if (typeof stack === \"string\") {
    Object.defineProperty(copy, \"stack\", {
      value: stack,
      writable: true,
      enumerable: false,
      configurable: true,
    });
  }
  return copy;
}

// own enumerable string keys, read once each, become plain data properties
function copyProperties(from, to, memory) {
  for (const key of Object.keys(from)) {
    if (!Object.hasOwn(from, key)) continue;
    const value = cloned(from[key], memory);
    if (key === \"__proto__\") {
      Object.defineProperty(to, key, {
        value,
        writable: true,
        enumerable: true,
        configurable: true,
      });
    } else {
      to[key] = value;
    }
  }
}

function cloned(value, memory) {
  switch (typeof value) {
    case \"object\":
      if (value === null) return null;
      break;
    case \"symbol\":
    case \"function\":
      throw refused(`${described(value)} could not be cloned.`);
    default:
      return value;
  }
  const before = memory.get(value);
  if (before !== undefined) return before;

  const itsKind = kind(value);
  let copy;
  switch (itsKind) {
    case \"boolean_object\":
      copy = new Boolean(Boolean.prototype.valueOf.call(value));
      break;
    case \"number_object\":
      copy = new Number(Number.prototype.valueOf.call(value));
      break;
    case \"string_object\":
      copy = new String(String.prototype.valueOf.call(value));
      break;
    case \"bigint_object\":
      copy = Object(BigInt.prototype.valueOf.call(value));
      break;
    case \"date\":
      copy = new Date(Date.prototype.getTime.call(value));
      break;
    case \"regexp\":
      copy = new RegExp(value.source, value.flags);
      break;
    case \"array_buffer\":
      copy = clonedBuffer(value);
      break;
    case \"shared_array_buffer\":
      // the same memory either way; there is no second handle to make
      copy = value;
      break;
    case \"typed_array\": {
      const buffer = cloned(bufferOf.call(value), memory);
      const Kind = typedArrays[typedArrayName.call(value)];
      copy = tracksLength(value)
        ? new Kind(buffer, offsetOf.call(value))
        : new Kind(buffer, offsetOf.call(value), lengthOf.call(value));
      break;
    }
    case \"data_view\": {
      const original = viewBufferOf.call(value);
      const buffer = cloned(original, memory);
      copy = tracksLength(value)
        ? new DataView(buffer, viewOffsetOf.call(value))
        : new DataView(buffer, viewOffsetOf.call(value), viewLengthOf.call(value));
      break;
    }
    case \"map\": {
      copy = new Map();
      memory.set(value, copy);
      for (const [key, item] of [...Map.prototype.entries.call(value)]) {
        copy.set(cloned(key, memory), cloned(item, memory));
      }
      return copy;
    }
    case \"set\": {
      copy = new Set();
      memory.set(value, copy);
      for (const item of [...Set.prototype.values.call(value)]) {
        copy.add(cloned(item, memory));
      }
      return copy;
    }
    case \"error\":
      return clonedError(value, memory);
    case \"array\":
      copy = new Array(value.length);
      memory.set(value, copy);
      copyProperties(value, copy, memory);
      return copy;
    case \"object\":
      if (isUncloneable(value)) {
        throw refused(\"Cannot clone object of unsupported type.\");
      }
      copy = {};
      memory.set(value, copy);
      copyProperties(value, copy, memory);
      return copy;
    default:
      throw refused(`${described(value, itsKind)} could not be cloned.`);
  }
  memory.set(value, copy);
  return copy;
}

function transferList(options) {
  if (options === undefined || options === null) return [];
  if (typeof options !== \"object\" && typeof options !== \"function\") {
    throw typeError(
      \"ERR_INVALID_ARG_TYPE\",
      \"Failed to execute 'structuredClone': Options cannot be converted to a dictionary\",
    );
  }
  const list = options.transfer;
  if (list === undefined) return [];
  if (
    list === null ||
    typeof list !== \"object\" ||
    typeof list[Symbol.iterator] !== \"function\"
  ) {
    throw typeError(
      \"ERR_INVALID_ARG_TYPE\",
      \"Failed to execute 'structuredClone': transfer in Options cannot be converted to sequence.\",
    );
  }
  const buffers = [...list];
  const seen = new Set();
  for (const buffer of buffers) {
    if (kind(buffer) !== \"array_buffer\") {
      throw refused(\"Found invalid value in transferList.\");
    }
    if (seen.has(buffer)) {
      throw refused(\"Transfer list contains duplicate ArrayBuffer\");
    }
    seen.add(buffer);
  }
  for (const buffer of buffers) {
    if (buffer.detached) {
      throw refused(\"Cannot transfer object of unsupported type.\");
    }
  }
  return buffers;
}

function structuredClone(value, options) {
  if (arguments.length === 0) {
    throw typeError(
      \"ERR_MISSING_ARGS\",
      'The \"The value argument must be specified\" argument must be specified',
    );
  }
  const buffers = transferList(options);
  const copy = cloned(value, new Map());
  // a transferred buffer's bytes were copied above; only now, with the whole
  // clone made, does the original give them up
  for (const buffer of buffers) buffer.transfer(0);
  return copy;
}

module.exports = { structuredClone };
"
