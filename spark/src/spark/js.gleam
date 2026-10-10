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
        enumerable: false,
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

const { inspect } = native;

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

function nodeErrorClass(Base) {
  return class NodeError extends Base {
    constructor(code, message) {
      super(message);
      this.code = code;
    }

    toString() {
      return `${this.name} [${this.code}]: ${this.message}`;
    }
  };
}

const NodeError = nodeErrorClass(Error);
const NodeTypeError = nodeErrorClass(TypeError);
const NodeRangeError = nodeErrorClass(RangeError);

function error(code, message) {
  return new NodeError(code, message);
}

function typeError(code, message) {
  return new NodeTypeError(code, message);
}

function rangeError(code, message) {
  return new NodeRangeError(code, message);
}

const NodeURIError = nodeErrorClass(URIError);

function uriError(code, message) {
  return new NodeURIError(code, message);
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
      return `${inspect(value)}`;
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
  invalidArgType,
  invalidArgValue,
  systemError,
  validateFunction,
} = require(\"spark:errors\");

const F_OK = 0;
const R_OK = 4;
const W_OK = 2;
const X_OK = 1;

const constants = { __proto__: null, F_OK, R_OK, W_OK, X_OK };

function getValidatedPath(path, name = \"path\") {
  if (typeof path !== \"string\") {
    throw invalidArgType(name, [\"string\", \"Buffer\", \"URL\"], path);
  }
  if (path.includes(\"\\u0000\")) {
    throw invalidArgValue(
      name,
      path,
      \"must be a string, Uint8Array, or URL without null bytes\",
    );
  }
  return path;
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

// results come back as strings only; there is no Buffer yet
function getEncoding(options) {
  const encoding =
    options === undefined || options === null
      ? \"utf8\"
      : typeof options === \"string\"
        ? options
        : options.encoding === undefined || options.encoding === null
          ? \"utf8\"
          : options.encoding;
  if (encoding !== \"utf8\" && encoding !== \"utf-8\") {
    throw new Error(\"fs: only the utf8 encoding is supported, got \" + String(encoding));
  }
}

function realpathSyncNative(path, options) {
  getEncoding(options);
  path = getValidatedPath(path);
  try {
    return native.realpath(path);
  } catch (thrown) {
    throw failed(thrown, \"realpath\", path);
  }
}

// one question to the embedder, where node walks the path itself; a failure
// is reported for the whole path with syscall realpath
function realpathSync(path, options) {
  getEncoding(options);
  if (typeof path !== \"string\") path += \"\";
  return realpathSyncNative(path);
}
realpathSync.native = realpathSyncNative;

function realpathNative(path, options, callback) {
  if (typeof options === \"function\") {
    callback = options;
    options = undefined;
  }
  validateFunction(callback, \"cb\");
  getEncoding(options);
  path = getValidatedPath(path);
  native.realpathLater(path).then(
    (resolved) => callback(null, resolved),
    (thrown) => callback(failed(thrown, \"realpath\", path)),
  );
}

function realpath(path, options, callback) {
  if (typeof options === \"function\") {
    callback = options;
    options = undefined;
  }
  if (typeof path !== \"string\") path += \"\";
  realpathNative(path, options, callback);
}
realpath.native = realpathNative;

const promises = {
  async realpath(path, options) {
    getEncoding(options);
    path = getValidatedPath(path);
    try {
      return await native.realpathLater(path);
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
  constants,
};

module.exports = {
  access,
  accessSync,
  exists,
  existsSync,
  realpath,
  realpathSync,
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

module.exports = {
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
};
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

class TextEncoder {
  #encoding = \"utf-8\";

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

class TextDecoder {
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
} = require(\"spark:errors\");
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

const inspect = Object.assign((value) => describe(value), {
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

function markTransferMode() {}

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
    inspect: (value) => errors.inspect(value),
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
  \"events\": function (module, exports, require, process) {
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
  \"internal/events/abort_listener\": function (module, exports, require, process) {
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
  \"internal/events/symbols\": function (module, exports, require, process) {
    \"use strict\";

    const kFirstEventParam = Symbol(\"kFirstEventParam\");

    module.exports = {
      kFirstEventParam,
    };
  },
  \"internal/fixed_queue\": function (module, exports, require, process) {
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
    throw new Error(\"no module \" + name + \" in this port\");
  }
  const made = { exports: {} };
  loaded.set(name, made);
  files[name](made, made.exports, load, shims.process());
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
  \"string_decoder\": function (module, exports, require, process) {
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
    throw new Error(\"no module \" + name + \" in this port\");
  }
  const made = { exports: {} };
  loaded.set(name, made);
  files[name](made, made.exports, load, shims.process());
  return made.exports;
}

module.exports = load(\"string_decoder\");
"
