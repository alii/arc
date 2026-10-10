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
