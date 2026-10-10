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
  systemError,
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
