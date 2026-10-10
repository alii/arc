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

const NodeTypeError = nodeErrorClass(TypeError);
const NodeRangeError = nodeErrorClass(RangeError);

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
  #brand = true;

  static #check(encoder) {
    if (encoder == null || !(#brand in encoder)) throw invalidThis(\"TextEncoder\");
  }

  get encoding() {
    TextEncoder.#check(this);
    return \"utf-8\";
  }

  encode(input = \"\") {
    TextEncoder.#check(this);
    return new Uint8Array(native.encode(`${input}`));
  }

  // read counts string positions as this engine indexes them
  encodeInto(src, dest) {
    TextEncoder.#check(this);
    validateString(src, \"src\");
    if (!dest || !(dest instanceof Uint8Array)) {
      throw invalidArgType(\"dest\", \"Uint8Array\", dest);
    }
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

const { typeError } = require(\"spark:errors\");
const { now, wallClock } = native;

const startedAt = now();
const timeOrigin = wallClock();
let constructing = false;

class Performance {
  constructor() {
    if (!constructing) {
      throw typeError(\"ERR_ILLEGAL_CONSTRUCTOR\", \"Illegal constructor\");
    }
  }

  // milliseconds since this module was first loaded
  now() {
    return now() - startedAt;
  }

  get timeOrigin() {
    return timeOrigin;
  }

  toJSON() {
    return { timeOrigin };
  }

  get [Symbol.toStringTag]() {
    return \"Performance\";
  }
}

constructing = true;
const performance = new Performance();
constructing = false;

module.exports = { performance };
"

pub const fs_promises = "\"use strict\";

module.exports = require(\"node:fs\").promises;
"

pub const global = "\"use strict\";

module.exports = { global: globalThis };
"
