"use strict";

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
} = require("spark:errors");
const { Buffer } = require("node:buffer");

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
function getValidatedPath(fileURLOrPath, name = "path") {
  let path = fileURLOrPath;
  if (isURL(path)) {
    const url = optional("node:url");
    if (url !== undefined) path = url.fileURLToPath(path);
  }
  const isBytes = ArrayBuffer.isView(path) && path.BYTES_PER_ELEMENT === 1;
  if (typeof path !== "string" && !isBytes) {
    throw invalidArgType(name, ["string", "Buffer", "URL"], path);
  }
  if (isBytes) {
    path = Buffer.from(path.buffer, path.byteOffset, path.byteLength).toString();
  }
  if (path.includes("\u0000")) {
    throw invalidArgValue(
      name,
      fileURLOrPath,
      "must be a string, Uint8Array, or URL without null bytes",
    );
  }
  return path;
}

function assertEncoding(encoding) {
  if (encoding && !Buffer.isEncoding(encoding)) {
    throw invalidArgValue("encoding", encoding, "is invalid encoding");
  }
}

function getOptions(options, defaultOptions = kEmptyObject) {
  if (options == null || typeof options === "function") {
    return defaultOptions;
  }

  if (typeof options === "string") {
    defaultOptions = { ...defaultOptions };
    defaultOptions.encoding = options;
    options = defaultOptions;
  } else if (typeof options !== "object") {
    throw invalidArgType("options", ["string", "Object"], options);
  }

  if (options.encoding !== "buffer") assertEncoding(options.encoding);

  if (options.signal !== undefined) {
    validateAbortSignal(options.signal, "options.signal");
  }

  return options;
}

// a name or a path comes from the embedder as text; node hands it on as
// text, as a Buffer, or as that Buffer read in another encoding
function inEncoding(text, encoding) {
  if (!encoding || encoding === "utf8" || encoding === "utf-8") return text;
  const bytes = Buffer.from(text);
  return encoding === "buffer" ? bytes : bytes.toString(encoding);
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
  if (typeof mode !== "number") {
    throw modeError(
      TypeError,
      "ERR_INVALID_ARG_TYPE",
      "mode must be int32 or null/undefined",
    );
  }
  if (!Number.isFinite(mode)) {
    throw modeError(RangeError, "ERR_OUT_OF_RANGE", "mode is out of range");
  }
  const int = Math.trunc(mode);
  if (int < 0 || int > 7) {
    throw modeError(
      RangeError,
      "ERR_OUT_OF_RANGE",
      "mode is out of range: >= 0 && <= 7",
    );
  }
  return int;
}

// a want that failed is thrown or rejected as its error code, a string
function failed(thrown, syscall, path) {
  return typeof thrown === "string" ? systemError(thrown, syscall, path) : thrown;
}

function accessSync(path, mode) {
  path = getValidatedPath(path);
  mode = getValidMode(mode);
  try {
    native.access(path, mode);
  } catch (thrown) {
    throw failed(thrown, "access", path);
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
    if (typeof thrown === "string") return false;
    throw thrown;
  }
}

function access(path, mode, callback) {
  if (typeof mode === "function") {
    callback = mode;
    mode = F_OK;
  }

  path = getValidatedPath(path);
  validateFunction(callback, "cb");
  mode = getValidMode(mode);

  native.accessLater(path, mode).then(
    () => callback(null),
    (thrown) => callback(failed(thrown, "access", path)),
  );
}

function exists(path, callback) {
  validateFunction(callback, "cb");

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
    throw failed(thrown, "realpath", path);
  }
}

// one question to the embedder, where node walks the path itself; a failure
// is reported for the whole path with syscall realpath
function realpathSync(path, options) {
  options = getOptions(options);
  if (typeof path !== "string" && !isURL(path) && !ArrayBuffer.isView(path)) {
    path += "";
  }
  return realpathSyncNative(path, options);
}
realpathSync.native = realpathSyncNative;

function realpathNative(path, options, callback) {
  if (typeof options === "function") {
    callback = options;
    options = undefined;
  }
  validateFunction(callback, "cb");
  options = getOptions(options);
  path = getValidatedPath(path);
  native.realpathLater(path).then(
    (resolved) => callback(null, inEncoding(resolved, options.encoding)),
    (thrown) => callback(failed(thrown, "realpath", path)),
  );
}

function realpath(path, options, callback) {
  if (typeof options === "function") {
    callback = options;
    options = undefined;
  }
  if (typeof path !== "string" && !isURL(path) && !ArrayBuffer.isView(path)) {
    path += "";
  }
  realpathNative(path, options, callback);
}
realpath.native = realpathNative;

// ---- what stat returns, as node's lib/internal/fs/utils.js builds it

const kPartialAtimeNs = Symbol("kPartialAtimeNs");
const kPartialMtimeNs = Symbol("kPartialMtimeNs");
const kPartialCtimeNs = Symbol("kPartialCtimeNs");
const kPartialBirthtimeNs = Symbol("kPartialBirthtimeNs");

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
  ["atime", "mtime", "ctime", "birthtime"],
  (stats, name) => dateFromMs(stats[`${name}Ms`]),
);

const partialNs = {
  __proto__: null,
  atimeInstant: kPartialAtimeNs,
  mtimeInstant: kPartialMtimeNs,
  ctimeInstant: kPartialCtimeNs,
  birthtimeInstant: kPartialBirthtimeNs,
};
const instantNames = ["atimeInstant", "mtimeInstant", "ctimeInstant", "birthtimeInstant"];

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

const kType = Symbol("type");

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
  return thrown === "ENOENT" || (follow && thrown === "ENOTDIR");
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
  return statSyncOf(false, "lstat", path, options);
}

function statSync(path, options = { __proto__: null, bigint: false, throwIfNoEntry: true }) {
  return statSyncOf(true, "stat", path, options);
}

function statOf(follow, syscall, path, options, callback) {
  if (typeof options === "function") {
    callback = options;
    options = kEmptyObject;
  } else if (options === null || typeof options !== "object") {
    options = kEmptyObject;
  } else {
    options = getOptions(options, { bigint: false });
  }
  validateFunction(callback, "cb");
  path = getValidatedPath(path);
  if (options.signal !== undefined) {
    validateAbortSignal(options.signal, "options.signal");
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
  statOf(false, "lstat", path, options, callback);
}

function stat(path, options = { __proto__: null, bigint: false, throwIfNoEntry: true }, callback) {
  statOf(true, "stat", path, options, callback);
}

async function statPromise(follow, syscall, path, options) {
  validateObject(options, "options");
  const { signal } = options;
  if (signal !== undefined) validateAbortSignal(signal, "options.signal");
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
    throw failed(thrown, "readlink", path);
  }
}

function readlink(path, options, callback) {
  if (typeof options === "function") {
    callback = options;
    options = undefined;
  }
  validateFunction(callback, "cb");
  options = getOptions(options);
  path = getValidatedPath(path);
  native.readlinkLater(path).then(
    (target) => callback(null, inEncoding(target, options.encoding)),
    (thrown) => callback(failed(thrown, "readlink", path)),
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
  const path = optional("node:path");
  if (path !== undefined) return path.join(dir, name);
  return dir.endsWith("/") ? dir + name : `${dir}/${name}`;
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
    throw failed(thrown, "scandir", path);
  }
}

async function scandir(path) {
  try {
    return listed(await native.readdirLater(path));
  } catch (thrown) {
    throw failed(thrown, "scandir", path);
  }
}

// a symlink counts as a directory to walk into when it leads to one
function leadsToDirectorySync(path) {
  try {
    return (native.stat(path, true)[1] & S_IFMT) === S_IFDIR;
  } catch (thrown) {
    if (typeof thrown === "string") return false;
    throw thrown;
  }
}

async function leadsToDirectory(path) {
  try {
    return ((await native.statLater(path, true))[1] & S_IFMT) === S_IFDIR;
  } catch (thrown) {
    if (typeof thrown === "string") return false;
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
    const relative = prefix === "" ? name : `${prefix}/${name}`;
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
  const walk = { results: [], dirs: [basePath], prefixes: [""] };
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
  const walk = { results: [], dirs: [basePath], prefixes: [""] };
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
    validateBoolean(options.recursive, "options.recursive");
  }
  if (options.recursive) return readdirSyncRecursive(path, options);
  const [names, types] = scandirSync(path);
  return entriesOf(path, names, types, options);
}

async function readdirPromise(path, options) {
  options = getOptions(options);
  path = getValidatedPath(path);
  if (options.recursive != null) {
    validateBoolean(options.recursive, "options.recursive");
  }
  if (options.recursive) return readdirRecursive(path, options);
  const [names, types] = await scandir(path);
  return entriesOf(path, names, types, options);
}

function readdir(path, options, callback) {
  if (typeof options === "function") {
    callback = options;
    options = undefined;
  }
  validateFunction(callback, "cb");
  options = getOptions(options);
  path = getValidatedPath(path);
  if (options.recursive != null) {
    validateBoolean(options.recursive, "options.recursive");
  }
  readdirPromise(path, options).then(
    (entries) => callback(null, entries),
    (error) => callback(error),
  );
}

// ---- readFile

// only reading is here, so only the flags that read
function checkReadFlag(flag) {
  if (flag === undefined || flag === null || flag === "r" || flag === "rs" || flag === "sr") {
    return;
  }
  if (typeof flag === "number") {
    throw new codes.ERR_METHOD_NOT_IMPLEMENTED("readFile with numeric flags");
  }
  if (!["r+", "rs+", "sr+", "w", "wx", "xw", "w+", "wx+", "xw+", "a", "ax", "xa", "as", "sa", "a+", "ax+", "xa+", "as+", "sa+"].includes(flag)) {
    throw invalidArgValue("flags", flag);
  }
  throw new codes.ERR_METHOD_NOT_IMPLEMENTED(`readFile with the flag '${flag}'`);
}

function readTarget(path) {
  if (typeof path === "number") {
    throw new codes.ERR_METHOD_NOT_IMPLEMENTED("readFile from a file descriptor");
  }
  return getValidatedPath(path);
}

// node opens, then reads: a missing file fails at open, a directory at
// read. Its sync read names no path in that error; the others do
function readFailed(thrown, path, namesPath) {
  if (thrown === "EISDIR") {
    return systemError("EISDIR", "read", namesPath ? path : undefined);
  }
  return failed(thrown, "open", path);
}

function fileContent(bytes, options) {
  const buffer = Buffer.from(bytes);
  return options.encoding ? buffer.toString(options.encoding) : buffer;
}

function readFileSync(path, options) {
  options = getOptions(options, { flag: "r" });
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
  validateFunction(callback, "cb");
  options = getOptions(options, { flag: "r" });
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
  options = getOptions(options, { flag: "r" });
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
      throw failed(thrown, "realpath", path);
    }
  },
  async access(path, mode = F_OK) {
    path = getValidatedPath(path);
    mode = getValidMode(mode);
    try {
      await native.accessLater(path, mode);
    } catch (thrown) {
      throw failed(thrown, "access", path);
    }
  },
  stat(path, options = { bigint: false }) {
    return statPromise(true, "stat", path, options);
  },
  lstat(path, options = { bigint: false }) {
    return statPromise(false, "lstat", path, options);
  },
  async readlink(path, options) {
    options = getOptions(options);
    path = getValidatedPath(path, "oldPath");
    try {
      return inEncoding(await native.readlinkLater(path), options.encoding);
    } catch (thrown) {
      throw failed(thrown, "readlink", path);
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
