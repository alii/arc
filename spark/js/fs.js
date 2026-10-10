"use strict";

const {
  invalidArgType,
  invalidArgValue,
  systemError,
  validateFunction,
} = require("spark:errors");

const F_OK = 0;
const R_OK = 4;
const W_OK = 2;
const X_OK = 1;

const constants = { __proto__: null, F_OK, R_OK, W_OK, X_OK };

function getValidatedPath(path, name = "path") {
  if (typeof path !== "string") {
    throw invalidArgType(name, ["string", "Buffer", "URL"], path);
  }
  if (path.includes("\u0000")) {
    throw invalidArgValue(
      name,
      path,
      "must be a string, Uint8Array, or URL without null bytes",
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

// results come back as strings only; there is no Buffer yet
function getEncoding(options) {
  const encoding =
    options === undefined || options === null
      ? "utf8"
      : typeof options === "string"
        ? options
        : options.encoding === undefined || options.encoding === null
          ? "utf8"
          : options.encoding;
  if (encoding !== "utf8" && encoding !== "utf-8") {
    throw new Error("fs: only the utf8 encoding is supported, got " + String(encoding));
  }
}

function realpathSyncNative(path, options) {
  getEncoding(options);
  path = getValidatedPath(path);
  try {
    return native.realpath(path);
  } catch (thrown) {
    throw failed(thrown, "realpath", path);
  }
}

// one question to the embedder, where node walks the path itself; a failure
// is reported for the whole path with syscall realpath
function realpathSync(path, options) {
  getEncoding(options);
  if (typeof path !== "string") path += "";
  return realpathSyncNative(path);
}
realpathSync.native = realpathSyncNative;

function realpathNative(path, options, callback) {
  if (typeof options === "function") {
    callback = options;
    options = undefined;
  }
  validateFunction(callback, "cb");
  getEncoding(options);
  path = getValidatedPath(path);
  native.realpathLater(path).then(
    (resolved) => callback(null, resolved),
    (thrown) => callback(failed(thrown, "realpath", path)),
  );
}

function realpath(path, options, callback) {
  if (typeof options === "function") {
    callback = options;
    options = undefined;
  }
  if (typeof path !== "string") path += "";
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
