"use strict";

const { facts, write, now, getContext, setContext } = native;
const { invalidArgType, systemError, validateFunction } = require("spark:errors");
const loop = require("spark:loop");
const EventEmitter = require("node:events");

const given = facts();
const startedAt = now();

function nextTick(callback, ...args) {
  validateFunction(callback, "callback");
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
      if (typeof encoding === "function") {
        callback = encoding;
      }
      if (typeof chunk !== "string") {
        throw invalidArgType(
          "chunk",
          ["string", "Buffer", "TypedArray", "DataView"],
          chunk,
        );
      }
      let error = null;
      try {
        write(fd, chunk);
      } catch (thrown) {
        if (typeof thrown !== "string") throw thrown;
        error = systemError(thrown, "write");
      }
      if (typeof callback === "function") {
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
  value: "process",
});
