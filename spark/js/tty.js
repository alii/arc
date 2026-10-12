"use strict";

const { codes } = require("spark:errors");

const terminals = native.terminals();

function isatty(fd) {
  return Number.isInteger(fd) && fd >= 0 && fd <= 2147483647 && terminals.includes(fd);
}

// a terminal read from or written to as a stream; nothing here can open one
function ReadStream() {
  throw new codes.ERR_METHOD_NOT_IMPLEMENTED("tty.ReadStream");
}

function WriteStream() {
  throw new codes.ERR_METHOD_NOT_IMPLEMENTED("tty.WriteStream");
}

module.exports = { isatty, ReadStream, WriteStream };
