"use strict";

const { typeError } = require("spark:errors");
const { now, wallClock } = native;

const startedAt = now();
const timeOrigin = wallClock();
let constructing = false;

class Performance {
  constructor() {
    if (!constructing) {
      throw typeError("ERR_ILLEGAL_CONSTRUCTOR", "Illegal constructor");
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
    return "Performance";
  }
}

constructing = true;
const performance = new Performance();
constructing = false;

module.exports = { performance };
