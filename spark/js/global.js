"use strict";

const { validateFunction } = require("spark:errors");

function queueMicrotask(callback) {
  validateFunction(callback, "callback");
  Promise.resolve().then(callback);
}

module.exports = { global: globalThis, queueMicrotask };
