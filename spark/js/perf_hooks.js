"use strict";

const {
  error,
  typeError,
  invalidArgType,
  invalidArgValue,
  validateObject,
  validateString,
} = require("spark:errors");
const { DOMException } = require("spark:domexception");
const { now, wallClock } = native;

const startedAt = now();
const timeOrigin = wallClock();
const sinceStart = () => now() - startedAt;

const kSkipThrow = Symbol("skipThrow");
const kName = Symbol("name");
const kEntryType = Symbol("entryType");
const kStartTime = Symbol("startTime");
const kDuration = Symbol("duration");
const kDetail = Symbol("detail");
const kBrand = Symbol("performance");

function illegalConstructor() {
  return typeError("ERR_ILLEGAL_CONSTRUCTOR", "Illegal constructor");
}

function missingArgs(name) {
  return typeError(
    "ERR_MISSING_ARGS",
    `The "${name}" argument must be specified`,
  );
}

function checkField(value, key, type) {
  if (typeof value !== "object" || value === null || !(key in value)) {
    throw typeError(
      "ERR_INVALID_THIS",
      `Value of "this" must be of type ${type}`,
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
    checkField(this, kName, "PerformanceEntry");
    return this[kName];
  }

  get entryType() {
    checkField(this, kEntryType, "PerformanceEntry");
    return this[kEntryType];
  }

  get startTime() {
    checkField(this, kStartTime, "PerformanceEntry");
    return this[kStartTime];
  }

  get duration() {
    checkField(this, kDuration, "PerformanceEntry");
    return this[kDuration];
  }

  toJSON() {
    checkField(this, kName, "PerformanceEntry");
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
  "nodeStart",
  "v8Start",
  "environment",
  "loopStart",
  "loopExit",
  "bootstrapComplete",
]);

let markEntryBuffer = [];
let measureEntryBuffer = [];

function invalidTimestamp(value) {
  return typeError(
    "ERR_PERFORMANCE_INVALID_TIMESTAMP",
    `${value} is not a valid timestamp`,
  );
}

function getMark(name) {
  if (name === undefined) return;
  if (typeof name === "number") {
    if (name < 0) throw invalidTimestamp(name);
    return name;
  }
  name = `${name}`;
  const ts = markTimings.get(name);
  if (ts === undefined) {
    throw new DOMException(
      `The "${name}" performance mark has not been set`,
      "SyntaxError",
    );
  }
  return ts;
}

// a detail is copied, which needs the embedder to have given structuredClone
function cloneDetail(detail) {
  if (detail == null) return null;
  if (typeof globalThis.structuredClone !== "function") {
    throw error(
      "ERR_METHOD_NOT_IMPLEMENTED",
      "The structuredClone method is not implemented",
    );
  }
  return globalThis.structuredClone(detail);
}

class PerformanceMark extends PerformanceEntry {
  constructor(name, options = undefined) {
    if (arguments.length === 0) throw missingArgs("name");
    name = `${name}`;
    if (reservedMarkNames.has(name)) throw invalidArgValue("name", name);
    if (options != null) validateObject(options, "options");
    const startTime = options?.startTime ?? sinceStart();
    if (typeof startTime !== "number") {
      throw invalidArgType("startTime", "number", startTime);
    }
    if (startTime < 0) throw invalidTimestamp(startTime);
    markTimings.set(name, startTime);
    const detail = cloneDetail(options?.detail);

    super(kSkipThrow, name, "mark", startTime, 0);
    this[kDetail] = detail;
  }

  get detail() {
    checkField(this, kDetail, "PerformanceMark");
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
  [Symbol.toStringTag]: { configurable: true, value: "PerformanceMark" },
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
    checkField(this, kDetail, "PerformanceMeasure");
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
  [Symbol.toStringTag]: { configurable: true, value: "PerformanceMeasure" },
});

function invalidMeasureOptions(message) {
  return typeError("ERR_PERFORMANCE_MEASURE_INVALID_OPTIONS", message);
}

function calculateStartDuration(startOrMeasureOptions, endMark) {
  startOrMeasureOptions ??= 0;
  let start;
  let end;
  let duration;
  let optionsValid = false;
  if (typeof startOrMeasureOptions === "object") {
    ({ start, end, duration } = startOrMeasureOptions);
    optionsValid = start !== undefined || end !== undefined;
  }
  if (optionsValid) {
    if (endMark !== undefined) {
      throw invalidMeasureOptions("endMark must not be specified");
    }
    if (start === undefined && end === undefined) {
      throw invalidMeasureOptions(
        "One of options.start or options.end is required",
      );
    }
    if (start !== undefined && end !== undefined && duration !== undefined) {
      throw invalidMeasureOptions(
        "Must not have options.start, options.end, and " +
          "options.duration specified",
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

  if (typeof startOrMeasureOptions === "string") {
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
  if (type === "mark") {
    markEntryBuffer =
      name === undefined
        ? []
        : markEntryBuffer.filter((entry) => entry.name !== name);
  } else if (type === "measure") {
    measureEntryBuffer =
      name === undefined
        ? []
        : measureEntryBuffer.filter((entry) => entry.name !== name);
  }
}

function filterBufferByNameAndType(name, type) {
  let bufferList;
  if (type === "mark") {
    bufferList = markEntryBuffer;
  } else if (type === "measure") {
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
    checkField(this, kBrand, "Performance");
    if (name !== undefined) {
      name = `${name}`;
      if (reservedMarkNames.has(name)) throw invalidArgValue("name", name);
      markTimings.delete(name);
    } else {
      markTimings.clear();
    }
    clearEntriesFromBuffer("mark", name);
  }

  clearMeasures(name = undefined) {
    checkField(this, kBrand, "Performance");
    if (name !== undefined) name = `${name}`;
    clearEntriesFromBuffer("measure", name);
  }

  getEntries() {
    checkField(this, kBrand, "Performance");
    return filterBufferByNameAndType();
  }

  getEntriesByName(name, type = undefined) {
    checkField(this, kBrand, "Performance");
    if (arguments.length === 0) throw missingArgs("name");
    name = `${name}`;
    if (type !== undefined) type = `${type}`;
    return filterBufferByNameAndType(name, type);
  }

  getEntriesByType(type) {
    checkField(this, kBrand, "Performance");
    if (arguments.length === 0) throw missingArgs("type");
    type = `${type}`;
    return filterBufferByNameAndType(undefined, type);
  }

  mark(name, options = {}) {
    checkField(this, kBrand, "Performance");
    if (arguments.length === 0) throw missingArgs("name");
    const entry = new PerformanceMark(name, options);
    markEntryBuffer.push(entry);
    return entry;
  }

  measure(name, startOrMeasureOptions = {}, endMark = undefined) {
    checkField(this, kBrand, "Performance");
    if (arguments.length === 0) throw missingArgs("name");
    validateString(name, "name");
    const { start, duration } = calculateStartDuration(
      startOrMeasureOptions,
      endMark,
    );
    const detail = cloneDetail(startOrMeasureOptions?.detail);
    const entry = new PerformanceMeasure(
      kSkipThrow,
      name,
      "measure",
      start,
      duration,
    );
    entry[kDetail] = detail;
    measureEntryBuffer.push(entry);
    return entry;
  }

  // milliseconds since this module was first loaded
  now() {
    checkField(this, kBrand, "Performance");
    return sinceStart();
  }

  get timeOrigin() {
    checkField(this, kBrand, "Performance");
    return timeOrigin;
  }

  toJSON() {
    checkField(this, kBrand, "Performance");
    return { timeOrigin };
  }

  get [Symbol.toStringTag]() {
    return "Performance";
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
