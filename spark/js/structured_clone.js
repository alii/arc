"use strict";

// the html structured clone algorithm, as node's global structuredClone
// behaves: a deep copy that keeps shared and circular references, and
// refuses what cannot be copied with a DataCloneError

const { kind, tracksLength } = native;
const { typeError, isUncloneable } = require("spark:errors");
const { DOMException } = require("spark:domexception");

const TypedArray = Object.getPrototypeOf(Uint8Array);
const typedArrayName = Object.getOwnPropertyDescriptor(
  TypedArray.prototype,
  Symbol.toStringTag,
).get;
const typedArrays = {
  __proto__: null,
  Int8Array,
  Uint8Array,
  Uint8ClampedArray,
  Int16Array,
  Uint16Array,
  Int32Array,
  Uint32Array,
  Float16Array: globalThis.Float16Array,
  Float32Array,
  Float64Array,
  BigInt64Array,
  BigUint64Array,
};
// the only names an error keeps; any other becomes a plain Error
const errors = {
  __proto__: null,
  Error,
  EvalError,
  RangeError,
  ReferenceError,
  SyntaxError,
  TypeError,
  URIError,
};
const getter = (prototype, key) =>
  Object.getOwnPropertyDescriptor(prototype, key).get;
const bufferOf = getter(TypedArray.prototype, "buffer");
const offsetOf = getter(TypedArray.prototype, "byteOffset");
const lengthOf = getter(TypedArray.prototype, "length");
const viewBufferOf = getter(DataView.prototype, "buffer");
const viewOffsetOf = getter(DataView.prototype, "byteOffset");
const viewLengthOf = getter(DataView.prototype, "byteLength");
const objectToString = Object.prototype.toString;

function refused(message) {
  return new DOMException(message, "DataCloneError");
}

// how v8 names a value it will not clone
function described(value, itsKind) {
  if (typeof value === "symbol") return String(value);
  if (typeof value === "function") {
    return Function.prototype.toString.call(value);
  }
  switch (itsKind) {
    case "symbol_object":
    case "generator_object":
    case "array_iterator":
    case "string_iterator":
    case "map_iterator":
    case "set_iterator":
    case "iterator_helper":
      return objectToString.call(value);
  }
  let name;
  try {
    name = value.constructor.name;
  } catch {
    name = undefined;
  }
  return `#<${typeof name === "string" && name !== "" ? name : "Object"}>`;
}

function clonedBuffer(buffer) {
  if (buffer.detached) {
    throw refused("An ArrayBuffer is detached and could not be cloned.");
  }
  if (!buffer.resizable) return buffer.slice(0);
  const copy = new ArrayBuffer(buffer.byteLength, {
    maxByteLength: buffer.maxByteLength,
  });
  new Uint8Array(copy).set(new Uint8Array(buffer));
  return copy;
}

function clonedError(value, memory) {
  const stack = value.stack;
  let copy;
  if (value instanceof DOMException) {
    copy = new DOMException(value.message, value.name);
    memory.set(value, copy);
  } else {
    const name = value.name;
    const Kind = (typeof name === "string" && errors[name]) || Error;
    const message = Object.getOwnPropertyDescriptor(value, "message");
    copy =
      message !== undefined && "value" in message
        ? new Kind(`${message.value}`)
        : new Kind();
    memory.set(value, copy);
    if (Object.hasOwn(value, "cause")) {
      Object.defineProperty(copy, "cause", {
        value: cloned(value.cause, memory),
        writable: true,
        enumerable: false,
        configurable: true,
      });
    }
  }
  if (typeof stack === "string") {
    Object.defineProperty(copy, "stack", {
      value: stack,
      writable: true,
      enumerable: false,
      configurable: true,
    });
  }
  return copy;
}

// own enumerable string keys, read once each, become plain data properties
function copyProperties(from, to, memory) {
  for (const key of Object.keys(from)) {
    if (!Object.hasOwn(from, key)) continue;
    const value = cloned(from[key], memory);
    if (key === "__proto__") {
      Object.defineProperty(to, key, {
        value,
        writable: true,
        enumerable: true,
        configurable: true,
      });
    } else {
      to[key] = value;
    }
  }
}

function cloned(value, memory) {
  switch (typeof value) {
    case "object":
      if (value === null) return null;
      break;
    case "symbol":
    case "function":
      throw refused(`${described(value)} could not be cloned.`);
    default:
      return value;
  }
  const before = memory.get(value);
  if (before !== undefined) return before;

  const itsKind = kind(value);
  let copy;
  switch (itsKind) {
    case "boolean_object":
      copy = new Boolean(Boolean.prototype.valueOf.call(value));
      break;
    case "number_object":
      copy = new Number(Number.prototype.valueOf.call(value));
      break;
    case "string_object":
      copy = new String(String.prototype.valueOf.call(value));
      break;
    case "bigint_object":
      copy = Object(BigInt.prototype.valueOf.call(value));
      break;
    case "date":
      copy = new Date(Date.prototype.getTime.call(value));
      break;
    case "regexp":
      copy = new RegExp(value.source, value.flags);
      break;
    case "array_buffer":
      copy = clonedBuffer(value);
      break;
    case "shared_array_buffer":
      // the same memory either way; there is no second handle to make
      copy = value;
      break;
    case "typed_array": {
      const buffer = cloned(bufferOf.call(value), memory);
      const Kind = typedArrays[typedArrayName.call(value)];
      copy = tracksLength(value)
        ? new Kind(buffer, offsetOf.call(value))
        : new Kind(buffer, offsetOf.call(value), lengthOf.call(value));
      break;
    }
    case "data_view": {
      const original = viewBufferOf.call(value);
      const buffer = cloned(original, memory);
      copy = tracksLength(value)
        ? new DataView(buffer, viewOffsetOf.call(value))
        : new DataView(buffer, viewOffsetOf.call(value), viewLengthOf.call(value));
      break;
    }
    case "map": {
      copy = new Map();
      memory.set(value, copy);
      for (const [key, item] of [...Map.prototype.entries.call(value)]) {
        copy.set(cloned(key, memory), cloned(item, memory));
      }
      return copy;
    }
    case "set": {
      copy = new Set();
      memory.set(value, copy);
      for (const item of [...Set.prototype.values.call(value)]) {
        copy.add(cloned(item, memory));
      }
      return copy;
    }
    case "error":
      return clonedError(value, memory);
    case "array":
      copy = new Array(value.length);
      memory.set(value, copy);
      copyProperties(value, copy, memory);
      return copy;
    case "object":
      if (isUncloneable(value)) {
        throw refused("Cannot clone object of unsupported type.");
      }
      copy = {};
      memory.set(value, copy);
      copyProperties(value, copy, memory);
      return copy;
    default:
      throw refused(`${described(value, itsKind)} could not be cloned.`);
  }
  memory.set(value, copy);
  return copy;
}

function transferList(options) {
  if (options === undefined || options === null) return [];
  if (typeof options !== "object" && typeof options !== "function") {
    throw typeError(
      "ERR_INVALID_ARG_TYPE",
      "Failed to execute 'structuredClone': Options cannot be converted to a dictionary",
    );
  }
  const list = options.transfer;
  if (list === undefined) return [];
  if (
    list === null ||
    typeof list !== "object" ||
    typeof list[Symbol.iterator] !== "function"
  ) {
    throw typeError(
      "ERR_INVALID_ARG_TYPE",
      "Failed to execute 'structuredClone': transfer in Options cannot be converted to sequence.",
    );
  }
  const buffers = [...list];
  const seen = new Set();
  for (const buffer of buffers) {
    if (kind(buffer) !== "array_buffer") {
      throw refused("Found invalid value in transferList.");
    }
    if (seen.has(buffer)) {
      throw refused("Transfer list contains duplicate ArrayBuffer");
    }
    seen.add(buffer);
  }
  for (const buffer of buffers) {
    if (buffer.detached) {
      throw refused("Cannot transfer object of unsupported type.");
    }
  }
  return buffers;
}

function structuredClone(value, options) {
  if (arguments.length === 0) {
    throw typeError(
      "ERR_MISSING_ARGS",
      'The "The value argument must be specified" argument must be specified',
    );
  }
  const buffers = transferList(options);
  const copy = cloned(value, new Map());
  // a transferred buffer's bytes were copied above; only now, with the whole
  // clone made, does the original give them up
  for (const buffer of buffers) buffer.transfer(0);
  return copy;
}

module.exports = { structuredClone };
