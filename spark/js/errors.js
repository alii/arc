"use strict";

const { inspect } = native;

const classRegExp = /^[A-Z][a-zA-Z0-9]*$/;

const kTypes = [
  "string",
  "function",
  "number",
  "object",
  "Function",
  "Object",
  "boolean",
  "bigint",
  "symbol",
];

// libuv's message and, where linux and macos agree on it, the errno
const systemErrors = {
  EACCES: ["permission denied", -13],
  EAGAIN: ["resource temporarily unavailable"],
  EBADF: ["bad file descriptor", -9],
  EBUSY: ["resource busy or locked", -16],
  EEXIST: ["file already exists", -17],
  EINVAL: ["invalid argument", -22],
  EIO: ["i/o error", -5],
  EISDIR: ["illegal operation on a directory", -21],
  ELOOP: ["too many symbolic links encountered"],
  EMFILE: ["too many open files", -24],
  ENAMETOOLONG: ["name too long"],
  ENFILE: ["file table overflow", -23],
  ENOENT: ["no such file or directory", -2],
  ENOMEM: ["not enough memory", -12],
  ENOSPC: ["no space left on device", -28],
  ENOTDIR: ["not a directory", -20],
  ENOTEMPTY: ["directory not empty"],
  EPERM: ["operation not permitted", -1],
  EROFS: ["read-only file system", -30],
  ETXTBSY: ["text file is busy", -26],
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

const NodeURIError = nodeErrorClass(URIError);

function uriError(code, message) {
  return new NodeURIError(code, message);
}

function formatList(array, type) {
  switch (array.length) {
    case 0:
      return "";
    case 1:
      return `${array[0]}`;
    case 2:
      return `${array[0]} ${type} ${array[1]}`;
    case 3:
      return `${array[0]}, ${array[1]}, ${type} ${array[2]}`;
    default:
      return `${array.slice(0, -1).join(", ")}, ${type} ${array[array.length - 1]}`;
  }
}

function determineSpecificType(value) {
  if (value === null) {
    return "null";
  } else if (value === undefined) {
    return "undefined";
  }

  const type = typeof value;

  switch (type) {
    case "bigint":
      return `type bigint (${value}n)`;
    case "number":
      if (value === 0) {
        return 1 / value === -Infinity ? "type number (-0)" : "type number (0)";
      } else if (value !== value) {
        return "type number (NaN)";
      } else if (value === Infinity) {
        return "type number (Infinity)";
      } else if (value === -Infinity) {
        return "type number (-Infinity)";
      }
      return `type number (${value})`;
    case "boolean":
      return value ? "type boolean (true)" : "type boolean (false)";
    case "symbol":
      return `type symbol (${String(value)})`;
    case "function":
      return `function ${value.name}`;
    case "object":
      if (value.constructor && "name" in value.constructor) {
        return `an instance of ${value.constructor.name}`;
      }
      return `${inspect(value)}`;
    case "string":
      value.length > 28 && (value = `${value.slice(0, 25)}...`);
      if (value.indexOf("'") === -1) {
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

  let msg = "The ";
  if (name.endsWith(" argument")) {
    msg += `${name} `;
  } else {
    const type = name.includes(".") ? "property" : "argument";
    msg += `"${name}" ${type} `;
  }
  msg += "must be ";

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
    const pos = types.indexOf("object");
    if (pos !== -1) {
      types.splice(pos, 1);
      instances.push("Object");
    }
  }

  if (types.length > 0) {
    msg += `${types.length > 1 ? "one of type" : "of type"} ${formatList(types, "or")}`;
    if (instances.length > 0 || other.length > 0) msg += " or ";
  }

  if (instances.length > 0) {
    msg += `an instance of ${formatList(instances, "or")}`;
    if (other.length > 0) msg += " or ";
  }

  if (other.length > 0) {
    if (other.length > 1) {
      msg += `one of ${formatList(other, "or")}`;
    } else {
      if (other[0].toLowerCase() !== other[0]) msg += "an ";
      msg += `${other[0]}`;
    }
  }

  msg += `. Received ${determineSpecificType(actual)}`;

  return typeError("ERR_INVALID_ARG_TYPE", msg);
}

function invalidArgValue(name, value, reason = "is invalid") {
  let inspected = inspect(value);
  if (inspected.length > 128) {
    inspected = `${inspected.slice(0, 128)}...`;
  }
  const type = name.includes(".") ? "property" : "argument";
  return typeError(
    "ERR_INVALID_ARG_VALUE",
    `The ${type} '${name}' ${reason}. Received ${inspected}`,
  );
}

// a failed want as node reports a failed syscall
function systemError(code, syscall, path) {
  const known = systemErrors[code];
  let message = `${code}: ${known ? known[0] : "unknown error"}, ${syscall}`;
  if (path) message += ` '${path}'`;
  const error = new Error(message);
  if (known && known[1] !== undefined) error.errno = known[1];
  error.code = code;
  error.syscall = syscall;
  if (path) error.path = path;
  return error;
}

function addNumericalSeparator(val) {
  let res = "";
  let i = val.length;
  const start = val[0] === "-" ? 1 : 0;
  for (; i >= start + 4; i -= 3) {
    res = `_${val.slice(i - 3, i)}${res}`;
  }
  return `${val.slice(0, i)}${res}`;
}

function outOfRange(name, range, input, replaceDefaultBoolean = false) {
  let msg = replaceDefaultBoolean
    ? name
    : `The value of "${name}" is out of range.`;
  let received;
  if (Number.isInteger(input) && Math.abs(input) > 2 ** 32) {
    received = addNumericalSeparator(String(input));
  } else if (typeof input === "bigint") {
    received = String(input);
    const limit = BigInt(2) ** BigInt(32);
    if (input > limit || input < -limit) {
      received = addNumericalSeparator(received);
    }
    received += "n";
  } else {
    received = inspect(input);
  }
  msg += ` It must be ${range}. Received ${received}`;
  return rangeError("ERR_OUT_OF_RANGE", msg);
}

function validateInteger(
  value,
  name,
  min = Number.MIN_SAFE_INTEGER,
  max = Number.MAX_SAFE_INTEGER,
) {
  if (typeof value !== "number") throw invalidArgType(name, "number", value);
  if (!Number.isInteger(value)) throw outOfRange(name, "an integer", value);
  if (value < min || value > max) {
    throw outOfRange(name, `>= ${min} && <= ${max}`, value);
  }
}

function validateNumber(value, name, min = undefined, max) {
  if (typeof value !== "number") throw invalidArgType(name, "number", value);
  if (
    (min != null && value < min) ||
    (max != null && value > max) ||
    ((min != null || max != null) && Number.isNaN(value))
  ) {
    throw outOfRange(
      name,
      `${min != null ? `>= ${min}` : ""}${min != null && max != null ? " && " : ""}${max != null ? `<= ${max}` : ""}`,
      value,
    );
  }
}

function validateArray(value, name, minLength = 0) {
  if (!Array.isArray(value)) throw invalidArgType(name, "Array", value);
  if (value.length < minLength) {
    throw invalidArgValue(name, value, `must be longer than ${minLength}`);
  }
}

function validateBuffer(buffer, name = "buffer") {
  if (!ArrayBuffer.isView(buffer)) {
    throw invalidArgType(name, ["Buffer", "TypedArray", "DataView"], buffer);
  }
}

function validateFunction(value, name) {
  if (typeof value !== "function") {
    throw invalidArgType(name, "Function", value);
  }
}

function validateObject(value, name) {
  if (value === null || Array.isArray(value) || typeof value !== "object") {
    throw invalidArgType(name, "Object", value);
  }
}

function validateString(value, name) {
  if (typeof value !== "string") {
    throw invalidArgType(name, "string", value);
  }
}

module.exports = {
  error,
  typeError,
  rangeError,
  uriError,
  inspect,
  invalidArgType,
  invalidArgValue,
  outOfRange,
  determineSpecificType,
  systemError,
  validateArray,
  validateBuffer,
  validateInteger,
  validateNumber,
  validateFunction,
  validateObject,
  validateString,
};
