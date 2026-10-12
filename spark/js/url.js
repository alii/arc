"use strict";

// node's lib/internal/url.js and lib/url.js over a native parser. Not here:
// URLPattern, URL.createObjectURL and URL.revokeObjectURL, and the warning
// url.parse gives

const {
  typeError,
  invalidArgType,
  invalidArgValue,
  inspect: describe,
  markUncloneable,
  validateFunction,
  validateString,
} = require("spark:errors");
const path = require("node:path");
const { Buffer } = require("node:buffer");
const querystring = require("node:querystring");
const { encodeStr, hexTable, isHexTable } = require("spark:querystring");
const process = require("node:process");

const CHAR_TAB = 9;
const CHAR_LINE_FEED = 10;
const CHAR_CARRIAGE_RETURN = 13;
const CHAR_SPACE = 32;
const CHAR_DOUBLE_QUOTE = 34;
const CHAR_HASH = 35;
const CHAR_PERCENT = 37;
const CHAR_SINGLE_QUOTE = 39;
const CHAR_FORWARD_SLASH = 47;
const CHAR_COLON = 58;
const CHAR_SEMICOLON = 59;
const CHAR_LEFT_ANGLE_BRACKET = 60;
const CHAR_RIGHT_ANGLE_BRACKET = 62;
const CHAR_QUESTION_MARK = 63;
const CHAR_AT = 64;
const CHAR_LEFT_SQUARE_BRACKET = 91;
const CHAR_BACKWARD_SLASH = 92;
const CHAR_RIGHT_SQUARE_BRACKET = 93;
const CHAR_CIRCUMFLEX_ACCENT = 94;
const CHAR_GRAVE_ACCENT = 96;
const CHAR_LOWERCASE_A = 97;
const CHAR_LOWERCASE_Z = 122;
const CHAR_LEFT_CURLY_BRACKET = 123;
const CHAR_VERTICAL_LINE = 124;
const CHAR_RIGHT_CURLY_BRACKET = 125;
const CHAR_NO_BREAK_SPACE = 160;
const CHAR_ZERO_WIDTH_NOBREAK_SPACE = 65279;

const isWindows = process.platform === "win32";
const platform = process.platform;
const kEnumerableProperty = { __proto__: null, enumerable: true };
const kEmptyObject = Object.freeze({ __proto__: null });
const kValidateObjectAllowObjects = true;
const IteratorPrototype = Object.getPrototypeOf(
  Object.getPrototypeOf([][Symbol.iterator]()),
);

const inspect = Object.assign((value, options) => describe(value, options), {
  custom: Symbol.for("nodejs.util.inspect.custom"),
});

function removeColors(str) {
  return String(str).replace(/\u001b\[\d\d?m/g, "");
}

function getConstructorOf(obj) {
  while (obj) {
    const descriptor = Object.getOwnPropertyDescriptor(obj, "constructor");
    if (
      descriptor !== undefined &&
      typeof descriptor.value === "function" &&
      descriptor.value.name !== ""
    ) {
      return descriptor.value;
    }
    obj = Object.getPrototypeOf(obj);
  }
  return null;
}

function markTransferMode(object, cloneable) {
  if (!cloneable) markUncloneable(object);
}

function spliceOne(list, index) {
  for (; index + 1 < list.length; index++) list[index] = list[index + 1];
  list.pop();
}

// with allowObjects, arrays and functions pass too
function validateObject(value, name, allowObjects = false) {
  const fine = allowObjects
    ? value !== null &&
      (typeof value === "object" || typeof value === "function")
    : value !== null && !Array.isArray(value) && typeof value === "object";
  if (!fine) throw invalidArgType(name, "Object", value);
}

function isHexByte(byte) {
  return (
    (byte >= 0x30 && byte <= 0x39) ||
    (byte >= 0x41 && byte <= 0x46) ||
    (byte >= 0x61 && byte <= 0x66)
  );
}

// node's internal/data_url percentDecode
function percentDecode(input) {
  const length = input.length;
  const output = new Uint8Array(length);
  let j = 0;
  for (let i = 0; i < length; ++i) {
    const byte = input[i];
    if (byte !== 0x25) {
      output[j++] = byte;
    } else if (!(isHexByte(input[i + 1]) && isHexByte(input[i + 2]))) {
      output[j++] = 0x25;
    } else {
      output[j++] = Number.parseInt(
        String.fromCharCode(input[i + 1], input[i + 2]),
        16,
      );
      i += 2;
    }
  }
  return length === j ? output : output.subarray(0, j);
}

// called with `new` in the code below, as node's error classes are
function ERR_ARG_NOT_ITERABLE(name) {
  return typeError("ERR_ARG_NOT_ITERABLE", `${name} must be iterable`);
}

function ERR_INVALID_ARG_TYPE(name, expected, actual) {
  return invalidArgType(name, expected, actual);
}

function ERR_INVALID_ARG_VALUE(name, value, reason) {
  return invalidArgValue(name, value, reason);
}

function ERR_INVALID_FILE_URL_HOST(os) {
  return typeError(
    "ERR_INVALID_FILE_URL_HOST",
    `File URL host must be "localhost" or empty on ${os}`,
  );
}

function ERR_INVALID_FILE_URL_PATH(reason, input) {
  const error = typeError(
    "ERR_INVALID_FILE_URL_PATH",
    `File URL path ${reason}`,
  );
  if (input !== undefined) error.input = input;
  return error;
}

function ERR_INVALID_THIS(type) {
  return typeError(
    "ERR_INVALID_THIS",
    `Value of "this" must be of type ${type}`,
  );
}

function ERR_INVALID_TUPLE(name, kind) {
  return typeError(
    "ERR_INVALID_TUPLE",
    `${name} must be an iterable ${kind} tuple`,
  );
}

function ERR_INVALID_URL(input, base = null) {
  const error = typeError("ERR_INVALID_URL", "Invalid URL");
  error.input = input;
  if (base != null) error.base = base;
  return error;
}

function ERR_INVALID_URL_SCHEME(expected) {
  if (typeof expected === "string") expected = [expected];
  const res =
    expected.length === 2
      ? `one of scheme ${expected[0]} or ${expected[1]}`
      : `of scheme ${expected[0]}`;
  return typeError("ERR_INVALID_URL_SCHEME", `The URL must be ${res}`);
}

function ERR_MISSING_ARGS(...args) {
  let msg = "The ";
  const len = args.length;
  const wrap = (a) => `"${a}"`;
  args = args.map((a) =>
    Array.isArray(a) ? a.map(wrap).join(" or ") : wrap(a),
  );
  switch (len) {
    case 1:
      msg += `${args[0]} argument`;
      break;
    case 2:
      msg += `${args[0]} and ${args[1]} arguments`;
      break;
    default:
      msg += args.slice(0, len - 1).join(", ");
      msg += `, and ${args[len - 1]} arguments`;
      break;
  }
  return typeError("ERR_MISSING_ARGS", `${msg} must be specified`);
}

const kHostnameAction = 2;
const kPathnameAction = 6;

// what a file path has escaped before it becomes a url path
function escapeUnsafeChars(text, windows) {
  let out = "";
  for (const ch of text) {
    switch (ch) {
      case "\0":
        out += "%00";
        break;
      case "\t":
        out += "%09";
        break;
      case "\n":
        out += "%0A";
        break;
      case "\r":
        out += "%0D";
        break;
      case " ":
        out += "%20";
        break;
      case '"':
        out += "%22";
        break;
      case "#":
        out += "%23";
        break;
      case "%":
        out += "%25";
        break;
      case "?":
        out += "%3F";
        break;
      case "[":
        out += "%5B";
        break;
      case "\\":
        out += windows ? "/" : "%5C";
        break;
      case "]":
        out += "%5D";
        break;
      case "^":
        out += "%5E";
        break;
      case "|":
        out += "%7C";
        break;
      case "~":
        out += "%7E";
        break;
      default:
        out += ch;
    }
  }
  return out;
}

// node's native url binding, over the parser. A parse or an update leaves the
// positions of the url's parts in urlComponents
const bindingUrl = {
  urlComponents: [0, 0, 0, 0, 0, 0, 0, 0, 1],

  publish(result) {
    this.urlComponents = result[1];
    return result[0];
  },

  parse(input, base, raiseException) {
    const result = native.parse(input, base);
    if (result === undefined) {
      if (raiseException) throw new ERR_INVALID_URL(input, base);
      return undefined;
    }
    return this.publish(result);
  },

  canParse(input, base) {
    return native.canParse(input, base);
  },

  update(href, action, value) {
    const result = native.update(href, action, value);
    return result === undefined ? false : this.publish(result);
  },

  pathToFileURL(input, windows, hostname) {
    let result = native.parse("file://", undefined);
    if (windows && hostname !== undefined) {
      result = native.update(result[0], kHostnameAction, hostname) ?? result;
    }
    result =
      native.update(
        result[0],
        kPathnameAction,
        escapeUnsafeChars(input, windows),
      ) ?? result;
    return this.publish(result);
  },

  // a url as text with parts left out, and the host as unicode on request
  format(href, fragment, unicode, search, auth) {
    let result = native.parse(href, undefined);
    const drop = (action) => {
      result = native.update(result[0], action, "") ?? result;
    };
    if (!fragment) drop(8);
    if (!search) drop(7);
    if (!auth) {
      drop(4);
      drop(5);
    }
    const [text, positions] = result;
    if (!unicode) return text;
    const start = positions[2] + (text[positions[2]] === "@" ? 1 : 0);
    return (
      text.slice(0, start) +
      native.domainToUnicode(text.slice(start, positions[3])) +
      text.slice(positions[3])
    );
  },

  getOrigin(url) {
    return new URL(url).origin;
  },

  domainToASCII: native.domainToASCII,
  domainToUnicode: native.domainToUnicode,
  toASCII: native.toASCII,
};

const FORWARD_SLASH = /\//g;

const contextForInspect = Symbol("context");

const unsafeProtocol = new Set([
  "javascript",
  "javascript:",
]);
const hostlessProtocol = new Set([
  "javascript",
  "javascript:",
]);
const slashedProtocol = new Set([
  "http",
  "http:",
  "https",
  "https:",
  "ftp",
  "ftp:",
  "gopher",
  "gopher:",
  "file",
  "file:",
  "ws",
  "ws:",
  "wss",
  "wss:",
]);

const updateActions = {
  kProtocol: 0,
  kHost: 1,
  kHostname: 2,
  kPort: 3,
  kUsername: 4,
  kPassword: 5,
  kPathname: 6,
  kSearch: 7,
  kHash: 8,
  kHref: 9,
};

function setURLContextFromBinding(ctx, href) {
  const c = bindingUrl.urlComponents;
  ctx.href = href;
  ctx.protocol_end = c[0];
  ctx.username_end = c[1];
  ctx.host_start = c[2];
  ctx.host_end = c[3];
  ctx.pathname_start = c[5];
  ctx.search_start = c[6];
  ctx.hash_start = c[7];
  ctx.port = c[4];
  ctx.scheme_type = c[8];
}

class URLContext {
  static #omitted = 4294967295;

    constructor(href) {
    if (href === undefined) {
      this.href = "";
      this.protocol_end = 0;
      this.username_end = 0;
      this.host_start = 0;
      this.host_end = 0;
      this.pathname_start = 0;
      this.search_start = 0;
      this.hash_start = 0;
      this.port = 0;
      this.scheme_type = 1;
      return;
    }
    setURLContextFromBinding(this, href);
  }

  get hasPort() {
    return this.port !== URLContext.#omitted;
  }

  get hasSearch() {
    return this.search_start !== URLContext.#omitted;
  }

  get hasHash() {
    return this.hash_start !== URLContext.#omitted;
  }
}

let setURLSearchParamsModified;
let setURLSearchParamsContext;
let getURLSearchParamsList;
let setURLSearchParams;
let isURLInstance;
let getURLHref;

class URLSearchParamsIterator {
  #target;
  #kind;
  #index;

  constructor(target, kind) {
    this.#target = target;
    this.#kind = kind;
    this.#index = 0;
  }

  next() {
    if (typeof this !== "object" || this === null || !(#target in this))
      throw new ERR_INVALID_THIS("URLSearchParamsIterator");

    const index = this.#index;
    const values = getURLSearchParamsList(this.#target);
    const len = values.length;
    if (index >= len) {
      return {
        done: true,
        value: undefined,
      };
    }

    const name = values[index];
    const value = values[index + 1];
    this.#index = index + 2;

    let result;
    if (this.#kind === "key") {
      result = name;
    } else if (this.#kind === "value") {
      result = value;
    } else {
      result = [name, value];
    }

    return {
      done: false,
      value: result,
    };
  }

  [inspect.custom](recurseTimes, ctx) {
    if (!this || typeof this !== "object" || !(#target in this))
      throw new ERR_INVALID_THIS("URLSearchParamsIterator");

    if (typeof recurseTimes === "number" && recurseTimes < 0)
      return ctx.stylize("[Object]", "special");

    const innerOpts = { ...ctx };
    if (recurseTimes !== null) {
      innerOpts.depth = recurseTimes - 1;
    }
    const index = this.#index;
    const values = getURLSearchParamsList(this.#target);
    const output = [];
    for (let i = index; i < values.length; i++) {
      const isKey = ((i - index) % 2) === 0;
      if (this.#kind === "key") {
        if (isKey) output.push(values[i]);
      } else if (!isKey) {
        output.push(this.#kind === "value" ? values[i] : [values[i - 1], values[i]]);
      }
    }
    const hasBreak = inspect(output, innerOpts).includes("\n");
    const outputStrs = output.map((p) => inspect(p, innerOpts));
    let outputStr;
    if (hasBreak) {
      outputStr = `\n  ${outputStrs.join(",\n  ")}`;
    } else {
      outputStr = ` ${outputStrs.join(", ")}`;
    }
    return `${this[Symbol.toStringTag]} {${outputStr} }`;
  }
}

delete URLSearchParamsIterator.prototype.constructor;
Object.setPrototypeOf(URLSearchParamsIterator.prototype, IteratorPrototype);

Object.defineProperties(URLSearchParamsIterator.prototype, {
  [Symbol.toStringTag]: { __proto__: null, configurable: true, value: "URLSearchParams Iterator" },
  next: kEnumerableProperty,
});

class URLSearchParams {
  #searchParams = [];

  #context;

  #serialized;

  static {
    setURLSearchParamsContext = (obj, ctx) => {
      obj.#context = ctx;
    };
    getURLSearchParamsList = (obj) => obj.#searchParams;
    setURLSearchParams = (obj, query) => {
      obj.#serialized = undefined;
      if (query === undefined) {
        obj.#searchParams = [];
      } else {
        obj.#searchParams = parseParams(query);
      }
    };
  }

  #markUpdated() {
    this.#serialized = undefined;
    if (this.#context) {
      setURLSearchParamsModified(this.#context);
    }
  }

  constructor(init = undefined) {
    markTransferMode(this, false, false);

    if (init === undefined) {
    } else if (init !== null && (typeof init === "object" || typeof init === "function")) {
      const method = init[Symbol.iterator];
      if (method === this[Symbol.iterator] && #searchParams in init) {
        const childParams = init.#searchParams;
        this.#searchParams = childParams.slice();
        this.#serialized = init.#serialized;
      } else if (method != null) {
        if (typeof method !== "function") {
          throw new ERR_ARG_NOT_ITERABLE("Query pairs");
        }

        for (const pair of init) {
          if (pair == null) {
            throw new ERR_INVALID_TUPLE("Each query pair", "[name, value]");
          } else if (Array.isArray(pair)) {
            if (pair.length !== 2) {
              throw new ERR_INVALID_TUPLE("Each query pair", "[name, value]");
            }
            this.#searchParams.push(toUSVString(pair[0]), toUSVString(pair[1]));
          } else {
            if (((typeof pair !== "object" && typeof pair !== "function") ||
                typeof pair[Symbol.iterator] !== "function")) {
              throw new ERR_INVALID_TUPLE("Each query pair", "[name, value]");
            }

            let length = 0;

            for (const element of pair) {
              length++;
              this.#searchParams.push(toUSVString(element));
            }

            if (length !== 2) {
              throw new ERR_INVALID_TUPLE("Each query pair", "[name, value]");
            }
          }
        }
      } else {
        let visited;
        const keys = Reflect.ownKeys(init);
        for (let i = 0; i < keys.length; i++) {
          const key = keys[i];
          const desc = Reflect.getOwnPropertyDescriptor(init, key);
          if (desc !== undefined && desc.enumerable) {
            const typedKey = toUSVString(key);
            const typedValue = toUSVString(init[key]);

            if (visited === undefined && typedKey === key) {
              this.#searchParams.push(typedKey, typedValue);
              continue;
            }

            if (visited === undefined) {
              visited = new Map();
              for (let j = 0; j < this.#searchParams.length; j += 2) {
                visited.set(this.#searchParams[j], j + 1);
              }
            }

            const keyIdx = visited.get(typedKey);
            if (keyIdx !== undefined) {
              this.#searchParams[keyIdx] = typedValue;
            } else {
              visited.set(typedKey, this.#searchParams.push(typedKey, typedValue) - 1);
            }
          }
        }
      }
    } else {
      init = toUSVString(init);
      this.#searchParams = init ? parseParams(init) : [];
    }
  }

  [inspect.custom](recurseTimes, ctx) {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    if (typeof recurseTimes === "number" && recurseTimes < 0)
      return ctx.stylize("[Object]", "special");

    const separator = ", ";
    const innerOpts = { ...ctx };
    if (recurseTimes !== null) {
      innerOpts.depth = recurseTimes - 1;
    }
    const innerInspect = (v) => inspect(v, innerOpts);

    const list = this.#searchParams;
    const output = [];
    for (let i = 0; i < list.length; i += 2)
      output.push(`${innerInspect(list[i])} => ${innerInspect(list[i + 1])}`);

    let length = -separator.length;
    for (let i = 0; i < output.length; i++) {
      length += removeColors(output[i]).length + separator.length;
    }
    if (length > ctx.breakLength) {
      return `${this.constructor.name} {\n` +
      `  ${output.join(",\n  ")} }`;
    } else if (output.length) {
      return `${this.constructor.name} { ` +
      `${output.join(separator)} }`;
    }
    return `${this.constructor.name} {}`;
  }

  get size() {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");
    return this.#searchParams.length / 2;
  }

  append(name, value) {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    if (arguments.length < 2) {
      throw new ERR_MISSING_ARGS("name", "value");
    }

    name = toUSVString(name);
    value = toUSVString(value);
    this.#searchParams.push(name, value);
    this.#markUpdated();
  }

  delete(name, value = undefined) {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    if (arguments.length < 1) {
      throw new ERR_MISSING_ARGS("name");
    }

    const list = this.#searchParams;
    name = toUSVString(name);
    const { length } = list;
    let write = 0;

    if (value !== undefined) {
      value = toUSVString(value);
      for (let i = 0; i < length; i += 2) {
        if (list[i] === name && list[i + 1] === value) {
          continue;
        }
        if (write !== i) {
          list[write] = list[i];
          list[write + 1] = list[i + 1];
        }
        write += 2;
      }
    } else {
      for (let i = 0; i < length; i += 2) {
        if (list[i] === name) {
          continue;
        }
        if (write !== i) {
          list[write] = list[i];
          list[write + 1] = list[i + 1];
        }
        write += 2;
      }
    }

    if (write !== length) {
      list.length = write;
      this.#serialized = undefined;
    }

    if (this.#context) {
      setURLSearchParamsModified(this.#context);
    }
  }

  get(name) {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    if (arguments.length < 1) {
      throw new ERR_MISSING_ARGS("name");
    }

    const list = this.#searchParams;
    name = toUSVString(name);
    const { length } = list;
    for (let i = 0; i < length; i += 2) {
      if (list[i] === name) {
        return list[i + 1];
      }
    }
    return null;
  }

  getAll(name) {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    if (arguments.length < 1) {
      throw new ERR_MISSING_ARGS("name");
    }

    const list = this.#searchParams;
    const values = [];
    name = toUSVString(name);
    const { length } = list;
    for (let i = 0; i < length; i += 2) {
      if (list[i] === name) {
        values.push(list[i + 1]);
      }
    }
    return values;
  }

  has(name, value = undefined) {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    if (arguments.length < 1) {
      throw new ERR_MISSING_ARGS("name");
    }

    const list = this.#searchParams;
    name = toUSVString(name);

    if (value !== undefined) {
      value = toUSVString(value);
    }

    const { length } = list;
    for (let i = 0; i < length; i += 2) {
      if (list[i] === name) {
        if (value === undefined || list[i + 1] === value) {
          return true;
        }
      }
    }

    return false;
  }

  set(name, value) {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    if (arguments.length < 2) {
      throw new ERR_MISSING_ARGS("name", "value");
    }

    const list = this.#searchParams;
    name = toUSVString(name);
    value = toUSVString(value);
    const { length } = list;

    let found = false;
    let write = 0;
    for (let i = 0; i < length; i += 2) {
      const cur = list[i];
      let keep = true;
      if (cur === name) {
        if (!found) {
          list[write] = cur;
          list[write + 1] = value;
          found = true;
        } else {
          keep = false;
        }
      } else if (write !== i) {
        list[write] = cur;
        list[write + 1] = list[i + 1];
      }
      if (keep)
        write += 2;
    }

    if (found && write !== length) {
      list.length = write;
    }

    if (!found) {
      list.push(name, value);
    }

    this.#markUpdated();
  }

  sort() {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    const a = this.#searchParams;
    const len = a.length;

    if (len <= 2) {
    } else if (len < 100) {
      for (let i = 2; i < len; i += 2) {
        const curKey = a[i];
        const curVal = a[i + 1];
        let j;
        for (j = i - 2; j >= 0; j -= 2) {
          if (a[j] > curKey) {
            a[j + 2] = a[j];
            a[j + 3] = a[j + 1];
          } else {
            break;
          }
        }
        a[j + 2] = curKey;
        a[j + 3] = curVal;
      }
    } else {
      const lBuffer = new Array(len);
      const rBuffer = new Array(len);
      for (let step = 2; step < len; step *= 2) {
        for (let start = 0; start < len - 2; start += 2 * step) {
          const mid = start + step;
          let end = mid + step;
          end = end < len ? end : len;
          if (mid > end)
            continue;
          merge(a, start, mid, end, lBuffer, rBuffer);
        }
      }
    }

    this.#markUpdated();
  }

  entries() {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    return new URLSearchParamsIterator(this, "key+value");
  }

  forEach(callback, thisArg = undefined) {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    validateFunction(callback, "callback");

    let list = this.#searchParams;

    let i = 0;
    while (i < list.length) {
      const key = list[i];
      const value = list[i + 1];
      callback.call(thisArg, value, key, this);
      list = this.#searchParams;
      i += 2;
    }
  }

  keys() {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    return new URLSearchParamsIterator(this, "key");
  }

  values() {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    return new URLSearchParamsIterator(this, "value");
  }

  toString() {
    if (typeof this !== "object" || this === null || !(#searchParams in this))
      throw new ERR_INVALID_THIS("URLSearchParams");

    if (this.#serialized !== undefined) {
      return this.#serialized;
    }
    const serialized = serializeParams(this.#searchParams);
    this.#serialized = serialized;
    return serialized;
  }
}

Object.defineProperties(URLSearchParams.prototype, {
  append: kEnumerableProperty,
  delete: kEnumerableProperty,
  get: kEnumerableProperty,
  getAll: kEnumerableProperty,
  has: kEnumerableProperty,
  set: kEnumerableProperty,
  size: kEnumerableProperty,
  sort: kEnumerableProperty,
  entries: kEnumerableProperty,
  forEach: kEnumerableProperty,
  keys: kEnumerableProperty,
  values: kEnumerableProperty,
  toString: kEnumerableProperty,
  [Symbol.toStringTag]: { __proto__: null, configurable: true, value: "URLSearchParams" },

  [Symbol.iterator]: {
    __proto__: null,
    configurable: true,
    writable: true,
    value: URLSearchParams.prototype.entries,
  },
});

function isURL(self) {
  return Boolean(self?.href && self.protocol && self.auth === undefined && self.path === undefined);
}

const kParseURLSymbol = Symbol("kParseURL");
const kCreateURLFromPosixPathSymbol = Symbol("kCreateURLFromPosixPath");
const kCreateURLFromWindowsPathSymbol = Symbol("kCreateURLFromWindowsPath");

class URL {
  #context;
  #searchParams;
  #searchParamsModified;

  static {
    isURLInstance = (value) => typeof value === "object" && value !== null && #context in value;

    getURLHref = (value) => {
      value.#ensureSearchParamsUpdated();
      return value.#context.href;
    };

    setURLSearchParamsModified = (obj) => {
      obj.#searchParamsModified = true;

      if (obj.#context.hasSearch) {
        obj.#updateContext(bindingUrl.update(obj.#context.href, updateActions.kSearch, ""));
      }
    };
  }

  constructor(input, base = undefined, parseSymbol = undefined) {
    if (arguments.length === 0) {
      throw new ERR_MISSING_ARGS("url");
    }

    if (typeof input !== "string") {
      input = `${input}`;
    }

    if (base !== undefined && typeof base !== "string") {
      base = `${base}`;
    }

    let href;
    if (arguments.length < 3) {
      href = bindingUrl.parse(input, base, true);
    } else {
      const raiseException = parseSymbol !== kParseURLSymbol;
      const interpretAsWindowsPath = parseSymbol === kCreateURLFromWindowsPathSymbol;
      const pathToFileURL = interpretAsWindowsPath || (parseSymbol === kCreateURLFromPosixPathSymbol);
      href = pathToFileURL ?
        bindingUrl.pathToFileURL(input, interpretAsWindowsPath, base) :
        bindingUrl.parse(input, base, raiseException);
    }

    this.#context = href ? new URLContext(href) : new URLContext();
    markTransferMode(this, false, false);
  }

  static parse(input, base = undefined) {
    if (arguments.length === 0) {
      throw new ERR_MISSING_ARGS("url");
    }
    const parsedURLObject = new URL(input, base, kParseURLSymbol);
    return parsedURLObject.href ? parsedURLObject : null;
  }

  [inspect.custom](depth, opts) {
    if (typeof depth === "number" && depth < 0)
      return this;

    const constructor = getConstructorOf(this) || URL;
    const obj = { __proto__: { constructor } };

    obj.href = this.href;
    obj.origin = this.origin;
    obj.protocol = this.protocol;
    obj.username = this.username;
    obj.password = this.password;
    obj.host = this.host;
    obj.hostname = this.hostname;
    obj.port = this.port;
    obj.pathname = this.pathname;
    obj.search = this.search;
    obj.searchParams = this.searchParams;
    obj.hash = this.hash;

    if (opts.showHidden) {
      obj[contextForInspect] = this.#context;
    }

    return `${constructor.name} ${inspect(obj, opts)}`;
  }

  #getSearchFromContext() {
    if (!this.#context.hasSearch) return "";
    let endsAt = this.#context.href.length;
    if (this.#context.hasHash) endsAt = this.#context.hash_start;
    if (endsAt - this.#context.search_start <= 1) return "";
    return this.#context.href.slice(this.#context.search_start, endsAt);
  }

  #getSearchFromParams() {
    if (!this.#searchParams?.size) return "";
    return `?${this.#searchParams}`;
  }

  #ensureSearchParamsUpdated() {
    if (this.#searchParamsModified) {
      this.#searchParamsModified = false;
      this.#updateContext(bindingUrl.update(this.#context.href, updateActions.kSearch, this.#getSearchFromParams()));
    }
  }

    #updateContext(href, shouldUpdateSearchParams = false) {
    const previousSearch = shouldUpdateSearchParams && this.#searchParams &&
      (this.#searchParamsModified ? this.#getSearchFromParams() : this.#getSearchFromContext());

    setURLContextFromBinding(this.#context, href);

    if (this.#searchParams) {
      if (shouldUpdateSearchParams) {
        const currentSearch = this.#getSearchFromContext();
        if (previousSearch !== currentSearch) {
          setURLSearchParams(this.#searchParams, currentSearch);
          this.#searchParamsModified = false;
        }
      }

      this.#ensureSearchParamsUpdated();
    }
  }

  toString() {
    this.#ensureSearchParamsUpdated();
    return this.#context.href;
  }

  get href() {
    this.#ensureSearchParamsUpdated();
    return this.#context.href;
  }

  set href(value) {
    value = `${value}`;
    const href = bindingUrl.update(this.#context.href, updateActions.kHref, value);
    if (!href) { throw new ERR_INVALID_URL(value); }
    this.#updateContext(href, true);
  }

  get origin() {
    const protocol = this.#context.href.slice(0, this.#context.protocol_end);

    if (this.#context.scheme_type !== 1) {
      if (this.#context.scheme_type === 6) {
        return "null";
      }
      return `${protocol}//${this.host}`;
    }

    if (protocol === "blob:") {
      const path = this.pathname;
      if (path.length > 0) {
        try {
          const out = new URL(path);
          if (out.#context.scheme_type === 0 || out.#context.scheme_type === 2) {
            return `${out.protocol}//${out.host}`;
          }
        } catch {
        }
      }
    }

    return "null";
  }

  get protocol() {
    return this.#context.href.slice(0, this.#context.protocol_end);
  }

  set protocol(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kProtocol, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get username() {
    if (this.#context.protocol_end + 2 < this.#context.username_end) {
      return this.#context.href.slice(this.#context.protocol_end + 2, this.#context.username_end);
    }
    return "";
  }

  set username(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kUsername, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get password() {
    if (this.#context.host_start - this.#context.username_end > 0) {
      return this.#context.href.slice(this.#context.username_end + 1, this.#context.host_start);
    }
    return "";
  }

  set password(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kPassword, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get host() {
    let startsAt = this.#context.host_start;
    if (this.#context.href[startsAt] === "@") {
      startsAt++;
    }
    if (startsAt === this.#context.host_end) {
      return "";
    }
    return this.#context.href.slice(startsAt, this.#context.pathname_start);
  }

  set host(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kHost, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get hostname() {
    let startsAt = this.#context.host_start;
    if (this.#context.href[startsAt] === "@") {
      startsAt++;
    }
    return this.#context.href.slice(startsAt, this.#context.host_end);
  }

  set hostname(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kHostname, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get port() {
    if (this.#context.hasPort) {
      return `${this.#context.port}`;
    }
    return "";
  }

  set port(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kPort, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get pathname() {
    let endsAt;
    if (this.#context.hasSearch) {
      endsAt = this.#context.search_start;
    } else if (this.#context.hasHash) {
      endsAt = this.#context.hash_start;
    }
    return this.#context.href.slice(this.#context.pathname_start, endsAt);
  }

  set pathname(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kPathname, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  get search() {
    this.#ensureSearchParamsUpdated();
    return this.#getSearchFromContext();
  }

  set search(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kSearch, `${value}`.toWellFormed());
    if (href) {
      this.#updateContext(href, true);
    }
  }

  get searchParams() {
    if (this.#searchParams == null) {
      this.#searchParams = new URLSearchParams(this.#getSearchFromContext());
      setURLSearchParamsContext(this.#searchParams, this);
      this.#searchParamsModified = false;
    }
    return this.#searchParams;
  }

  get hash() {
    if (!this.#context.hasHash || (this.#context.href.length - this.#context.hash_start <= 1)) {
      return "";
    }
    return this.#context.href.slice(this.#context.hash_start);
  }

  set hash(value) {
    const href = bindingUrl.update(this.#context.href, updateActions.kHash, `${value}`);
    if (href) {
      this.#updateContext(href);
    }
  }

  toJSON() {
    this.#ensureSearchParamsUpdated();
    return this.#context.href;
  }

  static canParse(url, base = undefined) {
    if (arguments.length === 0) {
      throw new ERR_MISSING_ARGS("url");
    }

    if (typeof url !== "string") {
      url = `${url}`;
    }

    if (base !== undefined) {
      return bindingUrl.canParse(url, typeof base === "string" ? base : `${base}`);
    }

    return bindingUrl.canParse(url);
  }
}

Object.defineProperties(URL.prototype, {
  [Symbol.toStringTag]: { __proto__: null, configurable: true, value: "URL" },
  toString: kEnumerableProperty,
  href: kEnumerableProperty,
  origin: kEnumerableProperty,
  protocol: kEnumerableProperty,
  username: kEnumerableProperty,
  password: kEnumerableProperty,
  host: kEnumerableProperty,
  hostname: kEnumerableProperty,
  port: kEnumerableProperty,
  pathname: kEnumerableProperty,
  search: kEnumerableProperty,
  searchParams: kEnumerableProperty,
  hash: kEnumerableProperty,
  toJSON: kEnumerableProperty,
});

Object.defineProperties(URL, {
  canParse: {
    __proto__: null,
    configurable: true,
    writable: true,
    enumerable: true,
  },
  parse: {
    __proto__: null,
    configurable: true,
    writable: true,
    enumerable: true,
  },
});

function toUSVString(value) {
  return typeof value === "string" ?
    value.toWellFormed() :
    `${value}`.toWellFormed();
}

function unescapeFormComponent(s) {
  try {
    return decodeURIComponent(s);
  } catch {
    return querystring.unescapeBuffer(s).toString();
  }
}

function hasPercentHex(s) {
  const end = s.length - 2;
  for (let i = 0; i < end; i++) {
    if (s.charCodeAt(i) === 37 && // "%"
        isHexTable[s.charCodeAt(i + 1)] === 1 &&
        isHexTable[s.charCodeAt(i + 2)] === 1) {
      return true;
    }
  }
  return false;
}

function decodeFormComponent(qs, start, end) {
  if (start >= end) {
    return "";
  }
  const s = qs.slice(start, end);
  const plus = s.indexOf("+");
  const pct = s.indexOf("%");
  if (plus === -1 && pct === -1) {
    return s;
  }
  const replaced = plus === -1 ? s : s.replaceAll("+", " ");
  if (pct === -1 || !hasPercentHex(replaced)) {
    return replaced;
  }
  return unescapeFormComponent(replaced);
}

function parseParams(qs) {
  const len = qs.length;
  let i = qs[0] === "?" ? 1 : 0;
  if (i >= len) {
    return [];
  }

  const out = [];
  const encoded = qs.indexOf("+", i) !== -1 || qs.indexOf("%", i) !== -1;
  while (i < len) {
    let amp = qs.indexOf("&", i);
    if (amp === -1) {
      amp = len;
    }
    if (amp !== i) {
      const eq = qs.indexOf("=", i);
      if (eq === -1 || eq > amp) {
        out.push(encoded ? decodeFormComponent(qs, i, amp) : qs.slice(i, amp), "");
      } else {
        out.push(
          encoded ? decodeFormComponent(qs, i, eq) : qs.slice(i, eq),
          encoded ? decodeFormComponent(qs, eq + 1, amp) : qs.slice(eq + 1, amp),
        );
      }
    }
    i = amp + 1;
  }
  return out;
}

const noEscape = new Int8Array([
/*
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
*/
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x00 - 0x0F
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x10 - 0x1F
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, // 0x20 - 0x2F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, // 0x30 - 0x3F
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x40 - 0x4F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, // 0x50 - 0x5F
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x60 - 0x6F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,  // 0x70 - 0x7F
]);

const paramHexTable = hexTable.slice();
paramHexTable[0x20] = "+";

function serializeParams(array) {
  const len = array.length;
  if (len === 0)
    return "";

  if (len === 2) {
    return encodeStr(array[0], noEscape, paramHexTable) + "=" +
           encodeStr(array[1], noEscape, paramHexTable);
  }

  const pairs = new Array(len / 2);
  for (let i = 0, j = 0; i < len; i += 2, ++j) {
    pairs[j] = encodeStr(array[i], noEscape, paramHexTable) + "=" +
               encodeStr(array[i + 1], noEscape, paramHexTable);
  }
  return pairs.join("&");
}

function merge(out, start, mid, end, lBuffer, rBuffer) {
  const sizeLeft = mid - start;
  const sizeRight = end - mid;
  let l, r, o;

  for (l = 0; l < sizeLeft; l++)
    lBuffer[l] = out[start + l];
  for (r = 0; r < sizeRight; r++)
    rBuffer[r] = out[mid + r];

  l = 0;
  r = 0;
  o = start;
  while (l < sizeLeft && r < sizeRight) {
    if (lBuffer[l] <= rBuffer[r]) {
      out[o++] = lBuffer[l++];
      out[o++] = lBuffer[l++];
    } else {
      out[o++] = rBuffer[r++];
      out[o++] = rBuffer[r++];
    }
  }
  while (l < sizeLeft)
    out[o++] = lBuffer[l++];
  while (r < sizeRight)
    out[o++] = rBuffer[r++];
}

function domainToASCII(domain) {
  if (arguments.length < 1)
    throw new ERR_MISSING_ARGS("domain");

  return bindingUrl.domainToASCII(`${domain}`);
}

function domainToUnicode(domain) {
  if (arguments.length < 1)
    throw new ERR_MISSING_ARGS("domain");

  return bindingUrl.domainToUnicode(`${domain}`);
}

function urlToHttpOptions(url) {
  validateObject(url, "url", kValidateObjectAllowObjects);
  const { hostname, pathname, port, username, password, search } = url;
  const options = {
    __proto__: null,
    ...url, // In case the url object was extended by the user.
    protocol: url.protocol,
    hostname: hostname && hostname[0] === "[" ?
      hostname.slice(1, -1) :
      hostname,
    hash: url.hash,
    search: search,
    pathname: pathname,
    path: `${pathname || ""}${search || ""}`,
    href: url.href,
  };
  if (port !== "") {
    options.port = Number(port);
  }
  if (username || password) {
    options.auth = `${decodeURIComponent(username)}:${decodeURIComponent(password)}`;
  }
  return options;
}

function getPathFromURLWin32(url) {
  const hostname = url.hostname;
  let pathname = url.pathname;
  for (let n = 0; n < pathname.length; n++) {
    if (pathname[n] === "%") {
      const third = pathname.codePointAt(n + 2) | 0x20;
      if ((pathname[n + 1] === "2" && third === 102) || // 2f 2F /
          (pathname[n + 1] === "5" && third === 99)) {  // 5c 5C \
        throw new ERR_INVALID_FILE_URL_PATH(
          "must not include encoded \\ or / characters", url,
        );
      }
    }
  }
  pathname = pathname.replace(FORWARD_SLASH, "\\");
  if (pathname.includes("%")) {
    pathname = decodeURIComponent(pathname);
  }
  if (hostname !== "") {
    return `\\\\${domainToUnicode(hostname)}${pathname}`;
  }
  const letter = pathname.codePointAt(1) | 0x20;
  const sep = pathname.charAt(2);
  if (letter < CHAR_LOWERCASE_A || letter > CHAR_LOWERCASE_Z ||   // a..z A..Z
      (sep !== ":")) {
    throw new ERR_INVALID_FILE_URL_PATH("must be absolute", url);
  }
  return pathname.slice(1);
}

function getPathBufferFromURLWin32(url) {
  const hostname = url.hostname;
  let pathname = url.pathname;

  pathname = pathname.replace(FORWARD_SLASH, "\\");

  const decodedu8 = percentDecode(Buffer.from(pathname, "utf8"));
  const decodedPathname = Buffer.from(decodedu8.buffer,
                                      decodedu8.byteOffset,
                                      decodedu8.byteLength);
  if (hostname !== "") {

    const prefix = Buffer.from("\\\\", "ascii");
    const domain = Buffer.from(domainToUnicode(hostname), "utf8");

    return Buffer.concat([prefix, domain, decodedPathname]);
  }

  const letter = decodedPathname[1] | 0x20;
  const sep = decodedPathname[2];

  if (letter < CHAR_LOWERCASE_A || letter > CHAR_LOWERCASE_Z ||   // a..z A..Z
      (sep !== CHAR_COLON)) {
    throw new ERR_INVALID_FILE_URL_PATH("must be absolute", url);
  }

  return decodedPathname.subarray(1);
}

function getPathFromURLPosix(url) {
  if (url.hostname !== "") {
    throw new ERR_INVALID_FILE_URL_HOST(platform);
  }
  const pathname = url.pathname;
  for (let n = 0; n < pathname.length; n++) {
    if (pathname[n] === "%") {
      const third = pathname.codePointAt(n + 2) | 0x20;
      if (pathname[n + 1] === "2" && third === 102) {
        throw new ERR_INVALID_FILE_URL_PATH(
          "must not include encoded / characters",
          url,
        );
      }
    }
  }
  return pathname.includes("%") ? decodeURIComponent(pathname) : pathname;
}

function getPathBufferFromURLPosix(url) {
  if (url.hostname !== "") {
    throw new ERR_INVALID_FILE_URL_HOST(platform);
  }
  const pathname = url.pathname;

  const u8 = percentDecode(Buffer.from(pathname, "utf8"));
  return Buffer.from(u8.buffer,
                     u8.byteOffset,
                     u8.byteLength);
}

function fileURLToPath(path, options = kEmptyObject) {
  const windows = options?.windows;
  if (typeof path === "string")
    path = new URL(path);
  else if (!isURL(path))
    throw new ERR_INVALID_ARG_TYPE("path", ["string", "URL"], path);
  if (path.protocol !== "file:")
    throw new ERR_INVALID_URL_SCHEME("file");
  return (windows ?? isWindows) ? getPathFromURLWin32(path) : getPathFromURLPosix(path);
}

function fileURLToPathBuffer(path, options = kEmptyObject) {
  const windows = options?.windows;
  if (typeof path === "string") {
    path = new URL(path);
  } else if (!isURL(path)) {
    throw new ERR_INVALID_ARG_TYPE("path", ["string", "URL"], path);
  }
  if (path.protocol !== "file:") {
    throw new ERR_INVALID_URL_SCHEME("file");
  }
  return (windows ?? isWindows) ? getPathBufferFromURLWin32(path) : getPathBufferFromURLPosix(path);
}

function _pathToFileURL(filepath, options = kEmptyObject) {
  const windows = options?.windows ?? isWindows;
  const isUNC = windows && filepath.startsWith("\\\\");
  let resolved = isUNC ?
    filepath :
    (windows ? path.win32.resolve(filepath) : path.posix.resolve(filepath));
  if (isUNC || (windows && resolved.startsWith("\\\\"))) {
    const isExtendedUNC = resolved.startsWith("\\\\?\\UNC\\");
    const prefixLength = isExtendedUNC ? 8 : 2;
    const hostnameEndIndex = resolved.indexOf("\\", prefixLength);
    if (hostnameEndIndex === -1) {
      throw new ERR_INVALID_ARG_VALUE(
        "path",
        resolved,
        "Missing UNC resource path",
      );
    }
    if (hostnameEndIndex === 2) {
      throw new ERR_INVALID_ARG_VALUE(
        "path",
        resolved,
        "Empty UNC servername",
      );
    }
    const hostname = resolved.slice(prefixLength, hostnameEndIndex);
    return new URL(resolved.slice(hostnameEndIndex), hostname, kCreateURLFromWindowsPathSymbol);
  }
  const filePathLast = filepath.charCodeAt(filepath.length - 1);
  if ((filePathLast === CHAR_FORWARD_SLASH ||
       ((windows ?? isWindows) && filePathLast === CHAR_BACKWARD_SLASH)) &&
      resolved[resolved.length - 1] !== path.sep)
    resolved += "/";

  return new URL(resolved, undefined, windows ? kCreateURLFromWindowsPathSymbol : kCreateURLFromPosixPathSymbol);
}

function toPathIfFileURL(fileURLOrPath) {
  if (!isURL(fileURLOrPath))
    return fileURLOrPath;
  return fileURLToPath(fileURLOrPath);
}

function getURLOrigin(url) {
  return bindingUrl.getOrigin(url);
}

function Url() {
  this.protocol = null;
  this.slashes = null;
  this.auth = null;
  this.host = null;
  this.port = null;
  this.hostname = null;
  this.hash = null;
  this.search = null;
  this.query = null;
  this.pathname = null;
  this.path = null;
  this.href = null;
}

const protocolPattern = /^[a-z0-9.+-]+:/i;
const portPattern = /:[0-9]*$/;
const hostPattern = /^\/\/[^@/]+@[^@/]+/;

const simplePathPattern = /^(\/\/?(?!\/)[^?\s]*)(\?[^\s]*)?$/;

const hostnameMaxLen = 255;
function urlParse(url, parseQueryString, slashesDenoteHost) {
  if (url instanceof Url) return url;

  const urlObject = new Url();
  urlObject.parse(url, parseQueryString, slashesDenoteHost);
  return urlObject;
}

function isIpv6Hostname(hostname) {
  return (
    hostname.charCodeAt(0) === CHAR_LEFT_SQUARE_BRACKET &&
    hostname.charCodeAt(hostname.length - 1) ===
    CHAR_RIGHT_SQUARE_BRACKET
  );
}

const forbiddenHostChars = /[\0\t\n\r #%/:<>?@[\\\]^|]/;
const forbiddenHostCharsIpv6 = /[\0\t\n\r #%/<>?@\\^|]/;

Url.prototype.parse = function parse(url, parseQueryString, slashesDenoteHost) {
  validateString(url, "url");

  let hasHash = false;
  let hasAt = false;
  let start = -1;
  let end = -1;
  let rest = "";
  let lastPos = 0;
  for (let i = 0, inWs = false, split = false; i < url.length; ++i) {
    const code = url.charCodeAt(i);

    const isWs = code < 33 ||
                 code === CHAR_NO_BREAK_SPACE ||
                 code === CHAR_ZERO_WIDTH_NOBREAK_SPACE;
    if (start === -1) {
      if (isWs)
        continue;
      lastPos = start = i;
    } else if (inWs) {
      if (!isWs) {
        end = -1;
        inWs = false;
      }
    } else if (isWs) {
      end = i;
      inWs = true;
    }

    if (!split) {
      switch (code) {
        case CHAR_AT:
          hasAt = true;
          break;
        case CHAR_HASH:
          hasHash = true;
        case CHAR_QUESTION_MARK:
          split = true;
          break;
        case CHAR_BACKWARD_SLASH:
          if (i - lastPos > 0)
            rest += url.slice(lastPos, i);
          rest += "/";
          lastPos = i + 1;
          break;
      }
    } else if (!hasHash && code === CHAR_HASH) {
      hasHash = true;
    }
  }

  if (start !== -1) {
    if (lastPos === start) {

      if (end === -1) {
        if (start === 0)
          rest = url;
        else
          rest = url.slice(start);
      } else {
        rest = url.slice(start, end);
      }
    } else if (end === -1 && lastPos < url.length) {
      rest += url.slice(lastPos);
    } else if (end !== -1 && lastPos < end) {
      rest += url.slice(lastPos, end);
    }
  }

  if (!slashesDenoteHost && !hasHash && !hasAt) {
    const simplePath = simplePathPattern.exec(rest);
    if (simplePath) {
      this.path = rest;
      this.href = rest;
      this.pathname = simplePath[1];
      if (simplePath[2]) {
        this.search = simplePath[2];
        if (parseQueryString) {
          this.query = querystring.parse(this.search.slice(1));
        } else {
          this.query = this.search.slice(1);
        }
      } else if (parseQueryString) {
        this.search = null;
        this.query = { __proto__: null };
      }
      return this;
    }
  }

  let proto = protocolPattern.exec(rest);
  let lowerProto;
  if (proto) {
    proto = proto[0];
    lowerProto = proto.toLowerCase();
    this.protocol = lowerProto;
    rest = rest.slice(proto.length);
  }

  let slashes;
  if (slashesDenoteHost || proto || hostPattern.test(rest)) {
    slashes = rest.charCodeAt(0) === CHAR_FORWARD_SLASH &&
              rest.charCodeAt(1) === CHAR_FORWARD_SLASH;
    if (slashes && !(proto && hostlessProtocol.has(lowerProto))) {
      rest = rest.slice(2);
      this.slashes = true;
    }
  }

  if (!hostlessProtocol.has(lowerProto) &&
      (slashes || (proto && !slashedProtocol.has(proto)))) {

    let hostEnd = -1;
    let atSign = -1;
    let nonHost = -1;
    for (let i = 0; i < rest.length; ++i) {
      switch (rest.charCodeAt(i)) {
        case CHAR_TAB:
        case CHAR_LINE_FEED:
        case CHAR_CARRIAGE_RETURN:
          rest = rest.slice(0, i) + rest.slice(i + 1);
          i -= 1;
          break;
        case CHAR_SPACE:
        case CHAR_DOUBLE_QUOTE:
        case CHAR_PERCENT:
        case CHAR_SINGLE_QUOTE:
        case CHAR_SEMICOLON:
        case CHAR_LEFT_ANGLE_BRACKET:
        case CHAR_RIGHT_ANGLE_BRACKET:
        case CHAR_BACKWARD_SLASH:
        case CHAR_CIRCUMFLEX_ACCENT:
        case CHAR_GRAVE_ACCENT:
        case CHAR_LEFT_CURLY_BRACKET:
        case CHAR_VERTICAL_LINE:
        case CHAR_RIGHT_CURLY_BRACKET:
          if (nonHost === -1)
            nonHost = i;
          break;
        case CHAR_HASH:
        case CHAR_FORWARD_SLASH:
        case CHAR_QUESTION_MARK:
          if (nonHost === -1)
            nonHost = i;
          hostEnd = i;
          break;
        case CHAR_AT:
          atSign = i;
          nonHost = -1;
          break;
      }
      if (hostEnd !== -1)
        break;
    }
    start = 0;
    if (atSign !== -1) {
      this.auth = decodeURIComponent(rest.slice(0, atSign));
      start = atSign + 1;
    }
    if (nonHost === -1) {
      this.host = rest.slice(start);
      rest = "";
    } else {
      this.host = rest.slice(start, nonHost);
      rest = rest.slice(nonHost);
    }

    this.parseHost();

    if (typeof this.hostname !== "string")
      this.hostname = "";

    const hostname = this.hostname;

    const ipv6Hostname = isIpv6Hostname(hostname);

    if (!ipv6Hostname) {
      rest = getHostname(this, rest, hostname, url);
    }

    if (this.hostname.length > hostnameMaxLen) {
      this.hostname = "";
    } else {
      this.hostname = this.hostname.toLowerCase();
    }

    if (this.hostname !== "") {
      if (ipv6Hostname) {
        if (forbiddenHostCharsIpv6.test(this.hostname)) {
          throw new ERR_INVALID_URL(url);
        }
      } else {
        this.hostname = bindingUrl.toASCII(this.hostname);

        if (this.hostname === "" || forbiddenHostChars.test(this.hostname)) {
          throw new ERR_INVALID_URL(url);
        }
      }
    }

    const p = this.port ? ":" + this.port : "";
    const h = this.hostname || "";
    this.host = h + p;

    if (ipv6Hostname) {
      this.hostname = this.hostname.slice(1, -1);
      if (rest[0] !== "/") {
        rest = "/" + rest;
      }
    }
  }

  if (!unsafeProtocol.has(lowerProto)) {
    rest = autoEscapeStr(rest);
  }

  let questionIdx = -1;
  let hashIdx = -1;
  for (let i = 0; i < rest.length; ++i) {
    const code = rest.charCodeAt(i);
    if (code === CHAR_HASH) {
      this.hash = rest.slice(i);
      hashIdx = i;
      break;
    } else if (code === CHAR_QUESTION_MARK && questionIdx === -1) {
      questionIdx = i;
    }
  }

  if (questionIdx !== -1) {
    if (hashIdx === -1) {
      this.search = rest.slice(questionIdx);
      this.query = rest.slice(questionIdx + 1);
    } else {
      this.search = rest.slice(questionIdx, hashIdx);
      this.query = rest.slice(questionIdx + 1, hashIdx);
    }
    if (parseQueryString) {
      this.query = querystring.parse(this.query);
    }
  } else if (parseQueryString) {
    this.search = null;
    this.query = { __proto__: null };
  }

  const useQuestionIdx =
    questionIdx !== -1 && (hashIdx === -1 || questionIdx < hashIdx);
  const firstIdx = useQuestionIdx ? questionIdx : hashIdx;
  if (firstIdx === -1) {
    if (rest.length > 0)
      this.pathname = rest;
  } else if (firstIdx > 0) {
    this.pathname = rest.slice(0, firstIdx);
  }
  if (slashedProtocol.has(lowerProto) &&
      this.hostname && !this.pathname) {
    this.pathname = "/";
  }

  if (this.pathname || this.search) {
    const p = this.pathname || "";
    const s = this.search || "";
    this.path = p + s;
  }

  this.href = this.format();
  return this;
};

function getHostname(self, rest, hostname, url) {
  for (let i = 0; i < hostname.length; ++i) {
    const code = hostname.charCodeAt(i);
    const isValid = (code !== CHAR_FORWARD_SLASH &&
                     code !== CHAR_BACKWARD_SLASH &&
                     code !== CHAR_HASH &&
                     code !== CHAR_QUESTION_MARK &&
                     code !== CHAR_COLON);

    if (!isValid) {
      if (code === CHAR_COLON) {
        throw new ERR_INVALID_ARG_VALUE("url", "Invalid port in url", url);
      }
      self.hostname = hostname.slice(0, i);
      return `/${hostname.slice(i)}${rest}`;
    }
  }
  return rest;
}

const escapedCodes = [
  /* 0 - 9 */ "", "", "", "", "", "", "", "", "", "%09",
  /* 10 - 19 */ "%0A", "", "", "%0D", "", "", "", "", "", "",
  /* 20 - 29 */ "", "", "", "", "", "", "", "", "", "",
  /* 30 - 39 */ "", "", "%20", "", "%22", "", "", "", "", "%27",
  /* 40 - 49 */ "", "", "", "", "", "", "", "", "", "",
  /* 50 - 59 */ "", "", "", "", "", "", "", "", "", "",
  /* 60 - 69 */ "%3C", "", "%3E", "", "", "", "", "", "", "",
  /* 70 - 79 */ "", "", "", "", "", "", "", "", "", "",
  /* 80 - 89 */ "", "", "", "", "", "", "", "", "", "",
  /* 90 - 99 */ "", "", "%5C", "", "%5E", "", "%60", "", "", "",
  /* 100 - 109 */ "", "", "", "", "", "", "", "", "", "",
  /* 110 - 119 */ "", "", "", "", "", "", "", "", "", "",
  /* 120 - 125 */ "", "", "", "%7B", "%7C", "%7D",
];

function autoEscapeStr(rest) {
  let escaped = "";
  let lastEscapedPos = 0;
  for (let i = 0; i < rest.length; ++i) {
    const escapedChar = escapedCodes[rest.charCodeAt(i)];
    if (escapedChar) {
      if (i > lastEscapedPos)
        escaped += rest.slice(lastEscapedPos, i);
      escaped += escapedChar;
      lastEscapedPos = i + 1;
    }
  }
  if (lastEscapedPos === 0)  // Nothing has been escaped.
    return rest;

  if (lastEscapedPos < rest.length)
    escaped += rest.slice(lastEscapedPos);

  return escaped;
}

function urlFormat(urlObject, options) {
  if (typeof urlObject === "string") {
    urlObject = urlParse(urlObject);
  } else if (typeof urlObject !== "object" || urlObject === null) {
    throw new ERR_INVALID_ARG_TYPE("urlObject",
                                   ["Object", "string"], urlObject);
  } else if (urlObject instanceof URL) {
    let fragment = true;
    let unicode = false;
    let search = true;
    let auth = true;

    if (options) {
      validateObject(options, "options");

      if (options.fragment != null) {
        fragment = Boolean(options.fragment);
      }

      if (options.unicode != null) {
        unicode = Boolean(options.unicode);
      }

      if (options.search != null) {
        search = Boolean(options.search);
      }

      if (options.auth != null) {
        auth = Boolean(options.auth);
      }
    }

    return bindingUrl.format(urlObject.href, fragment, unicode, search, auth);
  }

  return Url.prototype.format.call(urlObject);
}

const noEscapeAuth = new Int8Array([
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x00 - 0x0F
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x10 - 0x1F
  0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, // 0x20 - 0x2F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, // 0x30 - 0x3F
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x40 - 0x4F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, // 0x50 - 0x5F
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0x60 - 0x6F
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, // 0x70 - 0x7F
]);

Url.prototype.format = function format() {
  let auth = this.auth || "";
  if (auth) {
    auth = encodeStr(auth, noEscapeAuth, hexTable);
    auth += "@";
  }

  let protocol = this.protocol || "";
  if (protocol && protocol.charCodeAt(protocol.length - 1) !== 58 /* : */) {
    protocol += ":";
  }

  let pathname = this.pathname || "";
  let hash = this.hash || "";
  let host = "";
  let query = "";

  if (this.host) {
    host = auth + this.host;
  } else if (this.hostname) {
    host = auth + (
      this.hostname.indexOf(":") !== -1 && !isIpv6Hostname(this.hostname) ?
        "[" + this.hostname + "]" :
        this.hostname
    );
    if (this.port) {
      host += ":" + this.port;
    }
  }

  if (this.query !== null && typeof this.query === "object") {
    query = querystring.stringify(this.query);
  }
  let search = this.search || (query && ("?" + query)) || "";

  if (pathname.indexOf("#") !== -1 || pathname.indexOf("?") !== -1) {
    let newPathname = "";
    let lastPos = 0;
    const len = pathname.length;
    for (let i = 0; i < len; i++) {
      const code = pathname.charCodeAt(i);
      if (code === CHAR_HASH || code === CHAR_QUESTION_MARK) {
        if (i > lastPos) {
          newPathname += pathname.slice(lastPos, i);
        }
        newPathname += (code === CHAR_HASH ? "%23" : "%3F");
        lastPos = i + 1;
      }
    }
    if (lastPos < len) {
      newPathname += pathname.slice(lastPos);
    }
    pathname = newPathname;
  }

  if (this.slashes || slashedProtocol.has(protocol)) {
    if (this.slashes || host) {
      if (pathname && pathname.charCodeAt(0) !== CHAR_FORWARD_SLASH)
        pathname = "/" + pathname;
      host = "//" + host;
    } else if (protocol.length >= 4 &&
               protocol.charCodeAt(0) === 102/* f */ &&
               protocol.charCodeAt(1) === 105/* i */ &&
               protocol.charCodeAt(2) === 108/* l */ &&
               protocol.charCodeAt(3) === 101/* e */) {
      host = "//";
    }
  }

  if (search.indexOf("#") !== -1) {
    search = search.replaceAll("#", "%23");
  }

  if (hash && hash.charCodeAt(0) !== CHAR_HASH) {
    hash = "#" + hash;
  }
  if (search && search.charCodeAt(0) !== CHAR_QUESTION_MARK) {
    search = "?" + search;
  }

  return protocol + host + pathname + search + hash;
};

function urlResolve(source, relative) {
  return urlParse(source, false, true).resolve(relative);
}

Url.prototype.resolve = function resolve(relative) {
  return this.resolveObject(urlParse(relative, false, true)).format();
};

function urlResolveObject(source, relative) {
  if (!source) return relative;
  return urlParse(source, false, true).resolveObject(relative);
}

Url.prototype.resolveObject = function resolveObject(relative) {
  if (typeof relative === "string") {
    const rel = new Url();
    rel.parse(relative, false, true);
    relative = rel;
  }

  const result = new Url();
  Object.assign(result, this);

  result.hash = relative.hash;

  if (relative.href === "") {
    result.href = result.format();
    return result;
  }

  if (relative.slashes && !relative.protocol) {
    const relativeWithoutProtocol = Object.keys(relative).reduce((acc, key) => {
      if (key !== "protocol") {
        acc[key] = relative[key];
      }
      return acc;
    }, {});
    Object.assign(result, relativeWithoutProtocol);

    if (slashedProtocol.has(result.protocol) &&
        result.hostname && !result.pathname) {
      result.path = result.pathname = "/";
    }

    result.href = result.format();
    return result;
  }

  if (relative.protocol && relative.protocol !== result.protocol) {
    if (!slashedProtocol.has(relative.protocol)) {
      Object.assign(result, relative);
      result.href = result.format();
      return result;
    }

    result.protocol = relative.protocol;
    if (!relative.host &&
        !/^file:?$/.test(relative.protocol) &&
        !hostlessProtocol.has(relative.protocol)) {
      const relPath = (relative.pathname || "").split("/");
      while (relPath.length && !(relative.host = relPath.shift()));
      relative.host ||= "";
      relative.hostname ||= "";
      if (relPath[0] !== "") relPath.unshift("");
      if (relPath.length < 2) relPath.unshift("");
      result.pathname = relPath.join("/");
    } else {
      result.pathname = relative.pathname;
    }
    result.search = relative.search;
    result.query = relative.query;
    result.host = relative.host || "";
    result.auth = relative.auth;
    result.hostname = relative.hostname || relative.host;
    result.port = relative.port;
    if (result.pathname || result.search) {
      const p = result.pathname || "";
      const s = result.search || "";
      result.path = p + s;
    }
    result.slashes ||= relative.slashes;
    result.href = result.format();
    return result;
  }

  const isSourceAbs = (result.pathname && result.pathname.charAt(0) === "/");
  const isRelAbs = (
    relative.host || (relative.pathname && relative.pathname.charAt(0) === "/")
  );
  let mustEndAbs = (isRelAbs || isSourceAbs ||
                    (result.host && relative.pathname));
  const removeAllDots = mustEndAbs;
  let srcPath = (result.pathname && result.pathname.split("/")) || [];
  const relPath = (relative.pathname && relative.pathname.split("/")) || [];
  const noLeadingSlashes = result.protocol &&
      !slashedProtocol.has(result.protocol);

  if (noLeadingSlashes) {
    result.hostname = "";
    result.port = null;
    if (result.host) {
      if (srcPath[0] === "") srcPath[0] = result.host;
      else srcPath.unshift(result.host);
    }
    result.host = "";
    if (relative.protocol) {
      relative.hostname = null;
      relative.port = null;
      result.auth = null;
      if (relative.host) {
        if (relPath[0] === "") relPath[0] = relative.host;
        else relPath.unshift(relative.host);
      }
      relative.host = null;
    }
    mustEndAbs &&= (relPath[0] === "" || srcPath[0] === "");
  }

  if (isRelAbs) {
    if (relative.host || relative.host === "") {
      if (result.host !== relative.host) result.auth = null;
      result.host = relative.host;
      result.port = relative.port;
    }
    if (relative.hostname || relative.hostname === "") {
      if (result.hostname !== relative.hostname) result.auth = null;
      result.hostname = relative.hostname;
    }
    result.search = relative.search;
    result.query = relative.query;
    srcPath = relPath;
  } else if (relPath.length) {
    srcPath ||= [];
    srcPath.pop();
    srcPath = srcPath.concat(relPath);
    result.search = relative.search;
    result.query = relative.query;
  } else if (relative.search !== null && relative.search !== undefined) {
    if (noLeadingSlashes) {
      result.hostname = result.host = srcPath.shift();
      const authInHost =
        result.host && result.host.indexOf("@") > 0 && result.host.split("@");
      if (authInHost) {
        result.auth = authInHost.shift();
        result.host = result.hostname = authInHost.shift();
      }
    }
    result.search = relative.search;
    result.query = relative.query;
    if (result.pathname !== null || result.search !== null) {
      result.path = (result.pathname ? result.pathname : "") +
                    (result.search ? result.search : "");
    }
    result.href = result.format();
    return result;
  }

  if (!srcPath.length) {
    result.pathname = null;
    if (result.search) {
      result.path = "/" + result.search;
    } else {
      result.path = null;
    }
    result.href = result.format();
    return result;
  }

  let last = srcPath[srcPath.length - 1];
  const hasTrailingSlash = (
    ((result.host || relative.host || srcPath.length > 1) &&
    (last === "." || last === "..")) || last === "");

  let up = 0;
  for (let i = srcPath.length - 1; i >= 0; i--) {
    last = srcPath[i];
    if (last === ".") {
      spliceOne(srcPath, i);
    } else if (last === "..") {
      spliceOne(srcPath, i);
      up++;
    } else if (up) {
      spliceOne(srcPath, i);
      up--;
    }
  }

  if (!mustEndAbs && !removeAllDots) {
    while (up--) {
      srcPath.unshift("..");
    }
  }

  if (mustEndAbs && srcPath[0] !== "" &&
      (!srcPath[0] || srcPath[0].charAt(0) !== "/")) {
    srcPath.unshift("");
  }

  if (hasTrailingSlash && srcPath.join("/").at(-1) !== "/") {
    srcPath.push("");
  }

  const isAbsolute = srcPath[0] === "" ||
      (srcPath[0] && srcPath[0].charAt(0) === "/");

  if (noLeadingSlashes) {
    result.hostname =
      result.host = isAbsolute ? "" : srcPath.length ? srcPath.shift() : "";
    const authInHost = result.host && result.host.indexOf("@") > 0 ?
      result.host.split("@") : false;
    if (authInHost) {
      result.auth = authInHost.shift();
      result.host = result.hostname = authInHost.shift();
    }
  }

  mustEndAbs ||= (result.host && srcPath.length);

  if (mustEndAbs && !isAbsolute) {
    srcPath.unshift("");
  }

  if (!srcPath.length) {
    result.pathname = null;
    result.path = null;
  } else {
    result.pathname = srcPath.join("/");
  }

  if (result.pathname !== null || result.search !== null) {
    result.path = (result.pathname ? result.pathname : "") +
                  (result.search ? result.search : "");
  }
  result.auth = relative.auth || result.auth;
  result.slashes ||= relative.slashes;
  result.href = result.format();
  return result;
};

Url.prototype.parseHost = function parseHost() {
  let host = this.host;
  let port = portPattern.exec(host);
  if (port) {
    port = port[0];
    if (port !== ":") {
      this.port = port.slice(1);
    }
    host = host.slice(0, host.length - port.length);
  }
  if (host) this.hostname = host;
};

function pathToFileURL(path, options) {
  validateString(path, "path");

  return _pathToFileURL(path, options);
}

module.exports = {
  Url,
  parse: urlParse,
  resolve: urlResolve,
  resolveObject: urlResolveObject,
  format: urlFormat,

  URL,
  URLSearchParams,
  domainToASCII,
  domainToUnicode,

  pathToFileURL,
  fileURLToPath,
  fileURLToPathBuffer,
  urlToHttpOptions,
};
