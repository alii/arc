"use strict";

// node's lib/string_decoder.js. The decoding it leaves to c++ is written out
// below, after src/string_decoder.cc

const errors = require("spark:errors");
const { Buffer } = require("node:buffer");

function normalizeEncoding(enc) {
  if (enc == null || enc === "utf8" || enc === "utf-8") return "utf8";
  return slowCases(enc);
}

function slowCases(enc) {
  switch (enc.length) {
    case 4:
      if (enc === "UTF8") return "utf8";
      if (enc === "ucs2" || enc === "UCS2") return "utf16le";
      enc = enc.toLowerCase();
      if (enc === "utf8") return "utf8";
      if (enc === "ucs2") return "utf16le";
      break;
    case 3:
      if (enc === "hex" || enc === "HEX" ||
      enc.toLowerCase() === "hex")
        return "hex";
      break;
    case 5:
      if (enc === "ascii") return "ascii";
      if (enc === "ucs-2") return "utf16le";
      if (enc === "UTF-8") return "utf8";
      if (enc === "ASCII") return "ascii";
      if (enc === "UCS-2") return "utf16le";
      enc = enc.toLowerCase();
      if (enc === "utf-8") return "utf8";
      if (enc === "ascii") return "ascii";
      if (enc === "ucs-2") return "utf16le";
      break;
    case 6:
      if (enc === "base64") return "base64";
      if (enc === "latin1" || enc === "binary") return "latin1";
      if (enc === "BASE64") return "base64";
      if (enc === "LATIN1" || enc === "BINARY") return "latin1";
      enc = enc.toLowerCase();
      if (enc === "base64") return "base64";
      if (enc === "latin1" || enc === "binary") return "latin1";
      break;
    case 7:
      if (enc === "utf16le" || enc === "UTF16LE" ||
      enc.toLowerCase() === "utf16le")
        return "utf16le";
      break;
    case 8:
      if (enc === "utf-16le" || enc === "UTF-16LE" ||
      enc.toLowerCase() === "utf-16le")
        return "utf16le";
      break;
    case 9:
      if (enc === "base64url" || enc === "BASE64URL" ||
      enc.toLowerCase() === "base64url")
        return "base64url";
      break;
    default:
      if (enc === "") return "utf8";
  }
}

// the decoder's state is seven bytes: up to four of a character cut short,
// how many more it needs, how many are held, and the encoding
const kIncompleteCharactersStart = 0;
const kIncompleteCharactersEnd = 4;
const kMissingBytes = 4;
const kBufferedBytes = 5;
const kEncodingField = 6;
const kSize = 7;

const encodings = ["ascii", "utf8", "base64", "utf16le", "latin1", "hex", "base64url"];
const encodingsMap = { __proto__: null };
for (let i = 0; i < encodings.length; ++i) encodingsMap[encodings[i]] = i;

function makeString(bytes, encoding) {
  return Buffer.from(bytes.buffer, bytes.byteOffset, bytes.byteLength).toString(
    encoding,
  );
}

function hold(state, data, from, count) {
  const at = state[kBufferedBytes];
  for (let i = 0; i < count; i++) state[at + i] = data[from + i];
}

function decode(state, view) {
  const data = new Uint8Array(view.buffer, view.byteOffset, view.byteLength);
  const encoding = encodings[state[kEncodingField]];
  if (encoding === "ascii" || encoding === "hex" || encoding === "latin1") {
    return makeString(data, encoding);
  }

  let start = 0;
  let nread = data.length;
  let prepend;

  // bytes owed to a character the last chunk cut short
  if (state[kMissingBytes] > 0) {
    if (encoding === "utf8") {
      for (let i = 0; i < nread && i < state[kMissingBytes]; ++i) {
        if ((data[start + i] & 0xc0) !== 0x80) {
          // not a continuation byte: the cut character ends here, unfinished
          state[kMissingBytes] = 0;
          hold(state, data, start, i);
          state[kBufferedBytes] += i;
          start += i;
          nread -= i;
          break;
        }
      }
    }
    const found = Math.min(nread, state[kMissingBytes]);
    hold(state, data, start, found);
    start += found;
    nread -= found;
    state[kMissingBytes] -= found;
    state[kBufferedBytes] += found;
    if (state[kMissingBytes] === 0) {
      prepend = makeString(state.subarray(0, state[kBufferedBytes]), encoding);
      state[kBufferedBytes] = 0;
    }
  }

  if (nread === 0) return prepend ?? "";

  // a character this chunk cuts short is held back for the next one
  if (encoding === "utf8" && data[start + nread - 1] & 0x80) {
    for (let i = nread - 1; ; --i) {
      state[kBufferedBytes]++;
      const byte = data[start + i];
      if ((byte & 0xc0) === 0x80) {
        if (state[kBufferedBytes] >= 4 || i === 0) {
          state[kBufferedBytes] = 0;
          break;
        }
      } else {
        if ((byte & 0xe0) === 0xc0) {
          state[kMissingBytes] = 2;
        } else if ((byte & 0xf0) === 0xe0) {
          state[kMissingBytes] = 3;
        } else if ((byte & 0xf8) === 0xf0) {
          state[kMissingBytes] = 4;
        } else {
          state[kBufferedBytes] = 0;
          break;
        }
        if (state[kBufferedBytes] >= state[kMissingBytes]) {
          state[kMissingBytes] = 0;
          state[kBufferedBytes] = 0;
        }
        state[kMissingBytes] -= state[kBufferedBytes];
        break;
      }
    }
  } else if (encoding === "utf16le") {
    if (nread % 2 === 1) {
      state[kBufferedBytes] = 1;
      state[kMissingBytes] = 1;
    } else if ((data[start + nread - 1] & 0xfc) === 0xd8) {
      state[kBufferedBytes] = 2;
      state[kMissingBytes] = 2;
    }
  } else if (encoding === "base64" || encoding === "base64url") {
    state[kBufferedBytes] = nread % 3;
    if (state[kBufferedBytes] > 0) {
      state[kMissingBytes] = 3 - state[kBufferedBytes];
    }
  }

  const held = state[kBufferedBytes];
  if (held > 0) {
    nread -= held;
    for (let i = 0; i < held; i++) state[i] = data[start + nread + i];
  }
  const body =
    nread > 0 ? makeString(data.subarray(start, start + nread), encoding) : "";
  return prepend === undefined ? body : prepend + body;
}

function flush(state) {
  const encoding = encodings[state[kEncodingField]];
  if (encoding === "utf16le" && state[kBufferedBytes] % 2 === 1) {
    state[kMissingBytes]--;
    state[kBufferedBytes]--;
  }
  if (state[kBufferedBytes] === 0) return "";
  const text = makeString(state.subarray(0, state[kBufferedBytes]), encoding);
  state[kMissingBytes] = 0;
  state[kBufferedBytes] = 0;
  return text;
}

const shims = {
  process: () => undefined,
  buffer: () => ({ Buffer }),
  "internal/errors": () => errors,
  "internal/util": () => ({ encodingsMap, normalizeEncoding }),
  "binding:string_decoder": () => ({
    kIncompleteCharactersStart,
    kIncompleteCharactersEnd,
    kMissingBytes,
    kBufferedBytes,
    kEncodingField,
    kSize,
    decode,
    flush,
  }),
};

const files = {
  "string_decoder": function (module, exports, require, process) {
    "use strict";

    const { Buffer } = require("buffer");
    const {
      kIncompleteCharactersStart,
      kIncompleteCharactersEnd,
      kMissingBytes,
      kBufferedBytes,
      kEncodingField,
      kSize,
      decode,
      flush,
    } = require("binding:string_decoder");
    const {
      encodingsMap,
      normalizeEncoding,
    } = require("internal/util");
    const {
      ERR_INVALID_ARG_TYPE,
      ERR_INVALID_THIS,
      ERR_UNKNOWN_ENCODING,
    } = require("internal/errors").codes;

    const kNativeDecoder = Symbol("kNativeDecoder");

    function StringDecoder(encoding) {
      this.encoding = normalizeEncoding(encoding);
      if (this.encoding === undefined) {
        throw new ERR_UNKNOWN_ENCODING(encoding);
      }
      this[kNativeDecoder] = Buffer.alloc(kSize);
      this[kNativeDecoder][kEncodingField] = encodingsMap[this.encoding];
    }

    StringDecoder.prototype.write = function write(buf) {
      if (typeof buf === "string")
        return buf;
      if (!ArrayBuffer.isView(buf))
        throw new ERR_INVALID_ARG_TYPE("buf",
                                       ["Buffer", "TypedArray", "DataView"],
                                       buf);
      if (!this[kNativeDecoder]) {
        throw new ERR_INVALID_THIS("StringDecoder");
      }
      return decode(this[kNativeDecoder], buf);
    };

    StringDecoder.prototype.end = function end(buf) {
      const ret = buf === undefined ? "" : this.write(buf);
      if (this[kNativeDecoder][kBufferedBytes] > 0)
        return ret + flush(this[kNativeDecoder]);
      return ret;
    };

    /* Everything below this line is undocumented legacy stuff. */
    StringDecoder.prototype.text = function text(buf, offset) {
      this[kNativeDecoder][kMissingBytes] = 0;
      this[kNativeDecoder][kBufferedBytes] = 0;
      return this.write(buf.slice(offset));
    };

    Object.defineProperties(StringDecoder.prototype, {
      lastChar: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() {
          return this[kNativeDecoder].subarray(kIncompleteCharactersStart, kIncompleteCharactersEnd);
        },
      },
      lastNeed: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() {
          return this[kNativeDecoder][kMissingBytes];
        },
      },
      lastTotal: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() {
          return this[kNativeDecoder][kBufferedBytes] +
                 this[kNativeDecoder][kMissingBytes];
        },
      },
    });

    exports.StringDecoder = StringDecoder;
  },
};

const loaded = new Map();

// node's own module names, resolved inside this file
function load(name) {
  if (loaded.has(name)) return loaded.get(name).exports;
  if (Object.hasOwn(shims, name)) {
    const made = { exports: shims[name]() };
    loaded.set(name, made);
    return made.exports;
  }
  if (!Object.hasOwn(files, name)) {
    throw new Error("no module " + name + " in this port");
  }
  const made = { exports: {} };
  loaded.set(name, made);
  files[name](made, made.exports, load, shims.process());
  return made.exports;
}

module.exports = load("string_decoder");
