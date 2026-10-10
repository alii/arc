"use strict";

// node's lib/buffer.js. Two things differ on purpose: small buffers are not
// carved out of a shared pool, so each has its own memory (byteOffset 0), and
// Buffer() gives no deprecation warning

const {
  typeError,
  rangeError,
  invalidArgType,
  invalidArgValue,
  outOfRange,
  validateArray,
  validateBuffer,
  validateInteger,
  validateNumber,
  validateString,
} = require("spark:errors");
const { DOMException } = require("spark:domexception");
const {
  encode,
  decode,
  byteLengthUtf8,
  compare: compareBytes,
  indexOf: indexOfBytes,
  swap: swapBytes,
  atob: decodeBase64Strict,
  isAscii: bytesAreAscii,
  isUtf8: bytesAreUtf8,
  isLatin1: stringIsLatin1,
  utf16Units,
} = native;

const TypedArrayPrototype = Object.getPrototypeOf(Uint8Array.prototype);
const getter = (proto, name) =>
  Object.getOwnPropertyDescriptor(proto, name).get;
const typedArrayTag = getter(TypedArrayPrototype, Symbol.toStringTag);
const typedArrayLength = getter(TypedArrayPrototype, "length");
const typedArrayByteLength = getter(TypedArrayPrototype, "byteLength");
const typedArrayByteOffset = getter(TypedArrayPrototype, "byteOffset");
const typedArrayBuffer = getter(TypedArrayPrototype, "buffer");
const typedArraySet = TypedArrayPrototype.set;
const typedArrayFill = TypedArrayPrototype.fill;
const typedArraySubarray = TypedArrayPrototype.subarray;
const typedArrayCopyWithin = TypedArrayPrototype.copyWithin;
const arrayBufferByteLength = getter(ArrayBuffer.prototype, "byteLength");
const sharedArrayBufferByteLength =
  typeof SharedArrayBuffer === "function"
    ? getter(SharedArrayBuffer.prototype, "byteLength")
    : undefined;

const customInspectSymbol = Symbol.for("nodejs.util.inspect.custom");
const kIsEncodingSymbol = Symbol("kIsEncodingSymbol");

const kMaxLength = Number.MAX_SAFE_INTEGER;
const kStringMaxLength = 2 ** 29 - 24;
const kMaxAlignment = 2 ** 30;

function isUint8Array(value) {
  return typedArrayTag.call(value) === "Uint8Array";
}

function isTypedArray(value) {
  return typedArrayTag.call(value) !== undefined;
}

function hasByteLength(read, value) {
  try {
    read.call(value);
    return true;
  } catch {
    return false;
  }
}

function isAnyArrayBuffer(value) {
  if (typeof value !== "object" || value === null) return false;
  return (
    hasByteLength(arrayBufferByteLength, value) ||
    (sharedArrayBufferByteLength !== undefined &&
      hasByteLength(sharedArrayBufferByteLength, value))
  );
}

function bufferOutOfBounds(name) {
  return rangeError(
    "ERR_BUFFER_OUT_OF_BOUNDS",
    name
      ? `"${name}" is outside of buffer bounds`
      : "Attempt to access memory outside buffer bounds",
  );
}

function unknownEncoding(encoding) {
  return typeError("ERR_UNKNOWN_ENCODING", `Unknown encoding: ${encoding}`);
}

function invalidBufferSize(unit) {
  return rangeError(
    "ERR_INVALID_BUFFER_SIZE",
    `Buffer size must be a multiple of ${unit}`,
  );
}

function missingArgs(name) {
  return typeError(
    "ERR_MISSING_ARGS",
    `The "${name}" argument must be specified`,
  );
}

function indexOutOfRange() {
  return rangeError("ERR_OUT_OF_RANGE", "Index out of range");
}

const validateOffset = (value, name, min = 0, max = kMaxLength) =>
  validateInteger(value, name, min, max);

function normalizeEncoding(enc) {
  if (enc == null || enc === "utf8" || enc === "utf-8") return "utf8";
  return slowCases(enc);
}

function slowCases(enc) {
  switch (enc.length) {
    case 4:
      if (enc === "UTF8") return "utf8";
      if (enc === "ucs2" || enc === "UCS2") return "utf16le";
      enc = `${enc}`.toLowerCase();
      if (enc === "utf8") return "utf8";
      if (enc === "ucs2") return "utf16le";
      break;
    case 3:
      if (enc === "hex" || enc === "HEX" || `${enc}`.toLowerCase() === "hex") {
        return "hex";
      }
      break;
    case 5:
      if (enc === "ascii") return "ascii";
      if (enc === "ucs-2") return "utf16le";
      if (enc === "UTF-8") return "utf8";
      if (enc === "ASCII") return "ascii";
      if (enc === "UCS-2") return "utf16le";
      enc = `${enc}`.toLowerCase();
      if (enc === "utf-8") return "utf8";
      if (enc === "ascii") return "ascii";
      if (enc === "ucs-2") return "utf16le";
      break;
    case 6:
      if (enc === "base64") return "base64";
      if (enc === "latin1" || enc === "binary") return "latin1";
      if (enc === "BASE64") return "base64";
      if (enc === "LATIN1" || enc === "BINARY") return "latin1";
      enc = `${enc}`.toLowerCase();
      if (enc === "base64") return "base64";
      if (enc === "latin1" || enc === "binary") return "latin1";
      break;
    case 7:
      if (
        enc === "utf16le" ||
        enc === "UTF16LE" ||
        `${enc}`.toLowerCase() === "utf16le"
      ) {
        return "utf16le";
      }
      break;
    case 8:
      if (
        enc === "utf-16le" ||
        enc === "UTF-16LE" ||
        `${enc}`.toLowerCase() === "utf-16le"
      ) {
        return "utf16le";
      }
      break;
    case 9:
      if (
        enc === "base64url" ||
        enc === "BASE64URL" ||
        `${enc}`.toLowerCase() === "base64url"
      ) {
        return "base64url";
      }
      break;
    default:
      if (enc === "") return "utf8";
  }
}

// what node's native slice functions do with their start and end
function sliceIndex(value, fallback) {
  if (value === undefined) return fallback;
  const index = Math.trunc(Number(value)) || 0;
  if (index < 0) throw indexOutOfRange();
  return index;
}

function slicer(encoding) {
  return function slice(buf, start, end) {
    const length = typedArrayByteLength.call(buf);
    start = sliceIndex(start, 0);
    end = sliceIndex(end, length);
    if (end <= start) return "";
    if (end > length) throw indexOutOfRange();
    return decode(buf, encoding, start, end);
  };
}

const asciiSlice = slicer("ascii");
const base64Slice = slicer("base64");
const base64urlSlice = slicer("base64url");
const latin1Slice = slicer("latin1");
const hexSlice = slicer("hex");
const ucs2Slice = slicer("utf16le");
const utf8Slice = slicer("utf8");

// writes what fits of the string, never part of a character, and says how
// many bytes that was
function writeBytes(buf, string, encoding, offset, length) {
  if (typeof string !== "string") {
    throw typeError("ERR_INVALID_ARG_TYPE", "argument must be a string");
  }
  if (length <= 0) return 0;
  const bytes = new Uint8Array(encode(string, encoding, length));
  typedArraySet.call(buf, bytes, offset);
  return bytes.length;
}

// node's native write functions: base64, base64url, hex and ucs2
function writer(encoding) {
  return function write(buf, string, offset, length) {
    if (typeof string !== "string") {
      throw typeError("ERR_INVALID_ARG_TYPE", "argument must be a string");
    }
    const size = typedArrayByteLength.call(buf);
    offset = sliceIndex(offset, 0);
    if (offset > size) throw bufferOutOfBounds("offset");
    length = Math.min(size - offset, sliceIndex(length, size - offset));
    return writeBytes(buf, string, encoding, offset, length);
  };
}

const base64Write = writer("base64");
const base64urlWrite = writer("base64url");
const hexWrite = writer("hex");
const ucs2Write = writer("utf16le");

// lib/internal/buffer.js: ascii, latin1 and utf8
function checkedWriter(encoding) {
  return function write(buf, string, offset = 0, length) {
    offset = Number(offset);
    if (offset < 0 || offset > buf.byteLength) {
      throw bufferOutOfBounds("offset");
    }
    if (length === undefined) length = buf.byteLength - offset;
    else length = Number(length);
    if (length < 0 || length > buf.byteLength - offset) {
      throw bufferOutOfBounds("length");
    }
    return writeBytes(
      buf,
      string,
      encoding,
      Math.trunc(offset) || 0,
      Math.trunc(length) || 0,
    );
  };
}

const asciiWrite = checkedWriter("ascii");
const latin1Write = checkedWriter("latin1");
const utf8Write = checkedWriter("utf8");

function _compare(a, b) {
  return compareBytes(a, b);
}

function compareOffset(
  source,
  target,
  targetStart,
  sourceStart,
  targetEnd,
  sourceEnd,
) {
  const sourceLength = typedArrayByteLength.call(source);
  if (targetStart > typedArrayByteLength.call(target)) {
    throw rangeError(
      "ERR_OUT_OF_RANGE",
      'The value of "targetStart" is out of range.',
    );
  }
  if (sourceStart > sourceLength) {
    throw rangeError(
      "ERR_OUT_OF_RANGE",
      'The value of "sourceStart" is out of range.',
    );
  }
  const sourceSpan = sourceEnd - sourceStart;
  const targetSpan = targetEnd - targetStart;
  const count = Math.min(sourceSpan, targetSpan, sourceLength - sourceStart);
  const order =
    count > 0
      ? compareBytes(
          typedArraySubarray.call(source, sourceStart, sourceStart + count),
          typedArraySubarray.call(target, targetStart, targetStart + count),
        )
      : 0;
  if (order !== 0) return order;
  if (sourceSpan > targetSpan) return 1;
  if (sourceSpan < targetSpan) return -1;
  return 0;
}

// the bytes of any view, as a Uint8Array over the same memory
function bytesOf(view) {
  if (isUint8Array(view)) return view;
  return new Uint8Array(view.buffer, view.byteOffset, view.byteLength);
}

function _copy(source, target, targetStart, sourceStart, nb) {
  const from = bytesOf(source);
  typedArraySet.call(
    bytesOf(target),
    typedArraySubarray.call(from, sourceStart, sourceStart + nb),
    targetStart,
  );
  return nb;
}

// -1 for a value with no bytes, -2 for a range outside the buffer
function bindingFill(buf, value, offset, end, encoding) {
  if (offset > end || end > typedArrayByteLength.call(buf)) return -2;
  const fillLength = end - offset;
  let pattern;
  if (typeof value === "string") {
    pattern = new Uint8Array(
      encode(value, normalizeEncoding(encoding) ?? "utf8"),
    );
  } else if (ArrayBuffer.isView(value)) {
    pattern = bytesOf(value);
  } else {
    typedArrayFill.call(buf, (value >>> 0) & 255, offset, end);
    return 0;
  }
  let done = Math.min(pattern.length, fillLength);
  if (done > 0) {
    typedArraySet.call(buf, typedArraySubarray.call(pattern, 0, done), offset);
  }
  if (done >= fillLength) return 0;
  if (done === 0) return -1;
  // each pass doubles the stretch already filled
  while (done < fillLength) {
    const more = Math.min(done, fillLength - done);
    typedArrayCopyWithin.call(buf, offset + done, offset, offset + more);
    done += more;
  }
  return 0;
}

// `end` cuts the haystack short
function haystack(buf, end) {
  const length = typedArrayByteLength.call(buf);
  end = +end;
  if (!(end < length)) return bytesOf(buf);
  return typedArraySubarray.call(bytesOf(buf), 0, end > 0 ? Math.trunc(end) : 0);
}

function searchOffset(byteOffset) {
  return Math.trunc(byteOffset) || 0;
}

function indexOfBuffer(buf, val, byteOffset, step, dir, end) {
  return indexOfBytes(
    haystack(buf, end),
    bytesOf(val),
    searchOffset(byteOffset),
    dir,
    step,
  );
}

const oneByte = new Uint8Array(1);

function indexOfNumber(buf, val, byteOffset, dir, end) {
  oneByte[0] = val;
  return indexOfBytes(
    haystack(buf, end),
    oneByte,
    searchOffset(byteOffset),
    dir,
    1,
  );
}

function indexOfString(buf, val, byteOffset, encoding, dir, end) {
  return indexOfBytes(
    haystack(buf, end),
    encode(val, encoding),
    searchOffset(byteOffset),
    dir,
    encoding === "utf16le" ? 2 : 1,
  );
}

class FastBuffer extends Uint8Array {}

function createUnsafeBuffer(size) {
  return new FastBuffer(size);
}

const constants = Object.defineProperties(
  {},
  {
    MAX_LENGTH: {
      __proto__: null,
      value: kMaxLength,
      writable: false,
      enumerable: true,
    },
    MAX_STRING_LENGTH: {
      __proto__: null,
      value: kStringMaxLength,
      writable: false,
      enumerable: true,
    },
  },
);

function toInteger(n, defaultVal) {
  n = +n;
  if (
    !Number.isNaN(n) &&
    n >= Number.MIN_SAFE_INTEGER &&
    n <= Number.MAX_SAFE_INTEGER
  ) {
    return n % 1 === 0 ? n : Math.floor(n);
  }
  return defaultVal;
}

function copyImpl(source, target, targetStart, sourceStart, sourceEnd) {
  if (!ArrayBuffer.isView(source)) {
    throw invalidArgType("source", ["Buffer", "Uint8Array"], source);
  }
  if (!ArrayBuffer.isView(target)) {
    throw invalidArgType("target", ["Buffer", "Uint8Array"], target);
  }

  if (targetStart === undefined) {
    targetStart = 0;
  } else {
    targetStart = Number.isInteger(targetStart)
      ? targetStart
      : toInteger(targetStart, 0);
    if (targetStart < 0) throw outOfRange("targetStart", ">= 0", targetStart);
  }

  if (sourceStart === undefined) {
    sourceStart = 0;
  } else {
    sourceStart = Number.isInteger(sourceStart)
      ? sourceStart
      : toInteger(sourceStart, 0);
    if (sourceStart < 0 || sourceStart > source.byteLength) {
      throw outOfRange(
        "sourceStart",
        `>= 0 && <= ${source.byteLength}`,
        sourceStart,
      );
    }
  }

  if (sourceEnd === undefined) {
    sourceEnd = source.byteLength;
  } else {
    sourceEnd = Number.isInteger(sourceEnd)
      ? sourceEnd
      : toInteger(sourceEnd, 0);
    if (sourceEnd < 0) throw outOfRange("sourceEnd", ">= 0", sourceEnd);
  }

  if (targetStart >= target.byteLength || sourceStart >= sourceEnd) return 0;

  return _copyActual(source, target, targetStart, sourceStart, sourceEnd);
}

function _copyActual(source, target, targetStart, sourceStart, sourceEnd) {
  if (sourceEnd - sourceStart > target.byteLength - targetStart) {
    sourceEnd = sourceStart + target.byteLength - targetStart;
  }

  let nb = sourceEnd - sourceStart;
  const sourceLen = source.byteLength - sourceStart;
  if (nb > sourceLen) nb = sourceLen;

  if (nb <= 0) return 0;

  return _copy(source, target, targetStart, sourceStart, nb);
}

function Buffer(arg, encodingOrOffset, length) {
  if (typeof arg === "number") {
    if (typeof encodingOrOffset === "string") {
      throw invalidArgType("string", "string", arg);
    }
    return Buffer.alloc(arg);
  }
  return Buffer.from(arg, encodingOrOffset, length);
}

FastBuffer.prototype.constructor = Buffer;
Buffer.prototype = FastBuffer.prototype;

Object.defineProperty(Buffer, Symbol.species, {
  __proto__: null,
  enumerable: false,
  configurable: true,
  get() {
    return FastBuffer;
  },
});

Buffer.poolSize = 64 * 1024;

Buffer.from = function from(value, encodingOrOffset, length) {
  if (typeof value === "string") return fromString(value, encodingOrOffset);

  if (typeof value === "object" && value !== null) {
    if (isAnyArrayBuffer(value)) {
      return fromArrayBuffer(value, encodingOrOffset, length);
    }

    const valueOf = value.valueOf && value.valueOf();
    if (
      valueOf != null &&
      valueOf !== value &&
      (typeof valueOf === "string" || typeof valueOf === "object")
    ) {
      return from(valueOf, encodingOrOffset, length);
    }

    const b = fromObject(value);
    if (b) return b;

    if (typeof value[Symbol.toPrimitive] === "function") {
      const primitive = value[Symbol.toPrimitive]("string");
      if (typeof primitive === "string") {
        return fromString(primitive, encodingOrOffset);
      }
    }
  }

  throw invalidArgType(
    "first argument",
    ["string", "Buffer", "ArrayBuffer", "Array", "Array-like Object"],
    value,
  );
};

Buffer.copyBytesFrom = function copyBytesFrom(view, offset, length) {
  if (!isTypedArray(view)) {
    throw invalidArgType("view", ["TypedArray"], view);
  }

  const viewLength = typedArrayLength.call(view);
  if (viewLength === 0) return new FastBuffer();

  let start = 0;
  let end = viewLength;

  if (offset !== undefined) {
    validateInteger(offset, "offset", 0);
    if (offset >= viewLength) return new FastBuffer();
    start = offset;
  }

  if (length !== undefined) {
    validateInteger(length, "length", 0);
    end = Math.min(start + length, viewLength);
  }

  if (end <= start) return new FastBuffer();

  const viewByteLength = typedArrayByteLength.call(view);
  const elementSize = viewByteLength / viewLength;
  const srcByteOffset = typedArrayByteOffset.call(view) + start * elementSize;
  const srcByteLength = (end - start) * elementSize;

  return fromArrayLike(
    new Uint8Array(typedArrayBuffer.call(view), srcByteOffset, srcByteLength),
  );
};

const of = (...items) => {
  const len = items.length;
  const newObj = new FastBuffer(len);
  for (let k = 0; k < len; k++) newObj[k] = items[k];
  return newObj;
};
Buffer.of = of;

Object.setPrototypeOf(Buffer, Uint8Array);

Buffer.alloc = function alloc(size, fill, encoding) {
  validateNumber(size, "size", 0, kMaxLength);
  if (fill !== undefined && fill !== 0 && size > 0) {
    const buf = createUnsafeBuffer(size);
    return _fill(buf, fill, 0, buf.length, encoding);
  }
  return new FastBuffer(size);
};

Buffer.allocUnsafe = function allocUnsafe(size, alignment) {
  validateNumber(size, "size", 0, kMaxLength);
  if (alignment !== undefined) validateAlignment(size, alignment);
  return allocate(size);
};

Buffer.allocUnsafeSlow = function allocUnsafeSlow(size, alignment) {
  validateNumber(size, "size", 0, kMaxLength);
  if (alignment !== undefined) validateAlignment(size, alignment);
  return createUnsafeBuffer(size);
};

function validateAlignment(size, alignment) {
  validateInteger(alignment, "alignment", 1, kMaxAlignment);
  if ((alignment & (alignment - 1)) !== 0) {
    throw invalidArgValue("alignment", alignment, "must be a power of two");
  }
  const padding = Math.max(alignment, 8);
  if (size > kMaxLength - padding) {
    throw outOfRange("size", `<= ${kMaxLength - padding}`, size);
  }
}

function allocate(size) {
  if (size <= 0) return new FastBuffer();
  return createUnsafeBuffer(size);
}

function fromString(string, encoding) {
  let ops;
  if (!encoding || encoding === "utf8" || typeof encoding !== "string") {
    ops = encodingOps.utf8;
  } else {
    ops = getEncodingOps(encoding);
    if (ops === undefined) throw unknownEncoding(encoding);
  }

  return string.length === 0
    ? new FastBuffer()
    : new FastBuffer(encode(string, ops.encodingVal));
}

function fromArrayBuffer(obj, byteOffset, length) {
  if (byteOffset === undefined) {
    byteOffset = 0;
  } else {
    byteOffset = +byteOffset;
    if (Number.isNaN(byteOffset)) byteOffset = 0;
  }

  const maxLength = obj.byteLength - byteOffset;

  if (maxLength < 0) throw bufferOutOfBounds("offset");

  if (length !== undefined) {
    length = +length;
    if (length > 0) {
      if (length > maxLength) throw bufferOutOfBounds("length");
    } else {
      length = 0;
    }
  }

  return new FastBuffer(obj, byteOffset, length);
}

function fromArrayLike(obj) {
  const { length } = obj;
  if (length <= 0) return new FastBuffer();
  if (length < Buffer.poolSize >>> 1) {
    const b = new FastBuffer(length);
    typedArraySet.call(b, obj, 0);
    return b;
  }
  return new FastBuffer(obj);
}

function fromObject(obj) {
  if (obj.length !== undefined || isAnyArrayBuffer(obj.buffer)) {
    if (typeof obj.length !== "number") return new FastBuffer();
    return fromArrayLike(obj);
  }

  if (obj.type === "Buffer" && Array.isArray(obj.data)) {
    return fromArrayLike(obj.data);
  }
}

Buffer.isBuffer = function isBuffer(b) {
  return b instanceof Buffer;
};

Buffer.compare = function compare(buf1, buf2) {
  if (!isUint8Array(buf1)) {
    throw invalidArgType("buf1", ["Buffer", "Uint8Array"], buf1);
  }

  if (!isUint8Array(buf2)) {
    throw invalidArgType("buf2", ["Buffer", "Uint8Array"], buf2);
  }

  if (buf1 === buf2) return 0;

  return _compare(buf1, buf2);
};

Buffer.isEncoding = function isEncoding(encoding) {
  return (
    typeof encoding === "string" &&
    encoding.length !== 0 &&
    normalizeEncoding(encoding) !== undefined
  );
};
Buffer[kIsEncodingSymbol] = Buffer.isEncoding;

Buffer.concat = function concat(list, length) {
  validateArray(list, "list");

  if (list.length === 0) return new FastBuffer();

  if (length === undefined) {
    length = 0;
    for (let i = 0; i < list.length; i++) {
      const buf = list[i];
      if (!isUint8Array(buf)) {
        throw invalidArgType(`list[${i}]`, ["Buffer", "Uint8Array"], buf);
      }
      length += typedArrayByteLength.call(buf);
    }

    const buffer = allocate(length);
    let pos = 0;
    for (let i = 0; i < list.length; i++) {
      const buf = list[i];
      const bufLength = typedArrayByteLength.call(buf);
      typedArraySet.call(buffer, buf, pos);
      pos += bufLength;
    }

    if (pos < length) typedArrayFill.call(buffer, 0, pos, length);
    return buffer;
  }

  validateOffset(length, "length");
  for (let i = 0; i < list.length; i++) {
    if (!isUint8Array(list[i])) {
      throw invalidArgType(`list[${i}]`, ["Buffer", "Uint8Array"], list[i]);
    }
  }

  const buffer = allocate(length);
  let pos = 0;
  for (let i = 0; i < list.length; i++) {
    const buf = list[i];
    const bufLength = typedArrayByteLength.call(buf);
    if (pos + bufLength > length) {
      typedArraySet.call(
        buffer,
        typedArraySubarray.call(buf, 0, length - pos),
        pos,
      );
      pos = length;
      break;
    }
    typedArraySet.call(buffer, buf, pos);
    pos += bufLength;
  }

  if (pos < length) typedArrayFill.call(buffer, 0, pos, length);
  return buffer;
};

function base64ByteLength(str, bytes) {
  if (str.charCodeAt(bytes - 1) === 0x3d) bytes--;
  if (bytes > 1 && str.charCodeAt(bytes - 1) === 0x3d) bytes--;

  // base64 ratio: 3/4
  return (bytes * 3) >>> 2;
}

// a string as the bytes to look for; none of them are utf-16 text, where a
// match has to sit on a unit edge
function searchEncoded(encoding) {
  return (buf, val, byteOffset, dir, end) =>
    indexOfBuffer(
      buf,
      new Uint8Array(encode(val, encoding)),
      byteOffset,
      1,
      dir,
      end,
    );
}

const encodingOps = {
  utf8: {
    encoding: "utf8",
    encodingVal: "utf8",
    byteLength: byteLengthUtf8,
    write: utf8Write,
    slice: utf8Slice,
    indexOf: (buf, val, byteOffset, dir, end) =>
      indexOfString(buf, val, byteOffset, "utf8", dir, end),
  },
  ucs2: {
    encoding: "ucs2",
    encodingVal: "utf16le",
    byteLength: (string) => string.length * 2,
    write: ucs2Write,
    slice: ucs2Slice,
    indexOf: (buf, val, byteOffset, dir, end) =>
      indexOfString(buf, val, byteOffset, "utf16le", dir, end),
  },
  utf16le: {
    encoding: "utf16le",
    encodingVal: "utf16le",
    byteLength: (string) => string.length * 2,
    write: ucs2Write,
    slice: ucs2Slice,
    indexOf: (buf, val, byteOffset, dir, end) =>
      indexOfString(buf, val, byteOffset, "utf16le", dir, end),
  },
  latin1: {
    encoding: "latin1",
    encodingVal: "latin1",
    byteLength: (string) => string.length,
    write: latin1Write,
    slice: latin1Slice,
    indexOf: (buf, val, byteOffset, dir, end) =>
      indexOfString(buf, val, byteOffset, "latin1", dir, end),
  },
  ascii: {
    encoding: "ascii",
    encodingVal: "ascii",
    byteLength: (string) => string.length,
    write: asciiWrite,
    slice: asciiSlice,
    indexOf: searchEncoded("ascii"),
  },
  base64: {
    encoding: "base64",
    encodingVal: "base64",
    byteLength: (string) => base64ByteLength(string, string.length),
    write: base64Write,
    slice: base64Slice,
    indexOf: searchEncoded("base64"),
  },
  base64url: {
    encoding: "base64url",
    encodingVal: "base64url",
    byteLength: (string) => base64ByteLength(string, string.length),
    write: base64urlWrite,
    slice: base64urlSlice,
    indexOf: searchEncoded("base64url"),
  },
  hex: {
    encoding: "hex",
    encodingVal: "hex",
    byteLength: (string) => string.length >>> 1,
    write: hexWrite,
    slice: hexSlice,
    indexOf: searchEncoded("hex"),
  },
};

function getEncodingOps(encoding) {
  encoding += "";
  switch (encoding.length) {
    case 4:
      if (encoding === "utf8") return encodingOps.utf8;
      if (encoding === "ucs2") return encodingOps.ucs2;
      encoding = encoding.toLowerCase();
      if (encoding === "utf8") return encodingOps.utf8;
      if (encoding === "ucs2") return encodingOps.ucs2;
      break;
    case 5:
      if (encoding === "utf-8") return encodingOps.utf8;
      if (encoding === "ascii") return encodingOps.ascii;
      if (encoding === "ucs-2") return encodingOps.ucs2;
      encoding = encoding.toLowerCase();
      if (encoding === "utf-8") return encodingOps.utf8;
      if (encoding === "ascii") return encodingOps.ascii;
      if (encoding === "ucs-2") return encodingOps.ucs2;
      break;
    case 7:
      if (encoding === "utf16le" || encoding.toLowerCase() === "utf16le") {
        return encodingOps.utf16le;
      }
      break;
    case 8:
      if (encoding === "utf-16le" || encoding.toLowerCase() === "utf-16le") {
        return encodingOps.utf16le;
      }
      break;
    case 6:
      if (encoding === "latin1" || encoding === "binary") {
        return encodingOps.latin1;
      }
      if (encoding === "base64") return encodingOps.base64;
      encoding = encoding.toLowerCase();
      if (encoding === "latin1" || encoding === "binary") {
        return encodingOps.latin1;
      }
      if (encoding === "base64") return encodingOps.base64;
      break;
    case 3:
      if (encoding === "hex" || encoding.toLowerCase() === "hex") {
        return encodingOps.hex;
      }
      break;
    case 9:
      if (encoding === "base64url" || encoding.toLowerCase() === "base64url") {
        return encodingOps.base64url;
      }
      break;
  }
}

function byteLength(string, encoding) {
  if (typeof string !== "string") {
    if (ArrayBuffer.isView(string) || isAnyArrayBuffer(string)) {
      return string.byteLength;
    }

    throw invalidArgType("string", ["string", "Buffer", "ArrayBuffer"], string);
  }

  const len = string.length;
  if (len === 0) return 0;

  if (!encoding || encoding === "utf8") return byteLengthUtf8(string);

  if (encoding === "ascii") return len;

  const ops = getEncodingOps(encoding);
  if (ops === undefined) return byteLengthUtf8(string);

  return ops.byteLength(string);
}

Buffer.byteLength = byteLength;
Buffer.stringLength = stringLength;

Object.defineProperty(Buffer.prototype, "parent", {
  __proto__: null,
  enumerable: true,
  get() {
    if (!(this instanceof Buffer)) return undefined;
    return this.buffer;
  },
});
Object.defineProperty(Buffer.prototype, "offset", {
  __proto__: null,
  enumerable: true,
  get() {
    if (!(this instanceof Buffer)) return undefined;
    return this.byteOffset;
  },
});

Buffer.prototype.copy = function copy(
  target,
  targetStart,
  sourceStart,
  sourceEnd,
) {
  return copyImpl(this, target, targetStart, sourceStart, sourceEnd);
};

Buffer.prototype.toString = function toString(encoding, start, end) {
  if (arguments.length === 0) {
    return utf8Slice(this, 0, this.length);
  }

  const bufferLength = typedArrayLength.call(this);

  if (start <= 0) start = 0;
  else if (start >= bufferLength) return "";
  else start = Math.trunc(start) || 0;

  if (end === undefined || end > bufferLength) end = bufferLength;
  else end = Math.trunc(end) || 0;

  if (end <= start) return "";

  if (encoding === undefined) return utf8Slice(this, start, end);

  const ops = getEncodingOps(encoding);
  if (ops === undefined) throw unknownEncoding(encoding);

  return ops.slice(this, start, end);
};

Buffer.prototype.equals = function equals(otherBuffer) {
  if (!isUint8Array(otherBuffer)) {
    throw invalidArgType(
      "otherBuffer",
      ["Buffer", "Uint8Array"],
      otherBuffer,
    );
  }

  if (this === otherBuffer) return true;
  const len = typedArrayByteLength.call(this);
  if (len !== typedArrayByteLength.call(otherBuffer)) return false;

  return len === 0 || _compare(this, otherBuffer) === 0;
};

let INSPECT_MAX_BYTES = 50;

// own properties beyond the bytes are not listed, that needs util.inspect
Buffer.prototype[customInspectSymbol] = function inspect(recurseTimes, ctx) {
  const max = INSPECT_MAX_BYTES;
  const actualMax = Math.min(max, this.length);
  const remaining = this.length - max;
  let str = hexSlice(this, 0, actualMax)
    .replace(/(.{2})/g, "$1 ")
    .trim();
  if (remaining > 0) {
    str += ` ... ${remaining} more byte${remaining > 1 ? "s" : ""}`;
  }
  let constructorName = "Buffer";
  try {
    const { constructor } = this;
    if (
      typeof constructor === "function" &&
      Object.prototype.hasOwnProperty.call(constructor, "name")
    ) {
      constructorName = constructor.name;
    }
  } catch {
    // keep the default name
  }
  return `<${constructorName} ${str}>`;
};
Buffer.prototype.inspect = Buffer.prototype[customInspectSymbol];

Buffer.prototype.compare = function compare(
  target,
  targetStart,
  targetEnd,
  sourceStart,
  sourceEnd,
) {
  if (!isUint8Array(target)) {
    throw invalidArgType("target", ["Buffer", "Uint8Array"], target);
  }
  if (arguments.length === 1) return _compare(this, target);

  if (targetStart === undefined) targetStart = 0;
  else validateOffset(targetStart, "targetStart");

  if (targetEnd === undefined) targetEnd = target.length;
  else validateOffset(targetEnd, "targetEnd", 0, target.length);

  if (sourceStart === undefined) sourceStart = 0;
  else validateOffset(sourceStart, "sourceStart");

  if (sourceEnd === undefined) sourceEnd = this.length;
  else validateOffset(sourceEnd, "sourceEnd", 0, this.length);

  if (sourceStart >= sourceEnd) return targetStart >= targetEnd ? 0 : -1;

  if (targetStart >= targetEnd) return 1;

  return compareOffset(
    this,
    target,
    targetStart,
    sourceStart,
    targetEnd,
    sourceEnd,
  );
};

function bidirectionalIndexOf(buffer, val, byteOffset, end, encoding, dir) {
  validateBuffer(buffer);

  if (typeof byteOffset === "string") {
    encoding = byteOffset;
    byteOffset = undefined;
  } else if (byteOffset > 0x7fffffff) {
    byteOffset = 0x7fffffff;
  } else if (byteOffset < -0x80000000) {
    byteOffset = -0x80000000;
  }
  // Coerce to Number. Values like null and [] become 0.
  byteOffset = +byteOffset;
  // If the offset is undefined, "foo", {}, coerces to NaN, search whole buffer.
  if (Number.isNaN(byteOffset)) {
    byteOffset = dir ? 0 : buffer.length || buffer.byteLength;
  }
  dir = !!dir;

  if (typeof val === "number") {
    return indexOfNumber(buffer, val >>> 0, byteOffset, dir, end);
  }

  let ops;
  if (encoding === undefined) ops = encodingOps.utf8;
  else ops = getEncodingOps(encoding);

  if (typeof val === "string") {
    if (ops === undefined) throw unknownEncoding(encoding);
    return ops.indexOf(buffer, val, byteOffset, dir, end);
  }

  if (isUint8Array(val)) {
    const step = ops !== undefined && ops.encodingVal === "utf16le" ? 2 : 1;
    return indexOfBuffer(buffer, val, byteOffset, step, dir, end);
  }

  throw invalidArgType(
    "value",
    ["number", "string", "Buffer", "Uint8Array"],
    val,
  );
}

Buffer.prototype.indexOf = function indexOf(val, offset, end, encoding) {
  if (typeof end === "string") {
    encoding = end;
    end = this.length;
  } else if (end === undefined) {
    end = this.length;
  }
  return bidirectionalIndexOf(this, val, offset, end, encoding, true);
};

Buffer.prototype.lastIndexOf = function lastIndexOf(
  val,
  offset,
  end,
  encoding,
) {
  if (typeof end === "string") {
    encoding = end;
    end = this.length;
  } else if (end === undefined) {
    end = this.length;
  }
  return bidirectionalIndexOf(this, val, offset, end, encoding, false);
};

Buffer.prototype.includes = function includes(val, offset, end, encoding) {
  if (typeof end === "string") {
    encoding = end;
    end = this.length;
  } else if (end === undefined) {
    end = this.length;
  }
  return bidirectionalIndexOf(this, val, offset, end, encoding, true) !== -1;
};

Buffer.prototype.fill = function fill(value, offset, end, encoding) {
  return _fill(this, value, offset, end, encoding);
};

function _fill(buf, value, offset, end, encoding) {
  if (typeof value === "string") {
    if (offset === undefined || typeof offset === "string") {
      encoding = offset;
      offset = 0;
      end = buf.length;
    } else if (typeof end === "string") {
      encoding = end;
      end = buf.length;
    }

    const normalizedEncoding = normalizeEncoding(encoding);
    if (normalizedEncoding === undefined) {
      validateString(encoding, "encoding");
      throw unknownEncoding(encoding);
    }

    if (value.length === 0) {
      // If value === '' default to zero.
      value = 0;
    } else if (value.length === 1) {
      // Fast path: If `value` fits into a single byte, use that numeric value.
      if (normalizedEncoding === "utf8" || normalizedEncoding === "ascii") {
        const code = value.charCodeAt(0);
        if (code < 128) value = code;
      } else if (normalizedEncoding === "latin1") {
        value = value.charCodeAt(0);
      }
    }
  } else {
    encoding = undefined;
  }

  if (offset === undefined) {
    offset = 0;
    end = buf.length;
  } else {
    validateOffset(offset, "offset");
    // Invalid ranges are not set to a default, so can range check early.
    if (end === undefined) {
      end = buf.length;
    } else {
      validateOffset(end, "end", 0, buf.length);
    }
    if (offset >= end) return buf;
  }

  if (typeof value === "number") {
    const byteLen = typedArrayByteLength.call(buf);
    const fillLength = end - offset;
    if (offset > end || fillLength + offset > byteLen) {
      throw bufferOutOfBounds();
    }

    typedArrayFill.call(buf, value, offset, end);
  } else {
    const res = bindingFill(buf, value, offset, end, encoding);
    if (res < 0) {
      if (res === -1) throw invalidArgValue("value", value);
      throw bufferOutOfBounds();
    }
  }

  return buf;
}

Buffer.prototype.write = function write(string, offset, length, encoding) {
  const bufferLength = typedArrayLength.call(this);
  // Buffer#write(string);
  if (offset === undefined) {
    return utf8Write(this, string, 0, bufferLength);
  }
  // Buffer#write(string, encoding)
  if (length === undefined && typeof offset === "string") {
    encoding = offset;
    length = bufferLength;
    offset = 0;

    // Buffer#write(string, offset[, length][, encoding])
  } else {
    validateOffset(offset, "offset", 0, bufferLength);

    const remaining = bufferLength - offset;

    if (length === undefined) {
      length = remaining;
    } else if (typeof length === "string") {
      encoding = length;
      length = remaining;
    } else {
      validateOffset(length, "length", 0, bufferLength);
      if (length > remaining) length = remaining;
    }
  }

  if (!encoding || encoding === "utf8") {
    return utf8Write(this, string, offset, length);
  }
  if (encoding === "ascii") return asciiWrite(this, string, offset, length);

  const ops = getEncodingOps(encoding);
  if (ops === undefined) throw unknownEncoding(encoding);
  return ops.write(this, string, offset, length);
};

Buffer.prototype.toJSON = function toJSON() {
  const bufferLength = typedArrayLength.call(this);
  if (bufferLength > 0) {
    const data = new Array(bufferLength);
    for (let i = 0; i < bufferLength; ++i) data[i] = this[i];
    return { type: "Buffer", data };
  }
  return { type: "Buffer", data: [] };
};

function adjustOffset(offset, length) {
  // Use Math.trunc() to convert offset to an integer value that can be larger
  // than an Int32. Hence, don't use offset | 0 or similar techniques.
  offset = Math.trunc(offset);
  if (offset === 0) return 0;
  if (offset < 0) {
    offset += length;
    return offset > 0 ? offset : 0;
  }
  if (offset < length) return offset;
  return Number.isNaN(offset) ? 0 : length;
}

Buffer.prototype.subarray = function subarray(start, end) {
  const srcLength = typedArrayLength.call(this);
  start = adjustOffset(start, srcLength);
  end = end !== undefined ? adjustOffset(end, srcLength) : srcLength;
  const newLength = end > start ? end - start : 0;
  return new FastBuffer(this.buffer, this.byteOffset + start, newLength);
};

Buffer.prototype.slice = function slice(start, end) {
  return this.subarray(start, end);
};

function swap(b, n, m) {
  const i = b[n];
  b[n] = b[m];
  b[m] = i;
}

function swapWhole(buf, bits) {
  typedArraySet.call(buf, new Uint8Array(swapBytes(buf, bits)), 0);
}

Buffer.prototype.swap16 = function swap16() {
  // For Buffer.length <= 32, it's generally faster to
  // do the swap in javascript.
  const len = typedArrayLength.call(this);
  if (len % 2 !== 0) throw invalidBufferSize("16-bits");
  if (len <= 32) {
    for (let i = 0; i < len; i += 2) swap(this, i, i + 1);
    return this;
  }
  swapWhole(this, 16);
  return this;
};

Buffer.prototype.swap32 = function swap32() {
  const len = typedArrayLength.call(this);
  if (len % 4 !== 0) throw invalidBufferSize("32-bits");
  if (len <= 32) {
    for (let i = 0; i < len; i += 4) {
      swap(this, i, i + 3);
      swap(this, i + 1, i + 2);
    }
    return this;
  }
  swapWhole(this, 32);
  return this;
};

Buffer.prototype.swap64 = function swap64() {
  const len = typedArrayLength.call(this);
  if (len % 8 !== 0) throw invalidBufferSize("64-bits");
  if (len < 48) {
    for (let i = 0; i < len; i += 8) {
      swap(this, i, i + 7);
      swap(this, i + 1, i + 6);
      swap(this, i + 2, i + 5);
      swap(this, i + 3, i + 4);
    }
    return this;
  }
  swapWhole(this, 64);
  return this;
};

Buffer.prototype.toLocaleString = Buffer.prototype.toString;

// node's lib/internal/buffer.js: reading and writing numbers

const float32Array = new Float32Array(1);
const uInt8Float32Array = new Uint8Array(float32Array.buffer);
const float64Array = new Float64Array(1);
const uInt8Float64Array = new Uint8Array(float64Array.buffer);

float32Array[0] = -1;
const bigEndian = uInt8Float32Array[3] === 0;

function checkBounds(buf, offset, byteLength) {
  validateNumber(offset, "offset");
  if (buf[offset] === undefined || buf[offset + byteLength] === undefined)
    boundsError(offset, buf.length - (byteLength + 1));
}

function checkInt(value, min, max, buf, offset, byteLength) {
  if (value > max || value < min) {
    const n = typeof min === "bigint" ? "n" : "";
    let range;
    if (byteLength > 3) {
      if (min === 0 || min === 0n) {
        range = `>= 0${n} and < 2${n} ** ${(byteLength + 1) * 8}${n}`;
      } else {
        range = `>= -(2${n} ** ${(byteLength + 1) * 8 - 1}${n}) and ` +
                `< 2${n} ** ${(byteLength + 1) * 8 - 1}${n}`;
      }
    } else {
      range = `>= ${min}${n} and <= ${max}${n}`;
    }
    throw outOfRange("value", range, value);
  }
  checkBounds(buf, offset, byteLength);
}

function boundsError(value, length, type) {
  if (Math.floor(value) !== value) {
    validateNumber(value, type);
    throw outOfRange(type || "offset", "an integer", value);
  }

  if (length < 0)
    throw bufferOutOfBounds();

  throw outOfRange(type || "offset",
                             `>= ${type ? 1 : 0} and <= ${length}`,
                             value);
}

function readBigUInt64LE(offset = 0) {
  validateNumber(offset, "offset");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  const lo = first +
    this[++offset] * 2 ** 8 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 24;

  const hi = this[++offset] +
    this[++offset] * 2 ** 8 +
    this[++offset] * 2 ** 16 +
    last * 2 ** 24;

  return BigInt(lo) + (BigInt(hi) << 32n);
}

function readBigUInt64BE(offset = 0) {
  validateNumber(offset, "offset");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  const hi = first * 2 ** 24 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 8 +
    this[++offset];

  const lo = this[++offset] * 2 ** 24 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 8 +
    last;

  return (BigInt(hi) << 32n) + BigInt(lo);
}

function readBigInt64LE(offset = 0) {
  validateNumber(offset, "offset");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  const val = this[offset + 4] +
    this[offset + 5] * 2 ** 8 +
    this[offset + 6] * 2 ** 16 +
    (last << 24);
  return (BigInt(val) << 32n) +
    BigInt(first +
    this[++offset] * 2 ** 8 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 24);
}

function readBigInt64BE(offset = 0) {
  validateNumber(offset, "offset");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  const val = (first << 24) + // Overflow
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 8 +
    this[++offset];
  return (BigInt(val) << 32n) +
    BigInt(this[++offset] * 2 ** 24 +
    this[++offset] * 2 ** 16 +
    this[++offset] * 2 ** 8 +
    last);
}

function readUIntLE(offset, byteLength) {
  if (offset === undefined)
    throw invalidArgType("offset", "number", offset);
  if (byteLength === 6)
    return readUInt48LE(this, offset);
  if (byteLength === 5)
    return readUInt40LE(this, offset);
  if (byteLength === 3)
    return readUInt24LE(this, offset);
  if (byteLength === 4)
    return readUInt32LE(this, offset);
  if (byteLength === 2)
    return readUInt16LE(this, offset);
  if (byteLength === 1)
    return readUInt8(this, offset);

  boundsError(byteLength, 6, "byteLength");
}

function readUInt48LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 5];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 6);

  return first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 24 +
    (buf[++offset] + last * 2 ** 8) * 2 ** 32;
}

function readUInt40LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 4];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 5);

  return first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 24 +
    last * 2 ** 32;
}

function readUInt32LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 4);

  return first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    last * 2 ** 24;
}

function readUInt24LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 2];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 3);

  return first + buf[++offset] * 2 ** 8 + last * 2 ** 16;
}

function readUInt16LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 1];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 2);

  return first + last * 2 ** 8;
}

function readUInt8(buf, offset = 0) {
  validateNumber(offset, "offset");
  const val = buf[offset];
  if (val === undefined)
    boundsError(offset, buf.length - 1);

  return val;
}

function readUIntBE(offset, byteLength) {
  if (offset === undefined)
    throw invalidArgType("offset", "number", offset);
  if (byteLength === 6)
    return readUInt48BE(this, offset);
  if (byteLength === 5)
    return readUInt40BE(this, offset);
  if (byteLength === 3)
    return readUInt24BE(this, offset);
  if (byteLength === 4)
    return readUInt32BE(this, offset);
  if (byteLength === 2)
    return readUInt16BE(this, offset);
  if (byteLength === 1)
    return readUInt8(this, offset);

  boundsError(byteLength, 6, "byteLength");
}

function readUInt48BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 5];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 6);

  return (first * 2 ** 8 + buf[++offset]) * 2 ** 32 +
    buf[++offset] * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readUInt40BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 4];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 5);

  return first * 2 ** 32 +
    buf[++offset] * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readUInt32BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 4);

  return first * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readUInt24BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 2];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 3);

  return first * 2 ** 16 + buf[++offset] * 2 ** 8 + last;
}

function readUInt16BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 1];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 2);

  return first * 2 ** 8 + last;
}

function readIntLE(offset, byteLength) {
  if (offset === undefined)
    throw invalidArgType("offset", "number", offset);
  if (byteLength === 6)
    return readInt48LE(this, offset);
  if (byteLength === 5)
    return readInt40LE(this, offset);
  if (byteLength === 3)
    return readInt24LE(this, offset);
  if (byteLength === 4)
    return readInt32LE(this, offset);
  if (byteLength === 2)
    return readInt16LE(this, offset);
  if (byteLength === 1)
    return readInt8(this, offset);

  boundsError(byteLength, 6, "byteLength");
}

function readInt48LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 5];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 6);

  const val = buf[offset + 4] + last * 2 ** 8;
  return (val | (val & 2 ** 15) * 0x1fffe) * 2 ** 32 +
    first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 24;
}

function readInt40LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 4];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 5);

  return (last | (last & 2 ** 7) * 0x1fffffe) * 2 ** 32 +
    first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 24;
}

function readInt32LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 4);

  return first +
    buf[++offset] * 2 ** 8 +
    buf[++offset] * 2 ** 16 +
    (last << 24);
}

function readInt24LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 2];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 3);

  const val = first + buf[++offset] * 2 ** 8 + last * 2 ** 16;
  return val | (val & 2 ** 23) * 0x1fe;
}

function readInt16LE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 1];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 2);

  const val = first + last * 2 ** 8;
  return val | (val & 2 ** 15) * 0x1fffe;
}

function readInt8(buf, offset = 0) {
  validateNumber(offset, "offset");
  const val = buf[offset];
  if (val === undefined)
    boundsError(offset, buf.length - 1);

  return val | (val & 2 ** 7) * 0x1fffffe;
}

function readIntBE(offset, byteLength) {
  if (offset === undefined)
    throw invalidArgType("offset", "number", offset);
  if (byteLength === 6)
    return readInt48BE(this, offset);
  if (byteLength === 5)
    return readInt40BE(this, offset);
  if (byteLength === 3)
    return readInt24BE(this, offset);
  if (byteLength === 4)
    return readInt32BE(this, offset);
  if (byteLength === 2)
    return readInt16BE(this, offset);
  if (byteLength === 1)
    return readInt8(this, offset);

  boundsError(byteLength, 6, "byteLength");
}

function readInt48BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 5];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 6);

  const val = buf[++offset] + first * 2 ** 8;
  return (val | (val & 2 ** 15) * 0x1fffe) * 2 ** 32 +
    buf[++offset] * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readInt40BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 4];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 5);

  return (first | (first & 2 ** 7) * 0x1fffffe) * 2 ** 32 +
    buf[++offset] * 2 ** 24 +
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readInt32BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 4);

  return (first << 24) + // Overflow
    buf[++offset] * 2 ** 16 +
    buf[++offset] * 2 ** 8 +
    last;
}

function readInt24BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 2];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 3);

  const val = first * 2 ** 16 + buf[++offset] * 2 ** 8 + last;
  return val | (val & 2 ** 23) * 0x1fe;
}

function readInt16BE(buf, offset = 0) {
  validateNumber(offset, "offset");
  const first = buf[offset];
  const last = buf[offset + 1];
  if (first === undefined || last === undefined)
    boundsError(offset, buf.length - 2);

  const val = first * 2 ** 8 + last;
  return val | (val & 2 ** 15) * 0x1fffe;
}

function readFloatBackwards(offset = 0) {
  validateNumber(offset, "offset");
  const first = this[offset];
  const last = this[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 4);

  uInt8Float32Array[3] = first;
  uInt8Float32Array[2] = this[++offset];
  uInt8Float32Array[1] = this[++offset];
  uInt8Float32Array[0] = last;
  return float32Array[0];
}

function readFloatForwards(offset = 0) {
  validateNumber(offset, "offset");
  const first = this[offset];
  const last = this[offset + 3];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 4);

  uInt8Float32Array[0] = first;
  uInt8Float32Array[1] = this[++offset];
  uInt8Float32Array[2] = this[++offset];
  uInt8Float32Array[3] = last;
  return float32Array[0];
}

function readDoubleBackwards(offset = 0) {
  validateNumber(offset, "offset");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  uInt8Float64Array[7] = first;
  uInt8Float64Array[6] = this[++offset];
  uInt8Float64Array[5] = this[++offset];
  uInt8Float64Array[4] = this[++offset];
  uInt8Float64Array[3] = this[++offset];
  uInt8Float64Array[2] = this[++offset];
  uInt8Float64Array[1] = this[++offset];
  uInt8Float64Array[0] = last;
  return float64Array[0];
}

function readDoubleForwards(offset = 0) {
  validateNumber(offset, "offset");
  const first = this[offset];
  const last = this[offset + 7];
  if (first === undefined || last === undefined)
    boundsError(offset, this.length - 8);

  uInt8Float64Array[0] = first;
  uInt8Float64Array[1] = this[++offset];
  uInt8Float64Array[2] = this[++offset];
  uInt8Float64Array[3] = this[++offset];
  uInt8Float64Array[4] = this[++offset];
  uInt8Float64Array[5] = this[++offset];
  uInt8Float64Array[6] = this[++offset];
  uInt8Float64Array[7] = last;
  return float64Array[0];
}

function writeBigU_Int64LE(buf, value, offset, min, max) {
  checkInt(value, min, max, buf, offset, 7);

  let lo = Number(value & 0xffffffffn);
  buf[offset++] = lo;
  lo = lo >> 8;
  buf[offset++] = lo;
  lo = lo >> 8;
  buf[offset++] = lo;
  lo = lo >> 8;
  buf[offset++] = lo;
  let hi = Number(value >> 32n & 0xffffffffn);
  buf[offset++] = hi;
  hi = hi >> 8;
  buf[offset++] = hi;
  hi = hi >> 8;
  buf[offset++] = hi;
  hi = hi >> 8;
  buf[offset++] = hi;
  return offset;
}

function writeBigUInt64LE(value, offset = 0) {
  return writeBigU_Int64LE(this, value, offset, 0n, 0xffffffffffffffffn);
}

function writeBigU_Int64BE(buf, value, offset, min, max) {
  checkInt(value, min, max, buf, offset, 7);

  let lo = Number(value & 0xffffffffn);
  buf[offset + 7] = lo;
  lo = lo >> 8;
  buf[offset + 6] = lo;
  lo = lo >> 8;
  buf[offset + 5] = lo;
  lo = lo >> 8;
  buf[offset + 4] = lo;
  let hi = Number(value >> 32n & 0xffffffffn);
  buf[offset + 3] = hi;
  hi = hi >> 8;
  buf[offset + 2] = hi;
  hi = hi >> 8;
  buf[offset + 1] = hi;
  hi = hi >> 8;
  buf[offset] = hi;
  return offset + 8;
}

function writeBigUInt64BE(value, offset = 0) {
  return writeBigU_Int64BE(this, value, offset, 0n, 0xffffffffffffffffn);
}

function writeBigInt64LE(value, offset = 0) {
  return writeBigU_Int64LE(
    this, value, offset, -0x8000000000000000n, 0x7fffffffffffffffn);
}

function writeBigInt64BE(value, offset = 0) {
  return writeBigU_Int64BE(
    this, value, offset, -0x8000000000000000n, 0x7fffffffffffffffn);
}

function writeUIntLE(value, offset, byteLength) {
  if (byteLength === 6)
    return writeU_Int48LE(this, value, offset, 0, 0xffffffffffff);
  if (byteLength === 5)
    return writeU_Int40LE(this, value, offset, 0, 0xffffffffff);
  if (byteLength === 3)
    return writeU_Int24LE(this, value, offset, 0, 0xffffff);
  if (byteLength === 4)
    return writeU_Int32LE(this, value, offset, 0, 0xffffffff);
  if (byteLength === 2)
    return writeU_Int16LE(this, value, offset, 0, 0xffff);
  if (byteLength === 1)
    return writeU_Int8(this, value, offset, 0, 0xff);

  boundsError(byteLength, 6, "byteLength");
}

function writeU_Int48LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 5);

  const newVal = Math.floor(value * 2 ** -32);
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  buf[offset++] = newVal;
  buf[offset++] = (newVal >>> 8);
  return offset;
}

function writeU_Int40LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 4);

  const newVal = value;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  buf[offset++] = Math.floor(newVal * 2 ** -32);
  return offset;
}

function writeU_Int32LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 3);

  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  return offset;
}

function writeUInt32LE(value, offset = 0) {
  return writeU_Int32LE(this, value, offset, 0, 0xffffffff);
}

function writeU_Int24LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 2);

  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  value = value >>> 8;
  buf[offset++] = value;
  return offset;
}

function writeU_Int16LE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 1);

  buf[offset++] = value;
  buf[offset++] = (value >>> 8);
  return offset;
}

function writeUInt16LE(value, offset = 0) {
  return writeU_Int16LE(this, value, offset, 0, 0xffff);
}

function writeU_Int8(buf, value, offset, min, max) {
  value = +value;
  validateNumber(offset, "offset");
  if (value > max || value < min) {
    throw outOfRange("value", `>= ${min} and <= ${max}`, value);
  }
  if (buf[offset] === undefined)
    boundsError(offset, buf.length - 1);

  buf[offset] = value;
  return offset + 1;
}

function writeUInt8(value, offset = 0) {
  return writeU_Int8(this, value, offset, 0, 0xff);
}

function writeUIntBE(value, offset, byteLength) {
  if (byteLength === 6)
    return writeU_Int48BE(this, value, offset, 0, 0xffffffffffff);
  if (byteLength === 5)
    return writeU_Int40BE(this, value, offset, 0, 0xffffffffff);
  if (byteLength === 3)
    return writeU_Int24BE(this, value, offset, 0, 0xffffff);
  if (byteLength === 4)
    return writeU_Int32BE(this, value, offset, 0, 0xffffffff);
  if (byteLength === 2)
    return writeU_Int16BE(this, value, offset, 0, 0xffff);
  if (byteLength === 1)
    return writeU_Int8(this, value, offset, 0, 0xff);

  boundsError(byteLength, 6, "byteLength");
}

function writeU_Int48BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 5);

  const newVal = Math.floor(value * 2 ** -32);
  buf[offset++] = (newVal >>> 8);
  buf[offset++] = newVal;
  buf[offset + 3] = value;
  value = value >>> 8;
  buf[offset + 2] = value;
  value = value >>> 8;
  buf[offset + 1] = value;
  value = value >>> 8;
  buf[offset] = value;
  return offset + 4;
}

function writeU_Int40BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 4);

  buf[offset++] = Math.floor(value * 2 ** -32);
  buf[offset + 3] = value;
  value = value >>> 8;
  buf[offset + 2] = value;
  value = value >>> 8;
  buf[offset + 1] = value;
  value = value >>> 8;
  buf[offset] = value;
  return offset + 4;
}

function writeU_Int32BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 3);

  buf[offset + 3] = value;
  value = value >>> 8;
  buf[offset + 2] = value;
  value = value >>> 8;
  buf[offset + 1] = value;
  value = value >>> 8;
  buf[offset] = value;
  return offset + 4;
}

function writeUInt32BE(value, offset = 0) {
  return writeU_Int32BE(this, value, offset, 0, 0xffffffff);
}

function writeU_Int24BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 2);

  buf[offset + 2] = value;
  value = value >>> 8;
  buf[offset + 1] = value;
  value = value >>> 8;
  buf[offset] = value;
  return offset + 3;
}

function writeU_Int16BE(buf, value, offset, min, max) {
  value = +value;
  checkInt(value, min, max, buf, offset, 1);

  buf[offset++] = (value >>> 8);
  buf[offset++] = value;
  return offset;
}

function writeUInt16BE(value, offset = 0) {
  return writeU_Int16BE(this, value, offset, 0, 0xffff);
}

function writeIntLE(value, offset, byteLength) {
  if (byteLength === 6)
    return writeU_Int48LE(this, value, offset, -0x800000000000, 0x7fffffffffff);
  if (byteLength === 5)
    return writeU_Int40LE(this, value, offset, -0x8000000000, 0x7fffffffff);
  if (byteLength === 3)
    return writeU_Int24LE(this, value, offset, -0x800000, 0x7fffff);
  if (byteLength === 4)
    return writeU_Int32LE(this, value, offset, -0x80000000, 0x7fffffff);
  if (byteLength === 2)
    return writeU_Int16LE(this, value, offset, -0x8000, 0x7fff);
  if (byteLength === 1)
    return writeU_Int8(this, value, offset, -0x80, 0x7f);

  boundsError(byteLength, 6, "byteLength");
}

function writeInt32LE(value, offset = 0) {
  return writeU_Int32LE(this, value, offset, -0x80000000, 0x7fffffff);
}

function writeInt16LE(value, offset = 0) {
  return writeU_Int16LE(this, value, offset, -0x8000, 0x7fff);
}

function writeInt8(value, offset = 0) {
  return writeU_Int8(this, value, offset, -0x80, 0x7f);
}

function writeIntBE(value, offset, byteLength) {
  if (byteLength === 6)
    return writeU_Int48BE(this, value, offset, -0x800000000000, 0x7fffffffffff);
  if (byteLength === 5)
    return writeU_Int40BE(this, value, offset, -0x8000000000, 0x7fffffffff);
  if (byteLength === 3)
    return writeU_Int24BE(this, value, offset, -0x800000, 0x7fffff);
  if (byteLength === 4)
    return writeU_Int32BE(this, value, offset, -0x80000000, 0x7fffffff);
  if (byteLength === 2)
    return writeU_Int16BE(this, value, offset, -0x8000, 0x7fff);
  if (byteLength === 1)
    return writeU_Int8(this, value, offset, -0x80, 0x7f);

  boundsError(byteLength, 6, "byteLength");
}

function writeInt32BE(value, offset = 0) {
  return writeU_Int32BE(this, value, offset, -0x80000000, 0x7fffffff);
}

function writeInt16BE(value, offset = 0) {
  return writeU_Int16BE(this, value, offset, -0x8000, 0x7fff);
}

function writeDoubleForwards(val, offset = 0) {
  val = +val;
  checkBounds(this, offset, 7);

  float64Array[0] = val;
  this[offset++] = uInt8Float64Array[0];
  this[offset++] = uInt8Float64Array[1];
  this[offset++] = uInt8Float64Array[2];
  this[offset++] = uInt8Float64Array[3];
  this[offset++] = uInt8Float64Array[4];
  this[offset++] = uInt8Float64Array[5];
  this[offset++] = uInt8Float64Array[6];
  this[offset++] = uInt8Float64Array[7];
  return offset;
}

function writeDoubleBackwards(val, offset = 0) {
  val = +val;
  checkBounds(this, offset, 7);

  float64Array[0] = val;
  this[offset++] = uInt8Float64Array[7];
  this[offset++] = uInt8Float64Array[6];
  this[offset++] = uInt8Float64Array[5];
  this[offset++] = uInt8Float64Array[4];
  this[offset++] = uInt8Float64Array[3];
  this[offset++] = uInt8Float64Array[2];
  this[offset++] = uInt8Float64Array[1];
  this[offset++] = uInt8Float64Array[0];
  return offset;
}

function writeFloatForwards(val, offset = 0) {
  val = +val;
  checkBounds(this, offset, 3);

  float32Array[0] = val;
  this[offset++] = uInt8Float32Array[0];
  this[offset++] = uInt8Float32Array[1];
  this[offset++] = uInt8Float32Array[2];
  this[offset++] = uInt8Float32Array[3];
  return offset;
}

function writeFloatBackwards(val, offset = 0) {
  val = +val;
  checkBounds(this, offset, 3);

  float32Array[0] = val;
  this[offset++] = uInt8Float32Array[3];
  this[offset++] = uInt8Float32Array[2];
  this[offset++] = uInt8Float32Array[1];
  this[offset++] = uInt8Float32Array[0];
  return offset;
}

const proto = Buffer.prototype;
proto.readBigUInt64LE = readBigUInt64LE;
proto.readBigUInt64BE = readBigUInt64BE;
proto.readBigUint64LE = readBigUInt64LE;
proto.readBigUint64BE = readBigUInt64BE;
proto.readBigInt64LE = readBigInt64LE;
proto.readBigInt64BE = readBigInt64BE;
proto.writeBigUInt64LE = writeBigUInt64LE;
proto.writeBigUInt64BE = writeBigUInt64BE;
proto.writeBigUint64LE = writeBigUInt64LE;
proto.writeBigUint64BE = writeBigUInt64BE;
proto.writeBigInt64LE = writeBigInt64LE;
proto.writeBigInt64BE = writeBigInt64BE;
proto.readUIntLE = readUIntLE;
proto.readUInt32LE = function(offset) { return readUInt32LE(this, offset); };
proto.readUInt16LE = function(offset) { return readUInt16LE(this, offset); };
proto.readUInt8 = function(offset) { return readUInt8(this, offset); };
proto.readUIntBE = readUIntBE;
proto.readUInt32BE = function(offset) { return readUInt32BE(this, offset); };
proto.readUInt16BE = function(offset) { return readUInt16BE(this, offset); };
proto.readUintLE = readUIntLE;
proto.readUint32LE = proto.readUInt32LE;
proto.readUint16LE = proto.readUInt16LE;
proto.readUint8 = proto.readUInt8;
proto.readUintBE = readUIntBE;
proto.readUint32BE = proto.readUInt32BE;
proto.readUint16BE = proto.readUInt16BE;
proto.readIntLE = readIntLE;
proto.readInt32LE = function(offset) { return readInt32LE(this, offset); };
proto.readInt16LE = function(offset) { return readInt16LE(this, offset); };
proto.readInt8 = function(offset) { return readInt8(this, offset); };
proto.readIntBE = readIntBE;
proto.readInt32BE = function(offset) { return readInt32BE(this, offset); };
proto.readInt16BE = function(offset) { return readInt16BE(this, offset); };
proto.writeUIntLE = writeUIntLE;
proto.writeUInt32LE = writeUInt32LE;
proto.writeUInt16LE = writeUInt16LE;
proto.writeUInt8 = writeUInt8;
proto.writeUIntBE = writeUIntBE;
proto.writeUInt32BE = writeUInt32BE;
proto.writeUInt16BE = writeUInt16BE;
proto.writeUintLE = writeUIntLE;
proto.writeUint32LE = writeUInt32LE;
proto.writeUint16LE = writeUInt16LE;
proto.writeUint8 = writeUInt8;
proto.writeUintBE = writeUIntBE;
proto.writeUint32BE = writeUInt32BE;
proto.writeUint16BE = writeUInt16BE;
proto.writeIntLE = writeIntLE;
proto.writeInt32LE = writeInt32LE;
proto.writeInt16LE = writeInt16LE;
proto.writeInt8 = writeInt8;
proto.writeIntBE = writeIntBE;
proto.writeInt32BE = writeInt32BE;
proto.writeInt16BE = writeInt16BE;
proto.readFloatLE = bigEndian ? readFloatBackwards : readFloatForwards;
proto.readFloatBE = bigEndian ? readFloatForwards : readFloatBackwards;
proto.readDoubleLE = bigEndian ? readDoubleBackwards : readDoubleForwards;
proto.readDoubleBE = bigEndian ? readDoubleForwards : readDoubleBackwards;
proto.writeFloatLE = bigEndian ? writeFloatBackwards : writeFloatForwards;
proto.writeFloatBE = bigEndian ? writeFloatForwards : writeFloatBackwards;
proto.writeDoubleLE = bigEndian ? writeDoubleBackwards : writeDoubleForwards;
proto.writeDoubleBE = bigEndian ? writeDoubleForwards : writeDoubleBackwards;
proto.asciiSlice = function (start, end) {
  return asciiSlice(this, start, end);
};
proto.base64Slice = function (start, end) {
  return base64Slice(this, start, end);
};
proto.base64urlSlice = function (start, end) {
  return base64urlSlice(this, start, end);
};
proto.latin1Slice = function (start, end) {
  return latin1Slice(this, start, end);
};
proto.hexSlice = function (start, end) {
  return hexSlice(this, start, end);
};
proto.ucs2Slice = function (start, end) {
  return ucs2Slice(this, start, end);
};
proto.utf8Slice = function (start, end) {
  return utf8Slice(this, start, end);
};
proto.asciiWrite = function (string, offset, length) {
  return asciiWrite(this, string, offset, length);
};
proto.base64Write = function (string, offset, length) {
  return base64Write(this, string, offset, length);
};
proto.base64urlWrite = function (string, offset, length) {
  return base64urlWrite(this, string, offset, length);
};
proto.latin1Write = function (string, offset, length) {
  return latin1Write(this, string, offset, length);
};
proto.hexWrite = function (string, offset, length) {
  return hexWrite(this, string, offset, length);
};
proto.ucs2Write = function (string, offset, length) {
  return ucs2Write(this, string, offset, length);
};
proto.utf8Write = function (string, offset, length) {
  return utf8Write(this, string, offset, length);
};

function btoa(input) {
  if (arguments.length === 0) throw missingArgs("input");
  const text = `${input}`;
  if (!stringIsLatin1(text)) {
    throw new DOMException("Invalid character", "InvalidCharacterError");
  }
  return decode(encode(text, "latin1"), "base64", 0, text.length);
}

function atob(input) {
  if (arguments.length === 0) throw missingArgs("input");

  const result = decodeBase64Strict(`${input}`);

  switch (result) {
    case -2: // Invalid character
      throw new DOMException("Invalid character", "InvalidCharacterError");
    case -1: // Single character remained
      throw new DOMException(
        "The string to be decoded is not correctly encoded.",
        "InvalidCharacterError",
      );
    default:
      return result;
  }
}

function isUtf8(input) {
  if (isTypedArray(input) || isAnyArrayBuffer(input)) {
    return bytesAreUtf8(input);
  }

  throw invalidArgType("input", ["ArrayBuffer", "Buffer", "TypedArray"], input);
}

function isAscii(input) {
  if (isTypedArray(input) || isAnyArrayBuffer(input)) {
    return bytesAreAscii(input);
  }

  throw invalidArgType("input", ["ArrayBuffer", "Buffer", "TypedArray"], input);
}

function isLatin1(input) {
  validateString(input, "input");
  return stringIsLatin1(input);
}

function stringLength(input, encoding = "utf8") {
  if (!isTypedArray(input) && !isAnyArrayBuffer(input)) {
    throw invalidArgType(
      "input",
      ["ArrayBuffer", "Buffer", "TypedArray"],
      input,
    );
  }
  validateString(encoding, "encoding");
  const ops = getEncodingOps(encoding);
  if (ops === undefined) throw unknownEncoding(encoding);
  const length = input.byteLength;
  switch (ops.encodingVal) {
    case "utf8":
      return length === 0 ? 0 : utf16Units(input);
    case "utf16le":
      return Math.floor(length / 2);
    case "hex":
      return length * 2;
    case "base64":
      return Math.ceil(length / 3) * 4;
    case "base64url":
      return Math.ceil((length * 4) / 3);
    default: // latin1, ascii
      return length;
  }
}

module.exports = {
  Buffer,
  isUtf8,
  isAscii,
  isLatin1,
  kMaxLength,
  kStringMaxLength,
  btoa,
  atob,
};

Object.defineProperties(module.exports, {
  constants: {
    __proto__: null,
    configurable: false,
    enumerable: true,
    value: constants,
  },
  INSPECT_MAX_BYTES: {
    __proto__: null,
    configurable: true,
    enumerable: true,
    get() {
      return INSPECT_MAX_BYTES;
    },
    set(val) {
      validateNumber(val, "INSPECT_MAX_BYTES", 0);
      INSPECT_MAX_BYTES = val;
    },
  },
});
