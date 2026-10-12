"use strict";

// node's digests and random bytes: lib/internal/crypto/hash.js, random.js and
// util.js, with the handles they drive, c++ in node, written out below

const internal = require("spark:internal");
const loop = require("spark:loop");
const { Buffer } = require("node:buffer");
const { DOMException } = require("spark:domexception");

internal.addLoader(own);

function notHere(what) {
  const { codes } = internal.load("internal/errors");
  return new codes.ERR_METHOD_NOT_IMPLEMENTED(what);
}

function missing(name) {
  return class {
    constructor() {
      throw notHere(name);
    }
  };
}

// an encoding node does not know is read as utf8, as node's own code reads it
function bytesOf(data, encoding) {
  if (typeof data === "string") {
    return Buffer.from(data, Buffer.isEncoding(encoding) ? encoding : "utf8");
  }
  return new Uint8Array(data.buffer, data.byteOffset, data.byteLength).slice();
}

function joined(chunks) {
  let size = 0;
  for (const chunk of chunks) size += chunk.length;
  const all = new Uint8Array(size);
  let at = 0;
  for (const chunk of chunks) {
    all.set(chunk, at);
    at += chunk.length;
  }
  return all;
}

// and one it does not know for the digest gives the bytes
function encoded(bytes, encoding) {
  const buffer = Buffer.from(bytes);
  return Buffer.isEncoding(encoding) ? buffer.toString(encoding) : buffer;
}

// a digest in progress. It keeps what it was given and works the digest out
// at the end, in one call that leaves nothing behind
class Hash {
  #name;
  #length;
  #chunks;

  constructor(algorithm, length) {
    if (algorithm instanceof Hash) {
      this.#name = algorithm.#name;
      this.#length = length ?? algorithm.#length;
      this.#chunks = [...algorithm.#chunks];
      return;
    }
    if (native.hash(algorithm, new Uint8Array(0), length) === undefined) {
      throw new Error("Digest method not supported");
    }
    this.#name = algorithm;
    this.#length = length;
    this.#chunks = [];
  }

  update(data, encoding) {
    this.#chunks.push(bytesOf(data, encoding));
    return true;
  }

  digest(encoding) {
    return encoded(native.hash(this.#name, joined(this.#chunks), this.#length), encoding);
  }
}

class Hmac {
  #name;
  #key;
  #chunks = [];

  init(name, key) {
    const { codes } = internal.load("internal/errors");
    key = bytesOf(key);
    if (native.hmac(name, key, new Uint8Array(0)) === undefined) {
      throw new codes.ERR_CRYPTO_INVALID_DIGEST(name);
    }
    this.#name = name;
    this.#key = key;
  }

  update(data, encoding) {
    this.#chunks.push(bytesOf(data, encoding));
    return true;
  }

  digest(encoding) {
    return encoded(native.hmac(this.#name, this.#key, joined(this.#chunks)), encoding);
  }
}

function fill(target, offset, size) {
  if (size === 0) return;
  const view = ArrayBuffer.isView(target)
    ? new Uint8Array(target.buffer, target.byteOffset + offset, size)
    : new Uint8Array(target, offset, size);
  view.set(new Uint8Array(native.random(size)));
}

const kCryptoJobAsync = 0;
const kCryptoJobSync = 1;

class RandomBytesJob {
  #mode;
  #fill;

  constructor(mode, target, offset, size) {
    this.#mode = mode;
    this.#fill = () => fill(target, offset, size);
  }

  run() {
    this.#fill();
    if (this.#mode === kCryptoJobSync) return [undefined, undefined];
    loop.nextTick(() => this.ondone(undefined));
    return undefined;
  }
}

const crypto = {
  Hash,
  Hmac,
  oneShotDigest(algorithm, id, cache, input, outputEncoding, encodingId, outputLength) {
    const digest = native.hash(algorithm, bytesOf(input, "utf8"), outputLength);
    if (digest === undefined) {
      throw new Error(`Digest method ${algorithm} is not supported`);
    }
    return encoded(digest, outputEncoding);
  },
  HashJob: missing("subtle digests other than sha"),
  TurboShakeJob: missing("TurboSHAKE"),
  KangarooTwelveJob: missing("KangarooTwelve"),
  KmacJob: missing("KMAC"),
  Argon2Job: missing("argon2"),
  RandomPrimeJob: missing("generatePrime"),
  CheckPrimeJob: missing("checkPrime"),
  RandomBytesJob,
  kCryptoJobAsync,
  kCryptoJobSync,
  kCryptoJobWebCrypto: 2,
  randomFillSync: fill,
  secureBuffer: (size) => new Uint8Array(size),
  getHashes: () => native.hashes(),
  getCachedAliases: () => ({}),
  getCachedMacAliases: () => ({}),
  getCiphers: () => [],
  getCurves: () => [],
  getMacs: () => [],
  setEngine() {
    throw notHere("setEngine");
  },
  secureHeapUsed: () => undefined,
  getOpenSSLSecLevelCrypto: () => 1,
  getFipsCrypto: () => 0,
  getFipsCryptoGeneration: () => 0,
  isKeyAlgorithmAvailable: () => false,
};

class QuotaExceededError extends DOMException {
  constructor(message, options) {
    super(message, "QuotaExceededError");
    this.quota = options?.quota ?? null;
    this.requested = options?.requested ?? null;
  }
}

const shims = {
  process: internal.process,
  primordials: internal.primordials,
  internalBinding(name) {
    if (name === "crypto") return crypto;
    if (name === "messaging") return { DOMException, QuotaExceededError };
    return internal.internalBinding(name);
  },
  fallback: internal.find,
  stream: () => require("node:stream"),
  buffer: () => require("node:buffer"),
  "internal/buffer": () => ({
    FastBuffer: function FastBuffer(buffer, byteOffset, length) {
      if (buffer === undefined) return Buffer.alloc(0);
      if (typeof buffer === "number") return Buffer.alloc(buffer);
      return Buffer.from(buffer, byteOffset, length);
    },
  }),
  // a key is text or bytes here; node's key objects are not
  "internal/crypto/keys": () => ({
    prepareSecretKey(key, encoding) {
      const { codes } = internal.load("internal/errors");
      if (typeof key === "string") return Buffer.from(key, encoding);
      if (ArrayBuffer.isView(key)) return key;
      if (key instanceof ArrayBuffer) return new Uint8Array(key);
      throw new codes.ERR_INVALID_ARG_TYPE(
        "key",
        ["ArrayBuffer", "Buffer", "TypedArray", "DataView", "string", "KeyObject", "CryptoKey"],
        key,
      );
    },
  }),
};

const files = {
  "internal/crypto/hash": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      FunctionPrototypeCall,
      ObjectSetPrototypeOf,
      StringPrototypeReplace,
      StringPrototypeToLowerCase,
      Symbol,
      TypedArrayPrototypeIncludes,
    } = primordials;

    const {
      Hash: _Hash,
      HashJob,
      Hmac: _Hmac,
      kCryptoJobWebCrypto,
      oneShotDigest,
      TurboShakeJob,
      KangarooTwelveJob,
    } = internalBinding('crypto');

    const {
      getStringOption,
      jobPromise,
      normalizeHashName,
      validateAlgorithm,
      validateMaxBufferLength,
      kHandle,
      getCachedHashId,
      getHashCache,
      getArrayBufferOrView,
      getBufferSourceBytes,
      getOptionalByteLength,
    } = require('internal/crypto/util');

    const {
      prepareSecretKey,
    } = require('internal/crypto/keys');

    const {
      normalizeEncoding,
      encodingsMap,
      getDeprecationWarningEmitter,
    } = require('internal/util');

    const {
      Buffer,
    } = require('buffer');

    const {
      codes: {
        ERR_CRYPTO_HASH_FINALIZED,
        ERR_CRYPTO_HASH_UPDATE_FAILED,
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_ARG_VALUE,
      },
    } = require('internal/errors');

    const {
      validateEncoding,
      validateString,
      validateObject,
      validateUint32,
    } = require('internal/validators');

    const {
      isArrayBufferView,
    } = require('internal/util/types');

    const LazyTransform = require('internal/streams/lazy_transform');

    const kState = Symbol('kState');
    const kFinalized = Symbol('kFinalized');

    function normalizeAlgorithm(name) {
      return name.toLowerCase().replace('-', '');
    }

    const maybeEmitDeprecationWarning = getDeprecationWarningEmitter(
      'DEP0198',
      'Creating SHAKE128/256 digests without an explicit options.outputLength is deprecated.',
      undefined,
      false,
      (algorithm) => {
        const normalized = normalizeAlgorithm(algorithm);
        return normalized === 'shake128' || normalized === 'shake256';
      },
    );

    function normalizeCShakeParameter(value, name) {
      if (value === undefined) return undefined;

      value = getArrayBufferOrView(value, name);
      if (getBufferSourceBytes(value).includes(0)) {
        throw new ERR_INVALID_ARG_VALUE(
          name, value, 'must not contain NUL bytes');
      }
      return value;
    }

    function Hash(algorithm, options) {
      if (!new.target)
        return new Hash(algorithm, options);
      const isCopy = algorithm instanceof _Hash;
      if (!isCopy)
        validateString(algorithm, 'algorithm');
      let xofLen = typeof options === 'object' && options !== null ?
        options.outputLength : undefined;
      if (xofLen !== undefined) {
        validateUint32(xofLen, 'options.outputLength');
        xofLen += 0;
      }
      let functionName;
      let customization;
      if (!isCopy && options !== undefined && options !== null) {
        const functionNameOption = options.functionName;
        if (functionNameOption !== undefined) {
          functionName = normalizeCShakeParameter(
            functionNameOption, 'options.functionName');
        }
        const customizationOption = options.customization;
        if (customizationOption !== undefined) {
          customization = normalizeCShakeParameter(
            customizationOption, 'options.customization');
        }
      }
      const algorithmId = isCopy ? -1 : getCachedHashId(algorithm);
      if (functionName === undefined && customization === undefined) {
        this[kHandle] = new _Hash(
          algorithm, xofLen, algorithmId, getHashCache());
      } else {
        this[kHandle] = new _Hash(
          algorithm,
          xofLen,
          algorithmId,
          getHashCache(),
          functionName,
          customization,
        );
      }
      this[kState] = {
        [kFinalized]: false,
      };
      if (!isCopy && xofLen === undefined) {
        maybeEmitDeprecationWarning(algorithm);
      }
      FunctionPrototypeCall(LazyTransform, this, options);
    }

    Object.setPrototypeOf(Hash.prototype, LazyTransform.prototype);
    Object.setPrototypeOf(Hash, LazyTransform);

    Hash.prototype.copy = function copy(options) {
      const state = this[kState];
      if (state[kFinalized])
        throw new ERR_CRYPTO_HASH_FINALIZED();

      return new Hash(this[kHandle], options);
    };

    Hash.prototype._transform = function _transform(chunk, encoding, callback) {
      if (!this[kHandle].update(chunk, encoding))
        return callback(new ERR_CRYPTO_HASH_UPDATE_FAILED());
      callback();
    };

    Hash.prototype._flush = function _flush(callback) {
      this.push(this[kHandle].digest());
      callback();
    };

    Hash.prototype.update = function update(data, encoding) {
      const state = this[kState];
      if (state[kFinalized])
        throw new ERR_CRYPTO_HASH_FINALIZED();

      if (typeof data === 'string') {
        validateEncoding(data, encoding);
      } else if (!isArrayBufferView(data)) {
        throw new ERR_INVALID_ARG_TYPE(
          'data', ['string', 'Buffer', 'TypedArray', 'DataView'], data);
      }

      if (!this[kHandle].update(data, encoding))
        throw new ERR_CRYPTO_HASH_UPDATE_FAILED();
      return this;
    };

    Hash.prototype.digest = function digest(outputEncoding) {
      const state = this[kState];
      if (state[kFinalized])
        throw new ERR_CRYPTO_HASH_FINALIZED();

      const ret = this[kHandle].digest(outputEncoding && `${outputEncoding}`);
      state[kFinalized] = true;
      return ret;
    };

    function Hmac(hmac, key, options) {
      if (!(this instanceof Hmac))
        return new Hmac(hmac, key, options);
      validateString(hmac, 'hmac');
      const encoding = getStringOption(options, 'encoding');
      key = prepareSecretKey(key, encoding);
      this[kHandle] = new _Hmac();
      this[kHandle].init(hmac, key);
      this[kState] = {
        [kFinalized]: false,
      };
      FunctionPrototypeCall(LazyTransform, this, options);
    }

    Object.setPrototypeOf(Hmac.prototype, LazyTransform.prototype);
    Object.setPrototypeOf(Hmac, LazyTransform);

    Hmac.prototype.update = Hash.prototype.update;

    Hmac.prototype.digest = function digest(outputEncoding) {
      const state = this[kState];

      if (state[kFinalized]) {
        const buf = Buffer.from('');
        if (outputEncoding && outputEncoding !== 'buffer')
          return buf.toString(outputEncoding);
        return buf;
      }

      const ret = this[kHandle].digest(outputEncoding && `${outputEncoding}`);
      state[kFinalized] = true;
      return ret;
    };

    Hmac.prototype._flush = function _flush(callback) {
      this.push(this[kHandle].digest());
      this[kState][kFinalized] = true; // This diverges from Hash.prototype._flush:
      callback();
    };
    Hmac.prototype._transform = Hash.prototype._transform;

    function asyncDigest(algorithm, data) {
      validateMaxBufferLength(data, 'data');
      validateAlgorithm(algorithm, 'digest');

      switch (algorithm.name) {
        case 'SHA-1':
        case 'SHA-256':
        case 'SHA-384':
        case 'SHA-512':
        case 'SHA3-256':
        case 'SHA3-384':
        case 'SHA3-512':
          return jobPromise(() => new HashJob(
            kCryptoJobWebCrypto,
            normalizeHashName(algorithm.name),
            data));
        case 'cSHAKE128':
        case 'cSHAKE256': {
          const outputLength = algorithm.outputLength;
          if (getOptionalByteLength(algorithm.functionName) ||
              getOptionalByteLength(algorithm.customization)) {
            return jobPromise(() => new HashJob(
              kCryptoJobWebCrypto,
              algorithm.name.toLowerCase(),
              data,
              outputLength,
              algorithm.functionName,
              algorithm.customization));
          }

          return jobPromise(() => new HashJob(
            kCryptoJobWebCrypto,
            normalizeHashName(algorithm.name),
            data,
            outputLength));
        }
        case 'TurboSHAKE128':
        case 'TurboSHAKE256':
          return jobPromise(() => new TurboShakeJob(
            kCryptoJobWebCrypto,
            algorithm.name,
            algorithm.domainSeparation ?? 0x1f,
            algorithm.outputLength / 8,
            data));
        case 'KT128':
        case 'KT256':
          return jobPromise(() => new KangarooTwelveJob(
            kCryptoJobWebCrypto,
            algorithm.name,
            algorithm.customization,
            algorithm.outputLength / 8,
            data));
        /* c8 ignore start */
        default: {
          const assert = require('internal/assert');
          assert.fail('Unreachable code');
        }
        /* c8 ignore stop */
      }
    }

    function hash(algorithm, input, options) {
      validateString(algorithm, 'algorithm');
      if (typeof input !== 'string' && !isArrayBufferView(input)) {
        throw new ERR_INVALID_ARG_TYPE('input', ['Buffer', 'TypedArray', 'DataView', 'string'], input);
      }
      if (options === undefined) {
        maybeEmitDeprecationWarning(algorithm);
        return oneShotDigest(algorithm, getCachedHashId(algorithm), getHashCache(),
                             input, 'hex', encodingsMap.hex, undefined);
      }
      let outputEncoding;
      let outputLength;
      let functionName;
      let customization;

      if (typeof options === 'string') {
        outputEncoding = options;
      } else if (options !== undefined) {
        validateObject(options, 'options');
        outputLength = options.outputLength;
        outputEncoding = options.outputEncoding;
        const functionNameOption = options.functionName;
        if (functionNameOption !== undefined) {
          functionName = normalizeCShakeParameter(
            functionNameOption, 'options.functionName');
        }
        const customizationOption = options.customization;
        if (customizationOption !== undefined) {
          customization = normalizeCShakeParameter(
            customizationOption, 'options.customization');
        }
      }

      outputEncoding ??= 'hex';

      let normalized = outputEncoding;
      if (normalized !== 'hex') {
        validateString(outputEncoding, 'outputEncoding');
        normalized = normalizeEncoding(outputEncoding);
        if (normalized === undefined) {
          if (outputEncoding.toLowerCase() === 'buffer') {
            normalized = 'buffer';
          } else {
            throw new ERR_INVALID_ARG_VALUE('outputEncoding', outputEncoding);
          }
        }
      }

      if (outputLength !== undefined) {
        validateUint32(outputLength, 'outputLength');
        outputLength += 0;
      }

      if (outputLength === undefined) {
        maybeEmitDeprecationWarning(algorithm);
      }

      if (functionName === undefined && customization === undefined) {
        return oneShotDigest(algorithm, getCachedHashId(algorithm), getHashCache(),
                             input, normalized, encodingsMap[normalized], outputLength);
      }
      return oneShotDigest(algorithm, getCachedHashId(algorithm), getHashCache(),
                           input, normalized, encodingsMap[normalized], outputLength,
                           functionName, customization);
    }

    module.exports = {
      Hash,
      Hmac,
      asyncDigest,
      hash,
    };
  },
  "internal/crypto/random": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      Array,
      ArrayBufferPrototypeGetByteLength,
      ArrayPrototypeForEach,
      ArrayPrototypePush,
      ArrayPrototypeShift,
      ArrayPrototypeSplice,
      BigInt,
      BigIntPrototypeToString,
      DataView,
      DataViewPrototypeGetByteLength,
      DataViewPrototypeGetUint8,
      DateNow,
      FunctionPrototypeBind,
      FunctionPrototypeCall,
      MathMin,
      NumberIsNaN,
      NumberIsSafeInteger,
      NumberPrototypeToString,
      StringFromCharCodeApply,
      StringPrototypePadStart,
      TypedArrayPrototypeGetBuffer,
      TypedArrayPrototypeGetByteLength,
      TypedArrayPrototypeGetLength,
      TypedArrayPrototypeGetSymbolToStringTag,
      Uint8Array,
    } = primordials;

    const {
      RandomBytesJob,
      RandomPrimeJob,
      CheckPrimeJob,
      kCryptoJobAsync,
      kCryptoJobSync,
      randomFillSync: randomFillSyncImpl,
      secureBuffer,
    } = internalBinding('crypto');

    const {
      kEmptyObject,
      lazyDOMException,
    } = require('internal/util');

    const { Buffer, kMaxLength } = require('buffer');

    const {
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_OPERATION_FAILED,
        ERR_OUT_OF_RANGE,
      },
    } = require('internal/errors');

    const {
      validateNumber,
      validateBoolean,
      validateFunction,
      validateInt32,
      validateObject,
    } = require('internal/validators');

    const {
      isArrayBufferView,
      isAnyArrayBuffer,
      isDataView,
      isSharedArrayBuffer,
      isTypedArray,
      isFloat16Array,
      isFloat32Array,
      isFloat64Array,
    } = require('internal/util/types');

    const { FastBuffer } = require('internal/buffer');

    const kMaxInt32 = 2 ** 31 - 1;
    const kMaxPossibleLength = Math.min(kMaxLength, kMaxInt32);

    function getByteLength(buf) {
      if (isArrayBufferView(buf)) {
        return isDataView(buf) ?
          DataViewPrototypeGetByteLength(buf) : buf.byteLength;
      }
      if (isSharedArrayBuffer(buf))
        return (new Uint8Array(buf)).byteLength;
      return ArrayBufferPrototypeGetByteLength(buf);
    }

    function getElementSize(buf) {
      switch (TypedArrayPrototypeGetSymbolToStringTag(buf)) {
        case 'Int16Array':
        case 'Uint16Array':
        case 'Float16Array':
          return 2;
        case 'Int32Array':
        case 'Uint32Array':
        case 'Float32Array':
          return 4;
        case 'Float64Array':
        case 'BigInt64Array':
        case 'BigUint64Array':
          return 8;
        default:
          return 1;
      }
    }

    function assertOffset(offset, elementSize, length) {
      validateNumber(offset, 'offset');
      offset *= elementSize;

      const maxLength = Math.min(length, kMaxPossibleLength);
      if (Number.isNaN(offset) || offset > maxLength || offset < 0) {
        throw new ERR_OUT_OF_RANGE('offset', `>= 0 && <= ${maxLength}`, offset);
      }

      return offset >>> 0;  // Convert to uint32.
    }

    function assertSize(size, elementSize, offset, length) {
      validateNumber(size, 'size');
      size *= elementSize;

      if (Number.isNaN(size) || size > kMaxPossibleLength || size < 0) {
        throw new ERR_OUT_OF_RANGE('size',
                                   `>= 0 && <= ${kMaxPossibleLength}`, size);
      }

      if (size + offset > length) {
        throw new ERR_OUT_OF_RANGE('size + offset', `<= ${length}`, size + offset);
      }

      return size >>> 0;  // Convert to uint32.
    }

    function randomBytes(size, callback) {
      size = assertSize(size, 1, 0, Infinity);
      if (callback !== undefined) {
        validateFunction(callback, 'callback');
      }

      const buf = new FastBuffer(size);

      if (callback === undefined) {
        randomFillSync(buf.buffer, 0, size);
        return buf;
      }

      randomFill(buf.buffer, 0, size, function(error) {
        if (error) return FunctionPrototypeCall(callback, this, error);
        FunctionPrototypeCall(callback, this, null, buf);
      });
    }

    function randomFillSync(buf, offset = 0, size) {
      if (!isAnyArrayBuffer(buf) && !isArrayBufferView(buf)) {
        throw new ERR_INVALID_ARG_TYPE(
          'buf',
          ['ArrayBuffer', 'ArrayBufferView'],
          buf);
      }

      const elementSize = getElementSize(buf);
      const byteLength = getByteLength(buf);

      offset = assertOffset(offset, elementSize, byteLength);

      if (size === undefined) {
        size = byteLength - offset;
      } else {
        size = assertSize(size, elementSize, offset, byteLength);
      }

      if (size === 0)
        return buf;

      randomFillSyncImpl(buf, offset, size);
      return buf;
    }

    function randomFill(buf, offset, size, callback) {
      if (!isAnyArrayBuffer(buf) && !isArrayBufferView(buf)) {
        throw new ERR_INVALID_ARG_TYPE(
          'buf',
          ['ArrayBuffer', 'ArrayBufferView'],
          buf);
      }

      const elementSize = getElementSize(buf);

      if (typeof offset === 'function') {
        callback = offset;
        offset = 0;
        size = isTypedArray(buf) ? buf.length : undefined;
      } else if (typeof size === 'function') {
        callback = size;
        size = (isTypedArray(buf) ? buf.length : getByteLength(buf)) - offset;
      } else {
        validateFunction(callback, 'callback');
      }

      const byteLength = getByteLength(buf);
      offset = assertOffset(offset, elementSize, byteLength);

      if (size === undefined) {
        size = byteLength - offset;
      } else {
        size = assertSize(size, elementSize, offset, byteLength);
      }

      if (size === 0) {
        callback(null, buf);
        return;
      }

      const job = new RandomBytesJob(
        kCryptoJobAsync,
        buf,
        offset,
        size);
      job.ondone = FunctionPrototypeBind(onJobDone, job, buf, callback);
      job.run();
    }

    const RAND_MAX = 0xFFFF_FFFF_FFFF;

    const randomCache = new FastBuffer(6 * 1024);
    let randomCacheOffset = randomCache.length;
    let asyncCacheFillInProgress = false;
    const asyncCachePendingTasks = [];

    function randomInt(min, max, callback) {
      const minNotSpecified = typeof max === 'undefined' ||
        typeof max === 'function';

      if (minNotSpecified) {
        callback = max;
        max = min;
        min = 0;
      }

      const isSync = typeof callback === 'undefined';
      if (!isSync) {
        validateFunction(callback, 'callback');
      }
      if (!Number.isSafeInteger(min)) {
        throw new ERR_INVALID_ARG_TYPE('min', 'a safe integer', min);
      }
      if (!Number.isSafeInteger(max)) {
        throw new ERR_INVALID_ARG_TYPE('max', 'a safe integer', max);
      }
      if (max <= min) {
        throw new ERR_OUT_OF_RANGE(
          'max', `greater than the value of "min" (${min})`, max,
        );
      }

      const range = max - min;

      if (!(range <= RAND_MAX)) {
        throw new ERR_OUT_OF_RANGE(`max${minNotSpecified ? '' : ' - min'}`,
                                   `<= ${RAND_MAX}`, range);
      }

      const randLimit = RAND_MAX - (RAND_MAX % range);

      while (isSync || (randomCacheOffset < randomCache.length)) {
        if (randomCacheOffset === randomCache.length) {
          randomFillSync(randomCache);
          randomCacheOffset = 0;
        }

        const x = randomCache.readUIntBE(randomCacheOffset, 6);
        randomCacheOffset += 6;

        if (x < randLimit) {
          const n = (x % range) + min;
          if (isSync) return n;
          process.nextTick(callback, undefined, n);
          return;
        }
      }

      asyncCachePendingTasks.push({ min, max, callback });
      asyncRefillRandomIntCache();
    }

    function asyncRefillRandomIntCache() {
      if (asyncCacheFillInProgress)
        return;

      asyncCacheFillInProgress = true;
      randomFill(randomCache, (err) => {
        asyncCacheFillInProgress = false;

        const tasks = asyncCachePendingTasks;
        const errorReceiver = err && tasks.shift();
        if (!err)
          randomCacheOffset = 0;

        tasks.splice(0).forEach((task) => {
          randomInt(task.min, task.max, task.callback);
        });

        if (errorReceiver)
          errorReceiver.callback(err);
      });
    }

    function onJobDone(buf, callback, error) {
      if (error) return FunctionPrototypeCall(callback, this, error);
      FunctionPrototypeCall(callback, this, null, buf);
    }

    function getRandomValues(data) {
      if (!isTypedArray(data) ||
          isFloat16Array(data) ||
          isFloat32Array(data) ||
          isFloat64Array(data)) {
        throw lazyDOMException(
          'The data argument must be an integer-type TypedArray',
          'TypeMismatchError');
      }
      const byteLength = data.byteLength;
      if (byteLength > 65536) {
        const { QuotaExceededError } = internalBinding('messaging');
        throw new QuotaExceededError(
          'The requested length exceeds 65,536 bytes');
      }
      randomFillSync(data, 0);
      return data;
    }

    const kBatchSize = 128;
    let uuidData;
    let uuidNotBuffered;
    let uuidBatch = 0;

    let hexBytesCache;
    function getHexBytes() {
      if (hexBytesCache === undefined) {
        hexBytesCache = new Array(256);
        for (let i = 0; i < hexBytesCache.length; i++) {
          const hex = i.toString(16);
          hexBytesCache[i] = hex.padStart(2, '0');
        }
      }
      return hexBytesCache;
    }

    function serializeUUID(buf, version, variant, offset = 0) {
      const kHexBytes = getHexBytes();
      return kHexBytes[buf[offset]] +
        kHexBytes[buf[offset + 1]] +
        kHexBytes[buf[offset + 2]] +
        kHexBytes[buf[offset + 3]] +
        '-' +
        kHexBytes[buf[offset + 4]] +
        kHexBytes[buf[offset + 5]] +
        '-' +
        kHexBytes[(buf[offset + 6] & 0x0f) | version] +
        kHexBytes[buf[offset + 7]] +
        '-' +
        kHexBytes[(buf[offset + 8] & 0x3f) | variant] +
        kHexBytes[buf[offset + 9]] +
        '-' +
        kHexBytes[buf[offset + 10]] +
        kHexBytes[buf[offset + 11]] +
        kHexBytes[buf[offset + 12]] +
        kHexBytes[buf[offset + 13]] +
        kHexBytes[buf[offset + 14]] +
        kHexBytes[buf[offset + 15]];
    }

    function getBufferedUUID() {
      uuidData ??= secureBuffer(16 * kBatchSize);
      if (uuidData === undefined)
        throw new ERR_OPERATION_FAILED('Out of memory');

      if (uuidBatch === 0) randomFillSync(uuidData);
      uuidBatch = (uuidBatch + 1) % kBatchSize;
      return serializeUUID(uuidData, 0x40, 0x80, uuidBatch * 16);
    }

    function getUnbufferedUUID() {
      uuidNotBuffered ??= secureBuffer(16);
      if (uuidNotBuffered === undefined)
        throw new ERR_OPERATION_FAILED('Out of memory');
      randomFillSync(uuidNotBuffered);
      return serializeUUID(uuidNotBuffered, 0x40, 0x80);
    }

    function randomUUID(options) {
      if (options !== undefined)
        validateObject(options, 'options');
      const {
        disableEntropyCache = false,
      } = options || kEmptyObject;

      validateBoolean(disableEntropyCache, 'options.disableEntropyCache');

      return disableEntropyCache ? getUnbufferedUUID() : getBufferedUUID();
    }

    function writeTimestamp(buf, offset) {
      const now = Date.now();
      const msb = now / (2 ** 32);
      buf[offset] = msb >>> 8;
      buf[offset + 1] = msb;
      buf[offset + 2] = now >>> 24;
      buf[offset + 3] = now >>> 16;
      buf[offset + 4] = now >>> 8;
      buf[offset + 5] = now;
    }

    function getBufferedUUIDv7() {
      uuidData ??= secureBuffer(16 * kBatchSize);
      if (uuidData === undefined)
        throw new ERR_OPERATION_FAILED('Out of memory');

      if (uuidBatch === 0) randomFillSync(uuidData);
      uuidBatch = (uuidBatch + 1) % kBatchSize;
      const offset = uuidBatch * 16;
      writeTimestamp(uuidData, offset);
      return serializeUUID(uuidData, 0x70, 0x80, offset);
    }

    function getUnbufferedUUIDv7() {
      uuidNotBuffered ??= secureBuffer(16);
      if (uuidNotBuffered === undefined)
        throw new ERR_OPERATION_FAILED('Out of memory');
      randomFillSync(uuidNotBuffered, 6);
      writeTimestamp(uuidNotBuffered, 0);
      return serializeUUID(uuidNotBuffered, 0x70, 0x80);
    }

    function randomUUIDv7(options) {
      if (options !== undefined)
        validateObject(options, 'options');
      const {
        disableEntropyCache = false,
      } = options || kEmptyObject;

      validateBoolean(disableEntropyCache, 'options.disableEntropyCache');

      return disableEntropyCache ? getUnbufferedUUIDv7() : getBufferedUUIDv7();
    }

    function createRandomPrimeJob(type, size, options) {
      validateObject(options, 'options');

      const {
        safe = false,
        bigint = false,
      } = options;
      let {
        add,
        rem,
      } = options;

      validateBoolean(safe, 'options.safe');
      validateBoolean(bigint, 'options.bigint');

      if (add !== undefined) {
        if (typeof add === 'bigint') {
          add = unsignedBigIntToBuffer(add, 'options.add');
        } else if (!isAnyArrayBuffer(add) && !isArrayBufferView(add)) {
          throw new ERR_INVALID_ARG_TYPE(
            'options.add',
            [
              'ArrayBuffer',
              'TypedArray',
              'Buffer',
              'DataView',
              'bigint',
            ],
            add);
        }
      }

      if (rem !== undefined) {
        if (typeof rem === 'bigint') {
          rem = unsignedBigIntToBuffer(rem, 'options.rem');
        } else if (!isAnyArrayBuffer(rem) && !isArrayBufferView(rem)) {
          throw new ERR_INVALID_ARG_TYPE(
            'options.rem',
            [
              'ArrayBuffer',
              'TypedArray',
              'Buffer',
              'DataView',
              'bigint',
            ],
            rem);
        }
      }

      const job = new RandomPrimeJob(type, size, safe, add, rem);
      job.result = bigint ? arrayBufferToUnsignedBigInt : (p) => p;
      return job;
    }

    function generatePrime(size, options, callback) {
      validateInt32(size, 'size', 1);
      if (typeof options === 'function') {
        callback = options;
        options = kEmptyObject;
      }
      validateFunction(callback, 'callback');

      const job = createRandomPrimeJob(kCryptoJobAsync, size, options);
      job.ondone = (err, prime) => {
        if (err) {
          callback(err);
          return;
        }

        callback(
          undefined,
          job.result(prime));
      };
      job.run();
    }

    function generatePrimeSync(size, options = kEmptyObject) {
      validateInt32(size, 'size', 1);

      const job = createRandomPrimeJob(kCryptoJobSync, size, options);
      const { 0: err, 1: prime } = job.run();
      if (err)
        throw err;
      return job.result(prime);
    }

    const numberToHexCharCode = (number) => (number < 10 ? 48 : 87) + number;

    function arrayBufferToUnsignedBigInt(buf) {
      const length = ArrayBufferPrototypeGetByteLength(buf);
      const chars = Array(length * 2);
      const view = new DataView(buf);

      for (let i = 0; i < length; i++) {
        const val = DataViewPrototypeGetUint8(view, i);
        chars[2 * i] = numberToHexCharCode(val >> 4);
        chars[2 * i + 1] = numberToHexCharCode(val & 0xf);
      }

      return BigInt(`0x${StringFromCharCodeApply(chars)}`);
    }

    function unsignedBigIntToBuffer(bigint, name) {
      if (bigint < 0) {
        throw new ERR_OUT_OF_RANGE(name, '>= 0', bigint);
      }

      const hex = BigIntPrototypeToString(bigint, 16);
      const padded = hex.padStart(hex.length + (hex.length % 2), 0);
      return Buffer.from(padded, 'hex');
    }

    function checkPrime(candidate, options = kEmptyObject, callback) {
      if (typeof candidate === 'bigint')
        candidate = unsignedBigIntToBuffer(candidate, 'candidate');
      if (!isAnyArrayBuffer(candidate) && !isArrayBufferView(candidate)) {
        throw new ERR_INVALID_ARG_TYPE(
          'candidate',
          [
            'ArrayBuffer',
            'TypedArray',
            'Buffer',
            'DataView',
            'bigint',
          ],
          candidate,
        );
      }
      if (typeof options === 'function') {
        callback = options;
        options = kEmptyObject;
      }
      validateFunction(callback, 'callback');
      validateObject(options, 'options');
      let {
        checks = 0,
      } = options;

      validateInt32(checks, 'options.checks', 0);
      checks += 0;

      const job = new CheckPrimeJob(kCryptoJobAsync, candidate, checks);
      job.ondone = callback;
      job.run();
    }

    function checkPrimeSync(candidate, options = kEmptyObject) {
      if (typeof candidate === 'bigint')
        candidate = unsignedBigIntToBuffer(candidate, 'candidate');
      if (!isAnyArrayBuffer(candidate) && !isArrayBufferView(candidate)) {
        throw new ERR_INVALID_ARG_TYPE(
          'candidate',
          [
            'ArrayBuffer',
            'TypedArray',
            'Buffer',
            'DataView',
            'bigint',
          ],
          candidate,
        );
      }
      validateObject(options, 'options');
      let {
        checks = 0,
      } = options;

      validateInt32(checks, 'options.checks', 0);
      checks += 0;

      const job = new CheckPrimeJob(kCryptoJobSync, candidate, checks);
      const { 0: err, 1: result } = job.run();
      if (err)
        throw err;

      return result;
    }

    module.exports = {
      checkPrime,
      checkPrimeSync,
      randomBytes,
      randomFill,
      randomFillSync,
      randomInt,
      getRandomValues,
      randomUUID,
      randomUUIDv7,
      generatePrime,
      generatePrimeSync,
    };
  },
  "internal/crypto/util": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayBufferIsView,
      ArrayBufferPrototypeGetByteLength,
      ArrayBufferPrototypeGetDetached,
      ArrayPrototypeIncludes,
      ArrayPrototypePush,
      ArrayPrototypeSlice,
      BigInt,
      DataViewPrototypeGetBuffer,
      DataViewPrototypeGetByteLength,
      DataViewPrototypeGetByteOffset,
      MathFloor,
      MathMin,
      Number,
      ObjectDefineProperty,
      ObjectEntries,
      ObjectKeys,
      ObjectPrototypeHasOwnProperty,
      ObjectSetPrototypeOf,
      PromisePrototypeThen,
      PromiseReject,
      PromiseWithResolvers,
      SafeMap,
      SafeSet,
      StringPrototypeCharCodeAt,
      StringPrototypeStartsWith,
      StringPrototypeToUpperCase,
      Symbol,
      TypedArrayPrototypeGetBuffer,
      TypedArrayPrototypeGetByteLength,
      TypedArrayPrototypeGetByteOffset,
      TypedArrayPrototypeGetLength,
      Uint8Array,
    } = primordials;

    const {
      getCiphers: _getCiphers,
      getCurves: _getCurves,
      getHashes: _getHashes,
      getMacs: _getMacs,
      setEngine: _setEngine,
      secureHeapUsed: _secureHeapUsed,
      getCachedAliases,
      getCachedMacAliases,
      getOpenSSLSecLevelCrypto: getOpenSSLSecLevel,
      kKeyVariantAES_OCB_128: hasAesOcbMode,
      Argon2Job,
      getFipsCrypto,
      getFipsCryptoGeneration,
      KmacJob,
      isKeyAlgorithmAvailable,
    } = internalBinding('crypto');

    let fips;
    let fipsGeneration;
    function isFips() {
      const generation = getFipsCryptoGeneration();
      if (fipsGeneration !== generation) {
        fips = getFipsCrypto() === 1;
        fipsGeneration = generation;
      }
      return fips;
    }

    const { getOptionValue } = require('internal/options');

    const {
      crypto: {
        ENGINE_METHOD_ALL,
      },
    } = internalBinding('constants');

    const normalizeHashName = require('internal/crypto/hashnames');

    const {
      codes: {
        ERR_CRYPTO_CUSTOM_ENGINE_NOT_SUPPORTED,
        ERR_CRYPTO_ENGINE_UNKNOWN,
        ERR_INVALID_ARG_TYPE,
      },
      hideStackFrames,
    } = require('internal/errors');

    const {
      validateArray,
      validateNumber,
      validateString,
    } = require('internal/validators');

    const { Buffer } = require('buffer');

    const {
      emitExperimentalWarning,
      filterDuplicateStrings,
      lazyDOMException,
      setOwnProperty,
    } = require('internal/util');

    const {
      namespace: {
        isBuildingSnapshot,
        addSerializeCallback,
      },
    } = require('internal/v8/startup_snapshot');

    const {
      isDataView,
      isArrayBuffer,
      isArrayBufferView,
      isAnyArrayBuffer,
      isPromise,
    } = require('internal/util/types');

    const kHandle = Symbol('kHandle');

    function toBuf(val, encoding) {
      if (typeof val === 'string') {
        if (encoding === 'buffer')
          encoding = 'utf8';
        return Buffer.from(val, encoding);
      }
      return val;
    }

    let _hashCache;
    let _macCache;
    if (isBuildingSnapshot()) {
      addSerializeCallback(() => {
        fips = undefined;
        fipsGeneration = undefined;
        supportedAlgorithmsGeneration = undefined;
        _hashCache = undefined;
        _macCache = undefined;
      });
    }

    function getHashCache() {
      while (_hashCache === undefined) {
        const generation = getFipsCryptoGeneration();
        const cache = getCachedAliases();
        if (generation !== getFipsCryptoGeneration()) continue;
        _hashCache = cache;
      }
      return _hashCache;
    }

    function getMacCache() {
      while (_macCache === undefined) {
        const generation = getFipsCryptoGeneration();
        const cache = getCachedMacAliases();
        if (generation !== getFipsCryptoGeneration()) continue;
        _macCache = cache;
      }
      return _macCache;
    }

    function cachedArrayByFipsGeneration(fn, onRefresh) {
      let result;
      let generation;
      if (isBuildingSnapshot()) {
        addSerializeCallback(() => {
          result = undefined;
          generation = undefined;
        });
      }

      return () => {
        while (true) {
          const current = getFipsCryptoGeneration();
          if (result === undefined || generation !== current) {
            const next = fn();
            if (current !== getFipsCryptoGeneration()) continue;
            result = next;
            generation = current;
            if (onRefresh !== undefined) onRefresh();
          }
          return result.slice();
        }
      };
    }

    function getCachedHashId(algorithm) {
      const result = getHashCache()[algorithm];
      return result === undefined ? -1 : result;
    }

    function getCachedMacId(algorithm) {
      const result = getMacCache()[algorithm];
      return result === undefined ? -1 : result;
    }

    const getCiphers = cachedArrayByFipsGeneration(
      () => filterDuplicateStrings(_getCiphers()));
    const getHashes = cachedArrayByFipsGeneration(
      () => filterDuplicateStrings(_getHashes()),
      () => {
        _hashCache = undefined;
      });
    const getMacs = cachedArrayByFipsGeneration(
      () => filterDuplicateStrings(_getMacs(), true),
      () => {
        _macCache = undefined;
      });

    const getCurves = cachedArrayByFipsGeneration(
      () => filterDuplicateStrings(_getCurves()));

    function setEngine(id, flags) {
      validateString(id, 'id');
      if (flags)
        validateNumber(flags, 'flags');
      flags = flags >>> 0;

      if (flags === 0)
        flags = ENGINE_METHOD_ALL;

      if (typeof _setEngine !== 'function')
        throw new ERR_CRYPTO_CUSTOM_ENGINE_NOT_SUPPORTED();
      if (!_setEngine(id, flags))
        throw new ERR_CRYPTO_ENGINE_UNKNOWN(id);
    }

    const getArrayBufferOrView = hideStackFrames((buffer, name, encoding) => {
      if (isAnyArrayBuffer(buffer))
        return buffer;
      if (typeof buffer === 'string') {
        if (encoding === 'buffer')
          encoding = 'utf8';
        return Buffer.from(buffer, encoding);
      }
      if (!isArrayBufferView(buffer)) {
        throw new ERR_INVALID_ARG_TYPE.HideStackFramesError(
          name,
          [
            'string',
            'ArrayBuffer',
            'Buffer',
            'TypedArray',
            'DataView',
          ],
          buffer,
        );
      }
      return buffer;
    });

    const kMaxBufferLength = (2 ** 31) - 1;

    const kNamedCurveAliases = {
      'P-256': 'prime256v1',
      'P-384': 'secp384r1',
      'P-521': 'secp521r1',
    };

    const kAlgorithmDefinitions = {
      'AES-CBC': {
        'generateKey': 'AesKeyGenParams',
        'exportKey': null,
        'importKey': null,
        'encrypt': 'AesCbcParams',
        'decrypt': 'AesCbcParams',
        'get key length': 'AesDerivedKeyParams',
      },
      'AES-CTR': {
        'generateKey': 'AesKeyGenParams',
        'exportKey': null,
        'importKey': null,
        'encrypt': 'AesCtrParams',
        'decrypt': 'AesCtrParams',
        'get key length': 'AesDerivedKeyParams',
      },
      'AES-GCM': {
        'generateKey': 'AesKeyGenParams',
        'exportKey': null,
        'importKey': null,
        'encrypt': 'AeadParams',
        'decrypt': 'AeadParams',
        'get key length': 'AesDerivedKeyParams',
      },
      'AES-KW': {
        'generateKey': 'AesKeyGenParams',
        'exportKey': null,
        'importKey': null,
        'get key length': 'AesDerivedKeyParams',
        'wrapKey': null,
        'unwrapKey': null,
      },
      'AES-OCB': {
        'generateKey': 'AesKeyGenParams',
        'exportKey': null,
        'importKey': null,
        'encrypt': 'AeadParams',
        'decrypt': 'AeadParams',
        'get key length': 'AesDerivedKeyParams',
      },
      'Argon2d': {
        'deriveBits': 'Argon2Params',
        'get key length': null,
        'importKey': null,
      },
      'Argon2i': {
        'deriveBits': 'Argon2Params',
        'get key length': null,
        'importKey': null,
      },
      'Argon2id': {
        'deriveBits': 'Argon2Params',
        'get key length': null,
        'importKey': null,
      },
      'ChaCha20-Poly1305': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'encrypt': 'AeadParams',
        'decrypt': 'AeadParams',
        'get key length': null,
      },
      'cSHAKE128': { 'digest': 'CShakeParams' },
      'cSHAKE256': { 'digest': 'CShakeParams' },
      'KT128': { 'digest': 'KangarooTwelveParams' },
      'KT256': { 'digest': 'KangarooTwelveParams' },
      'TurboSHAKE128': { 'digest': 'TurboShakeParams' },
      'TurboSHAKE256': { 'digest': 'TurboShakeParams' },
      'ECDH': {
        'generateKey': 'EcKeyGenParams',
        'exportKey': null,
        'importKey': 'EcKeyImportParams',
        'deriveBits': 'EcdhKeyDeriveParams',
      },
      'ECDSA': {
        'generateKey': 'EcKeyGenParams',
        'exportKey': null,
        'importKey': 'EcKeyImportParams',
        'sign': 'EcdsaParams',
        'verify': 'EcdsaParams',
      },
      'Ed25519': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'sign': null,
        'verify': null,
      },
      'Ed448': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'sign': 'ContextParams',
        'verify': 'ContextParams',
      },
      'HKDF': {
        'importKey': null,
        'deriveBits': 'HkdfParams',
        'get key length': null,
      },
      'HMAC': {
        'generateKey': 'HmacKeyGenParams',
        'exportKey': null,
        'importKey': 'HmacImportParams',
        'sign': null,
        'verify': null,
        'get key length': 'HmacImportParams',
      },
      'KMAC128': {
        'generateKey': 'KmacKeyGenParams',
        'exportKey': null,
        'importKey': 'KmacImportParams',
        'sign': 'KmacParams',
        'verify': 'KmacParams',
        'get key length': 'KmacImportParams',
      },
      'KMAC256': {
        'generateKey': 'KmacKeyGenParams',
        'exportKey': null,
        'importKey': 'KmacImportParams',
        'sign': 'KmacParams',
        'verify': 'KmacParams',
        'get key length': 'KmacImportParams',
      },
      'ML-DSA-44': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'sign': 'ContextParams',
        'verify': 'ContextParams',
      },
      'ML-DSA-65': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'sign': 'ContextParams',
        'verify': 'ContextParams',
      },
      'ML-DSA-87': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'sign': 'ContextParams',
        'verify': 'ContextParams',
      },
      'ML-KEM-512': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'encapsulate': null,
        'decapsulate': null,
        'get shared key length': null,
      },
      'ML-KEM-768': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'encapsulate': null,
        'decapsulate': null,
        'get shared key length': null,
      },
      'ML-KEM-1024': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'encapsulate': null,
        'decapsulate': null,
        'get shared key length': null,
      },
      'MLKEM768-P256': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'encapsulate': null,
        'decapsulate': null,
        'get shared key length': null,
      },
      'MLKEM768-X25519': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'encapsulate': null,
        'decapsulate': null,
        'get shared key length': null,
      },
      'MLKEM1024-P384': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'encapsulate': null,
        'decapsulate': null,
        'get shared key length': null,
      },
      'PBKDF2': {
        'importKey': null,
        'deriveBits': 'Pbkdf2Params',
        'get key length': null,
      },
      'RSA-OAEP': {
        'generateKey': 'RsaHashedKeyGenParams',
        'exportKey': null,
        'importKey': 'RsaHashedImportParams',
        'encrypt': 'RsaOaepParams',
        'decrypt': 'RsaOaepParams',
      },
      'RSA-PSS': {
        'generateKey': 'RsaHashedKeyGenParams',
        'exportKey': null,
        'importKey': 'RsaHashedImportParams',
        'sign': 'RsaPssParams',
        'verify': 'RsaPssParams',
      },
      'RSASSA-PKCS1-v1_5': {
        'generateKey': 'RsaHashedKeyGenParams',
        'exportKey': null,
        'importKey': 'RsaHashedImportParams',
        'sign': null,
        'verify': null,
      },
      'SHA-1': { 'digest': null },
      'SHA-256': { 'digest': null },
      'SHA-384': { 'digest': null },
      'SHA-512': { 'digest': null },
      'SHA3-256': { 'digest': null },
      'SHA3-384': { 'digest': null },
      'SHA3-512': { 'digest': null },
      'X25519': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'deriveBits': 'EcdhKeyDeriveParams',
      },
      'X448': {
        'generateKey': null,
        'exportKey': null,
        'importKey': null,
        'deriveBits': 'EcdhKeyDeriveParams',
      },
    };

    function has(algorithms, name) {
      return algorithms.includes(name);
    }

    function getConditionalAlgorithms() {
      const mlKem768 = isKeyAlgorithmAvailable('ML-KEM-768');
      const mlKem1024 = isKeyAlgorithmAvailable('ML-KEM-1024');
      const ciphers = getCiphers();
      const hashes = getHashes();
      const macs = getMacs();

      const fips = isFips();

      return {
        'AES-OCB': !!hasAesOcbMode &&
          (has(ciphers, 'aes-128-ocb') ||
           has(ciphers, 'aes-192-ocb') ||
           has(ciphers, 'aes-256-ocb')),
        'Argon2d': !!Argon2Job && !fips,
        'Argon2i': !!Argon2Job && !fips,
        'Argon2id': !!Argon2Job && !fips,
        'ChaCha20-Poly1305': process.features.openssl_is_boringssl ||
          has(ciphers, 'chacha20-poly1305'),
        'cSHAKE128': has(hashes, 'shake128'),
        'cSHAKE256': has(hashes, 'shake256'),
        'Ed25519': isKeyAlgorithmAvailable('Ed25519'),
        'Ed448': isKeyAlgorithmAvailable('Ed448'),
        'KMAC128': !!KmacJob && has(macs, 'kmac128'),
        'KMAC256': !!KmacJob && has(macs, 'kmac256'),
        'KT128': !fips,
        'KT256': !fips,
        'ML-DSA-44': isKeyAlgorithmAvailable('ML-DSA-44'),
        'ML-DSA-65': isKeyAlgorithmAvailable('ML-DSA-65'),
        'ML-DSA-87': isKeyAlgorithmAvailable('ML-DSA-87'),
        'ML-KEM-512': isKeyAlgorithmAvailable('ML-KEM-512'),
        'ML-KEM-768': mlKem768,
        'ML-KEM-1024': mlKem1024,
        'MLKEM768-P256': !fips && mlKem768 &&
          (!process.features.openssl_is_boringssl ||
            (has(hashes, 'sha3-256') &&
             has(hashes, 'shake256'))),
        'MLKEM768-X25519': !fips && mlKem768 &&
          (!process.features.openssl_is_boringssl ||
            (has(hashes, 'sha3-256') &&
             has(hashes, 'shake256'))),
        'MLKEM1024-P384': !fips && mlKem1024 &&
          (!process.features.openssl_is_boringssl ||
            (has(hashes, 'sha3-256') &&
             has(hashes, 'shake256'))),
        'SHA-1': has(hashes, 'sha1'),
        'SHA-256': has(hashes, 'sha256'),
        'SHA-384': has(hashes, 'sha384'),
        'SHA-512': has(hashes, 'sha512'),
        'SHA3-256': has(hashes, 'sha3-256'),
        'SHA3-384': has(hashes, 'sha3-384'),
        'SHA3-512': has(hashes, 'sha3-512'),
        'TurboSHAKE128': !fips,
        'TurboSHAKE256': !fips,
        'X25519': isKeyAlgorithmAvailable('X25519'),
        'X448': isKeyAlgorithmAvailable('X448'),
      };
    }

    const experimentalAlgorithms = [
      'AES-OCB',
      'Argon2d',
      'Argon2i',
      'Argon2id',
      'ChaCha20-Poly1305',
      'cSHAKE128',
      'cSHAKE256',
      'Ed448',
      'KMAC128',
      'KMAC256',
      'ML-DSA-44',
      'ML-DSA-65',
      'ML-DSA-87',
      'ML-KEM-512',
      'ML-KEM-768',
      'ML-KEM-1024',
      'MLKEM768-P256',
      'MLKEM768-X25519',
      'MLKEM1024-P384',
      'SHA3-256',
      'SHA3-384',
      'SHA3-512',
      'TurboSHAKE128',
      'TurboSHAKE256',
      'KT128',
      'KT256',
      'X448',
    ];

    function createSupportedAlgorithms(algorithmDefs) {
      const conditionalAlgorithms = getConditionalAlgorithms();
      const result = {};
      const nameMap = {};

      const algorithmEntries = Object.entries(algorithmDefs);
      for (let i = 0; i < algorithmEntries.length; i++) {
        const { 0: algorithmName, 1: operations } = algorithmEntries[i];
        if (ObjectPrototypeHasOwnProperty(conditionalAlgorithms, algorithmName) &&
            !conditionalAlgorithms[algorithmName]) {
          continue;
        }

        const operationEntries = Object.entries(operations);
        for (let j = 0; j < operationEntries.length; j++) {
          const { 0: operation, 1: dict } = operationEntries[j];
          result[operation] ||= {};
          nameMap[operation] ||= new SafeMap();
          nameMap[operation].set(algorithmName.toUpperCase(), algorithmName);

          if (experimentalAlgorithms.includes(algorithmName)) {
            Object.defineProperty(result[operation], algorithmName, {
              get() {
                emitExperimentalWarning(`The ${algorithmName} Web Crypto API algorithm`);
                return dict;
              },
              __proto__: null,
              enumerable: true,
            });
          } else {
            result[operation][algorithmName] = dict;
          }
        }
      }

      const operations = Object.keys(result);
      for (let i = 0; i < operations.length; i++) {
        Object.setPrototypeOf(result[operations[i]], null);
      }
      Object.setPrototypeOf(result, null);
      Object.setPrototypeOf(nameMap, null);

      return { algorithms: result, nameMap };
    }

    let supportedAlgorithms;
    let supportedAlgorithmsGeneration;
    function getSupportedAlgorithms() {
      const generation = getFipsCryptoGeneration();
      if (supportedAlgorithmsGeneration !== generation) {
        supportedAlgorithms = createSupportedAlgorithms(kAlgorithmDefinitions);
        supportedAlgorithmsGeneration = generation;
      }
      return supportedAlgorithms;
    }

    const simpleAlgorithmDictionaries = {
      AesCbcParams: { iv: 'BufferSource' },
      AesCtrParams: { counter: 'BufferSource' },
      AeadParams: { iv: 'BufferSource', additionalData: 'BufferSource' },
      RsaHashedKeyGenParams: { hash: 'HashAlgorithmIdentifier', publicExponent: 'BufferSource' },
      EcKeyGenParams: {},
      HmacKeyGenParams: { hash: 'HashAlgorithmIdentifier' },
      RsaPssParams: {},
      EcdsaParams: { hash: 'HashAlgorithmIdentifier' },
      HmacImportParams: { hash: 'HashAlgorithmIdentifier' },
      HkdfParams: {
        hash: 'HashAlgorithmIdentifier',
        salt: 'BufferSource',
        info: 'BufferSource',
      },
      ContextParams: { context: 'BufferSource' },
      Pbkdf2Params: { hash: 'HashAlgorithmIdentifier', salt: 'BufferSource' },
      RsaOaepParams: { label: 'BufferSource' },
      RsaHashedImportParams: { hash: 'HashAlgorithmIdentifier' },
      EcKeyImportParams: {},
      CShakeParams: {
        functionName: 'BufferSource',
        customization: 'BufferSource',
      },
      Argon2Params: {
        associatedData: 'BufferSource',
        nonce: 'BufferSource',
        secretValue: 'BufferSource',
      },
      KmacParams: {
        customization: 'BufferSource',
      },
      KangarooTwelveParams: {
        customization: 'BufferSource',
      },
      TurboShakeParams: {},
    };

    const simpleAlgorithmDictionaryEntries =
      Object.entries(simpleAlgorithmDictionaries);
    for (let i = 0; i < simpleAlgorithmDictionaryEntries.length; i++) {
      const { 0: name, 1: types } = simpleAlgorithmDictionaryEntries[i];
      simpleAlgorithmDictionaries[name] = { keys: Object.keys(types), types };
    }
    Object.setPrototypeOf(simpleAlgorithmDictionaries, null);

    function validateMaxBufferLength(data, name, max = kMaxBufferLength) {
      if (getBufferSourceByteLength(data) > max) {
        throw lazyDOMException(
          `${name} must be at most ${max} bytes`,
          'OperationError');
      }
    }

    function validateKmacKeyLength(length) {
      if (length < 32 || length % 8 !== 0)
        throw lazyDOMException('Invalid key length', 'NotSupportedError');
    }

    function numBitsToBytes(length) {
      return Math.floor(length / 8) + Math.floor((7 + (length % 8)) / 8);
    }

    function truncateToBitLength(length, bytes) {
      const lengthBytes = numBitsToBytes(length);
      const isView = ArrayBuffer.isView(bytes);
      const byteView = isView ?
        new Uint8Array(
          getDataViewOrTypedArrayBuffer(bytes),
          getDataViewOrTypedArrayByteOffset(bytes),
          Math.min(lengthBytes, getDataViewOrTypedArrayByteLength(bytes)),
        ) :
        new Uint8Array(bytes, 0,
                       Math.min(lengthBytes, ArrayBufferPrototypeGetByteLength(bytes)));
      const result = new Uint8Array(byteView);

      const remainder = length % 8;
      if (remainder !== 0)
        result[lengthBytes - 1] &= (0xff << (8 - remainder)) & 0xff;

      return result;
    }

    let webidl;

    const kNormalizeAlgorithmOpts = {
      prefix: 'Failed to normalize algorithm',
      context: 'passed algorithm',
    };

    function normalizeAlgorithm(algorithm, op) {
      if (typeof algorithm === 'string')
        return normalizeAlgorithm({ name: algorithm }, op);

      webidl ??= require('internal/crypto/webidl');

      const initialAlg = webidl.converters.Algorithm(algorithm,
                                                     kNormalizeAlgorithmOpts);
      const { algorithms, nameMap } = getSupportedAlgorithms();
      const registeredAlgorithms = algorithms[op];
      let algName = initialAlg.name;

      const canonicalName = nameMap[op]?.get(
        algName.toUpperCase());
      if (canonicalName === undefined)
        throw lazyDOMException('Unrecognized algorithm name', 'NotSupportedError');

      if (algName !== canonicalName) {
        for (let i = 0; i < algName.length; i++) {
          if (algName.charCodeAt(i) > 0x7f)
            throw lazyDOMException('Unrecognized algorithm name', 'NotSupportedError');
        }
      }

      algName = canonicalName;
      const desiredType = registeredAlgorithms[algName];

      if (desiredType === null)
        return { name: algName };

      const normalizedAlgorithm = webidl.algorithmConverters[desiredType](
        algorithm,
        kNormalizeAlgorithmOpts,
      );
      normalizedAlgorithm.name = algName;

      const dictMeta = simpleAlgorithmDictionaries[desiredType];
      if (dictMeta) {
        const { keys: dictKeys, types: dictTypes } = dictMeta;
        for (let i = 0; i < dictKeys.length; i++) {
          const member = dictKeys[i];
          const idlType = dictTypes[member];
          const idlValue = normalizedAlgorithm[member];
          if (idlType === 'BufferSource' && idlValue) {
            normalizedAlgorithm[member] = new Uint8Array(
              getBufferSourceBytes(idlValue),
            );
          } else if (idlType === 'HashAlgorithmIdentifier') {
            const hash = normalizeAlgorithm(idlValue, 'digest');
            if (!hash.name.startsWith('SHA')) {
              throw lazyDOMException(
                `Only SHA hashes are supported in ${desiredType}`, 'NotSupportedError');
            }
            normalizedAlgorithm[member] = hash;
          } else if (idlType === 'AlgorithmIdentifier') {
            throw lazyDOMException('Not implemented.', 'NotSupportedError');
          }
        }
      }

      return normalizedAlgorithm;
    }

    function validateAlgorithm(algorithm, op) {
      if (op === 'get key length' && algorithm.name === 'HMAC')
        return;
      webidl ??= require('internal/crypto/webidl');
      const desiredType = getSupportedAlgorithms().algorithms[op]?.[algorithm.name];
      webidl.validators[desiredType]?.(algorithm);
    }

    function getDataViewOrTypedArrayBuffer(V) {
      return isDataView(V) ?
        DataViewPrototypeGetBuffer(V) : V.buffer;
    }

    function getDataViewOrTypedArrayByteOffset(V) {
      return isDataView(V) ?
        DataViewPrototypeGetByteOffset(V) : V.byteOffset;
    }

    function getDataViewOrTypedArrayByteLength(V) {
      return isDataView(V) ?
        DataViewPrototypeGetByteLength(V) : V.byteLength;
    }

    function getBufferSourceByteLength(V) {
      if (!ArrayBuffer.isView(V))
        return ArrayBufferPrototypeGetByteLength(V);
      if (!isDataView(V))
        return V.byteLength;
      const buffer = DataViewPrototypeGetBuffer(V);
      if (isArrayBuffer(buffer) && ArrayBufferPrototypeGetDetached(buffer))
        return 0;
      return DataViewPrototypeGetByteLength(V);
    }

    function getBufferSourceBytes(V) {
      if (!ArrayBuffer.isView(V)) {
        const length = ArrayBufferPrototypeGetByteLength(V);
        if (length === 0 && ArrayBufferPrototypeGetDetached(V))
          return new Uint8Array(0);
        return new Uint8Array(V, 0, length);
      }

      let buffer;
      let offset;
      let length;
      if (isDataView(V)) {
        buffer = DataViewPrototypeGetBuffer(V);
        if (isArrayBuffer(buffer) && ArrayBufferPrototypeGetDetached(buffer))
          return new Uint8Array(0);
        offset = DataViewPrototypeGetByteOffset(V);
        length = DataViewPrototypeGetByteLength(V);
      } else {
        buffer = V.buffer;
        offset = V.byteOffset;
        length = V.byteLength;
        if (length === 0 && isArrayBuffer(buffer) &&
            ArrayBufferPrototypeGetDetached(buffer))
          return new Uint8Array(0);
      }
      return new Uint8Array(buffer, offset, length);
    }

    function getOptionalByteLength(V) {
      return V === undefined ? 0 : V.byteLength;
    }

    function hasAnyNotIn(set, checks) {
      for (const s of set)
        if (!checks.includes(s))
          return true;
      return false;
    }

    const validateByteSource = hideStackFrames((val, name) => {
      val = toBuf(val);

      if (isAnyArrayBuffer(val) || isArrayBufferView(val))
        return val;

      throw new ERR_INVALID_ARG_TYPE.HideStackFramesError(
        name,
        [
          'string',
          'ArrayBuffer',
          'TypedArray',
          'DataView',
          'Buffer',
        ],
        val);
    });

    function jobPromise(getJob) {
      try {
        return getJob().run();
      } catch (err) {
        return Promise.reject(lazyDOMException(
          'The operation failed for an operation-specific reason',
          { name: 'OperationError', cause: err }));
      }
    }

    function prepareWebCryptoResult(value) {
      if ((value === null || typeof value !== 'object') &&
          typeof value !== 'function') {
        return false;
      }
      if (isPromise(value) || ObjectPrototypeHasOwnProperty(value, 'then'))
        return false;
      setOwnProperty(value, 'then', undefined);
      return true;
    }

    function cleanupWebCryptoResult(value) {
      delete value.then;
    }

    function resolveWebCryptoResult(resolve, value) {
      const shouldCleanupResult = prepareWebCryptoResult(value);
      try {
        resolve(value);
      } finally {
        if (shouldCleanupResult)
          cleanupWebCryptoResult(value);
      }
    }

    function settleJobPromise(handler, resolve, reject, value, isRejected) {
      try {
        if (typeof handler === 'function') {
          resolveWebCryptoResult(resolve, handler(value));
        } else if (isRejected) {
          reject(value);
        } else {
          resolveWebCryptoResult(resolve, value);
        }
      } catch (err) {
        reject(err);
      }
    }

    function jobPromiseThen(promise, onFulfilled, onRejected) {
      const {
        promise: resultPromise,
        resolve,
        reject,
      } = PromiseWithResolvers();
      setOwnProperty(promise, 'constructor', undefined);
      PromisePrototypeThen(
        promise,
        (value) => settleJobPromise(onFulfilled, resolve, reject, value, false),
        (value) => settleJobPromise(onRejected, resolve, reject, value, true));
      return resultPromise;
    }

    function bigIntArrayToUnsignedInt(input) {
      let result = 0;
      const length = input.length;

      for (let n = 0; n < length; ++n) {
        const n_reversed = length - n - 1;
        if (n_reversed >= 4 && input[n]) {
          throw lazyDOMException(
            'algorithm.publicExponent must fit in an unsigned 32-bit integer',
            'OperationError');
        }
        result |= input[n] << 8 * n_reversed;
      }

      return result >>> 0;
    }

    function bigIntArrayToUnsignedBigInt(input) {
      let result = 0n;
      const length = input.length;

      for (let n = 0; n < length; ++n) {
        const n_reversed = length - n - 1;
        result |= BigInt(input[n]) << 8n * BigInt(n_reversed);
      }

      return result;
    }

    function getStringOption(options, key) {
      let value;
      if (options && (value = options[key]) != null)
        validateString(value, `options.${key}`);
      return value;
    }

    const kCanonicalUsageOrder = [
      'encrypt', 'decrypt',
      'sign', 'verify',
      'deriveKey', 'deriveBits',
      'wrapKey', 'unwrapKey',
      'encapsulateKey', 'encapsulateBits',
      'decapsulateKey', 'decapsulateBits',
    ];

    const kUsageMasks = {
      __proto__: null,
    };
    const kUsageByMask = {
      __proto__: null,
    };
    for (let n = 0; n < kCanonicalUsageOrder.length; n++) {
      const usage = kCanonicalUsageOrder[n];
      const mask = 1 << n;
      kUsageMasks[usage] = mask;
      kUsageByMask[mask] = usage;
    }

    function toUsagesSet(usages) {
      const usagesSet = new SafeSet();
      for (let n = 0; n < usages.length; n++) {
        usagesSet.add(usages[n]);
      }
      return usagesSet;
    }

    function getUsagesMask(usageSet) {
      if (usageSet.size === 0) return 0;
      let mask = 0;
      for (const usage of usageSet) {
        mask |= kUsageMasks[usage];
      }
      return mask;
    }

    function hasUsage(mask, usage) {
      return (mask & kUsageMasks[usage]) !== 0;
    }

    function getUsagesFromMask(mask) {
      if (mask === 0) return [];
      if ((mask & (mask - 1)) === 0) {
        return [kUsageByMask[mask]];
      }
      const result = [];
      for (let n = 0; n < kCanonicalUsageOrder.length; n++) {
        if (mask & (1 << n))
          result.push(kCanonicalUsageOrder[n]);
      }
      return result;
    }

    function getBlockSize(name) {
      switch (name) {
        case 'SHA-1':
        case 'SHA-256':
          return 512;
        case 'SHA-384':
        case 'SHA-512':
          return 1024;
        case 'SHA3-256':
        case 'SHA3-384':
        case 'SHA3-512':
          throw lazyDOMException('Explicit algorithm length member is required', 'NotSupportedError');
      }
    }

    function validateKeyOps(keyOps, usagesSet) {
      if (keyOps === undefined) return;
      validateArray(keyOps, 'keyData.key_ops');
      let keyOpsMask = 0;
      const seen = new SafeSet();
      for (let n = 0; n < keyOps.length; n++) {
        const op = keyOps[n];
        if (seen.has(op))
          throw lazyDOMException('Duplicate key operation', 'DataError');
        seen.add(op);
        const opMask = kUsageMasks[op];
        if (opMask === undefined)
          continue;
        keyOpsMask |= opMask;

      }

      if (usagesSet !== undefined) {
        const usagesMask = getUsagesMask(usagesSet);
        if ((keyOpsMask & usagesMask) !== usagesMask) {
          throw lazyDOMException(
            'Key operations and usage mismatch',
            'DataError');
        }
      }
    }

    function secureHeapUsed() {
      const val = _secureHeapUsed();
      if (val === undefined)
        return { total: 0, used: 0, utilization: 0, min: 0 };
      const used = Number(_secureHeapUsed());
      const total = Number(getOptionValue('--secure-heap'));
      const min = Number(getOptionValue('--secure-heap-min'));
      const utilization = used / total;
      return { total, used, utilization, min };
    }

    module.exports = {
      getArrayBufferOrView,
      getBufferSourceByteLength,
      getBufferSourceBytes,
      getCiphers,
      getCurves,
      getDataViewOrTypedArrayBuffer,
      getHashes,
      getMacs,
      getOptionalByteLength,
      kHandle,
      setEngine,
      toBuf,

      kNamedCurveAliases,
      get kSupportedAlgorithms() {
        return getSupportedAlgorithms().algorithms;
      },
      isFips,
      normalizeAlgorithm,
      normalizeHashName,
      hasAnyNotIn,
      validateByteSource,
      validateKeyOps,
      validateAlgorithm,
      jobPromise,
      jobPromiseThen,
      cleanupWebCryptoResult,
      prepareWebCryptoResult,
      validateKmacKeyLength,
      resolveWebCryptoResult,
      validateMaxBufferLength,
      numBitsToBytes,
      truncateToBitLength,
      bigIntArrayToUnsignedBigInt,
      bigIntArrayToUnsignedInt,
      getBlockSize,
      getStringOption,
      getUsagesMask,
      getUsagesFromMask,
      toUsagesSet,
      hasUsage,
      secureHeapUsed,
      getCachedHashId,
      getHashCache,
      getCachedMacId,
      getMacCache,
      getOpenSSLSecLevel,
    };
  },
  "internal/crypto/hashnames": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectKeys,
      ObjectSetPrototypeOf,
    } = primordials;

    const kHashContextNode = 1;
    const kHashContextWebCrypto = 2;
    const kHashContextJwkRsa = 3;
    const kHashContextJwkRsaPss = 4;
    const kHashContextJwkRsaOaep = 5;
    const kHashContextJwkHmac = 6;

    const kHashNames = {
      'sha1': {
        [kHashContextNode]: 'sha1',
        [kHashContextWebCrypto]: 'SHA-1',
        [kHashContextJwkRsa]: 'RS1',
        [kHashContextJwkRsaPss]: 'PS1',
        [kHashContextJwkRsaOaep]: 'RSA-OAEP',
        [kHashContextJwkHmac]: 'HS1',
      },
      'sha256': {
        [kHashContextNode]: 'sha256',
        [kHashContextWebCrypto]: 'SHA-256',
        [kHashContextJwkRsa]: 'RS256',
        [kHashContextJwkRsaPss]: 'PS256',
        [kHashContextJwkRsaOaep]: 'RSA-OAEP-256',
        [kHashContextJwkHmac]: 'HS256',
      },
      'sha384': {
        [kHashContextNode]: 'sha384',
        [kHashContextWebCrypto]: 'SHA-384',
        [kHashContextJwkRsa]: 'RS384',
        [kHashContextJwkRsaPss]: 'PS384',
        [kHashContextJwkRsaOaep]: 'RSA-OAEP-384',
        [kHashContextJwkHmac]: 'HS384',
      },
      'sha512': {
        [kHashContextNode]: 'sha512',
        [kHashContextWebCrypto]: 'SHA-512',
        [kHashContextJwkRsa]: 'RS512',
        [kHashContextJwkRsaPss]: 'PS512',
        [kHashContextJwkRsaOaep]: 'RSA-OAEP-512',
        [kHashContextJwkHmac]: 'HS512',
      },
      'shake128': {
        [kHashContextNode]: 'shake128',
        [kHashContextWebCrypto]: 'cSHAKE128',
      },
      'shake256': {
        [kHashContextNode]: 'shake256',
        [kHashContextWebCrypto]: 'cSHAKE256',
      },
      'sha3-256': {
        [kHashContextNode]: 'sha3-256',
        [kHashContextWebCrypto]: 'SHA3-256',
      },
      'sha3-384': {
        [kHashContextNode]: 'sha3-384',
        [kHashContextWebCrypto]: 'SHA3-384',
      },
      'sha3-512': {
        [kHashContextNode]: 'sha3-512',
        [kHashContextWebCrypto]: 'SHA3-512',
      },
    };

    Object.setPrototypeOf(kHashNames, null);

    {
      const keys = Object.keys(kHashNames);
      for (let n = 0; n < keys.length; n++) {
        const entry = kHashNames[keys[n]];
        Object.setPrototypeOf(entry, null);
        const contexts = Object.keys(entry);
        for (let i = 0; i < contexts.length; i++) {
          const alias = entry[contexts[i]];
          if (kHashNames[alias] === undefined)
            kHashNames[alias] = entry;
        }
      }
    }

    function normalizeHashName(name, context = kHashContextNode) {
      return kHashNames[name]?.[context];
    }

    normalizeHashName.kContextNode = kHashContextNode;
    normalizeHashName.kContextWebCrypto = kHashContextWebCrypto;
    normalizeHashName.kContextJwkRsa = kHashContextJwkRsa;
    normalizeHashName.kContextJwkRsaPss = kHashContextJwkRsaPss;
    normalizeHashName.kContextJwkRsaOaep = kHashContextJwkRsaOaep;
    normalizeHashName.kContextJwkHmac = kHashContextJwkHmac;

    module.exports = normalizeHashName;
  },
  "internal/streams/lazy_transform": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectSetPrototypeOf,
    } = primordials;

    const stream = require('stream');

    module.exports = LazyTransform;

    function LazyTransform(options) {
      this._options = options;
    }
    Object.setPrototypeOf(LazyTransform.prototype, stream.Transform.prototype);
    Object.setPrototypeOf(LazyTransform, stream.Transform);

    function makeGetter(name) {
      return function() {
        stream.Transform.call(this, this._options);
        this._writableState.decodeStrings = false;
        return this[name];
      };
    }

    function makeSetter(name) {
      return function(val) {
        Object.defineProperty(this, name, {
          __proto__: null,
          value: val,
          enumerable: true,
          configurable: true,
          writable: true,
        });
      };
    }

    Object.defineProperties(LazyTransform.prototype, {
      _readableState: {
        __proto__: null,
        get: makeGetter('_readableState'),
        set: makeSetter('_readableState'),
        configurable: true,
        enumerable: true,
      },
      _writableState: {
        __proto__: null,
        get: makeGetter('_writableState'),
        set: makeSetter('_writableState'),
        configurable: true,
        enumerable: true,
      },
    });
  },
  "crypto": function (module, exports, require, process, primordials, internalBinding) {
    "use strict";

    const { Hash, Hmac, hash } = require("internal/crypto/hash");
    const {
      randomBytes,
      randomFill,
      randomFillSync,
      randomInt,
      randomUUID,
      getRandomValues: fillRandomValues,
    } = require("internal/crypto/random");
    const { getHashes } = require("internal/crypto/util");
    const {
      codes: { ERR_ILLEGAL_CONSTRUCTOR, ERR_INVALID_THIS },
    } = require("internal/errors");
    const { isAnyArrayBuffer, isArrayBufferView } = require("internal/util/types");
    const { deprecate, lazyDOMException } = require("internal/util");

    function createHash(algorithm, options) {
      return new Hash(algorithm, options);
    }

    function createHmac(hmac, key, options) {
      return new Hmac(hmac, key, options);
    }

    function bytesOf(value, name) {
      if (isAnyArrayBuffer(value)) return new Uint8Array(value);
      if (isArrayBufferView(value)) {
        return new Uint8Array(value.buffer, value.byteOffset, value.byteLength);
      }
      // worded as node's c++ words it, which does not say what it was given
      const error = new TypeError(
        `The "${name}" argument must be an instance of ArrayBuffer, Buffer, TypedArray, or DataView.`,
      );
      error.code = "ERR_INVALID_ARG_TYPE";
      throw error;
    }

    // every byte is looked at, whatever the earlier ones were
    function timingSafeEqual(buf1, buf2) {
      const left = bytesOf(buf1, "buf1");
      const right = bytesOf(buf2, "buf2");
      if (left.length !== right.length) {
        const error = new RangeError("Input buffers must have the same byte length");
        error.code = "ERR_CRYPTO_TIMING_SAFE_EQUAL_LENGTH";
        throw error;
      }
      let different = 0;
      for (let i = 0; i < left.length; i++) different |= left[i] ^ right[i];
      return different === 0;
    }

    function checkedRandomValues(array) {
      if (!isArrayBufferView(array)) {
        const error = new TypeError(
          "Failed to execute 'getRandomValues' on 'Crypto': 1st argument is not an ArrayBufferView.",
        );
        error.code = "ERR_INVALID_ARG_TYPE";
        throw error;
      }
      return fillRandomValues(array);
    }

    // the web's digest names, and node's for the same thing
    const webDigests = {
      __proto__: null,
      "SHA-1": "sha1",
      "SHA-256": "sha256",
      "SHA-384": "sha384",
      "SHA-512": "sha512",
    };

    class SubtleCrypto {
      constructor() {
        throw new ERR_ILLEGAL_CONSTRUCTOR();
      }

      async digest(algorithm, data) {
        if (!(this instanceof SubtleCrypto)) throw new ERR_INVALID_THIS("SubtleCrypto");
        const name = typeof algorithm === "string" ? algorithm : algorithm?.name;
        const known = webDigests[String(name).toUpperCase()];
        if (known === undefined) {
          throw lazyDOMException("Unrecognized algorithm name", "NotSupportedError");
        }
        const digest = new Hash(known).update(bytesOf(data, "data")).digest();
        return digest.buffer.slice(digest.byteOffset, digest.byteOffset + digest.byteLength);
      }
    }

    class Crypto {
      constructor() {
        throw new ERR_ILLEGAL_CONSTRUCTOR();
      }

      get subtle() {
        if (!(this instanceof Crypto)) throw new ERR_INVALID_THIS("Crypto");
        return subtle;
      }

      getRandomValues(array) {
        if (!(this instanceof Crypto)) throw new ERR_INVALID_THIS("Crypto");
        return checkedRandomValues(array);
      }

      randomUUID() {
        if (!(this instanceof Crypto)) throw new ERR_INVALID_THIS("Crypto");
        return randomUUID();
      }
    }

    for (const made of [SubtleCrypto, Crypto]) {
      Object.defineProperty(made.prototype, Symbol.toStringTag, {
        value: made.name,
        configurable: true,
      });
    }

    const subtle = Object.create(SubtleCrypto.prototype);
    const webcrypto = Object.create(Crypto.prototype);

    function getRandomValues(array) {
      return checkedRandomValues(array);
    }

    module.exports = {
      createHash,
      createHmac,
      getHashes,
      hash,
      randomBytes,
      randomFill,
      randomFillSync,
      randomInt,
      randomUUID,
      timingSafeEqual,
      Hash: deprecate(Hash, "crypto.Hash constructor is deprecated.", "DEP0179"),
      Hmac: deprecate(Hmac, "crypto.Hmac constructor is deprecated.", "DEP0181"),
      getRandomValues,
      webcrypto,
      subtle,
      // the web's global, which is the same object as webcrypto
      crypto: webcrypto,
    };

    Object.defineProperties(module.exports, {
      crypto: { enumerable: false },
      prng: { value: randomBytes, enumerable: false, configurable: true, writable: true },
      pseudoRandomBytes: { value: randomBytes, enumerable: false, configurable: true, writable: true },
      rng: { value: randomBytes, enumerable: false, configurable: true, writable: true },
    });
  },
};

const loaded = new Map();

// a module this file holds, by node's own name for it
function own(name) {
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
  files[name](
    made,
    made.exports,
    load,
    shims.process(),
    shims.primordials,
    shims.internalBinding,
  );
  shims.afterLoad?.[name]?.(made.exports);
  return made.exports;
}

// what the files here call require: this file's own modules first, then
// wherever else the name can be found
function load(name) {
  if (
    loaded.has(name) ||
    Object.hasOwn(shims, name) ||
    Object.hasOwn(files, name) ||
    shims.fallback === undefined
  ) {
    return own(name);
  }
  return shims.fallback(name);
}

module.exports = load("crypto");
