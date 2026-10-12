"use strict";

// node's lib/zlib.js. The handles it drives, c++ in node, are written out
// below over natives that keep nothing between calls

const internal = require("spark:internal");
const loop = require("spark:loop");
const { Buffer } = require("node:buffer");

internal.addLoader((name) => load(name));

// node's zlib constants, by name
const constants = {
  Z_NO_FLUSH: 0,
  Z_PARTIAL_FLUSH: 1,
  Z_SYNC_FLUSH: 2,
  Z_FULL_FLUSH: 3,
  Z_FINISH: 4,
  Z_BLOCK: 5,
  Z_OK: 0,
  Z_STREAM_END: 1,
  Z_NEED_DICT: 2,
  Z_ERRNO: -1,
  Z_STREAM_ERROR: -2,
  Z_DATA_ERROR: -3,
  Z_MEM_ERROR: -4,
  Z_BUF_ERROR: -5,
  Z_VERSION_ERROR: -6,
  Z_NO_COMPRESSION: 0,
  Z_BEST_SPEED: 1,
  Z_BEST_COMPRESSION: 9,
  Z_DEFAULT_COMPRESSION: -1,
  Z_FILTERED: 1,
  Z_HUFFMAN_ONLY: 2,
  Z_RLE: 3,
  Z_FIXED: 4,
  Z_DEFAULT_STRATEGY: 0,
  ZLIB_VERNUM: 4897,
  DEFLATE: 1,
  INFLATE: 2,
  GZIP: 3,
  GUNZIP: 4,
  DEFLATERAW: 5,
  INFLATERAW: 6,
  UNZIP: 7,
  BROTLI_DECODE: 8,
  BROTLI_ENCODE: 9,
  ZSTD_DECOMPRESS: 11,
  ZSTD_COMPRESS: 10,
  Z_MIN_WINDOWBITS: 8,
  Z_MAX_WINDOWBITS: 15,
  Z_DEFAULT_WINDOWBITS: 15,
  Z_MIN_CHUNK: 64,
  Z_MAX_CHUNK: Infinity,
  Z_DEFAULT_CHUNK: 16384,
  Z_MIN_MEMLEVEL: 1,
  Z_MAX_MEMLEVEL: 9,
  Z_DEFAULT_MEMLEVEL: 8,
  Z_MIN_LEVEL: -1,
  Z_MAX_LEVEL: 9,
  Z_DEFAULT_LEVEL: -1,
  BROTLI_OPERATION_PROCESS: 0,
  BROTLI_OPERATION_FLUSH: 1,
  BROTLI_OPERATION_FINISH: 2,
  BROTLI_OPERATION_EMIT_METADATA: 3,
  BROTLI_PARAM_MODE: 0,
  BROTLI_MODE_GENERIC: 0,
  BROTLI_MODE_TEXT: 1,
  BROTLI_MODE_FONT: 2,
  BROTLI_DEFAULT_MODE: 0,
  BROTLI_PARAM_QUALITY: 1,
  BROTLI_MIN_QUALITY: 0,
  BROTLI_MAX_QUALITY: 11,
  BROTLI_DEFAULT_QUALITY: 11,
  BROTLI_PARAM_LGWIN: 2,
  BROTLI_MIN_WINDOW_BITS: 10,
  BROTLI_MAX_WINDOW_BITS: 24,
  BROTLI_LARGE_MAX_WINDOW_BITS: 30,
  BROTLI_DEFAULT_WINDOW: 22,
  BROTLI_PARAM_LGBLOCK: 3,
  BROTLI_MIN_INPUT_BLOCK_BITS: 16,
  BROTLI_MAX_INPUT_BLOCK_BITS: 24,
  BROTLI_PARAM_DISABLE_LITERAL_CONTEXT_MODELING: 4,
  BROTLI_PARAM_SIZE_HINT: 5,
  BROTLI_PARAM_LARGE_WINDOW: 6,
  BROTLI_PARAM_NPOSTFIX: 7,
  BROTLI_PARAM_NDIRECT: 8,
  BROTLI_DECODER_RESULT_ERROR: 0,
  BROTLI_DECODER_RESULT_SUCCESS: 1,
  BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT: 2,
  BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT: 3,
  BROTLI_DECODER_PARAM_DISABLE_RING_BUFFER_REALLOCATION: 0,
  BROTLI_DECODER_PARAM_LARGE_WINDOW: 1,
  BROTLI_DECODER_NO_ERROR: 0,
  BROTLI_DECODER_SUCCESS: 1,
  BROTLI_DECODER_NEEDS_MORE_INPUT: 2,
  BROTLI_DECODER_NEEDS_MORE_OUTPUT: 3,
  BROTLI_DECODER_ERROR_FORMAT_EXUBERANT_NIBBLE: -1,
  BROTLI_DECODER_ERROR_FORMAT_RESERVED: -2,
  BROTLI_DECODER_ERROR_FORMAT_EXUBERANT_META_NIBBLE: -3,
  BROTLI_DECODER_ERROR_FORMAT_SIMPLE_HUFFMAN_ALPHABET: -4,
  BROTLI_DECODER_ERROR_FORMAT_SIMPLE_HUFFMAN_SAME: -5,
  BROTLI_DECODER_ERROR_FORMAT_CL_SPACE: -6,
  BROTLI_DECODER_ERROR_FORMAT_HUFFMAN_SPACE: -7,
  BROTLI_DECODER_ERROR_FORMAT_CONTEXT_MAP_REPEAT: -8,
  BROTLI_DECODER_ERROR_FORMAT_BLOCK_LENGTH_1: -9,
  BROTLI_DECODER_ERROR_FORMAT_BLOCK_LENGTH_2: -10,
  BROTLI_DECODER_ERROR_FORMAT_TRANSFORM: -11,
  BROTLI_DECODER_ERROR_FORMAT_DICTIONARY: -12,
  BROTLI_DECODER_ERROR_FORMAT_WINDOW_BITS: -13,
  BROTLI_DECODER_ERROR_FORMAT_PADDING_1: -14,
  BROTLI_DECODER_ERROR_FORMAT_PADDING_2: -15,
  BROTLI_DECODER_ERROR_FORMAT_DISTANCE: -16,
  BROTLI_DECODER_ERROR_DICTIONARY_NOT_SET: -19,
  BROTLI_DECODER_ERROR_INVALID_ARGUMENTS: -20,
  BROTLI_DECODER_ERROR_ALLOC_CONTEXT_MODES: -21,
  BROTLI_DECODER_ERROR_ALLOC_TREE_GROUPS: -22,
  BROTLI_DECODER_ERROR_ALLOC_CONTEXT_MAP: -25,
  BROTLI_DECODER_ERROR_ALLOC_RING_BUFFER_1: -26,
  BROTLI_DECODER_ERROR_ALLOC_RING_BUFFER_2: -27,
  BROTLI_DECODER_ERROR_ALLOC_BLOCK_TYPE_TREES: -30,
  BROTLI_DECODER_ERROR_UNREACHABLE: -31,
  ZSTD_e_continue: 0,
  ZSTD_e_flush: 1,
  ZSTD_e_end: 2,
  ZSTD_fast: 1,
  ZSTD_dfast: 2,
  ZSTD_greedy: 3,
  ZSTD_lazy: 4,
  ZSTD_lazy2: 5,
  ZSTD_btlazy2: 6,
  ZSTD_btopt: 7,
  ZSTD_btultra: 8,
  ZSTD_btultra2: 9,
  ZSTD_c_compressionLevel: 100,
  ZSTD_c_windowLog: 101,
  ZSTD_c_hashLog: 102,
  ZSTD_c_chainLog: 103,
  ZSTD_c_searchLog: 104,
  ZSTD_c_minMatch: 105,
  ZSTD_c_targetLength: 106,
  ZSTD_c_strategy: 107,
  ZSTD_c_enableLongDistanceMatching: 160,
  ZSTD_c_ldmHashLog: 161,
  ZSTD_c_ldmMinMatch: 162,
  ZSTD_c_ldmBucketSizeLog: 163,
  ZSTD_c_ldmHashRateLog: 164,
  ZSTD_c_contentSizeFlag: 200,
  ZSTD_c_checksumFlag: 201,
  ZSTD_c_dictIDFlag: 202,
  ZSTD_c_nbWorkers: 400,
  ZSTD_c_jobSize: 401,
  ZSTD_c_overlapLog: 402,
  ZSTD_d_windowLogMax: 100,
  ZSTD_CLEVEL_DEFAULT: 3,
  ZSTD_error_no_error: 0,
  ZSTD_error_GENERIC: 1,
  ZSTD_error_prefix_unknown: 10,
  ZSTD_error_version_unsupported: 12,
  ZSTD_error_frameParameter_unsupported: 14,
  ZSTD_error_frameParameter_windowTooLarge: 16,
  ZSTD_error_corruption_detected: 20,
  ZSTD_error_checksum_wrong: 22,
  ZSTD_error_literals_headerWrong: 24,
  ZSTD_error_dictionary_corrupted: 30,
  ZSTD_error_dictionary_wrong: 32,
  ZSTD_error_dictionaryCreation_failed: 34,
  ZSTD_error_parameter_unsupported: 40,
  ZSTD_error_parameter_combination_unsupported: 41,
  ZSTD_error_parameter_outOfBound: 42,
  ZSTD_error_tableLog_tooLarge: 44,
  ZSTD_error_maxSymbolValue_tooLarge: 46,
  ZSTD_error_maxSymbolValue_tooSmall: 48,
  ZSTD_error_stabilityCondition_notRespected: 50,
  ZSTD_error_stage_wrong: 60,
  ZSTD_error_init_missing: 62,
  ZSTD_error_memory_allocation: 64,
  ZSTD_error_workSpace_tooSmall: 66,
  ZSTD_error_dstSize_tooSmall: 70,
  ZSTD_error_srcSize_wrong: 72,
  ZSTD_error_dstBuffer_null: 74,
  ZSTD_error_noForwardProgress_destFull: 80,
  ZSTD_error_noForwardProgress_inputEmpty: 82,
};

const owner_symbol = Symbol("owner_symbol");
const EMPTY = new Uint8Array(0);

// one compression or decompression in progress. Node's javascript hands it
// input and a buffer for output; what does not fit waits for the next call
class Handle {
  #pending = EMPTY;
  // the last call filled its output buffer, so the next one only collects
  #draining = false;
  writeState;
  processCallback;

  // every byte of new output this write gives, or undefined after reporting
  // an error through onerror
  produce(chunk, flush) {
    throw new Error("not a real handle");
  }

  #step(flush, input, inOff, inLen, out, outOff, outLen) {
    if (!this.#draining) {
      const chunk =
        inLen > 0
          ? new Uint8Array(input.buffer, input.byteOffset + inOff, inLen).slice()
          : EMPTY;
      const produced = this.produce(chunk, flush);
      if (produced === undefined) return false;
      this.#pending = produced;
    }
    const count = Math.min(outLen, this.#pending.length);
    if (count > 0) {
      new Uint8Array(out.buffer, out.byteOffset + outOff, count).set(
        this.#pending.subarray(0, count),
      );
      this.#pending = this.#pending.subarray(count);
    }
    this.#draining = outLen - count === 0;
    this.writeState[0] = outLen - count;
    this.writeState[1] = 0;
    return true;
  }

  writeSync(flush, input, inOff, inLen, out, outOff, outLen) {
    this.#step(flush, input, inOff, inLen, out, outOff, outLen);
  }

  write(flush, input, inOff, inLen, out, outOff, outLen) {
    if (this.#step(flush, input, inOff, inLen, out, outOff, outLen)) {
      loop.nextTick(() => this.processCallback());
    }
  }

  cleared() {
    this.#pending = EMPTY;
    this.#draining = false;
  }

  close() {}
}

const formats = {
  __proto__: null,
  [constants.DEFLATE]: "deflate",
  [constants.INFLATE]: "deflate",
  [constants.GZIP]: "gzip",
  [constants.GUNZIP]: "gzip",
  [constants.DEFLATERAW]: "raw",
  [constants.INFLATERAW]: "raw",
  [constants.UNZIP]: "either",
};

// zlib gives a reason with each failure; the natives give only its code, so
// the reason is worked out from where the data went wrong
function reasonFor(code, format, input) {
  if (code === "trailing") return ["incorrect header check", constants.Z_DATA_ERROR, "Z_DATA_ERROR"];
  if (code === "Z_NEED_DICT") return ["Missing dictionary", constants.Z_NEED_DICT, code];
  if (code === "Z_BUF_ERROR") return ["unexpected end of file", constants.Z_BUF_ERROR, code];
  if (code !== "Z_DATA_ERROR") return [code, constants[code] ?? constants.Z_STREAM_ERROR, code];
  const gzip = input[0] === 0x1f && input[1] === 0x8b;
  const zlib = (input[0] & 0x0f) === 8 && ((input[0] << 8) | input[1]) % 31 === 0;
  let first;
  if (format === "raw") first = 0;
  else if (format === "gzip" && gzip) first = 10;
  else if (format === "deflate" && zlib) first = 2;
  else if (format === "either" && (gzip || zlib)) first = gzip ? 10 : 2;
  else return ["incorrect header check", constants.Z_DATA_ERROR, code];
  // the kind of the first block is in its first byte
  const message = [
    "invalid stored block lengths",
    "invalid distance too far back",
    "invalid code lengths set",
    "invalid block type",
  ][(input[first] >> 1) & 3];
  return [message, constants.Z_DATA_ERROR, code];
}

class Zlib extends Handle {
  #mode;
  #format;
  #compresses;
  #windowBits;
  #level;
  #memLevel;
  #strategy;
  #dictionary;
  // compressing: every write so far as bytes, flush, bytes, flush, ...
  #writes = [];
  // decompressing: every byte given so far
  #input = EMPTY;
  #emitted = 0;

  constructor(mode) {
    super();
    this.#mode = mode;
    this.#format = formats[mode];
    this.#compresses =
      mode === constants.DEFLATE ||
      mode === constants.GZIP ||
      mode === constants.DEFLATERAW;
  }

  init(windowBits, level, memLevel, strategy, writeState, processCallback, dictionary) {
    this.#windowBits = windowBits;
    this.#level = level;
    this.#memLevel = memLevel;
    this.#strategy = strategy;
    this.#dictionary = dictionary;
    this.writeState = writeState;
    this.processCallback = processCallback;
  }

  params(level, strategy) {
    this.#level = level;
    this.#strategy = strategy;
  }

  reset() {
    this.#writes = [];
    this.#input = EMPTY;
    this.#emitted = 0;
    this.cleared();
  }

  #fresh(total) {
    const bytes = new Uint8Array(total);
    const fresh = bytes.subarray(this.#emitted);
    this.#emitted = bytes.length;
    return fresh;
  }

  produce(chunk, flush) {
    try {
      if (this.#compresses) {
        this.#writes.push(chunk, flush);
        if (flush === constants.Z_NO_FLUSH || flush === constants.Z_BLOCK) {
          return EMPTY;
        }
        return this.#fresh(
          native.deflate(
            this.#format,
            this.#level,
            this.#windowBits,
            this.#memLevel,
            this.#strategy,
            this.#dictionary,
            ...this.#writes,
          ),
        );
      }
      if (chunk.length > 0) {
        const all = new Uint8Array(this.#input.length + chunk.length);
        all.set(this.#input);
        all.set(chunk, this.#input.length);
        this.#input = all;
      }
      const [total, finished] = native.inflate(
        this.#input,
        this.#format,
        this.#windowBits,
        this.#dictionary,
      );
      if (flush === constants.Z_FINISH && !finished) {
        this.onerror("unexpected end of file", constants.Z_BUF_ERROR, "Z_BUF_ERROR");
        return undefined;
      }
      return this.#fresh(total);
    } catch (thrown) {
      if (typeof thrown !== "string") throw thrown;
      const [message, errno, code] = reasonFor(thrown, this.#format, this.#input);
      this.onerror(message, errno, code);
      return undefined;
    }
  }
}

// the zstd failures the natives can name, with node's code and number for each
const zstdErrors = {
  __proto__: null,
  "Unknown frame descriptor": ["ZSTD_error_prefix_unknown", 10],
  "Version not supported": ["ZSTD_error_version_unsupported", 12],
  "Unsupported frame parameter": ["ZSTD_error_frameParameter_unsupported", 14],
  "Data corruption detected": ["ZSTD_error_corruption_detected", 20],
  "Restored data doesn't match checksum": ["ZSTD_error_checksum_wrong", 22],
  "Dictionary mismatch": ["ZSTD_error_dictionary_wrong", 32],
};

class Zstd extends Handle {
  #compresses;
  #level;
  #input = EMPTY;
  #emitted = 0;

  constructor(compresses) {
    super();
    this.#compresses = compresses;
  }

  init(params, pledgedSrcSize, writeState, processCallback) {
    const level = params[constants.ZSTD_c_compressionLevel] | 0;
    this.#level = this.#compresses && level !== -1 ? level : undefined;
    this.writeState = writeState;
    this.processCallback = processCallback;
  }

  params() {}

  reset() {
    this.#input = EMPTY;
    this.#emitted = 0;
    this.cleared();
  }

  produce(chunk, flush) {
    if (chunk.length > 0) {
      const all = new Uint8Array(this.#input.length + chunk.length);
      all.set(this.#input);
      all.set(chunk, this.#input.length);
      this.#input = all;
    }
    try {
      let total;
      if (this.#compresses) {
        if (flush !== constants.ZSTD_e_end) return EMPTY;
        total = native.zstdCompress(this.#input, this.#level);
      } else {
        let finished;
        [total, finished] = native.zstdDecompress(this.#input);
        if (flush === constants.ZSTD_e_end && !finished) {
          this.onerror("unexpected end of file", constants.Z_BUF_ERROR, "Z_BUF_ERROR");
          return undefined;
        }
      }
      const bytes = new Uint8Array(total);
      const fresh = bytes.subarray(this.#emitted);
      this.#emitted = bytes.length;
      return fresh;
    } catch (thrown) {
      if (typeof thrown !== "string") throw thrown;
      const [code, errno] = zstdErrors[thrown] ?? ["ZSTD_error_GENERIC", 1];
      this.onerror(thrown, errno, code);
      return undefined;
    }
  }
}

class ZstdCompress extends Zstd {
  constructor() {
    super(true);
  }
}

class ZstdDecompress extends Zstd {
  constructor() {
    super(false);
  }
}

function notHere(what) {
  const { codes } = internal.load("internal/errors");
  return new codes.ERR_METHOD_NOT_IMPLEMENTED(what);
}

class Brotli {
  constructor() {
    throw notHere("brotli");
  }
}

class Zip {
  constructor() {
    throw notHere("zip");
  }
}

function noZip() {
  throw notHere("zip");
}

const binding = {
  Zlib,
  ZstdCompress,
  ZstdDecompress,
  BrotliEncoder: Brotli,
  BrotliDecoder: Brotli,
  crc32(data, value) {
    return native.crc32(typeof data === "string" ? Buffer.from(data) : data, value);
  },
};

const shims = {
  process: internal.process,
  primordials: internal.primordials,
  internalBinding(name) {
    if (name === "zlib") return binding;
    if (name === "constants") {
      return { ...internal.internalBinding("constants"), zlib: constants };
    }
    return internal.internalBinding(name);
  },
  fallback: internal.load,
  stream: () => require("node:stream"),
  buffer: () => require("node:buffer"),
  "internal/buffer": () => ({
    FastBuffer: function FastBuffer(buffer, byteOffset, length) {
      return buffer === undefined
        ? Buffer.alloc(0)
        : Buffer.from(buffer, byteOffset, length);
    },
  }),
  "internal/async_hooks": () => ({ symbols: { owner_symbol } }),
  "internal/zip": () => ({
    ZipEntry: class ZipEntry extends Zip {},
    ZipFile: class ZipFile extends Zip {},
    ZipBuffer: class ZipBuffer extends Zip {},
    createZipArchive: noZip,
    createZipArchiveSync: noZip,
    zipFiles: noZip,
    getMaxZipContentSize: noZip,
    setMaxZipContentSize: noZip,
  }),
};

const files = {
  "zlib": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayBuffer,
      MathMax,
      NumberIsNaN,
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectEntries,
      ObjectFreeze,
      ObjectKeys,
      ObjectSetPrototypeOf,
      ReflectApply,
      Symbol,
      SymbolHasInstance,
      Uint32Array,
      Uint8Array,
    } = primordials;

    const {
      codes: {
        ERR_BROTLI_INVALID_PARAM,
        ERR_BUFFER_TOO_LARGE,
        ERR_INVALID_ARG_TYPE,
        ERR_OUT_OF_RANGE,
        ERR_TRAILING_JUNK_AFTER_STREAM_END,
        ERR_ZSTD_INVALID_PARAM,
      },
      genericNodeError,
    } = require('internal/errors');
    const { Transform, finished } = require('stream');
    const {
      assignFunctionName,
      deprecateInstantiation,
      emitExperimentalWarning,
    } = require('internal/util');
    const {
      isArrayBufferView,
      isAnyArrayBuffer,
      isUint8Array,
    } = require('internal/util/types');
    const binding = internalBinding('zlib');
    const { crc32: crc32Native } = binding;
    const assert = require('internal/assert');
    const {
      Buffer,
      kMaxLength,
    } = require('buffer');
    const { owner_symbol } = require('internal/async_hooks').symbols;
    const {
      checkRangesOrGetDefault,
      validateBoolean,
      validateFunction,
      validateUint32,
      validateFiniteNumber,
    } = require('internal/validators');
    const { FastBuffer } = require('internal/buffer');
    const {
      ZipEntry,
      ZipFile,
      ZipBuffer,
      createZipArchive,
      createZipArchiveSync,
      zipFiles,
      getMaxZipContentSize,
      setMaxZipContentSize,
    } = require('internal/zip');

    const kFlushFlag = Symbol('kFlushFlag');
    const kError = Symbol('kError');

    const constants = internalBinding('constants').zlib;
    const {
      Z_NO_FLUSH, Z_BLOCK, Z_PARTIAL_FLUSH, Z_SYNC_FLUSH, Z_FULL_FLUSH, Z_FINISH,
      Z_MIN_CHUNK, Z_MIN_WINDOWBITS, Z_MAX_WINDOWBITS, Z_MIN_LEVEL, Z_MAX_LEVEL,
      Z_MIN_MEMLEVEL, Z_MAX_MEMLEVEL, Z_DEFAULT_CHUNK, Z_DEFAULT_COMPRESSION,
      Z_DEFAULT_STRATEGY, Z_DEFAULT_WINDOWBITS, Z_DEFAULT_MEMLEVEL, Z_FIXED,
      DEFLATE, DEFLATERAW, INFLATE, INFLATERAW, GZIP, GUNZIP, UNZIP,
      BROTLI_DECODE, BROTLI_ENCODE,
      ZSTD_COMPRESS, ZSTD_DECOMPRESS,
      BROTLI_OPERATION_PROCESS, BROTLI_OPERATION_FLUSH,
      BROTLI_OPERATION_FINISH, BROTLI_OPERATION_EMIT_METADATA,
      ZSTD_e_continue, ZSTD_e_flush, ZSTD_e_end,
    } = constants;

    const codes = {
      Z_OK: constants.Z_OK,
      Z_STREAM_END: constants.Z_STREAM_END,
      Z_NEED_DICT: constants.Z_NEED_DICT,
      Z_ERRNO: constants.Z_ERRNO,
      Z_STREAM_ERROR: constants.Z_STREAM_ERROR,
      Z_DATA_ERROR: constants.Z_DATA_ERROR,
      Z_MEM_ERROR: constants.Z_MEM_ERROR,
      Z_BUF_ERROR: constants.Z_BUF_ERROR,
      Z_VERSION_ERROR: constants.Z_VERSION_ERROR,
    };

    for (const ckey of Object.keys(codes)) {
      codes[codes[ckey]] = ckey;
    }

    function zlibBuffer(engine, buffer, callback) {
      validateFunction(callback, 'callback');
      if (isArrayBufferView(buffer) && !isUint8Array(buffer)) {
        buffer = Buffer.from(buffer.buffer, buffer.byteOffset, buffer.byteLength);
      } else if (isAnyArrayBuffer(buffer)) {
        buffer = Buffer.from(buffer);
      }
      engine.buffers = null;
      engine.nread = 0;
      engine.cb = callback;
      engine.on('data', zlibBufferOnData);
      engine.on('error', zlibBufferOnError);
      engine.on('end', zlibBufferOnEnd);
      engine.end(buffer);
    }

    function zlibBufferOnData(chunk) {
      if (!this.buffers) {
        this.buffers = [chunk];
      } else {
        this.buffers.push(chunk);
      }
      this.nread += chunk.length;
      if (this.nread > this._maxOutputLength) {
        this.close();
        this.removeAllListeners('end');
        this.cb(new ERR_BUFFER_TOO_LARGE(this._maxOutputLength));
      }
    }

    function zlibBufferOnError(err) {
      this.removeAllListeners('end');
      this.cb(err);
    }

    function zlibBufferOnEnd() {
      let buf;
      if (this.nread === 0) {
        buf = new FastBuffer();
      } else {
        const bufs = this.buffers;
        buf = (bufs.length === 1 ? bufs[0] : Buffer.concat(bufs, this.nread));
      }
      this.close();
      if (this._info)
        this.cb(null, { buffer: buf, engine: this });
      else
        this.cb(null, buf);
    }

    function zlibBufferSync(engine, buffer) {
      if (typeof buffer === 'string') {
        buffer = Buffer.from(buffer);
      } else if (!isArrayBufferView(buffer)) {
        if (isAnyArrayBuffer(buffer)) {
          buffer = Buffer.from(buffer);
        } else {
          throw new ERR_INVALID_ARG_TYPE(
            'buffer',
            ['string', 'Buffer', 'TypedArray', 'DataView', 'ArrayBuffer'],
            buffer,
          );
        }
      }
      buffer = processChunkSync(engine, buffer, engine._finishFlushFlag);
      if (engine._info)
        return { buffer, engine };
      return buffer;
    }

    function zlibOnError(message, errno, code) {
      const self = this[owner_symbol];

      let error;
      if (code === 'ERR_TRAILING_JUNK_AFTER_STREAM_END') {
        error = new ERR_TRAILING_JUNK_AFTER_STREAM_END();
      } else {
        error = genericNodeError(message, { errno, code });
        error.errno = errno;
        error.code = code;
      }
      self.destroy(error);
      self[kError] = error;
    }

    const FLUSH_BOUND = [
      [ Z_NO_FLUSH, Z_BLOCK ],
      [ BROTLI_OPERATION_PROCESS, BROTLI_OPERATION_EMIT_METADATA ],
      [ ZSTD_e_continue, ZSTD_e_end ],
    ];
    const FLUSH_BOUND_IDX_NORMAL = 0;
    const FLUSH_BOUND_IDX_BROTLI = 1;
    const FLUSH_BOUND_IDX_ZSTD = 2;

    function ZlibBase(opts, mode, handle, { flush, finishFlush, fullFlush }) {
      let chunkSize = Z_DEFAULT_CHUNK;
      let maxOutputLength = kMaxLength;
      assert(typeof mode === 'number');
      assert(mode >= DEFLATE && mode <= ZSTD_DECOMPRESS);

      let flushBoundIdx;
      if (mode === BROTLI_ENCODE || mode === BROTLI_DECODE) {
        flushBoundIdx = FLUSH_BOUND_IDX_BROTLI;
      } else if (mode === ZSTD_COMPRESS || mode === ZSTD_DECOMPRESS) {
        flushBoundIdx = FLUSH_BOUND_IDX_ZSTD;
      } else {
        flushBoundIdx = FLUSH_BOUND_IDX_NORMAL;
      }

      if (opts) {
        chunkSize = opts.chunkSize;
        if (!validateFiniteNumber(chunkSize, 'options.chunkSize')) {
          chunkSize = Z_DEFAULT_CHUNK;
        } else if (chunkSize < Z_MIN_CHUNK) {
          throw new ERR_OUT_OF_RANGE('options.chunkSize',
                                     `>= ${Z_MIN_CHUNK}`, chunkSize);
        }

        flush = checkRangesOrGetDefault(
          opts.flush, 'options.flush',
          FLUSH_BOUND[flushBoundIdx][0], FLUSH_BOUND[flushBoundIdx][1], flush);

        finishFlush = checkRangesOrGetDefault(
          opts.finishFlush, 'options.finishFlush',
          FLUSH_BOUND[flushBoundIdx][0], FLUSH_BOUND[flushBoundIdx][1],
          finishFlush);

        maxOutputLength = checkRangesOrGetDefault(
          opts.maxOutputLength, 'options.maxOutputLength',
          1, kMaxLength, kMaxLength);

        if (opts.rejectGarbageAfterEnd !== undefined) {
          validateBoolean(
            opts.rejectGarbageAfterEnd,
            'options.rejectGarbageAfterEnd',
          );
        }

        if (opts.encoding || opts.objectMode || opts.writableObjectMode ||
              opts.decodeStrings === false) {
          opts = { ...opts };
          opts.encoding = null;
          opts.objectMode = false;
          opts.writableObjectMode = false;
          opts.decodeStrings = true;
        }
      }

      Transform.call(this, { autoDestroy: true, ...opts });
      this[kError] = null;
      this.bytesWritten = 0;
      this._handle = handle;
      handle[owner_symbol] = this;
      handle.onerror = zlibOnError;
      this._outBuffer = Buffer.allocUnsafe(chunkSize);
      this._outOffset = 0;

      this._chunkSize = chunkSize;
      this._defaultFlushFlag = flush;
      this._finishFlushFlag = finishFlush;
      this._defaultFullFlushFlag = fullFlush;
      this._flushBoundIdx = flushBoundIdx;
      this._info = opts?.info;
      this._maxOutputLength = maxOutputLength;

      this._rejectGarbageAfterEnd = opts?.rejectGarbageAfterEnd === true;
    }
    Object.setPrototypeOf(ZlibBase.prototype, Transform.prototype);
    Object.setPrototypeOf(ZlibBase, Transform);

    Object.defineProperty(ZlibBase.prototype, '_closed', {
      __proto__: null,
      configurable: true,
      enumerable: true,
      get() {
        return !this._handle;
      },
    });

    ZlibBase.prototype.reset = function() {
      assert(this._handle, 'zlib binding closed');
      return this._handle.reset();
    };

    ZlibBase.prototype._flush = function(callback) {
      this._transform(new FastBuffer(), '', callback);
    };

    ZlibBase.prototype._final = function(callback) {
      callback();
    };

    const flushiness = [];
    const kFlushFlagList = [Z_NO_FLUSH, Z_BLOCK, Z_PARTIAL_FLUSH,
                            Z_SYNC_FLUSH, Z_FULL_FLUSH, Z_FINISH];
    for (let i = 0; i < kFlushFlagList.length; i++) {
      flushiness[kFlushFlagList[i]] = i;
    }

    function maxFlush(a, b) {
      return flushiness[a] > flushiness[b] ? a : b;
    }

    const kFlushBuffers = [];
    {
      const dummyArrayBuffer = new ArrayBuffer();
      for (const flushFlag of kFlushFlagList) {
        kFlushBuffers[flushFlag] = Buffer.from(dummyArrayBuffer);
        kFlushBuffers[flushFlag][kFlushFlag] = flushFlag;
      }
    }

    ZlibBase.prototype.flush = function(kind, callback) {
      if (typeof kind === 'function' || (kind === undefined && !callback)) {
        callback = kind;
        kind = this._defaultFullFlushFlag;
      }

      kind = checkRangesOrGetDefault(
        kind, 'kind',
        FLUSH_BOUND[this._flushBoundIdx][0], FLUSH_BOUND[this._flushBoundIdx][1],
        this._defaultFullFlushFlag);

      if (this.writableFinished) {
        if (callback)
          process.nextTick(callback);
      } else if (this.writableEnded) {
        if (callback)
          this.once('end', callback);
      } else {
        this.write(kFlushBuffers[kind], '', callback);
      }
    };

    ZlibBase.prototype.close = function(callback) {
      if (callback) finished(this, callback);
      this.destroy();
    };

    ZlibBase.prototype._destroy = function(err, callback) {
      _close(this);
      callback(err);
    };

    ZlibBase.prototype._transform = function(chunk, encoding, cb) {
      let flushFlag = this._defaultFlushFlag;
      if (typeof chunk[kFlushFlag] === 'number') {
        flushFlag = chunk[kFlushFlag];
      }

      if (this.writableEnded && this.writableLength === chunk.byteLength) {
        flushFlag = maxFlush(flushFlag, this._finishFlushFlag);
      }
      processChunk(this, chunk, flushFlag, cb);
    };

    ZlibBase.prototype._processChunk = function(chunk, flushFlag, cb) {
      if (typeof cb === 'function')
        processChunk(this, chunk, flushFlag, cb);
      else
        return processChunkSync(this, chunk, flushFlag);
    };

    function processChunkSync(self, chunk, flushFlag) {
      let availInBefore = chunk.byteLength;
      let availOutBefore = self._chunkSize - self._outOffset;
      let inOff = 0;
      let availOutAfter;
      let availInAfter;

      const buffers = [];
      let nread = 0;
      let inputRead = 0;
      const state = self._writeState;
      const handle = self._handle;
      let buffer = self._outBuffer;
      let offset = self._outOffset;
      const chunkSize = self._chunkSize;

      let error;
      self.on('error', function onError(er) {
        error = er;
      });

      while (true) {
        handle.writeSync(flushFlag,
                         chunk, // in
                         inOff, // in_off
                         availInBefore, // in_len
                         buffer, // out
                         offset, // out_off
                         availOutBefore); // out_len
        if (error)
          throw error;
        else if (self[kError])
          throw self[kError];

        availOutAfter = state[0];
        availInAfter = state[1];

        const inDelta = (availInBefore - availInAfter);
        inputRead += inDelta;

        const have = availOutBefore - availOutAfter;
        if (have > 0) {
          const out = buffer.slice(offset, offset + have);
          offset += have;
          buffers.push(out);
          nread += out.byteLength;

          if (nread > self._maxOutputLength) {
            _close(self);
            throw new ERR_BUFFER_TOO_LARGE(self._maxOutputLength);
          }

        } else {
          assert(have === 0, 'have should not go down');
        }

        if (availOutAfter === 0 || offset >= chunkSize) {
          availOutBefore = chunkSize;
          offset = 0;
          buffer = Buffer.allocUnsafe(chunkSize);
        }

        if (availOutAfter === 0) {
          inOff += inDelta;
          availInBefore = availInAfter;
        } else {
          break;
        }
      }

      if (availInAfter > 0 && self._rejectGarbageAfterEnd) {
        _close(self);
        throw new ERR_TRAILING_JUNK_AFTER_STREAM_END();
      }

      self.bytesWritten = inputRead;
      _close(self);

      if (nread === 0)
        return new FastBuffer();

      return (buffers.length === 1 ? buffers[0] : Buffer.concat(buffers, nread));
    }

    function processChunk(self, chunk, flushFlag, cb) {
      const handle = self._handle;
      if (!handle) return process.nextTick(cb);

      handle.buffer = chunk;
      handle.cb = cb;
      handle.availOutBefore = self._chunkSize - self._outOffset;
      handle.availInBefore = chunk.byteLength;
      handle.inOff = 0;
      handle.flushFlag = flushFlag;

      handle.write(flushFlag,
                   chunk, // in
                   0, // in_off
                   handle.availInBefore, // in_len
                   self._outBuffer, // out
                   self._outOffset, // out_off
                   handle.availOutBefore); // out_len
    }

    function processCallback() {
      const handle = this;
      const self = this[owner_symbol];
      const state = self._writeState;

      if (self.destroyed) {
        this.buffer = null;
        this.cb();
        return;
      }

      const availOutAfter = state[0];
      const availInAfter = state[1];

      const inDelta = handle.availInBefore - availInAfter;
      self.bytesWritten += inDelta;

      const have = handle.availOutBefore - availOutAfter;
      let streamBufferIsFull = false;
      if (have > 0) {
        const out = self._outBuffer.slice(self._outOffset, self._outOffset + have);
        self._outOffset += have;
        streamBufferIsFull = !self.push(out);
      } else {
        assert(have === 0, 'have should not go down');
      }

      if (self.destroyed) {
        this.cb();
        return;
      }

      if (availOutAfter === 0 || self._outOffset >= self._chunkSize) {
        handle.availOutBefore = self._chunkSize;
        self._outOffset = 0;
        self._outBuffer = Buffer.allocUnsafe(self._chunkSize);
      }

      if (availOutAfter === 0) {
        handle.inOff += inDelta;
        handle.availInBefore = availInAfter;

        if (!streamBufferIsFull) {
          this.write(handle.flushFlag,
                     this.buffer, // in
                     handle.inOff, // in_off
                     handle.availInBefore, // in_len
                     self._outBuffer, // out
                     self._outOffset, // out_off
                     self._chunkSize); // out_len
        } else {
          const oldRead = self._read;
          self._read = (n) => {
            self._read = oldRead;
            this.write(handle.flushFlag,
                       this.buffer, // in
                       handle.inOff, // in_off
                       handle.availInBefore, // in_len
                       self._outBuffer, // out
                       self._outOffset, // out_off
                       self._chunkSize); // out_len
            self._read(n);
          };
        }
        return;
      }

      if (availInAfter > 0) {

        if (self._rejectGarbageAfterEnd) {
          const err = new ERR_TRAILING_JUNK_AFTER_STREAM_END();
          self.destroy(err);
          this.cb(err);
          return;
        }

        self.push(null);
      }

      this.buffer = null;
      this.cb();
    }

    function _close(engine) {
      engine._handle?.close();
      engine._handle = null;
    }

    const zlibDefaultOpts = {
      flush: Z_NO_FLUSH,
      finishFlush: Z_FINISH,
      fullFlush: Z_FULL_FLUSH,
    };
    function Zlib(opts, mode) {
      let windowBits = Z_DEFAULT_WINDOWBITS;
      let level = Z_DEFAULT_COMPRESSION;
      let memLevel = Z_DEFAULT_MEMLEVEL;
      let strategy = Z_DEFAULT_STRATEGY;
      let dictionary;

      if (opts) {
        if ((opts.windowBits == null || opts.windowBits === 0) &&
            (mode === INFLATE ||
             mode === GUNZIP ||
             mode === UNZIP)) {
          windowBits = 0;
        } else {
          const min = Z_MIN_WINDOWBITS + (mode === GZIP ? 1 : 0);
          windowBits = checkRangesOrGetDefault(
            opts.windowBits, 'options.windowBits',
            min, Z_MAX_WINDOWBITS, Z_DEFAULT_WINDOWBITS);
        }

        level = checkRangesOrGetDefault(
          opts.level, 'options.level',
          Z_MIN_LEVEL, Z_MAX_LEVEL, Z_DEFAULT_COMPRESSION);

        memLevel = checkRangesOrGetDefault(
          opts.memLevel, 'options.memLevel',
          Z_MIN_MEMLEVEL, Z_MAX_MEMLEVEL, Z_DEFAULT_MEMLEVEL);

        strategy = checkRangesOrGetDefault(
          opts.strategy, 'options.strategy',
          Z_DEFAULT_STRATEGY, Z_FIXED, Z_DEFAULT_STRATEGY);

        dictionary = opts.dictionary;
        if (dictionary !== undefined && !isArrayBufferView(dictionary)) {
          if (isAnyArrayBuffer(dictionary)) {
            dictionary = Buffer.from(dictionary);
          } else {
            throw new ERR_INVALID_ARG_TYPE(
              'options.dictionary',
              ['Buffer', 'TypedArray', 'DataView', 'ArrayBuffer'],
              dictionary,
            );
          }
        }
      }

      const handle = new binding.Zlib(mode);
      this._writeState = new Uint32Array(2);
      handle.init(windowBits,
                  level,
                  memLevel,
                  strategy,
                  this._writeState,
                  processCallback,
                  dictionary,
                  opts?.rejectGarbageAfterEnd === true);

      ZlibBase.call(this, opts, mode, handle, zlibDefaultOpts);

      this._level = level;
      this._strategy = strategy;
      this._mode = mode;
    }
    Object.setPrototypeOf(Zlib.prototype, ZlibBase.prototype);
    Object.setPrototypeOf(Zlib, ZlibBase);

    function paramsAfterFlushCallback(level, strategy, callback) {
      assert(this._handle, 'zlib binding closed');
      this._handle.params(level, strategy);
      if (!this.destroyed) {
        this._level = level;
        this._strategy = strategy;
        if (callback) callback();
      }
    }

    Zlib.prototype.params = function params(level, strategy, callback) {
      checkRangesOrGetDefault(level, 'level', Z_MIN_LEVEL, Z_MAX_LEVEL);
      checkRangesOrGetDefault(strategy, 'strategy', Z_DEFAULT_STRATEGY, Z_FIXED);

      if (this._level !== level || this._strategy !== strategy) {
        this.flush(
          Z_SYNC_FLUSH,
          paramsAfterFlushCallback.bind(this, level, strategy, callback),
        );
      } else {
        process.nextTick(callback);
      }
    };

    function Deflate(opts) {
      if (!(this instanceof Deflate)) {
        return deprecateInstantiation(Deflate, 'DEP0184', opts);
      }
      Zlib.call(this, opts, DEFLATE);
    }
    Object.setPrototypeOf(Deflate.prototype, Zlib.prototype);
    Object.setPrototypeOf(Deflate, Zlib);

    function Inflate(opts) {
      if (!(this instanceof Inflate)) {
        return deprecateInstantiation(Inflate, 'DEP0184', opts);
      }
      Zlib.call(this, opts, INFLATE);
    }
    Object.setPrototypeOf(Inflate.prototype, Zlib.prototype);
    Object.setPrototypeOf(Inflate, Zlib);

    function Gzip(opts) {
      if (!(this instanceof Gzip)) {
        return deprecateInstantiation(Gzip, 'DEP0184', opts);
      }
      Zlib.call(this, opts, GZIP);
    }
    Object.setPrototypeOf(Gzip.prototype, Zlib.prototype);
    Object.setPrototypeOf(Gzip, Zlib);

    function Gunzip(opts) {
      if (!(this instanceof Gunzip)) {
        return deprecateInstantiation(Gunzip, 'DEP0184', opts);
      }
      Zlib.call(this, opts, GUNZIP);
    }
    Object.setPrototypeOf(Gunzip.prototype, Zlib.prototype);
    Object.setPrototypeOf(Gunzip, Zlib);

    function DeflateRaw(opts) {
      if (opts && opts.windowBits === 8) opts.windowBits = 9;
      if (!(this instanceof DeflateRaw)) {
        return deprecateInstantiation(DeflateRaw, 'DEP0184', opts);
      }
      Zlib.call(this, opts, DEFLATERAW);
    }
    Object.setPrototypeOf(DeflateRaw.prototype, Zlib.prototype);
    Object.setPrototypeOf(DeflateRaw, Zlib);

    function InflateRaw(opts) {
      if (!(this instanceof InflateRaw)) {
        return deprecateInstantiation(InflateRaw, 'DEP0184', opts);
      }
      Zlib.call(this, opts, INFLATERAW);
    }
    Object.setPrototypeOf(InflateRaw.prototype, Zlib.prototype);
    Object.setPrototypeOf(InflateRaw, Zlib);

    function Unzip(opts) {
      if (!(this instanceof Unzip)) {
        return deprecateInstantiation(Unzip, 'DEP0184', opts);
      }
      Zlib.call(this, opts, UNZIP);
    }
    Object.setPrototypeOf(Unzip.prototype, Zlib.prototype);
    Object.setPrototypeOf(Unzip, Zlib);

    function createConvenienceMethod(ctor, sync, prepareOpts) {
      if (sync) {
        return function syncBufferWrapper(buffer, opts) {
          return zlibBufferSync(new ctor(opts), buffer);
        };
      }
      return function asyncBufferWrapper(buffer, opts, callback) {
        if (typeof opts === 'function') {
          callback = opts;
          opts = {};
        }
        if (prepareOpts !== undefined) {
          opts = prepareOpts(buffer, opts);
        }
        return zlibBuffer(new ctor(opts), buffer, callback);
      };
    }

    function withPledgedSrcSize(buffer, opts) {
      if (opts?.pledgedSrcSize !== undefined) {
        return opts;
      }
      let pledgedSrcSize;
      if (typeof buffer === 'string') {
        const encoding = opts?.defaultEncoding;
        if (encoding != null && encoding !== 'utf8' && encoding !== 'utf-8') {
          return opts;
        }
        pledgedSrcSize = Buffer.byteLength(buffer);
      } else if (isArrayBufferView(buffer) || isAnyArrayBuffer(buffer)) {
        pledgedSrcSize = buffer.byteLength;
      } else {
        return opts;
      }
      return { __proto__: null, ...opts, pledgedSrcSize };
    }

    const kMaxBrotliParam = Math.max(
      ...Object.entries(constants)
        .map(({ 0: key, 1: value }) => (key.startsWith('BROTLI_PARAM_') ? value : 0)),
    );
    const brotliInitParamsArray = new Uint32Array(kMaxBrotliParam + 1);

    const brotliDefaultOpts = {
      flush: BROTLI_OPERATION_PROCESS,
      finishFlush: BROTLI_OPERATION_FINISH,
      fullFlush: BROTLI_OPERATION_FLUSH,
    };
    function Brotli(opts, mode) {
      assert(mode === BROTLI_DECODE || mode === BROTLI_ENCODE);

      brotliInitParamsArray.fill(-1);
      if (opts?.params) {
        Object.keys(opts.params).forEach((origKey) => {
          const key = +origKey;
          if (Number.isNaN(key) || key < 0 || key > kMaxBrotliParam ||
              (brotliInitParamsArray[key] | 0) !== -1) {
            throw new ERR_BROTLI_INVALID_PARAM(origKey);
          }

          const value = opts.params[origKey];
          if (typeof value !== 'number' && typeof value !== 'boolean') {
            throw new ERR_INVALID_ARG_TYPE('options.params[key]',
                                           'number', opts.params[origKey]);
          }
          brotliInitParamsArray[key] = value;
        });
      }

      let dictionary = opts?.dictionary;
      if (dictionary !== undefined && !isArrayBufferView(dictionary)) {
        if (isAnyArrayBuffer(dictionary)) {
          dictionary = Buffer.from(dictionary);
        } else {
          throw new ERR_INVALID_ARG_TYPE(
            'options.dictionary',
            ['Buffer', 'TypedArray', 'DataView', 'ArrayBuffer'],
            dictionary,
          );
        }
      }

      const handle = mode === BROTLI_DECODE ?
        new binding.BrotliDecoder(mode) : new binding.BrotliEncoder(mode);

      this._writeState = new Uint32Array(2);
      handle.init(
        brotliInitParamsArray,
        this._writeState,
        processCallback,
        dictionary,
      );

      ZlibBase.call(this, opts, mode, handle, brotliDefaultOpts);
    }
    Object.setPrototypeOf(Brotli.prototype, Zlib.prototype);
    Object.setPrototypeOf(Brotli, Zlib);

    function BrotliCompress(opts) {
      if (!(this instanceof BrotliCompress)) {
        return deprecateInstantiation(BrotliCompress, 'DEP0184', opts);
      }
      Brotli.call(this, opts, BROTLI_ENCODE);
    }
    Object.setPrototypeOf(BrotliCompress.prototype, Brotli.prototype);
    Object.setPrototypeOf(BrotliCompress, Brotli);

    function BrotliDecompress(opts) {
      if (!(this instanceof BrotliDecompress)) {
        return deprecateInstantiation(BrotliDecompress, 'DEP0184', opts);
      }
      Brotli.call(this, opts, BROTLI_DECODE);
    }
    Object.setPrototypeOf(BrotliDecompress.prototype, Brotli.prototype);
    Object.setPrototypeOf(BrotliDecompress, Brotli);

    const zstdDefaultOpts = {
      flush: ZSTD_e_continue,
      finishFlush: ZSTD_e_end,
      fullFlush: ZSTD_e_flush,
    };
    class Zstd extends ZlibBase {
      constructor(opts, mode, initParamsArray, maxParam) {
        assert(mode === ZSTD_COMPRESS || mode === ZSTD_DECOMPRESS);

        initParamsArray.fill(-1);
        if (opts?.params) {
          Object.keys(opts.params).forEach((origKey) => {
            const key = +origKey;
            if (Number.isNaN(key) || key < 0 || key > maxParam ||
                (initParamsArray[key] | 0) !== -1) {
              throw new ERR_ZSTD_INVALID_PARAM(origKey);
            }

            const value = opts.params[origKey];
            if (typeof value !== 'number' && typeof value !== 'boolean') {
              throw new ERR_INVALID_ARG_TYPE('options.params[key]',
                                             'number', opts.params[origKey]);
            }
            initParamsArray[key] = value;
          });
        }

        let dictionary = opts?.dictionary;
        if (dictionary !== undefined && !isArrayBufferView(dictionary)) {
          if (isAnyArrayBuffer(dictionary)) {
            dictionary = new Uint8Array(dictionary);
          } else {
            throw new ERR_INVALID_ARG_TYPE(
              'options.dictionary',
              ['Buffer', 'TypedArray', 'DataView', 'ArrayBuffer'],
              dictionary,
            );
          }
        }

        const handle = mode === ZSTD_COMPRESS ?
          new binding.ZstdCompress() : new binding.ZstdDecompress();

        const pledgedSrcSize = opts?.pledgedSrcSize;

        const writeState = new Uint32Array(2);

        handle.init(
          initParamsArray,
          pledgedSrcSize,
          writeState,
          processCallback,
          dictionary,
          opts?.rejectGarbageAfterEnd === true,
        );

        super(opts, mode, handle, zstdDefaultOpts);
        this._writeState = writeState;
      }
    }

    const kMaxZstdCParam = Math.max(...Object.keys(constants).map(
      (key) => (key.startsWith('ZSTD_c_') ?
        constants[key] :
        0),
    ));

    const zstdInitCParamsArray = new Uint32Array(kMaxZstdCParam + 1);

    class ZstdCompress extends Zstd {
      constructor(opts) {
        super(opts, ZSTD_COMPRESS, zstdInitCParamsArray, kMaxZstdCParam);
      }
    }

    const kMaxZstdDParam = Math.max(...Object.keys(constants).map(
      (key) => (key.startsWith('ZSTD_d_') ?
        constants[key] :
        0),
    ));

    const zstdInitDParamsArray = new Uint32Array(kMaxZstdDParam + 1);

    class ZstdDecompress extends Zstd {
      constructor(opts) {
        super(opts, ZSTD_DECOMPRESS, zstdInitDParamsArray, kMaxZstdDParam);
      }
    }

    function createProperty(ctor) {
      return {
        __proto__: null,
        configurable: true,
        enumerable: true,
        value: assignFunctionName(`create${ctor.name}`, function(options) {
          return new ctor(options);
        }),
      };
    }

    function emitZipExperimentalWarning() {
      emitExperimentalWarning('The zlib ZIP archive API');
    }

    function experimentalZipFunction(fn, thisArg) {
      return function(...args) {
        emitZipExperimentalWarning();
        return Reflect.apply(fn, thisArg, args);
      };
    }

    function experimentalZipProperty(value) {
      return { __proto__: null, configurable: true, enumerable: true, value };
    }

    class ExperimentalZipEntry extends ZipEntry {
      static [SymbolHasInstance](instance) { return instance instanceof ZipEntry; }
    }

    class ExperimentalZipFile extends ZipFile {
      static [SymbolHasInstance](instance) { return instance instanceof ZipFile; }
    }

    class ExperimentalZipBuffer extends ZipBuffer {
      static [SymbolHasInstance](instance) { return instance instanceof ZipBuffer; }
      constructor(buffer) { emitZipExperimentalWarning(); super(buffer); }
    }

    for (const { 0: Wrapper, 1: Raw, 2: factories } of [
      [ExperimentalZipEntry, ZipEntry, ['read', 'create', 'createSync', 'createStream', 'createSymlink']],
      [ExperimentalZipFile, ZipFile, ['open', 'openSync']],
      [ExperimentalZipBuffer, ZipBuffer, []],
    ]) {
      for (const name of factories) {
        Object.defineProperty(Wrapper, name, {
          __proto__: null,
          configurable: true,
          writable: true,
          value: experimentalZipFunction(Raw[name], Raw),
        });
      }
      Object.defineProperty(Wrapper, 'name', { __proto__: null, value: Raw.name });
    }

    function crc32(data, value = 0) {
      if (typeof data !== 'string' && !isArrayBufferView(data)) {
        throw new ERR_INVALID_ARG_TYPE('data', ['Buffer', 'TypedArray', 'DataView', 'string'], data);
      }
      validateUint32(value, 'value');
      value += 0;
      return crc32Native(data, value);
    }

    Object.defineProperty(binding.Zlib.prototype, 'jsref', {
      __proto__: null,
      get() { return this[owner_symbol]; },
      set(v) { return this[owner_symbol] = v; },
    });

    module.exports = {
      crc32,
      Deflate,
      Inflate,
      Gzip,
      Gunzip,
      DeflateRaw,
      InflateRaw,
      Unzip,
      BrotliCompress,
      BrotliDecompress,
      ZstdCompress,
      ZstdDecompress,

      deflate: createConvenienceMethod(Deflate, false),
      deflateSync: createConvenienceMethod(Deflate, true),
      gzip: createConvenienceMethod(Gzip, false),
      gzipSync: createConvenienceMethod(Gzip, true),
      deflateRaw: createConvenienceMethod(DeflateRaw, false),
      deflateRawSync: createConvenienceMethod(DeflateRaw, true),
      unzip: createConvenienceMethod(Unzip, false),
      unzipSync: createConvenienceMethod(Unzip, true),
      inflate: createConvenienceMethod(Inflate, false),
      inflateSync: createConvenienceMethod(Inflate, true),
      gunzip: createConvenienceMethod(Gunzip, false),
      gunzipSync: createConvenienceMethod(Gunzip, true),
      inflateRaw: createConvenienceMethod(InflateRaw, false),
      inflateRawSync: createConvenienceMethod(InflateRaw, true),
      brotliCompress: createConvenienceMethod(BrotliCompress, false),
      brotliCompressSync: createConvenienceMethod(BrotliCompress, true),
      brotliDecompress: createConvenienceMethod(BrotliDecompress, false),
      brotliDecompressSync: createConvenienceMethod(BrotliDecompress, true),
      zstdCompress: createConvenienceMethod(ZstdCompress, false, withPledgedSrcSize),
      zstdCompressSync: createConvenienceMethod(ZstdCompress, true),
      zstdDecompress: createConvenienceMethod(ZstdDecompress, false),
      zstdDecompressSync: createConvenienceMethod(ZstdDecompress, true),
    };

    Object.defineProperties(module.exports, {
      createDeflate: createProperty(Deflate),
      createInflate: createProperty(Inflate),
      createDeflateRaw: createProperty(DeflateRaw),
      createInflateRaw: createProperty(InflateRaw),
      createGzip: createProperty(Gzip),
      createGunzip: createProperty(Gunzip),
      createUnzip: createProperty(Unzip),
      createBrotliCompress: createProperty(BrotliCompress),
      createBrotliDecompress: createProperty(BrotliDecompress),
      createZstdCompress: createProperty(ZstdCompress),
      createZstdDecompress: createProperty(ZstdDecompress),
      constants: {
        __proto__: null,
        configurable: false,
        enumerable: true,
        value: constants,
      },
      codes: {
        __proto__: null,
        enumerable: true,
        writable: false,
        value: Object.freeze(codes),
      },

      ZipEntry: experimentalZipProperty(ExperimentalZipEntry),
      ZipFile: experimentalZipProperty(ExperimentalZipFile),
      ZipBuffer: experimentalZipProperty(ExperimentalZipBuffer),
      createZipArchive: experimentalZipProperty(experimentalZipFunction(createZipArchive)),
      createZipArchiveSync: experimentalZipProperty(experimentalZipFunction(createZipArchiveSync)),
      zipFiles: experimentalZipProperty(experimentalZipFunction(zipFiles)),
      getMaxZipContentSize: experimentalZipProperty(experimentalZipFunction(getMaxZipContentSize)),
      setMaxZipContentSize: experimentalZipProperty(experimentalZipFunction(setMaxZipContentSize)),
    });

    for (const { 0: key, 1: value } of Object.entries(constants)) {
      if (key.startsWith('BROTLI')) continue;
      Object.defineProperty(module.exports, key, {
        __proto__: null,
        enumerable: false,
        value,
        writable: false,
      });
    }
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
    if (shims.fallback !== undefined) return shims.fallback(name);
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

module.exports = load("zlib");
