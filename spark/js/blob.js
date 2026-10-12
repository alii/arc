"use strict";

// node's Blob and File: lib/internal/blob.js and lib/internal/file.js. The
// store they keep their bytes in, c++ in node, is written out below

const internal = require("spark:internal");
// the web streams hold the webidl helpers these files use
require("node:stream/web");

internal.addLoader(own);

function notHere(what) {
  const { codes } = internal.load("internal/errors");
  return new codes.ERR_METHOD_NOT_IMPLEMENTED(what);
}

function joined(parts) {
  let size = 0;
  for (const part of parts) size += part.byteLength;
  const all = new Uint8Array(size);
  let at = 0;
  for (const part of parts) {
    all.set(ArrayBuffer.isView(part) ? part : new Uint8Array(part), at);
    at += part.byteLength;
  }
  return all;
}

// a blob's bytes, held whole. Node reads them out through a reader that
// gives a piece and then says it is done
class Stored {
  constructor(bytes) {
    this.bytes = bytes;
  }

  getReader() {
    let given = false;
    const bytes = this.bytes;
    return {
      pull(callback) {
        if (given || bytes.byteLength === 0) {
          callback(0);
          return;
        }
        given = true;
        callback(1, bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.byteLength));
      },
      setWakeup() {},
    };
  }

  slice(start, end) {
    return new Stored(this.bytes.subarray(start, end));
  }
}

const blob = {
  createBlob(sources) {
    return new Stored(joined(sources.map((source) => (source instanceof Stored ? source.bytes : source))));
  },
  createBlobFromFilePath() {
    throw notHere("a blob backed by a file");
  },
  concat(buffers) {
    return joined(buffers).buffer;
  },
  getDataObject() {
    return undefined;
  },
};

const shims = {
  process: internal.process,
  primordials: internal.primordials,
  internalBinding(name) {
    if (name === "blob") return blob;
    if (name === "buffer") {
      return { ...internal.internalBinding("buffer"), kMaxLength: require("node:buffer").kMaxLength };
    }
    return internal.internalBinding(name);
  },
  fallback: internal.find,
  timers: () => require("node:timers"),
  "internal/encoding": () => {
    const { TextEncoder, TextDecoder } = require("spark:encoding");
    return { TextEncoder, TextDecoder, getUtf8Decoder: () => new TextDecoder() };
  },
  "internal/url": () => require("node:url"),
  "internal/worker/js_transferable": () => ({
    kClone: Symbol("kClone"),
    kDeserialize: Symbol("kDeserialize"),
    kTransfer: Symbol("kTransfer"),
    kTransferList: Symbol("kTransferList"),
    markTransferMode() {},
  }),
};

const files = {
  "internal/blob": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypePush,
      MathMax,
      MathMin,
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectSetPrototypeOf,
      PromisePrototypeThen,
      PromiseReject,
      PromiseWithResolvers,
      RegExpPrototypeExec,
      RegExpPrototypeSymbolReplace,
      StringPrototypeSplit,
      StringPrototypeToLowerCase,
      Symbol,
      SymbolToStringTag,
      Uint8Array,
    } = primordials;

    const {
      createBlob: _createBlob,
      createBlobFromFilePath: _createBlobFromFilePath,
      concat,
      getDataObject,
    } = internalBinding('blob');
    const {
      kMaxLength,
    } = internalBinding('buffer');

    const {
      getUtf8Decoder,
      TextEncoder,
    } = require('internal/encoding');
    const { URL } = require('internal/url');

    const {
      markTransferMode,
      kClone,
      kDeserialize,
    } = require('internal/worker/js_transferable');

    const {
      isAnyArrayBuffer,
      isArrayBufferView,
    } = require('internal/util/types');

    const {
      customInspectSymbol: kInspect,
      kEmptyObject,
      kEnumerableProperty,
      lazyDOMException,
    } = require('internal/util');
    const { inspect } = require('internal/util/inspect');
    const {
      converters,
      createSequenceConverter,
    } = require('internal/webidl');

    const {
      codes: {
        ERR_BUFFER_TOO_LARGE,
        ERR_INVALID_ARG_VALUE,
        ERR_INVALID_STATE,
        ERR_INVALID_THIS,
      },
    } = require('internal/errors');

    const {
      validateDictionary,
    } = require('internal/validators');

    const {
      setImmediate,
    } = require('timers');

    const { queueMicrotask } = require('internal/process/task_queues');

    const kHandle = Symbol('kHandle');
    const kType = Symbol('kType');
    const kLength = Symbol('kLength');
    const kNotCloneable = Symbol('kNotCloneable');

    const disallowedTypeCharacters = /[^\u{0020}-\u{007E}]/u;

    let ReadableStream;
    let TextDecoderStream;

    const enc = new TextEncoder();

    function lazyReadableStream(options) {
      ReadableStream ??=
        require('internal/webstreams/readablestream').ReadableStream;
      return new ReadableStream(options);
    }

    function lazyTextDecoderStream() {
      TextDecoderStream ??=
        require('internal/webstreams/encoding').TextDecoderStream;
      return new TextDecoderStream();
    }

    const { EOL } = require('internal/constants');

    function isBlob(object) {
      return object?.[kHandle] !== undefined;
    }

    function getSource(source, endings) {
      if (isBlob(source))
        return [source.size, source[kHandle]];

      if (isAnyArrayBuffer(source)) {
        source = new Uint8Array(source);
      } else if (!isArrayBufferView(source)) {
        if (endings === 'native')
          source = RegExpPrototypeSymbolReplace(/\r\n|\r|\n/g, source, EOL);
        source = enc.encode(source);
      }

      const { buffer, byteOffset, byteLength } = source;
      const slice = buffer.slice(byteOffset, byteOffset + byteLength);
      return [byteLength, new Uint8Array(slice)];
    }

    const sourcesConverter = createSequenceConverter((source, opts = kEmptyObject) => {
      if (isBlob(source) || isAnyArrayBuffer(source) || isArrayBufferView(source)) {
        return source;
      }
      return converters.DOMString(source, opts);
    });

    class Blob {
      
        constructor(sources = [], options) {
        markTransferMode(this, true, false);

        const sources_ = sourcesConverter(sources);

        validateDictionary(options, 'options');
        let {
          endings = 'transparent',
          type = '',
        } = options ?? kEmptyObject;

        endings = `${endings}`;
        if (endings !== 'transparent' && endings !== 'native')
          throw new ERR_INVALID_ARG_VALUE('options.endings', endings);

        let length = 0;
        for (let i = 0; i < sources_.length; ++i) {
          const { 0: len, 1: src } = getSource(sources_[i], endings);
          length += len;
          sources_[i] = src;
        }

        if (length > kMaxLength)
          throw new ERR_BUFFER_TOO_LARGE(kMaxLength);

        this[kHandle] = _createBlob(sources_, length);
        this[kLength] = length;

        type = `${type}`;
        this[kType] = disallowedTypeCharacters.exec(type) !== null ?
          '' : type.toLowerCase();
      }

      [kInspect](depth, options) {
        if (depth < 0)
          return this;

        const opts = {
          ...options,
          depth: options.depth == null ? null : options.depth - 1,
        };

        return `Blob ${inspect({
          size: this.size,
          type: this.type,
        }, opts)}`;
      }

      [kClone]() {
        if (this[kNotCloneable]) {
          throw new ERR_INVALID_STATE.TypeError('File-backed Blobs are not cloneable');
        }
        const handle = this[kHandle];
        const type = this[kType];
        const length = this[kLength];
        return {
          data: { handle, type, length },
          deserializeInfo: 'internal/blob:Blob',
        };
      }

      [kDeserialize]({ handle, type, length }) {
        this[kHandle] = handle;
        this[kType] = type;
        this[kLength] = length;
      }

        get type() {
        if (!isBlob(this))
          throw new ERR_INVALID_THIS('Blob');
        return this[kType];
      }

        get size() {
        if (!isBlob(this))
          throw new ERR_INVALID_THIS('Blob');
        return this[kLength];
      }

        slice(start = 0, end = this[kLength], contentType = '') {
        if (!isBlob(this))
          throw new ERR_INVALID_THIS('Blob');

        start = converters['long long'](
          start,
          { __proto__: null, context: 'start', clamp: true },
        );
        end = converters['long long'](
          end,
          { __proto__: null, context: 'end', clamp: true },
        );

        if (start < 0) {
          start = Math.max(this[kLength] + start, 0);
        } else {
          start = Math.min(start, this[kLength]);
        }

        if (end < 0) {
          end = Math.max(this[kLength] + end, 0);
        } else {
          end = Math.min(end, this[kLength]);
        }

        contentType = `${contentType}`;
        if (disallowedTypeCharacters.exec(contentType) !== null) {
          contentType = '';
        } else {
          contentType = contentType.toLowerCase();
        }

        const span = Math.max(end - start, 0);

        return createBlob(
          this[kHandle].slice(start, start + span),
          span,
          contentType);
      }

        arrayBuffer() {
        if (!isBlob(this))
          return Promise.reject(new ERR_INVALID_THIS('Blob'));

        return arrayBuffer(this);
      }

        text() {
        if (!isBlob(this))
          return Promise.reject(new ERR_INVALID_THIS('Blob'));

        const dec = getUtf8Decoder();

        return PromisePrototypeThen(
          arrayBuffer(this),
          (buffer) => dec.decode(buffer));
      }

        bytes() {
        if (!isBlob(this))
          return Promise.reject(new ERR_INVALID_THIS('Blob'));

        return PromisePrototypeThen(
          arrayBuffer(this),
          (buffer) => new Uint8Array(buffer));
      }

        stream() {
        if (!isBlob(this))
          throw new ERR_INVALID_THIS('Blob');
        return createBlobReaderStream(this[kHandle].getReader());
      }

        textStream() {
        if (!isBlob(this))
          throw new ERR_INVALID_THIS('Blob');
        const stream = createBlobReaderStream(this[kHandle].getReader());
        const decoder = lazyTextDecoderStream();
        return stream.pipeThrough(decoder);
      }
    }

    function TransferableBlob(handle, length, type = '') {
      Object.setPrototypeOf(this, Blob.prototype);
      markTransferMode(this, true, false);
      this[kHandle] = handle;
      this[kType] = type;
      this[kLength] = length;
    }

    Object.setPrototypeOf(TransferableBlob.prototype, Blob.prototype);
    Object.setPrototypeOf(TransferableBlob, Blob);

    function createBlob(handle, length, type = '') {
      const transferredBlob = new TransferableBlob(handle, length, type);

      transferredBlob.constructor = Blob;

      return transferredBlob;
    }

    Object.defineProperty(Blob.prototype, SymbolToStringTag, {
      __proto__: null,
      configurable: true,
      value: 'Blob',
    });

    Object.defineProperties(Blob.prototype, {
      size: kEnumerableProperty,
      type: kEnumerableProperty,
      slice: kEnumerableProperty,
      stream: kEnumerableProperty,
      textStream: kEnumerableProperty,
      text: kEnumerableProperty,
      arrayBuffer: kEnumerableProperty,
      bytes: kEnumerableProperty,
    });

    function resolveObjectURL(url) {
      url = `${url}`;
      try {
        const parsed = new URL(url);

        const split = parsed.pathname.split(':', 3);

        if (split.length !== 2)
          return;

        const {
          0: base,
          1: id,
        } = split;

        if (base !== 'nodedata')
          return;

        const ret = getDataObject(id);

        if (ret === undefined)
          return;

        const {
          0: handle,
          1: length,
          2: type,
        } = ret;

        if (handle !== undefined)
          return createBlob(handle, length, type);
      } catch {
      }
    }

    function createBlobFromFilePath(path, options) {
      const maybeBlob = _createBlobFromFilePath(path);
      if (maybeBlob === undefined) {
        return lazyDOMException('The blob could not be read', 'NotReadableError');
      }
      const { 0: blob, 1: length } = maybeBlob;
      const res = createBlob(blob, length, options?.type);
      res[kNotCloneable] = true;
      return res;
    }

    function arrayBuffer(blob) {
      const { promise, resolve, reject } = PromiseWithResolvers();
      const reader = blob[kHandle].getReader();
      const buffers = [];
      const readNext = () => {
        reader.pull((status, buffer) => {
          if (status === 0) {
            resolve(concat(buffers));
            return;
          } else if (status < 0) {
            const error = lazyDOMException('The blob could not be read', 'NotReadableError');
            reject(error);
            return;
          }
          if (buffer !== undefined)
            buffers.push(buffer);
          queueMicrotask(() => readNext());
        });
      };
      readNext();
      return promise;
    }

    function getBlobDataSync(blob) {
      const reader = blob[kHandle].getReader();
      const buffers = [];
      let result;
      let ended = false;
      while (!ended) {
        let sync = false;
        reader.pull((status, buffer) => {
          sync = true;
          if (status === 0) {
            result = concat(buffers);
            ended = true;
            return;
          } else if (status < 0) {
            ended = true;
            return;
          }
          if (buffer !== undefined)
            buffers.push(buffer);
        });
        if (!sync) {
          break;
        }
      }
      return result;
    }

    function createBlobReaderStream(reader) {
      return new lazyReadableStream({
        type: 'bytes',
        start(c) {
          this.pendingPulls = [];
          let immediate;
          this.wakeup = () => {
            if (this.pendingPulls.length > 0) {
              immediate ??= setImmediate(() => {
                immediate = undefined;
                this.readNext(c);
              });
            }
          };
        },
        pull(c) {
          const { promise, resolve, reject } = PromiseWithResolvers();
          if (this.pendingPulls.length === 0) {
            reader.setWakeup(this.wakeup);
          }
          this.pendingPulls.push({ resolve, reject });
          this.readNext(c);
          return promise;
        },
        clearWakeupIfIdle() {
          if (this.pendingPulls.length === 0) {
            reader.setWakeup(undefined);
          }
        },
        readNext(c) {
          reader.pull((status, buffer) => {
            if (this.pendingPulls.length === 0) {
              reader.setWakeup(undefined);
              return;
            }
            if (status === 0) {
              reader.setWakeup(undefined);
              c.close();
              c.byobRequest?.respond(0);
              const pending = this.pendingPulls.shift();
              pending.resolve();
              return;
            } else if (status < 0) {
              reader.setWakeup(undefined);
              const error =
                lazyDOMException('The blob could not be read',
                                 'NotReadableError');
              const pending = this.pendingPulls.shift();
              c.error(error);
              pending.reject(error);
              return;
            } else if (status === 2) {
              return;
            }
            if (buffer !== undefined && buffer.byteLength !== 0) {
              c.enqueue(new Uint8Array(buffer));
            }
            setImmediate(() => {
              if (c.desiredSize < 0) {
                if (this.pendingPulls.length !== 0) {
                  const pending = this.pendingPulls.shift();
                  pending.resolve();
                  this.clearWakeupIfIdle();
                }
                return;
              }
              this.readNext(c);
            });
          });
        },
        cancel(reason) {
          reader.setWakeup(undefined);
          for (const pending of this.pendingPulls) {
            pending.reject(reason);
          }
          this.pendingPulls = [];
        },
      }, { highWaterMark: 0 });
    }

    const kMaxBatchChunks = 16;

    const kDefaultMaxBatchBytes = 65536;

    async function* createBlobReaderIterable(reader, options = kEmptyObject) {
      const { maxBatchBytes = kDefaultMaxBatchBytes } = options;
      let wakeup = PromiseWithResolvers();
      let immediate;
      reader.setWakeup(() => {
        immediate ??= setImmediate(() => {
          immediate = undefined;
          wakeup.resolve?.();
        });
      });

      try {
        while (true) {
          const batch = [];
          let batchBytes = 0;
          let blocked = false;
          let eos = false;
          let error = null;

          while (true) {
            let pullResult;
            reader.pull((status, buffer) => {
              pullResult = { status, buffer };
            });

            if (pullResult.status === 0) {
              eos = true;
              break;
            }
            if (pullResult.status < 0) {
              error = new ERR_INVALID_STATE('The reader is not readable');
              break;
            }
            if (pullResult.status === 2) {
              blocked = true;
              break;
            }
            const chunk = new Uint8Array(pullResult.buffer);
            batch.push(chunk);
            batchBytes += chunk.byteLength;
            if (batchBytes >= maxBatchBytes || batch.length >= kMaxBatchChunks) {
              break;
            }
          }

          if (batch.length > 0) {
            yield batch;
          }

          if (eos) return;
          if (error) throw error;

          if (blocked) {
            await wakeup.promise;
            wakeup = PromiseWithResolvers();
          }
        }
      } finally {
        reader.setWakeup(undefined);
      }
    }

    module.exports = {
      Blob,
      createBlob,
      createBlobFromFilePath,
      createBlobReaderIterable,
      createBlobReaderStream,
      getBlobDataSync,
      isBlob,
      kHandle,
      kType,
      resolveObjectURL,
      TransferableBlob,
    };
  },
  "internal/file": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      DateNow,
      FunctionPrototypeCall,
      NumberIsNaN,
      ObjectDefineProperties,
      ObjectSetPrototypeOf,
      StringPrototypeToWellFormed,
      Symbol,
      SymbolToStringTag,
    } = primordials;

    const {
      Blob,
      TransferableBlob,
    } = require('internal/blob');

    const {
      customInspectSymbol: kInspect,
      kEnumerableProperty,
      kEmptyObject,
    } = require('internal/util');

    const {
      codes: {
        ERR_INVALID_THIS,
        ERR_MISSING_ARGS,
      },
    } = require('internal/errors');

    const {
      inspect,
    } = require('internal/util/inspect');

    const {
      kClone,
      kDeserialize,
    } = require('internal/worker/js_transferable');

    const kState = Symbol('state');

    function isFile(object) {
      return object?.[kState] !== undefined;
    }

    class FileState {
      name;
      lastModified;

        constructor(name, lastModified) {
        this.name = name;
        this.lastModified = lastModified;
      }
    }

    class File extends Blob {
      constructor(fileBits, fileName, options = kEmptyObject) {
        if (arguments.length < 2) {
          throw new ERR_MISSING_ARGS('fileBits', 'fileName');
        }

        super(fileBits, options);

        let { lastModified } = options ?? kEmptyObject;

        if (lastModified !== undefined) {
          lastModified = +lastModified;

          if (Number.isNaN(lastModified)) {
            lastModified = 0;
          }
        } else {
          lastModified = Date.now();
        }

        this[kState] = new FileState(`${fileName}`.toWellFormed(), lastModified);
      }

      get name() {
        if (!isFile(this))
          throw new ERR_INVALID_THIS('File');

        return this[kState].name;
      }

      get lastModified() {
        if (!isFile(this))
          throw new ERR_INVALID_THIS('File');

        return this[kState].lastModified;
      }

      [kInspect](depth, options) {
        if (depth < 0) {
          return this;
        }

        const opts = {
          ...options,
          depth: options.depth == null ? null : options.depth - 1,
        };

        return `File ${inspect({
          size: this.size,
          type: this.type,
          name: this[kState].name,
          lastModified: this[kState].lastModified,
        }, opts)}`;
      }

      [kClone]() {
        return {
          data: { ...super[kClone]().data, ...this[kState] },
          deserializeInfo: 'internal/file:TransferableFile',
        };
      }

      [kDeserialize](data) {
        super[kDeserialize](data);

        this[kState] = new FileState(data.name, data.lastModified);
      }
    }

    function TransferableFile(handle, length, type = '') {
      FunctionPrototypeCall(TransferableBlob, this, handle, length, type);
      Object.setPrototypeOf(this, File.prototype);
    }

    Object.setPrototypeOf(TransferableFile.prototype, File.prototype);
    Object.setPrototypeOf(TransferableFile, File);

    Object.defineProperties(File.prototype, {
      name: kEnumerableProperty,
      lastModified: kEnumerableProperty,
      [SymbolToStringTag]: {
        __proto__: null,
        configurable: true,
        value: 'File',
      },
    });

    module.exports = {
      File,
      TransferableFile,
    };
  },
  "blob": function (module, exports, require, process, primordials, internalBinding) {
    "use strict";

    const { Blob, kHandle } = require("internal/blob");
    const { File } = require("internal/file");

    module.exports = { Blob, File };

    // for fetch, which sends a blob's bytes without waiting on a stream
    Object.defineProperty(module.exports, "bytesOf", {
      value: (blob) => blob[kHandle].bytes,
      enumerable: false,
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

module.exports = load("blob");
