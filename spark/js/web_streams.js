"use strict";

// node's web streams: lib/stream/web.js, lib/internal/webstreams/* and the
// webidl helpers they use

const internal = require("spark:internal");
const { markUncloneable } = require("spark:errors");

function optional(specifier) {
  try {
    return require(specifier);
  } catch {
    return undefined;
  }
}

internal.addLoader(shared, own);

function notHere(what) {
  const { codes } = internal.load("internal/errors");
  return new codes.ERR_METHOD_NOT_IMPLEMENTED(what);
}

const shims = {
  process: internal.process,
  primordials: internal.primordials,
  internalBinding(name) {
    if (name === "messaging") {
      return { DOMException: require("spark:domexception").DOMException };
    }
    // node's own socket streams, which nothing here has
    if (name === "stream_wrap") {
      return {
        WriteWrap: class WriteWrap {},
        ShutdownWrap: class ShutdownWrap {},
        kArrayBufferOffset: 0,
        kReadBytesOrError: 1,
        kLastWriteWasAsync: 2,
        streamBaseState: [0, 0, 0],
      };
    }
    return internal.internalBinding(name);
  },
  fallback: internal.find,
  stream: () => require("node:stream"),
  buffer: () => require("node:buffer"),
  zlib: () => require("node:zlib"),
  "internal/encoding": () => require("spark:encoding"),
  "internal/abort_controller": () => require("spark:dom_events"),
  "internal/events/abort_listener": () => ({
    addAbortListener: require("node:events").addAbortListener,
  }),
  // the stream module holds this file; loading it makes it findable
  "internal/streams/utils": () => {
    require("node:stream");
    return internal.find("internal/streams/utils");
  },
  // a stream cannot be handed to another thread: there are none
  "internal/worker/io": () => ({
    MessageChannel: class MessageChannel {
      constructor() {
        throw notHere("MessageChannel");
      }
    },
  }),
  "internal/worker/js_transferable": () => ({
    kClone: Symbol("kClone"),
    kDeserialize: Symbol("kDeserialize"),
    kTransfer: Symbol("kTransfer"),
    kTransferList: Symbol("kTransferList"),
    markTransferMode(object, cloneable) {
      if (!cloneable) markUncloneable(object);
    },
    structuredClone(value, options) {
      const clone = optional("spark:structured_clone")?.structuredClone;
      if (clone === undefined) throw notHere("structuredClone");
      return clone(value, options);
    },
  }),
};

const files = {
  "stream/web": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      TransformStream,
      TransformStreamDefaultController,
    } = require('internal/webstreams/transformstream');

    const {
      WritableStream,
      WritableStreamDefaultController,
      WritableStreamDefaultWriter,
    } = require('internal/webstreams/writablestream');

    const {
      ReadableStream,
      ReadableStreamDefaultReader,
      ReadableStreamBYOBReader,
      ReadableStreamBYOBRequest,
      ReadableByteStreamController,
      ReadableStreamDefaultController,
      isReadableStream,
      readableStreamTee,
    } = require('internal/webstreams/readablestream');

    const {
      codes: {
        ERR_INVALID_ARG_TYPE,
      },
    } = require('internal/errors');

    const {
      validateBoolean,
    } = require('internal/validators');

    const {
      ByteLengthQueuingStrategy,
      CountQueuingStrategy,
    } = require('internal/webstreams/queuingstrategies');

    const {
      TextEncoderStream,
      TextDecoderStream,
    } = require('internal/webstreams/encoding');

    const {
      CompressionStream,
      DecompressionStream,
    } = require('internal/webstreams/compression');

    function ReadableStreamTee(stream, cloneForBranch2 = false) {
      if (!isReadableStream(stream)) {
        throw new ERR_INVALID_ARG_TYPE('stream', 'ReadableStream', stream);
      }
      validateBoolean(cloneForBranch2, 'cloneForBranch2');
      return readableStreamTee(stream, cloneForBranch2);
    }

    module.exports = {
      ReadableStream,
      ReadableStreamTee,
      ReadableStreamDefaultReader,
      ReadableStreamBYOBReader,
      ReadableStreamBYOBRequest,
      ReadableByteStreamController,
      ReadableStreamDefaultController,
      TransformStream,
      TransformStreamDefaultController,
      WritableStream,
      WritableStreamDefaultWriter,
      WritableStreamDefaultController,
      ByteLengthQueuingStrategy,
      CountQueuingStrategy,
      TextEncoderStream,
      TextDecoderStream,
      CompressionStream,
      DecompressionStream,
    };
  },
  "internal/webidl": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayBufferIsView,
      ArrayBufferPrototypeGetResizable,
      ArrayIsArray,
      ArrayPrototypePush,
      ArrayPrototypeToSorted,
      BigInt,
      DataViewPrototypeGetBuffer,
      FunctionPrototypeCall,
      MathAbs,
      MathMax,
      MathMin,
      MathPow,
      MathSign,
      MathTrunc,
      Number,
      NumberIsFinite,
      NumberIsNaN,
      NumberMAX_SAFE_INTEGER,
      NumberMIN_SAFE_INTEGER,
      ObjectPrototypeHasOwnProperty,
      SafeArrayIterator,
      SafeSet,
      String,
      StringPrototypeToWellFormed,
      SymbolIterator,
      TypeError,
      TypedArrayPrototypeGetBuffer,
      TypedArrayPrototypeGetSymbolToStringTag,
    } = primordials;

    const { kEmptyObject, setOwnProperty } = require('internal/util');
    const {
      isSharedArrayBuffer,
      isTypedArray,
    } = require('internal/util/types');

    const { getSharedArrayBufferGrowable } = internalBinding('util');

    const BIGINT_2_63 = 1n << 63n;
    const BIGINT_2_64 = 1n << 64n;

    const converters = { __proto__: null };

    function codedTypeError(message, code) {
      const err = new TypeError(message);
      setOwnProperty(err, 'code', code);
      return err;
    }

    function makeException(message, options = kEmptyObject) {
      const prefix = options.prefix ? options.prefix + ': ' : '';
      const context = options.context?.length === 0 ?
        '' : (options.context ?? 'Value') + ' ';
      return codedTypeError(
        `${prefix}${context}${message}`,
        options.code || 'ERR_INVALID_ARG_TYPE',
      );
    }

    function makeOptions(options, context = options.context, code = options.code) {
      return {
        prefix: options.prefix,
        context,
        code,
        enforceRange: options.enforceRange,
        clamp: options.clamp,
        allowShared: options.allowShared,
        allowResizable: options.allowResizable,
      };
    }

    function type(V) {
      switch (typeof V) {
        case 'undefined':
          return 'Undefined';
        case 'boolean':
          return 'Boolean';
        case 'string':
          return 'String';
        case 'symbol':
          return 'Symbol';
        case 'number':
          return 'Number';
        case 'bigint':
          return 'BigInt';
        case 'object':
        case 'function':
        default:
          if (V === null) {
            return 'Null';
          }
          return 'Object';
      }
    }

    function integerPart(n) {
      const integer = Math.trunc(n);
      return integer === 0 ? 0 : integer;
    }

    function evenRound(x) {
      const i = integerPart(x);
      const remainder = Math.abs(x % 1);
      const sign = MathSign(x);
      if (remainder === 0.5) {
        return i % 2 === 0 ? i : i + sign;
      }
      const r = remainder < 0.5 ? i : i + sign;
      if (r === 0) {
        return 0;
      }
      return r;
    }

    function pow2(exponent) {
      if (exponent < 31) {
        return 1 << exponent;
      }
      if (exponent === 31) {
        return 0x8000_0000;
      }
      if (exponent === 32) {
        return 0x1_0000_0000;
      }
      return Math.pow(2, exponent);
    }

    function modulo(x, y) {
      const r = x % y;
      if (r === 0) {
        return 0;
      }
      return r > 0 ? r : r + y;
    }

    function bigIntModulo(x, y) {
      const r = x % y;
      return r >= 0n ? r : r + y;
    }

    function toNumber(V, options = kEmptyObject) {
      if (typeof V === 'bigint') {
        throw makeException(
          'is a BigInt and cannot be converted to a number.',
          options);
      }
      if (typeof V === 'symbol') {
        throw makeException(
          'is a Symbol and cannot be converted to a number.',
          options);
      }

      return +V;
    }

    function toString(V, options = kEmptyObject) {
      if (typeof V === 'symbol') {
        throw makeException(
          'is a Symbol and cannot be converted to a string.',
          options);
      }

      return String(V);
    }

    function convertToInt(
      V,
      bitLength,
      signedness = 'unsigned',
      options = kEmptyObject,
    ) {
      const signed = signedness === 'signed';
      let upperBound;
      let lowerBound;

      if (bitLength === 64) {
        upperBound = NumberMAX_SAFE_INTEGER;
        lowerBound = signed ? NumberMIN_SAFE_INTEGER : 0;
      } else if (!signed) {
        lowerBound = 0;
        if (bitLength === 8) {
          upperBound = 0xff;
        } else if (bitLength === 16) {
          upperBound = 0xffff;
        } else if (bitLength === 32) {
          upperBound = 0xffff_ffff;
        } else {
          upperBound = pow2(bitLength) - 1;
        }
      } else if (bitLength === 8) {
        lowerBound = -0x80;
        upperBound = 0x7f;
      } else if (bitLength === 16) {
        lowerBound = -0x8000;
        upperBound = 0x7fff;
      } else if (bitLength === 32) {
        lowerBound = -0x8000_0000;
        upperBound = 0x7fff_ffff;
      } else {
        lowerBound = -pow2(bitLength - 1);
        upperBound = pow2(bitLength - 1) - 1;
      }

      let x;
      if (typeof V === 'number') {
        if (V >= lowerBound && V <= upperBound) {
          const integer = Math.trunc(V);
          if (integer === V) {
            return V === 0 ? 0 : V;
          }
          if (options !== kEmptyObject && options.clamp) {
            return evenRound(V);
          }
          return integer === 0 ? 0 : integer;
        }
        if (options !== kEmptyObject && options.enforceRange) {
          if (!Number.isFinite(V)) {
            throw makeException(
              'is not a finite number.',
              options);
          }

          const integer = integerPart(V);
          if (integer < lowerBound || integer > upperBound) {
            throw makeException(
              `is outside the expected range of ${lowerBound} to ${upperBound}.`,
              makeOptions(options, options.context, 'ERR_OUT_OF_RANGE'));
          }

          return integer;
        }
        if (options !== kEmptyObject && options.clamp && !Number.isNaN(V)) {
          if (V <= lowerBound) {
            return lowerBound === 0 ? 0 : lowerBound;
          }
          if (V >= upperBound) {
            return upperBound === 0 ? 0 : upperBound;
          }
        }
        x = V;
      } else {
        x = toNumber(V, options);
      }
      if (x === 0) {
        x = 0;
      }

      if (options.enforceRange) {
        if (!Number.isFinite(x)) {
          throw makeException(
            'is not a finite number.',
            options);
        }

        x = integerPart(x);

        if (x < lowerBound || x > upperBound) {
          throw makeException(
            `is outside the expected range of ${lowerBound} to ${upperBound}.`,
            makeOptions(options, options.context, 'ERR_OUT_OF_RANGE'));
        }

        return x;
      }

      if (options.clamp && !Number.isNaN(x)) {
        x = Math.min(Math.max(x, lowerBound), upperBound);
        return evenRound(x);
      }

      if (!Number.isFinite(x) || x === 0) {
        return 0;
      }

      x = integerPart(x);

      if (x >= lowerBound && x <= upperBound) {
        return x;
      }

      if (bitLength === 64) {
        let xBigInt = BigInt(x);
        xBigInt = bigIntModulo(xBigInt, BIGINT_2_64);

        if (signed && xBigInt >= BIGINT_2_63) {
          return Number(xBigInt - BIGINT_2_64);
        }

        return Number(xBigInt);
      }

      if (bitLength === 8) {
        return signed ? (x << 24) >> 24 : x & 0xff;
      }
      if (bitLength === 16) {
        return signed ? (x << 16) >> 16 : x & 0xffff;
      }
      if (bitLength === 32) {
        return signed ? x | 0 : x >>> 0;
      }

      const twoToTheBitLength = pow2(bitLength);
      x = modulo(x, twoToTheBitLength);

      if (signed && x >= pow2(bitLength - 1)) {
        return x - twoToTheBitLength;
      }

      return x;
    }

    function createIntegerConverter(bitLength, signedness = 'unsigned') {
      return (V, options = kEmptyObject) => {
        return convertToInt(V, bitLength, signedness, options);
      };
    }

    converters.boolean = (V) => {
      return !!V;
    };

    converters.object = (V, options = kEmptyObject) => {
      if (type(V) !== 'Object') {
        throw makeException(
          'is not an object.',
          options,
        );
      }
      return V;
    };

    converters.octet = createIntegerConverter(8);

    converters['unsigned short'] = createIntegerConverter(16);

    converters['unsigned long'] = createIntegerConverter(32);

    converters['long long'] = createIntegerConverter(64, 'signed');

    converters.DOMString = function DOMString(V, options = kEmptyObject) {
      return toString(V, options);
    };

    converters.USVString = function USVString(V, options = kEmptyObject) {
      return converters.DOMString(V, options).toWellFormed();
    };

    function requiredArguments(length, required, options = kEmptyObject) {
      if (length < required) {
        throw makeException(
          `${required} argument${
            required === 1 ? '' : 's'
          } required, but only ${length} present.`,
          makeOptions(options, '', 'ERR_MISSING_ARGS'));
      }
    }

    function createEnumConverter(name, values) {
      const E = new SafeSet(new SafeArrayIterator(values));

      return function(V, options = kEmptyObject) {
        const S = toString(V, options);

        if (!E.has(S)) {
          throw makeException(
            `'${S}' is not a valid enum value of type ${name}.`,
            makeOptions(options, options.context, 'ERR_INVALID_ARG_VALUE'));
        }

        return S;
      };
    }

    function dictionaryMemberContext(key, options) {
      return options.context ? `${key} in ${options.context}` : key;
    }

    function sequenceElementContext(index, options) {
      return `${options.context ?? 'Value'}[${index}]`;
    }

    function missingDictionaryMemberMessage(dictionaryName, key) {
      return `cannot be converted to '${dictionaryName}' because ` +
        `'${key}' is required in '${dictionaryName}'.`;
    }

    function createDictionaryConverter(
      dictionaryName,
      members,
    ) {
      const compareMembers = (a, b) => {
        if (a.key === b.key) {
          return 0;
        }
        return a.key < b.key ? -1 : 1;
      };

      const dictionaries = Array.isArray(members[0]) ? members : [members];
      const sortedMembers = [];

      function ownMember(member, key) {
        return ObjectPrototypeHasOwnProperty(member, key) ? member[key] : undefined;
      }

      let anyMissingMemberHandling = false;

      for (let i = 0; i < dictionaries.length; i++) {
        const sortedDictionary = dictionaries[i].toSorted(compareMembers);
        for (let j = 0; j < sortedDictionary.length; j++) {
          const member = sortedDictionary[j];
          const defaultValue = ownMember(member, 'defaultValue');
          const required = ownMember(member, 'required');
          if (typeof defaultValue === 'function' || required) {
            anyMissingMemberHandling = true;
          }
          setOwnProperty(sortedMembers, sortedMembers.length, {
            key: ownMember(member, 'key'),
            converter: ownMember(member, 'converter'),
            defaultValue,
            required,
            validator: ownMember(member, 'validator'),
          });
        }
      }

      const hasMissingMemberHandling = anyMissingMemberHandling;

      return function(jsDict, options = kEmptyObject) {
        if (jsDict != null && type(jsDict) !== 'Object') {
          throw makeException(
            'cannot be converted to a dictionary',
            options,
          );
        }

        const idlDict = { __proto__: null };
        if (jsDict == null && !hasMissingMemberHandling) return idlDict;

        for (let i = 0; i < sortedMembers.length; i++) {
          const member = sortedMembers[i];
          const key = member.key;
          const jsMemberValue = jsDict == null ? undefined : jsDict[key];

          if (jsMemberValue !== undefined) {
            const converter = member.converter;
            const idlMemberValue = converter(
              jsMemberValue,
              makeOptions(options, dictionaryMemberContext(key, options)),
            );
            member.validator?.(idlMemberValue, jsDict);
            idlDict[key] = idlMemberValue;
          } else if (hasMissingMemberHandling) {
            if (typeof member.defaultValue === 'function') {
              idlDict[key] = member.defaultValue();
            } else if (member.required) {
              throw makeException(
                missingDictionaryMemberMessage(dictionaryName, key),
                makeOptions(options, options.context, 'ERR_MISSING_OPTION'));
            }
          }
        }

        return idlDict;
      };
    }

    function createSequenceConverter(converter) {
      return function(V, options = kEmptyObject, method = undefined) {
        if (type(V) !== 'Object') {
          throw makeException(
            'cannot be converted to sequence.',
            options);
        }

        if (method === undefined) {
          method = V[SymbolIterator];
        }
        if (typeof method !== 'function') {
          throw makeException(
            'cannot be converted to sequence.',
            options);
        }

        const iterator = FunctionPrototypeCall(method, V);
        if (type(iterator) !== 'Object') {
          throw makeException(
            'cannot be converted to sequence.',
            options);
        }
        const nextMethod = iterator.next;
        if (typeof nextMethod !== 'function') {
          throw makeException(
            'cannot be converted to sequence.',
            options);
        }

        const idlSequence = [];
        while (true) {
          const next = FunctionPrototypeCall(nextMethod, iterator);
          if (type(next) !== 'Object') {
            throw makeException(
              'cannot be converted to sequence.',
              options);
          }
          if (next.done) {
            break;
          }
          const idlValue = converter(
            next.value,
            makeOptions(options, sequenceElementContext(idlSequence.length, options)),
          );
          idlSequence.push(idlValue);
        }
        return idlSequence;
      };
    }

    function createInterfaceConverter(name, brandCheck) {
      return (V, options = kEmptyObject) => {
        if (brandCheck(V)) {
          return V;
        }
        throw makeException(
          `is not of type ${name}.`,
          options);
      };
    }

    function getViewedArrayBuffer(V) {
      return isTypedArray(V) ?
        V.buffer : DataViewPrototypeGetBuffer(V);
    }

    converters.ArrayBufferView = (V, options = kEmptyObject) => {
      if (!ArrayBuffer.isView(V)) {
        throw makeException('is not an ArrayBufferView.', options);
      }
      validateBufferSourceBacking(getViewedArrayBuffer(V), options);
      return V;
    };

    function validateBufferSourceBacking(buffer, options) {
      let resizable;
      try {
        resizable = ArrayBufferPrototypeGetResizable(buffer);
      } catch {
        if (!options.allowShared) {
          throw makeException(
            'is a view on a SharedArrayBuffer, which is not allowed.',
            options);
        }
        validateAllowGrowableSharedArrayBuffer(buffer, options);
        return;
      }

      validateAllowResizableArrayBuffer(resizable, options);
    }

    function validateAllowGrowableSharedArrayBuffer(buffer, options) {
      if (!options.allowResizable &&
          FunctionPrototypeCall(getSharedArrayBufferGrowable, buffer)) {
        throw makeException(
          'is backed by a growable SharedArrayBuffer, which is not allowed.',
          options);
      }
    }

    function validateAllowResizableArrayBuffer(resizable, options) {
      if (resizable && !options.allowResizable) {
        throw makeException(
          'is backed by a resizable ArrayBuffer, which is not allowed.',
          options);
      }
    }

    converters.Uint8Array = (V, options = kEmptyObject) => {
      if (!ArrayBuffer.isView(V) ||
        TypedArrayPrototypeGetSymbolToStringTag(V) !== 'Uint8Array') {
        throw makeException(
          'is not an Uint8Array object.',
          options);
      }
      validateBufferSourceBacking(V.buffer, options);

      return V;
    };

    converters.BufferSource = (V, options = kEmptyObject) => {
      if (ArrayBuffer.isView(V)) {
        const buffer = getViewedArrayBuffer(V);
        let resizable;
        try {
          resizable = ArrayBufferPrototypeGetResizable(buffer);
        } catch {
          throw makeException(
            'is a view on a SharedArrayBuffer, which is not allowed.',
            options);
        }
        if (resizable && !options.allowResizable) {
          throw makeException(
            'is backed by a resizable ArrayBuffer, which is not allowed.',
            options);
        }
        return V;
      }

      let resizable;
      try {
        resizable = ArrayBufferPrototypeGetResizable(V);
      } catch {
        throw makeException(
          'is not instance of ArrayBuffer, Buffer, TypedArray, or DataView.',
          options);
      }

      if (resizable && !options.allowResizable) {
        throw makeException(
          'is backed by a resizable ArrayBuffer, which is not allowed.',
          options);
      }

      return V;
    };

    converters.AllowSharedBufferSource = (V, options = kEmptyObject) => {
      if (ArrayBuffer.isView(V)) {
        const buffer = getViewedArrayBuffer(V);
        let resizable;
        try {
          resizable = ArrayBufferPrototypeGetResizable(buffer);
        } catch {
          validateAllowGrowableSharedArrayBuffer(buffer, options);
          return V;
        }
        validateAllowResizableArrayBuffer(resizable, options);
        return V;
      }

      let resizable;
      try {
        resizable = ArrayBufferPrototypeGetResizable(V);
      } catch {
        if (isSharedArrayBuffer(V)) {
          validateAllowGrowableSharedArrayBuffer(V, options);
          return V;
        }
        throw makeException(
          'is not instance of ArrayBuffer, SharedArrayBuffer, Buffer, ' +
          'TypedArray, or DataView.',
          options);
      }

      validateAllowResizableArrayBuffer(resizable, options);
      return V;
    };

    converters['sequence<DOMString>'] =
      createSequenceConverter(converters.DOMString);

    converters['sequence<object>'] =
      createSequenceConverter(converters.object);

    converters.StructuredSerializeOptions = createDictionaryConverter(
      'StructuredSerializeOptions',
      [
        {
          key: 'transfer',
          converter: converters['sequence<object>'],
          defaultValue: () => [],
        },
      ],
    );

    module.exports = {
      converters,
      convertToInt,
      createDictionaryConverter,
      createEnumConverter,
      createInterfaceConverter,
      createSequenceConverter,
      requiredArguments,
      type,
    };
  },
  "internal/webstreams/adapters": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypeFilter,
      ObjectKeys,
      PromisePrototypeThen,
      PromiseResolve,
      PromiseWithResolvers,
      SafePromiseAllReturnVoid,
      SafePromisePrototypeFinally,
      SafeSet,
      StringPrototypeStartsWith,
      Symbol,
      TypeError,
      TypedArrayPrototypeGetBuffer,
      TypedArrayPrototypeGetByteLength,
      TypedArrayPrototypeGetByteOffset,
      Uint8Array,
    } = primordials;

    const { TextEncoder } = require('internal/encoding');

    const {
      ReadableStream,
      isReadableStream,
      readableStreamDefaultReaderRead,
      kChunk,
      kClose,
      kError,
    } = require('internal/webstreams/readablestream');

    const {
      WritableStream,
      isWritableStream,
      writableStreamDefaultWriterWriteWithRequest,
    } = require('internal/webstreams/writablestream');

    const {
      CountQueuingStrategy,
      ByteLengthQueuingStrategy,
    } = require('internal/webstreams/queuingstrategies');

    const {
      Writable,
      Readable,
      Duplex,
      destroy,
    } = require('stream');

    const {
      isDestroyed,
      isReadable,
      isWritable,
      isWritableEnded,
    } = require('internal/streams/utils');

    const {
      Buffer,
    } = require('buffer');

    const {
      kResolvedPromise,
    } = require('internal/webstreams/util');

    const {
      isAnyArrayBuffer,
    } = require('internal/util/types');

    const {
      AbortError,
      ErrnoException,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_ARG_VALUE,
        ERR_INVALID_STATE,
        ERR_STREAM_PREMATURE_CLOSE,
      },
    } = require('internal/errors');

    const {
      getDeprecationWarningEmitter,
      kEmptyObject,
      normalizeEncoding,
      setOwnProperty,
    } = require('internal/util');

    const {
      validateBoolean,
      validateFunction,
      validateObject,
      validateOneOf,
    } = require('internal/validators');

    const {
      WriteWrap,
      ShutdownWrap,
      kArrayBufferOffset,
      kReadBytesOrError,
      kLastWriteWasAsync,
      streamBaseState,
    } = internalBinding('stream_wrap');

    const {
      eos,
      kEosNodeSynchronousCallback,
    } = require('internal/streams/end-of-stream');

    const { zlib } = internalBinding('constants');
    const { UV_EOF } = internalBinding('uv');

    const encoder = new TextEncoder();

    const kValidateChunk = Symbol('kValidateChunk');
    const kDestroyOnSyncError = Symbol('kDestroyOnSyncError');

    const ZLIB_FAILURES = new SafeSet(
      Object.keys(zlib).filter((code) => code === 'Z_NEED_DICT' || zlib[code] < 0),
    );

    function handleKnownInternalErrors(cause) {
      const causeCode = cause?.code;
      switch (true) {
        case causeCode === 'ERR_STREAM_PREMATURE_CLOSE': {
          return new AbortError(undefined, { cause });
        }
        case ZLIB_FAILURES.has(causeCode):
        case causeCode != null &&
          causeCode.startsWith('ERR__ERROR_'): {
          const error = new TypeError(undefined, { cause });
          setOwnProperty(error, 'code', causeCode);
          return error;
        }
        default:
          return cause;
      }
    }

    const noop = () => {};

    function createWriteTracker(getDestination) {
      return {
        promise: null,
        pending: 0,
        failed: false,
        failure: undefined,
        callback: undefined,
        error: undefined,
        hasError: false,
        dispatching: false,
        resolve() {
          this.pending--;
          if (!this.dispatching && this.pending === 0 &&
              this.callback !== undefined) {
            this.settle();
          }
        },
        reject(error) {
          this.pending--;
          if (!this.hasError) {
            this.hasError = true;
            this.error = error;
          }
          if (!this.dispatching && this.pending === 0 &&
              this.callback !== undefined) {
            this.settle();
          }
        },
        settle() {
          const callback = this.callback;
          this.callback = undefined;
          let error;
          if (this.hasError) {
            error = this.error;
            this.hasError = false;
            this.error = undefined;
          } else if (this.failed) {
            error = this.failure;
          }
          this.failed = false;
          this.failure = undefined;
          if (error !== undefined) {
            PromisePrototypeThen(kResolvedPromise, () => {
              try {
                callback(error);
              } catch (err) {
                process.nextTick(() => destroy(getDestination(), err));
              }
            });
            return;
          }
          try {
            callback(error);
          } catch (err) {
            process.nextTick(() => destroy(getDestination(), err));
          }
        },
      };
    }

    function dispatchWrite(writer, chunk, tracker) {
      tracker.dispatching = true;
      writableStreamDefaultWriterWriteWithRequest(writer, chunk, tracker);
      tracker.dispatching = false;
      if (tracker.pending === 0)
        tracker.settle();
    }

    function dispatchWritev(writer, chunks, tracker) {
      tracker.dispatching = true;
      for (let i = 0; i < chunks.length; i++)
        writableStreamDefaultWriterWriteWithRequest(writer, chunks[i].chunk, tracker);
      tracker.dispatching = false;
      if (tracker.pending === 0)
        tracker.settle();
    }

    function newWritableStreamFromStreamWritable(streamWritable, options = kEmptyObject) {
      const checkIfWritableOrOutgoingMessage =
        streamWritable &&
        typeof streamWritable?.write === 'function' &&
        typeof streamWritable?.on === 'function';
      if (!checkIfWritableOrOutgoingMessage) {
        throw new ERR_INVALID_ARG_TYPE(
          'streamWritable',
          'stream.Writable',
          streamWritable,
        );
      }

      if (isDestroyed(streamWritable) || !isWritable(streamWritable)) {
        const writable = new WritableStream();
        writable.close();
        return writable;
      }

      const highWaterMark = streamWritable.writableHighWaterMark;
      const strategy =
        streamWritable.writableObjectMode ?
          new CountQueuingStrategy({ highWaterMark }) :
          {
            highWaterMark,
            size(chunk) {
              return chunk?.byteLength ?? chunk?.length ?? 1;
            },
          };

      let controller;
      let backpressurePromise;
      let closed;

      function onDrain() {
        backpressurePromise?.resolve();
      }

      const cleanup = eos(streamWritable, (error) => {
        error = handleKnownInternalErrors(error);

        cleanup();
        streamWritable.on('error', () => {});
        if (error != null) {
          backpressurePromise?.reject(error);
          if (closed !== undefined) {
            closed.reject(error);
            closed = undefined;
          }
          controller?.error(error);
          controller = undefined;
          return;
        }

        if (closed !== undefined) {
          closed.resolve();
          closed = undefined;
          return;
        }
        controller?.error(new AbortError());
        controller = undefined;
      });

      streamWritable.on('drain', onDrain);

      return new WritableStream({
        start(c) { controller = c; },

        write(chunk) {
          try {
            options[kValidateChunk]?.(chunk);
            if (!streamWritable.writableObjectMode && isAnyArrayBuffer(chunk)) {
              chunk = new Uint8Array(chunk);
            }
            if (streamWritable.writableNeedDrain || !streamWritable.write(chunk)) {
              backpressurePromise = PromiseWithResolvers();
              if (!streamWritable.writableNeedDrain) {
                backpressurePromise.resolve();
              }
              return SafePromisePrototypeFinally(
                backpressurePromise.promise, () => {
                  backpressurePromise = undefined;
                });
            }
          } catch (error) {
            if (options[kDestroyOnSyncError]) {
              destroy(streamWritable, error);
            }
            throw error;
          }
        },

        abort(reason) {
          destroy(streamWritable, reason);
        },

        close() {
          if (closed === undefined && !isWritableEnded(streamWritable)) {
            closed = PromiseWithResolvers();
            streamWritable.end();
            return closed.promise;
          }

          controller = undefined;
          return Promise.resolve();
        },
      }, strategy);
    }

    function newStreamWritableFromWritableStream(writableStream, options = kEmptyObject) {
      if (!isWritableStream(writableStream)) {
        throw new ERR_INVALID_ARG_TYPE(
          'writableStream',
          'WritableStream',
          writableStream);
      }

      validateObject(options, 'options');
      const {
        highWaterMark,
        decodeStrings = true,
        objectMode = false,
        signal,
      } = options;

      validateBoolean(objectMode, 'options.objectMode');
      validateBoolean(decodeStrings, 'options.decodeStrings');

      const writer = writableStream.getWriter();
      let closed = false;
      const writeTracker = createWriteTracker(() => writable);

      const writable = new Writable({
        highWaterMark,
        objectMode,
        decodeStrings,
        signal,

        writev(chunks, callback) {
          writeTracker.callback = callback;
          dispatchWritev(writer, chunks, writeTracker);
        },

        write(chunk, encoding, callback) {
          if (typeof chunk === 'string' && decodeStrings && !objectMode) {
            const enc = normalizeEncoding(encoding);

            if (enc === 'utf8') {
              chunk = encoder.encode(chunk);
            } else {
              chunk = Buffer.from(chunk, encoding);
              chunk = new Uint8Array(
                chunk.buffer,
                chunk.byteOffset,
                chunk.byteLength,
              );
            }
          }

          writeTracker.callback = callback;
          dispatchWrite(writer, chunk, writeTracker);
        },

        destroy(error, callback) {
          function done() {
            try {
              callback(error);
            } catch (error) {
              process.nextTick(() => { throw error; });
            }
          }

          if (!closed) {
            if (error != null) {
              PromisePrototypeThen(
                writer.abort(error),
                done,
                done);
            } else {
              PromisePrototypeThen(
                writer.close(),
                done,
                done);
            }
            return;
          }

          done();
        },

        final(callback) {
          function done(error) {
            try {
              callback(error);
            } catch (error) {
              process.nextTick(() => destroy(writable, error));
            }
          }

          if (!closed) {
            PromisePrototypeThen(
              writer.close(),
              done,
              done);
          }
        },
      });

      PromisePrototypeThen(
        writer.closed,
        () => {
          closed = true;
          if (!isWritableEnded(writable))
            destroy(writable, new ERR_STREAM_PREMATURE_CLOSE());
        },
        (error) => {
          closed = true;
          destroy(writable, error);
        });

      return writable;
    }

    const kErrorSentinelAttached = Symbol('kErrorSentinelAttached');

    function newReadableStreamFromStreamReadable(streamReadable, options = kEmptyObject) {
      if (typeof streamReadable?._readableState !== 'object') {
        throw new ERR_INVALID_ARG_TYPE(
          'streamReadable',
          'stream.Readable',
          streamReadable);
      }
      validateObject(options, 'options');
      if (options.type !== undefined) {
        validateOneOf(options.type, 'options.type', ['bytes', undefined]);
      }

      const isBYOB = options.type === 'bytes';
      let controller;
      let wasCanceled = false;
      let strategy;

        const underlyingSource = {
        __proto__: null,
        type: isBYOB ? 'bytes' : undefined,
        start(c) { controller = c; },
        cancel(reason) {
          wasCanceled = true;
          destroy(streamReadable, reason);
        },
      };

      const readable = isReadable(streamReadable);
      const objectMode = streamReadable.readableObjectMode;
      if (readable) {
        underlyingSource.pull = function pull() {
          streamReadable.resume();
        };

        const highWaterMark = streamReadable.readableHighWaterMark;
        strategy = isBYOB ? { highWaterMark } :
          options.strategy ?? new (objectMode ? CountQueuingStrategy : ByteLengthQueuingStrategy)({ highWaterMark });
      }
      const readableStream = new ReadableStream(underlyingSource, strategy);

      let cleanup = noop;
      cleanup = eos(streamReadable, {
        __proto__: null,
        writable: false,
        [kEosNodeSynchronousCallback]: true,
      }, (error) => {
        error = handleKnownInternalErrors(error);

        cleanup();

        if (!(kErrorSentinelAttached in streamReadable)) {
          streamReadable.on('error', noop);
          streamReadable[kErrorSentinelAttached] = true;
        }
        if (wasCanceled) {
          return;
        }
        wasCanceled = true;
        if (error)
          return controller.error(error);
        controller.close();
        if (isBYOB)
          controller.byobRequest?.respond(0);
      });

      if (wasCanceled) {
        cleanup();
      } else if (readable) {
        streamReadable.pause();

        streamReadable.on('data', function onData(chunk) {
          if (wasCanceled) return;
          if (Buffer.isBuffer(chunk) && !objectMode)
            chunk = new Uint8Array(chunk);
          controller.enqueue(chunk);
          if (controller.desiredSize <= 0)
            streamReadable.pause();
        });
      }

      return readableStream;
    }

    function newStreamReadableFromReadableStream(readableStream, options = kEmptyObject) {
      if (!isReadableStream(readableStream)) {
        throw new ERR_INVALID_ARG_TYPE(
          'readableStream',
          'ReadableStream',
          readableStream);
      }

      validateObject(options, 'options');
      const {
        highWaterMark,
        encoding,
        objectMode = false,
        signal,
      } = options;

      if (encoding !== undefined && !Buffer.isEncoding(encoding))
        throw new ERR_INVALID_ARG_VALUE('options.encoding', encoding);
      validateBoolean(objectMode, 'options.objectMode');

      const reader = readableStream.getReader();
      let closed = false;
      let readRequest;
      let pendingChunk;

      function forwardChunk() {
        const chunk = pendingChunk;
        pendingChunk = undefined;
        readable.push(chunk);
      }

      const readable = new Readable({
        objectMode,
        highWaterMark,
        encoding,
        signal,

        read() {
          readRequest ??= {
            [kChunk](chunk) {
              pendingChunk = chunk;
              PromisePrototypeThen(kResolvedPromise, forwardChunk);
            },
            [kClose]() {
              readable.push(null);
            },
            [kError](error) {
              destroy(readable, error);
            },
          };
          readableStreamDefaultReaderRead(reader, readRequest);
        },

        destroy(error, callback) {
          function done() {
            try {
              callback(error);
            } catch (error) {
              process.nextTick(() => { throw error; });
            }
          }

          if (!closed) {
            PromisePrototypeThen(
              reader.cancel(error),
              done,
              done);
            return;
          }
          done();
        },
      });

      PromisePrototypeThen(
        reader.closed,
        () => {
          closed = true;
        },
        (error) => {
          closed = true;
          destroy(readable, error);
        });

      return readable;
    }

    const emitDEP0201 = getDeprecationWarningEmitter(
      'DEP0201',
      "Passing 'options.type' to Duplex.toWeb() is deprecated. " +
          "To specify the ReadableStream type, use 'options.readableType'.",
      newReadableWritablePairFromDuplex);

    function newReadableWritablePairFromDuplex(duplex, options = kEmptyObject) {
      if (typeof duplex?._writableState !== 'object' ||
          typeof duplex?._readableState !== 'object') {
        throw new ERR_INVALID_ARG_TYPE('duplex', 'stream.Duplex', duplex);
      }

      validateObject(options, 'options');

      const readableOptions = {
        __proto__: null,
        type: options.readableType,
      };

      if (options.readableType == null && options.type != null) {
        emitDEP0201();
        readableOptions.type = options.type;
      }

      if (isDestroyed(duplex)) {
        const writable = new WritableStream();
        const readable = new ReadableStream({ type: readableOptions.type });
        writable.close();
        readable.cancel();
        return { readable, writable };
      }

      const writableOptions = {
        __proto__: null,
        [kValidateChunk]: options[kValidateChunk],
        [kDestroyOnSyncError]: options[kDestroyOnSyncError],
      };

      const writable =
        isWritable(duplex) ?
          newWritableStreamFromStreamWritable(duplex, writableOptions) :
          new WritableStream();

      if (!isWritable(duplex))
        writable.close();

      const readable =
        isReadable(duplex) ?
          newReadableStreamFromStreamReadable(duplex, readableOptions) :
          new ReadableStream({ type: readableOptions.type });

      if (!isReadable(duplex))
        readable.cancel();

      return { writable, readable };
    }

    function newStreamDuplexFromReadableWritablePair(pair = kEmptyObject, options = kEmptyObject) {
      validateObject(pair, 'pair');
      const {
        readable: readableStream,
        writable: writableStream,
      } = pair;

      if (!isReadableStream(readableStream)) {
        throw new ERR_INVALID_ARG_TYPE(
          'pair.readable',
          'ReadableStream',
          readableStream);
      }
      if (!isWritableStream(writableStream)) {
        throw new ERR_INVALID_ARG_TYPE(
          'pair.writable',
          'WritableStream',
          writableStream);
      }

      validateObject(options, 'options');
      const {
        allowHalfOpen = false,
        objectMode = false,
        encoding,
        decodeStrings = true,
        highWaterMark,
        signal,
      } = options;

      validateBoolean(objectMode, 'options.objectMode');
      if (encoding !== undefined && !Buffer.isEncoding(encoding))
        throw new ERR_INVALID_ARG_VALUE('options.encoding', encoding);

      const writer = writableStream.getWriter();
      const reader = readableStream.getReader();
      let writableClosed = false;
      let readableClosed = false;
      let readRequest;
      let pendingChunk;
      const writeTracker = createWriteTracker(() => duplex);

      function forwardChunk() {
        const chunk = pendingChunk;
        pendingChunk = undefined;
        duplex.push(chunk);
      }

      const duplex = new Duplex({
        allowHalfOpen,
        highWaterMark,
        objectMode,
        encoding,
        decodeStrings,
        signal,

        writev(chunks, callback) {
          writeTracker.callback = callback;
          dispatchWritev(writer, chunks, writeTracker);
        },

        write(chunk, encoding, callback) {
          if (typeof chunk === 'string' && decodeStrings && !objectMode) {
            const enc = normalizeEncoding(encoding);

            if (enc === 'utf8') {
              chunk = encoder.encode(chunk);
            } else {
              chunk = Buffer.from(chunk, encoding);
              chunk = new Uint8Array(
                chunk.buffer,
                chunk.byteOffset,
                chunk.byteLength,
              );
            }
          }

          writeTracker.callback = callback;
          dispatchWrite(writer, chunk, writeTracker);
        },

        final(callback) {
          function done(error) {
            try {
              callback(error);
            } catch (error) {
              process.nextTick(() => destroy(duplex, error));
            }
          }

          if (!writableClosed) {
            PromisePrototypeThen(
              writer.close(),
              done,
              done);
          }
        },

        read() {
          readRequest ??= {
            [kChunk](chunk) {
              pendingChunk = chunk;
              PromisePrototypeThen(kResolvedPromise, forwardChunk);
            },
            [kClose]() {
              duplex.push(null);
            },
            [kError](error) {
              destroy(duplex, error);
            },
          };
          readableStreamDefaultReaderRead(reader, readRequest);
        },

        destroy(error, callback) {
          function done() {
            try {
              callback(error);
            } catch (error) {
              process.nextTick(() => { throw error; });
            }
          }

          async function closeWriter() {
            if (!writableClosed)
              await writer.abort(error);
          }

          async function closeReader() {
            if (!readableClosed)
              await reader.cancel(error);
          }

          if (!writableClosed || !readableClosed) {
            PromisePrototypeThen(
              SafePromiseAllReturnVoid([
                closeWriter(),
                closeReader(),
              ]),
              done,
              done);
            return;
          }

          done();
        },
      });

      PromisePrototypeThen(
        writer.closed,
        () => {
          writableClosed = true;
          if (!isWritableEnded(duplex))
            destroy(duplex, new ERR_STREAM_PREMATURE_CLOSE());
        },
        (error) => {
          writableClosed = true;
          readableClosed = true;
          destroy(duplex, error);
        });

      PromisePrototypeThen(
        reader.closed,
        () => {
          readableClosed = true;
        },
        (error) => {
          writableClosed = true;
          readableClosed = true;
          destroy(duplex, error);
        });

      return duplex;
    }

    function newWritableStreamFromStreamBase(streamBase, strategy) {
      validateObject(streamBase, 'streamBase');

      let current;

      function createWriteWrap(controller, promise) {
        const req = new WriteWrap();
        req.handle = streamBase;
        req.oncomplete = onWriteComplete;
        req.async = false;
        req.bytes = 0;
        req.buffer = null;
        req.controller = controller;
        req.promise = promise;
        return req;
      }

      function onWriteComplete(status) {
        if (status < 0) {
          const error = new ErrnoException(status, 'write', this.error);
          this.promise.reject(error);
          this.controller.error(error);
          return;
        }
        this.promise.resolve();
      }

      function doWrite(chunk, controller) {
        const promise = PromiseWithResolvers();
        let ret;
        let req;
        try {
          req = createWriteWrap(controller, promise);
          ret = streamBase.writeBuffer(req, chunk);
          if (streamBaseState[kLastWriteWasAsync])
            req.buffer = chunk;
          req.async = !!streamBaseState[kLastWriteWasAsync];
        } catch (error) {
          promise.reject(error);
        }

        if (ret !== 0)
          promise.reject(new ErrnoException(ret, 'write', req));
        else if (!req.async)
          promise.resolve();

        return promise.promise;
      }

      return new WritableStream({
        write(chunk, controller) {
          current = current !== undefined ?
            PromisePrototypeThen(
              current,
              () => doWrite(chunk, controller),
              (error) => controller.error(error)) :
            doWrite(chunk, controller);
          return current;
        },

        close() {
          const promise = PromiseWithResolvers();
          const req = new ShutdownWrap();
          req.oncomplete = () => promise.resolve();
          const err = streamBase.shutdown(req);
          if (err === 1)
            promise.resolve();
          return promise.promise;
        },
      }, strategy);
    }

    function newReadableStreamFromStreamBase(streamBase, strategy, options = kEmptyObject) {
      validateObject(streamBase, 'streamBase');
      validateObject(options, 'options');

      const {
        ondone = () => {},
      } = options;

      if (typeof streamBase.onread === 'function')
        throw new ERR_INVALID_STATE('StreamBase already has a consumer');

      validateFunction(ondone, 'options.ondone');

      let controller;

      streamBase.onread = (arrayBuffer) => {
        const nread = streamBaseState[kReadBytesOrError];

        if (nread === 0)
          return;

        try {
          if (nread === UV_EOF) {
            controller.close();
            streamBase.readStop();
            try {
              ondone();
            } catch (error) {
              controller.error(error);
            }
            return;
          }

          const offset = streamBaseState[kArrayBufferOffset];
          controller.enqueue(new Uint8Array(arrayBuffer, offset, nread));

          if (controller.desiredSize <= 0)
            streamBase.readStop();
        } catch (error) {
          controller.error(error);
          streamBase.readStop();
        }
      };

      return new ReadableStream({
        start(c) { controller = c; },

        pull() {
          streamBase.readStart();
        },

        cancel() {
          const promise = PromiseWithResolvers();
          try {
            ondone();
          } catch (error) {
            promise.reject(error);
            return promise.promise;
          }
          const req = new ShutdownWrap();
          req.oncomplete = () => promise.resolve();
          const err = streamBase.shutdown(req);
          if (err === 1)
            promise.resolve();
          return promise.promise;
        },
      }, strategy);
    }

    module.exports = {
      newWritableStreamFromStreamWritable,
      newReadableStreamFromStreamReadable,
      newStreamWritableFromWritableStream,
      newStreamReadableFromReadableStream,
      newReadableWritablePairFromDuplex,
      newStreamDuplexFromReadableWritablePair,
      newWritableStreamFromStreamBase,
      newReadableStreamFromStreamBase,
      kValidateChunk,
      kDestroyOnSyncError,
    };
  },
  "internal/webstreams/compression": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectDefineProperties,
      SymbolToStringTag,
    } = primordials;

    const {
      newReadableWritablePairFromDuplex,
      kValidateChunk,
      kDestroyOnSyncError,
    } = require('internal/webstreams/adapters');

    const { customInspect } = require('internal/webstreams/util');

    const {
      isArrayBufferView,
      isSharedArrayBuffer,
    } = require('internal/util/types');

    const {
      customInspectSymbol: kInspect,
      kEnumerableProperty,
    } = require('internal/util');

    const {
      codes: {
        ERR_INVALID_ARG_TYPE,
      },
    } = require('internal/errors');

    const { createEnumConverter } = require('internal/webidl');

    let zlib;
    function lazyZlib() {
      zlib ??= require('zlib');
      return zlib;
    }

    function validateBufferSourceChunk(chunk) {
      if (isSharedArrayBuffer(isArrayBufferView(chunk) ? chunk.buffer : chunk)) {
        throw new ERR_INVALID_ARG_TYPE(
          'chunk',
          ['ArrayBuffer', 'Buffer', 'TypedArray', 'DataView'],
          chunk,
        );
      }
    }

    const formatConverter = createEnumConverter('CompressionFormat', [
      'deflate',
      'deflate-raw',
      'gzip',
      'brotli',
    ]);

    class CompressionStream {
      #handle;
      #transform;

        constructor(format) {
        format = formatConverter(format, {
          prefix: "Failed to construct 'CompressionStream'",
          context: '1st argument',
        });
        switch (format) {
          case 'deflate':
            this.#handle = lazyZlib().createDeflate();
            break;
          case 'deflate-raw':
            this.#handle = lazyZlib().createDeflateRaw();
            break;
          case 'gzip':
            this.#handle = lazyZlib().createGzip();
            break;
          case 'brotli':
            this.#handle = lazyZlib().createBrotliCompress();
            break;
        }
        this.#transform = newReadableWritablePairFromDuplex(this.#handle, {
          [kValidateChunk]: validateBufferSourceChunk,
          [kDestroyOnSyncError]: true,
        });
      }

        get readable() {
        return this.#transform.readable;
      }

        get writable() {
        return this.#transform.writable;
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, 'CompressionStream', {
          readable: this.#transform.readable,
          writable: this.#transform.writable,
        });
      }
    }

    class DecompressionStream {
      #handle;
      #transform;

        constructor(format) {
        format = formatConverter(format, {
          prefix: "Failed to construct 'DecompressionStream'",
          context: '1st argument',
        });
        switch (format) {
          case 'deflate':
            this.#handle = lazyZlib().createInflate({
              rejectGarbageAfterEnd: true,
            });
            break;
          case 'deflate-raw':
            this.#handle = lazyZlib().createInflateRaw({
              rejectGarbageAfterEnd: true,
            });
            break;
          case 'gzip':
            this.#handle = lazyZlib().createGunzip({
              rejectGarbageAfterEnd: true,
            });
            break;
          case 'brotli':
            this.#handle = lazyZlib().createBrotliDecompress({
              rejectGarbageAfterEnd: true,
            });
            break;
        }
        this.#transform = newReadableWritablePairFromDuplex(this.#handle, {
          [kValidateChunk]: validateBufferSourceChunk,
          [kDestroyOnSyncError]: true,
        });
      }

        get readable() {
        return this.#transform.readable;
      }

        get writable() {
        return this.#transform.writable;
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, 'DecompressionStream', {
          readable: this.#transform.readable,
          writable: this.#transform.writable,
        });
      }
    }

    Object.defineProperties(CompressionStream.prototype, {
      readable: kEnumerableProperty,
      writable: kEnumerableProperty,
      [SymbolToStringTag]: {
        __proto__: null,
        configurable: true,
        value: 'CompressionStream',
      },
    });

    Object.defineProperties(DecompressionStream.prototype, {
      readable: kEnumerableProperty,
      writable: kEnumerableProperty,
      [SymbolToStringTag]: {
        __proto__: null,
        configurable: true,
        value: 'DecompressionStream',
      },
    });

    module.exports = {
      CompressionStream,
      DecompressionStream,
    };
  },
  "internal/webstreams/encoding": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectDefineProperties,
      String,
      StringPrototypeCharCodeAt,
      StringPrototypeSlice,
      Uint8Array,
    } = primordials;

    const {
      TextDecoder,
      TextEncoder,
    } = require('internal/encoding');

    const {
      TransformStream,
    } = require('internal/webstreams/transformstream');

    const { customInspect } = require('internal/webstreams/util');

    const {
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_THIS,
      },
    } = require('internal/errors');

    const {
      customInspectSymbol: kInspect,
      kEmptyObject,
      kEnumerableProperty,
    } = require('internal/util');

    const kDecodeStreamingOptions = { __proto__: null, stream: true };

    class TextEncoderStream {
      #pendingHighSurrogate = null;
      #handle;
      #transform;

      constructor() {
        this.#handle = new TextEncoder();
        this.#transform = new TransformStream({
          transform: (chunk, controller) => {
            chunk = String(chunk);
            if (chunk.length === 0)
              return;
            if (this.#pendingHighSurrogate !== null) {
              chunk = this.#pendingHighSurrogate + chunk;
              this.#pendingHighSurrogate = null;
            }
            const lastCodeUnit =
              chunk.charCodeAt(chunk.length - 1);
            if (0xD800 <= lastCodeUnit && lastCodeUnit <= 0xDBFF) {
              this.#pendingHighSurrogate =
                chunk.slice(-1);
              chunk = chunk.slice(0, -1);
              if (chunk.length === 0)
                return;
            }
            controller.enqueue(this.#handle.encode(chunk));
          },
          flush: (controller) => {
            if (this.#pendingHighSurrogate !== null) {
              controller.enqueue(new Uint8Array([0xEF, 0xBF, 0xBD]));
            }
          },
        });
      }

        get encoding() {
        return this.#handle.encoding;
      }

        get readable() {
        return this.#transform.readable;
      }

        get writable() {
        return this.#transform.writable;
      }

      [kInspect](depth, options) {
        if (this == null)
          throw new ERR_INVALID_THIS('TextEncoderStream');
        return customInspect(depth, options, 'TextEncoderStream', {
          encoding: this.#handle.encoding,
          readable: this.#transform.readable,
          writable: this.#transform.writable,
        });
      }
    }

    class TextDecoderStream {
      #handle;
      #transform;

        constructor(encoding = 'utf-8', options = kEmptyObject) {
        this.#handle = new TextDecoder(encoding, options);
        this.#transform = new TransformStream({
          transform: (chunk, controller) => {
            if (chunk === undefined) {
              throw new ERR_INVALID_ARG_TYPE('chunk', 'string', chunk);
            }
            const value = this.#handle.decode(chunk, kDecodeStreamingOptions);
            if (value)
              controller.enqueue(value);
          },
          flush: (controller) => {
            const value = this.#handle.decode();
            if (value)
              controller.enqueue(value);
            controller.terminate();
          },
        });
      }

        get encoding() {
        return this.#handle.encoding;
      }

        get fatal() {
        return this.#handle.fatal;
      }

        get ignoreBOM() {
        return this.#handle.ignoreBOM;
      }

        get readable() {
        return this.#transform.readable;
      }

        get writable() {
        return this.#transform.writable;
      }

      [kInspect](depth, options) {
        if (this == null)
          throw new ERR_INVALID_THIS('TextDecoderStream');
        return customInspect(depth, options, 'TextDecoderStream', {
          encoding: this.#handle.encoding,
          fatal: this.#handle.fatal,
          ignoreBOM: this.#handle.ignoreBOM,
          readable: this.#transform.readable,
          writable: this.#transform.writable,
        });
      }
    }

    Object.defineProperties(TextEncoderStream.prototype, {
      encoding: kEnumerableProperty,
      readable: kEnumerableProperty,
      writable: kEnumerableProperty,
    });

    Object.defineProperties(TextDecoderStream.prototype, {
      encoding: kEnumerableProperty,
      fatal: kEnumerableProperty,
      ignoreBOM: kEnumerableProperty,
      readable: kEnumerableProperty,
      writable: kEnumerableProperty,
    });

    module.exports = {
      TextEncoderStream,
      TextDecoderStream,
    };
  },
  "internal/webstreams/queuingstrategies": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ObjectDefineProperties,
      ObjectDefineProperty,
      SymbolToStringTag,
    } = primordials;

    const {
      codes: {
        ERR_MISSING_OPTION,
      },
    } = require('internal/errors');

    const {
      customInspectSymbol: kInspect,
      kEnumerableProperty,
    } = require('internal/util');

    const {
      customInspect,
      getNonWritablePropertyDescriptor,
    } = require('internal/webstreams/util');

    const {
      validateObject,
    } = require('internal/validators');

    const nameDescriptor = { __proto__: null, value: 'size' };
    const byteSizeFunction = Object.defineProperty(
      (chunk) => chunk.byteLength,
      'name',
      nameDescriptor,
    );
    const countSizeFunction = Object.defineProperty(() => 1, 'name', nameDescriptor);

    class ByteLengthQueuingStrategy {
      #state;
      #byteSizeFunction = byteSizeFunction;

        constructor(init) {
        validateObject(init, 'init');
        if (init.highWaterMark === undefined)
          throw new ERR_MISSING_OPTION('init.highWaterMark');

        this.#state = {
          highWaterMark: +init.highWaterMark,
        };
      }

        get highWaterMark() {
        return this.#state.highWaterMark;
      }

        get size() {
        return this.#byteSizeFunction;
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, 'ByteLengthQueuingStrategy', {
          highWaterMark: this.highWaterMark,
        });
      }
    }

    Object.defineProperties(ByteLengthQueuingStrategy.prototype, {
      highWaterMark: kEnumerableProperty,
      size: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(ByteLengthQueuingStrategy.name),
    });

    class CountQueuingStrategy {
      #state;
      #countSizeFunction = countSizeFunction;

        constructor(init) {
        validateObject(init, 'init');
        if (init.highWaterMark === undefined)
          throw new ERR_MISSING_OPTION('init.highWaterMark');

        this.#state = {
          highWaterMark: +init.highWaterMark,
        };
      }

        get highWaterMark() {
        return this.#state.highWaterMark;
      }

        get size() {
        return this.#countSizeFunction;
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, 'CountQueuingStrategy', {
          highWaterMark: this.highWaterMark,
        });
      }
    }

    Object.defineProperties(CountQueuingStrategy.prototype, {
      highWaterMark: kEnumerableProperty,
      size: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(CountQueuingStrategy.name),
    });

    module.exports = {
      ByteLengthQueuingStrategy,
      CountQueuingStrategy,
    };
  },
  "internal/webstreams/readablestream": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayBuffer,
      ArrayBufferPrototypeGetByteLength,
      ArrayBufferPrototypeGetDetached,
      ArrayBufferPrototypeSlice,
      ArrayBufferPrototypeTransfer,
      ArrayPrototypePush,
      AsyncIteratorPrototype,
      DataView,
      FunctionPrototypeBind,
      FunctionPrototypeCall,
      MathMin,
      NumberIsInteger,
      ObjectDefineProperties,
      ObjectSetPrototypeOf,
      Promise,
      PromisePrototypeThen,
      PromiseReject,
      PromiseResolve,
      PromiseWithResolvers,
      SafePromiseAll,
      Symbol,
      SymbolAsyncIterator,
      SymbolDispose,
      SymbolIterator,
      SymbolToStringTag,
      TypedArrayPrototypeGetLength,
      Uint8Array,
    } = primordials;

    const {
      AbortError,
      codes: {
        ERR_ARG_NOT_ITERABLE,
        ERR_ILLEGAL_CONSTRUCTOR,
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_ARG_VALUE,
        ERR_INVALID_STATE,
        ERR_INVALID_THIS,
        ERR_OUT_OF_RANGE,
      },
    } = require('internal/errors');

    const {
      DOMException,
    } = internalBinding('messaging');

    const {
      isArrayBufferView,
      isDataView,
      isSharedArrayBuffer,
    } = require('internal/util/types');

    const {
      customInspectSymbol: kInspect,
      kEmptyObject,
      kEnumerableProperty,
      SideEffectFreeRegExpPrototypeSymbolReplace,
    } = require('internal/util');

    const {
      validateAbortSignal,
      validateBuffer,
      validateNumber,
      validateObject,
      kValidateObjectAllowObjects,
      kValidateObjectAllowObjectsAndNull,
    } = require('internal/validators');

    const {
      MessageChannel,
    } = require('internal/worker/io');

    const {
      kDeserialize,
      kTransfer,
      kTransferList,
      markTransferMode,
      structuredClone,
    } = require('internal/worker/js_transferable');

    const {
      queueMicrotask,
    } = require('internal/process/task_queues');

    const {
      kIsDisturbed,
      kIsErrored,
      kIsReadable,
      kIsClosedPromise,
      kControllerErrorFunction,
    } = require('internal/streams/utils');

    const {
      ArrayBufferViewGetBuffer,
      ArrayBufferViewGetByteLength,
      ArrayBufferViewGetByteOffset,
      Queue,
      canCopyArrayBuffer,
      cloneAsUint8Array,
      copyArrayBuffer,
      createPromiseCallback1Param,
      createRawCallback1Param,
      customInspect,
      defaultSizeAlgorithm,
      dequeueValue,
      enqueueValueWithSize,
      extractHighWaterMark,
      extractSizeAlgorithm,
      getNonWritablePropertyDescriptor,
      isBrandCheck,
      kEmptyQueue,
      kParkedAlgorithmResult,
      kResolvedPromise,
      kState,
      kType,
      lazyTransfer,
      materializeQueue,
      nonOpCallback,
      nonOpCancel,
      rejectedHandledRecord,
      resetQueue,
      resolvedRecord,
      setPromiseHandled,
      thenAlgorithmResult,
    } = require('internal/webstreams/util');

    const {
      WritableStreamDefaultWriter,

      isWritableStream,
      isWritableStreamLocked,
      isWritableStreamDefaultController,
      isWritableStreamDefaultWriter,

      writableStreamAbort,
      writableStreamCloseQueuedOrInFlight,
      writableStreamDefaultWriterCloseWithErrorPropagation,
      writableStreamDefaultWriterRelease,
      writableStreamDefaultWriterWriteWithRequest,
      writerClosedPromise,
    } = require('internal/webstreams/writablestream');

    const { Buffer } = require('buffer');

    const assert = require('internal/assert');

    const kCancel = Symbol('kCancel');
    const kClose = Symbol('kClose');
    const kChunk = Symbol('kChunk');
    const kError = Symbol('kError');
    const kPull = Symbol('kPull');
    const kRelease = Symbol('kRelease');
    const kSkipThrow = Symbol('kSkipThrow');

    let releasedError;
    let releasingError;
    let addAbortListener;

    const userModuleRegExp = /^ {4}at (?:[^/\\(]+ \()(?!node:(.+):\d+:\d+\)$).*/gm;

    function lazyReadableReleasedError() {
      if (releasedError) {
        return releasedError;
      }

      releasedError = new ERR_INVALID_STATE.TypeError('Reader released');
      releasedError.stack = releasedError.stack.replace(userModuleRegExp, '');
      return releasedError;
    }

    function lazyReadableReleasingError() {
      if (releasingError) {
        return releasingError;
      }
      releasingError = new ERR_INVALID_STATE.TypeError('Releasing reader');
      releasingError.stack = releasingError.stack.replace(userModuleRegExp, '');
      return releasingError;
    }

    class ReadableStream {
      [kType] = 'ReadableStream';

        constructor(source = kEmptyObject, strategy = kEmptyObject) {
        markTransferMode(this, false, true);
        if (source === kSkipThrow) {
          this[kState] = createReadableStreamState();
          return;
        }
        validateObject(source, 'source', kValidateObjectAllowObjects);
        validateObject(strategy, 'strategy', kValidateObjectAllowObjectsAndNull);
        this[kState] = createReadableStreamState();

        const size = strategy?.size;
        const highWaterMark = strategy?.highWaterMark;
        const type = source.type;

        if (`${type}` === 'bytes') {
          if (size !== undefined)
            throw new ERR_INVALID_ARG_VALUE.RangeError('strategy.size', size);
          setupReadableByteStreamControllerFromSource(
            this,
            source,
            extractHighWaterMark(highWaterMark, 0));
        } else {
          if (type !== undefined)
            throw new ERR_INVALID_ARG_VALUE('source.type', type);
          setupReadableStreamDefaultControllerFromSource(
            this,
            source,
            extractHighWaterMark(highWaterMark, 1),
            extractSizeAlgorithm(size));
        }
      }

      get [kIsDisturbed]() {
        return this[kState].disturbed;
      }

      get [kIsErrored]() {
        return this[kState].state === 'errored';
      }

      get [kIsReadable]() {
        return this[kState].state === 'readable';
      }

      [kControllerErrorFunction](error) {
        const controller = this[kState].controller;
        if (isReadableStreamDefaultController(controller))
          controller.error(error);
      }

      get [kIsClosedPromise]() {
        let cache = this[kState].closedPromise;
        if (cache === undefined) {
          switch (this[kState].state) {
            case 'readable':
              cache = PromiseWithResolvers();
              break;
            case 'closed':
              cache = resolvedRecord();
              break;
            default:
              cache = rejectedHandledRecord(this[kState].storedError);
          }
          this[kState].closedPromise = cache;
        }
        return cache;
      }

        get locked() {
        if (!isReadableStream(this))
          throw new ERR_INVALID_THIS('ReadableStream');
        return isReadableStreamLocked(this);
      }

      static from(iterable) {
        return readableStreamFromIterable(iterable);
      }

        cancel(reason = undefined) {
        if (!isReadableStream(this))
          return Promise.reject(new ERR_INVALID_THIS('ReadableStream'));
        if (isReadableStreamLocked(this)) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError('ReadableStream is locked'));
        }
        return readableStreamCancel(this, reason);
      }

        getReader(options = kEmptyObject) {
        if (!isReadableStream(this))
          throw new ERR_INVALID_THIS('ReadableStream');
        validateObject(options, 'options', kValidateObjectAllowObjectsAndNull);
        const mode = options?.mode;

        if (mode === undefined)
          return new ReadableStreamDefaultReader(this);

        if (`${mode}` !== 'byob')
          throw new ERR_INVALID_ARG_VALUE('options.mode', mode);
        return new ReadableStreamBYOBReader(this);
      }

        pipeThrough(transform, options = kEmptyObject) {
        if (!isReadableStream(this))
          throw new ERR_INVALID_THIS('ReadableStream');
        const readable = transform?.readable;
        if (!isReadableStream(readable)) {
          throw new ERR_INVALID_ARG_TYPE(
            'transform.readable',
            'ReadableStream',
            readable);
        }
        const writable = transform?.writable;
        if (!isWritableStream(writable)) {
          throw new ERR_INVALID_ARG_TYPE(
            'transform.writable',
            'WritableStream',
            writable);
        }

        validateObject(options, 'options', kValidateObjectAllowObjectsAndNull);
        const preventAbort = options?.preventAbort;
        const preventCancel = options?.preventCancel;
        const preventClose = options?.preventClose;
        const signal = options?.signal;

        if (signal !== undefined) {
          validateAbortSignal(signal, 'options.signal');
        }

        if (isReadableStreamLocked(this))
          throw new ERR_INVALID_STATE.TypeError('The ReadableStream is locked');
        if (isWritableStreamLocked(writable))
          throw new ERR_INVALID_STATE.TypeError('The WritableStream is locked');

        const promise = readableStreamPipeTo(
          this,
          writable,
          !!preventClose,
          !!preventAbort,
          !!preventCancel,
          signal);
        setPromiseHandled(promise);

        return readable;
      }

        pipeTo(destination, options = kEmptyObject) {
        try {
          if (!isReadableStream(this))
            throw new ERR_INVALID_THIS('ReadableStream');
          if (!isWritableStream(destination)) {
            throw new ERR_INVALID_ARG_TYPE(
              'transform.writable',
              'WritableStream',
              destination);
          }

          validateObject(options, 'options', kValidateObjectAllowObjectsAndNull);
          const preventAbort = options?.preventAbort;
          const preventCancel = options?.preventCancel;
          const preventClose = options?.preventClose;
          const signal = options?.signal;

          if (signal !== undefined) {
            validateAbortSignal(signal, 'options.signal');
          }

          if (isReadableStreamLocked(this))
            throw new ERR_INVALID_STATE.TypeError('The ReadableStream is locked');
          if (isWritableStreamLocked(destination))
            throw new ERR_INVALID_STATE.TypeError('The WritableStream is locked');

          return readableStreamPipeTo(
            this,
            destination,
            !!preventClose,
            !!preventAbort,
            !!preventCancel,
            signal);
        } catch (error) {
          return Promise.reject(error);
        }
      }

        tee() {
        if (!isReadableStream(this))
          throw new ERR_INVALID_THIS('ReadableStream');
        return readableStreamTee(this, false);
      }

        values(options = kEmptyObject) {
        if (!isReadableStream(this))
          throw new ERR_INVALID_THIS('ReadableStream');
        validateObject(options, 'options', kValidateObjectAllowObjectsAndNull);
        const preventCancel = !!(options?.preventCancel);

        const reader = new ReadableStreamDefaultReader(this);
        const state = new ReadableStreamAsyncIteratorReadRequest(reader, preventCancel);
        return new ReadableStreamAsyncIterator(state);
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          locked: this.locked,
          state: this[kState].state,
          supportsBYOB:
            this[kState].controller instanceof ReadableByteStreamController,
        });
      }

      [kTransfer]() {
        if (!isReadableStream(this))
          throw new ERR_INVALID_THIS('ReadableStream');
        if (this.locked) {
          this[kState].transfer.port1?.close();
          this[kState].transfer.port1 = undefined;
          this[kState].transfer.port2 = undefined;
          throw new DOMException(
            'Cannot transfer a locked ReadableStream',
            'DataCloneError');
        }

        const {
          writable,
          promise,
        } = lazyTransfer().newCrossRealmWritableSink(
          this,
          this[kState].transfer.port1);

        this[kState].transfer.writable = writable;
        this[kState].transfer.promise = promise;

        return {
          data: { port: this[kState].transfer.port2 },
          deserializeInfo:
            'internal/webstreams/readablestream:TransferredReadableStream',
        };
      }

      [kTransferList]() {
        const { port1, port2 } = new MessageChannel();
        this[kState].transfer.port1 = port1;
        this[kState].transfer.port2 = port2;
        return [ port2 ];
      }

      [kDeserialize]({ port }) {
        const transfer = lazyTransfer();
        setupReadableStreamDefaultControllerFromSource(
          this,
          new transfer.CrossRealmTransformReadableSource(port, true),
          0, defaultSizeAlgorithm);
      }
    }

    Object.defineProperties(ReadableStream.prototype, {
      [SymbolAsyncIterator]: {
        __proto__: null,
        configurable: true,
        enumerable: false,
        writable: true,
        value: ReadableStream.prototype.values,
      },
      locked: kEnumerableProperty,
      cancel: kEnumerableProperty,
      getReader: kEnumerableProperty,
      pipeThrough: kEnumerableProperty,
      pipeTo: kEnumerableProperty,
      tee: kEnumerableProperty,
      values: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(ReadableStream.name),
    });
    Object.defineProperties(ReadableStream, {
      from: kEnumerableProperty,
    });

    function TransferredReadableStream() {
      return new ReadableStream(kSkipThrow);
    }

    TransferredReadableStream.prototype[kDeserialize] = () => {};

    class ReadableStreamBYOBRequest {
      [kType] = 'ReadableStreamBYOBRequest';

      constructor(skipThrowSymbol = undefined) {
        if (skipThrowSymbol !== kSkipThrow) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
      }

        get view() {
        if (!isReadableStreamBYOBRequest(this))
          throw new ERR_INVALID_THIS('ReadableStreamBYOBRequest');
        return this[kState].view;
      }

        respond(bytesWritten) {
        if (!isReadableStreamBYOBRequest(this))
          throw new ERR_INVALID_THIS('ReadableStreamBYOBRequest');
        const {
          view,
          controller,
        } = this[kState];
        if (controller === undefined) {
          throw new ERR_INVALID_STATE.TypeError(
            'This BYOB request has been invalidated');
        }

        const viewByteLength = ArrayBufferViewGetByteLength(view);
        const viewBuffer = ArrayBufferViewGetBuffer(view);
        const viewBufferByteLength = ArrayBufferPrototypeGetByteLength(viewBuffer);

        if (ArrayBufferPrototypeGetDetached(viewBuffer)) {
          throw new ERR_INVALID_STATE.TypeError('Viewed ArrayBuffer is detached');
        }

        assert(viewByteLength > 0);
        assert(viewBufferByteLength > 0);

        readableByteStreamControllerRespond(controller, bytesWritten);
      }

        respondWithNewView(view) {
        if (!isReadableStreamBYOBRequest(this))
          throw new ERR_INVALID_THIS('ReadableStreamBYOBRequest');
        const {
          controller,
        } = this[kState];

        if (controller === undefined) {
          throw new ERR_INVALID_STATE.TypeError(
            'This BYOB request has been invalidated');
        }

        validateBuffer(view, 'view');

        if (ArrayBufferPrototypeGetDetached(view.buffer)) {
          throw new ERR_INVALID_STATE.TypeError('Viewed ArrayBuffer is detached');
        }

        readableByteStreamControllerRespondWithNewView(controller, view);
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          view: this.view,
          controller: this[kState].controller,
        });
      }
    }

    Object.defineProperties(ReadableStreamBYOBRequest.prototype, {
      view: kEnumerableProperty,
      respond: kEnumerableProperty,
      respondWithNewView: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(ReadableStreamBYOBRequest.name),
    });

    function createReadableStreamBYOBRequest(controller, view) {
      const stream = new ReadableStreamBYOBRequest(kSkipThrow);

      stream[kState] = {
        controller,
        view,
      };

      return stream;
    }

    class ReadableStreamAsyncIteratorReadRequest {
      constructor(reader, preventCancel) {
        this.reader = reader;
        this.preventCancel = preventCancel;
        this.done = false;
        this.started = false;
        this.current = undefined;
        this.promise = undefined;
        this.chainedNextSteps = undefined;
      }

      [kChunk](chunk) {
        this.current = undefined;
        this.promise.resolve({ done: false, value: chunk });
      }

      [kClose]() {
        this.current = undefined;
        this.done = true;
        readableStreamReaderGenericRelease(this.reader);
        this.promise.resolve({ done: true, value: undefined });
      }

      [kError](error) {
        this.current = undefined;
        this.done = true;
        readableStreamReaderGenericRelease(this.reader);
        this.promise.reject(error);
      }
    }

    function readableStreamAsyncIteratorNextSteps(state) {
      if (state.done)
        return Promise.resolve({ done: true, value: undefined });

      const reader = state.reader;
      if (reader[kState].stream === undefined) {
        return Promise.reject(
          new ERR_INVALID_STATE.TypeError(
            'The reader is not bound to a ReadableStream'));
      }
      const promise = PromiseWithResolvers();

      state.promise = promise;
      readableStreamDefaultReaderRead(reader, state);
      return promise.promise;
    }

    function readableStreamAsyncIteratorReturnSteps(state, value) {
      const iterResult = { done: true, value };
      if (state.done)
        return Promise.resolve(iterResult);
      state.done = true;

      try {
        const reader = state.reader;
        if (reader[kState].stream === undefined) {
          throw new ERR_INVALID_STATE.TypeError(
            'The reader is not bound to a ReadableStream');
        }
        assert(!reader[kState].readRequests.length);
        if (!state.preventCancel) {
          const result = readableStreamReaderGenericCancel(reader, value);
          readableStreamReaderGenericRelease(reader);
          return PromisePrototypeThen(result, () => iterResult);
        }

        readableStreamReaderGenericRelease(reader);
        return Promise.resolve(iterResult);
      } catch (error) {
        return Promise.reject(error);
      }
    }

    class ReadableStreamAsyncIterator {
      #state;

      constructor(state) {
        this.#state = state;
      }

      next() {
        if (typeof this !== 'object' || this === null || !(#state in this)) {
          return Promise.reject(
            new ERR_INVALID_THIS('ReadableStreamAsyncIterator'));
        }
        const state = this.#state;
        if (!state.started) {
          state.current = kResolvedPromise;
          state.started = true;
        }
        if (state.current !== undefined) {
          let steps = state.chainedNextSteps;
          if (steps === undefined) {
            steps = state.chainedNextSteps =
              () => readableStreamAsyncIteratorNextSteps(state);
          }
          state.current = PromisePrototypeThen(state.current, steps, steps);
          return state.current;
        }
        const stream = state.reader[kState].stream;
        if (!state.done && stream !== undefined &&
            stream[kState].state === 'readable') {
          const controller = stream[kState].controller;
          if (isReadableStreamDefaultController(controller)) {
            if (controller[kState].queue.length > 0) {
              stream[kState].disturbed = true;
              const chunk = dequeueValue(controller);

              if (controller[kState].closeRequested &&
                  !controller[kState].queue.length) {
                readableStreamDefaultControllerClearAlgorithms(controller);
                readableStreamClose(stream);
              } else if (!controller[kState].closeRequested &&
                         controller[kState].started &&
                         controller[kState].highWaterMark -
                           controller[kState].queueTotalSize > 0) {
                readableStreamDefaultControllerPull(controller);
              }

              return Promise.resolve({ done: false, value: chunk });
            }
          } else if (controller[kState].queueTotalSize > 0) {
            stream[kState].disturbed = true;
            return Promise.resolve({
              done: false,

              value: readableByteStreamControllerDequeueChunk(controller),
            });
          }
        }
        state.current = readableStreamAsyncIteratorNextSteps(state);
        return state.current;
      }

      return(value) {
        if (typeof this !== 'object' || this === null || !(#state in this)) {
          return Promise.reject(
            new ERR_INVALID_THIS('ReadableStreamAsyncIterator'));
        }
        const state = this.#state;
        state.started = true;
        state.current = state.current !== undefined ?
          PromisePrototypeThen(
            state.current,
            () => readableStreamAsyncIteratorReturnSteps(state, value),
            () => readableStreamAsyncIteratorReturnSteps(state, value)) :
          readableStreamAsyncIteratorReturnSteps(state, value);
        return state.current;
      }
    }

    delete ReadableStreamAsyncIterator.prototype.constructor;
    Object.setPrototypeOf(ReadableStreamAsyncIterator.prototype,
                         AsyncIteratorPrototype);
    Object.defineProperties(ReadableStreamAsyncIterator.prototype, {
      next: kEnumerableProperty,
      return: kEnumerableProperty,
      [SymbolToStringTag]:
        getNonWritablePropertyDescriptor('ReadableStream AsyncIterator'),
    });

    class DefaultReadRequest {
      constructor() {
        this[kState] = PromiseWithResolvers();
      }

      [kChunk](value) {
        this[kState].resolve({ done: false, value });
      }

      [kClose]() {
        this[kState].resolve({ done: true, value: undefined });
      }

      [kError](error) {
        this[kState].reject(error);
      }

      get promise() { return this[kState].promise; }
    }

    class StepsReadRequest {
      constructor(chunkSteps, closeSteps, errorSteps) {
        this.chunkSteps = chunkSteps;
        this.closeSteps = closeSteps;
        this.errorSteps = errorSteps;
      }

      [kChunk](chunk) {
        this.chunkSteps(chunk);
      }

      [kClose](chunk) {
        this.closeSteps(chunk);
      }

      [kError](error) {
        this.errorSteps(error);
      }
    }

    class ReadIntoRequest {
      constructor() {
        this[kState] = PromiseWithResolvers();
      }

      [kChunk](value) {
        this[kState].resolve({ done: false, value });
      }

      [kClose](value) {
        this[kState].resolve({ done: true, value });
      }

      [kError](error) {
        this[kState].reject(error);
      }

      get promise() { return this[kState].promise; }
    }

    class ReadableStreamDefaultReader {
      [kType] = 'ReadableStreamDefaultReader';

        constructor(stream) {
        if (!isReadableStream(stream))
          throw new ERR_INVALID_ARG_TYPE('stream', 'ReadableStream', stream);
        this[kState] = {
          readRequests: undefined,
          stream: undefined,
          close: undefined,
        };
        setupReadableStreamDefaultReader(this, stream);
      }

        read() {
        if (!isReadableStreamDefaultReader(this))
          return Promise.reject(new ERR_INVALID_THIS('ReadableStreamDefaultReader'));
        if (this[kState].stream === undefined) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError(
              'The reader is not attached to a stream'));
        }

        const stream = this[kState].stream;
        const controller = stream[kState].controller;

        if (stream[kState].state === 'readable') {
          if (isReadableStreamDefaultController(controller)) {
            if (controller[kState].queue.length > 0) {
              stream[kState].disturbed = true;
              const chunk = dequeueValue(controller);

              if (controller[kState].closeRequested && !controller[kState].queue.length) {
                readableStreamDefaultControllerClearAlgorithms(controller);
                readableStreamClose(stream);
              } else if (!controller[kState].closeRequested &&
                         controller[kState].started &&
                         controller[kState].highWaterMark -
                           controller[kState].queueTotalSize > 0) {
                readableStreamDefaultControllerPull(controller);
              }

              return Promise.resolve({ done: false, value: chunk });
            }
          } else if (controller[kState].queueTotalSize > 0) {
            stream[kState].disturbed = true;
            return Promise.resolve({
              done: false,

              value: readableByteStreamControllerDequeueChunk(controller),
            });
          }
        }

        const readRequest = new DefaultReadRequest();
        readableStreamDefaultReaderRead(this, readRequest);
        return readRequest.promise;
      }

      releaseLock() {
        if (!isReadableStreamDefaultReader(this))
          throw new ERR_INVALID_THIS('ReadableStreamDefaultReader');
        if (this[kState].stream === undefined)
          return;
        readableStreamDefaultReaderRelease(this);
      }

        get closed() {
        if (!isReadableStreamDefaultReader(this))
          return Promise.reject(new ERR_INVALID_THIS('ReadableStreamDefaultReader'));
        return readerClosedPromise(this).promise;
      }

        cancel(reason = undefined) {
        if (!isReadableStreamDefaultReader(this))
          return Promise.reject(new ERR_INVALID_THIS('ReadableStreamDefaultReader'));
        if (this[kState].stream === undefined) {
          return Promise.reject(new ERR_INVALID_STATE.TypeError(
            'The reader is not attached to a stream'));
        }
        return readableStreamReaderGenericCancel(this, reason);
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          stream: this[kState].stream,
          readRequests: this[kState].readRequests.length,
          close: readerClosedPromise(this).promise,
        });
      }
    }

    Object.defineProperties(ReadableStreamDefaultReader.prototype, {
      closed: kEnumerableProperty,
      read: kEnumerableProperty,
      releaseLock: kEnumerableProperty,
      cancel: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(ReadableStreamDefaultReader.name),
    });

    class ReadableStreamBYOBReader {
      [kType] = 'ReadableStreamBYOBReader';

        constructor(stream) {
        if (!isReadableStream(stream))
          throw new ERR_INVALID_ARG_TYPE('stream', 'ReadableStream', stream);
        this[kState] = {
          stream: undefined,
          readIntoRequests: undefined,
          close: undefined,
        };
        setupReadableStreamBYOBReader(this, stream);
      }

        read(view, options = kEmptyObject) {
        try {
          return readableStreamBYOBReaderReadView(this, view, options);
        } catch (error) {
          return Promise.reject(error);
        }
      }

      releaseLock() {
        if (!isReadableStreamBYOBReader(this))
          throw new ERR_INVALID_THIS('ReadableStreamBYOBReader');
        if (this[kState].stream === undefined)
          return;
        readableStreamBYOBReaderRelease(this);
      }

        get closed() {
        if (!isReadableStreamBYOBReader(this))
          return Promise.reject(new ERR_INVALID_THIS('ReadableStreamBYOBReader'));
        return readerClosedPromise(this).promise;
      }

        cancel(reason = undefined) {
        if (!isReadableStreamBYOBReader(this))
          return Promise.reject(new ERR_INVALID_THIS('ReadableStreamBYOBReader'));
        if (this[kState].stream === undefined) {
          return Promise.reject(new ERR_INVALID_STATE.TypeError(
            'The reader is not attached to a stream'));
        }
        return readableStreamReaderGenericCancel(this, reason);
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          stream: this[kState].stream,
          readIntoRequests: this[kState].readIntoRequests.length,
          close: readerClosedPromise(this).promise,
        });
      }
    }

    Object.defineProperties(ReadableStreamBYOBReader.prototype, {
      closed: kEnumerableProperty,
      read: kEnumerableProperty,
      releaseLock: kEnumerableProperty,
      cancel: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(ReadableStreamBYOBReader.name),
    });

    class ReadableStreamDefaultController {
      [kType] = 'ReadableStreamDefaultController';
      [kState];

      constructor(skipThrowSymbol = undefined) {
        if (skipThrowSymbol !== kSkipThrow) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
      }

        get desiredSize() {
        return readableStreamDefaultControllerGetDesiredSize(this);
      }

      close() {
        if (!readableStreamDefaultControllerCanCloseOrEnqueue(this))
          throw new ERR_INVALID_STATE.TypeError('Controller is already closed');
        readableStreamDefaultControllerClose(this);
      }

        enqueue(chunk = undefined) {
        if (!readableStreamDefaultControllerCanCloseOrEnqueue(this))
          throw new ERR_INVALID_STATE.TypeError('Controller is already closed');
        readableStreamDefaultControllerEnqueue(this, chunk);
      }

        error(error = undefined) {
        readableStreamDefaultControllerError(this, error);
      }

      [kCancel](reason) {
        return readableStreamDefaultControllerCancelSteps(this, reason);
      }

      [kPull](readRequest) {
        readableStreamDefaultControllerPullSteps(this, readRequest);
      }

      [kRelease]() {}

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], { });
      }
    }

    Object.defineProperties(ReadableStreamDefaultController.prototype, {
      desiredSize: kEnumerableProperty,
      close: kEnumerableProperty,
      enqueue: kEnumerableProperty,
      error: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(ReadableStreamDefaultController.name),
    });

    class ReadableByteStreamController {
      [kType] = 'ReadableByteStreamController';
      [kState];

      constructor(skipThrowSymbol = undefined) {
        if (skipThrowSymbol !== kSkipThrow) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
      }

        get byobRequest() {
        if (!isReadableByteStreamController(this))
          throw new ERR_INVALID_THIS('ReadableByteStreamController');
        if (this[kState].byobRequest === null &&
            this[kState].pendingPullIntos.length) {
          const {
            buffer,
            byteOffset,
            bytesFilled,
            byteLength,
          } = this[kState].pendingPullIntos.peek();
          const view =
            new Uint8Array(
              buffer,
              byteOffset + bytesFilled,
              byteLength - bytesFilled);
          this[kState].byobRequest = createReadableStreamBYOBRequest(this, view);
        }
        return this[kState].byobRequest;
      }

        get desiredSize() {
        if (!isReadableByteStreamController(this))
          throw new ERR_INVALID_THIS('ReadableByteStreamController');
        return readableByteStreamControllerGetDesiredSize(this);
      }

      close() {
        if (!isReadableByteStreamController(this))
          throw new ERR_INVALID_THIS('ReadableByteStreamController');
        if (this[kState].closeRequested)
          throw new ERR_INVALID_STATE.TypeError('Controller is already closed');
        if (this[kState].stream[kState].state !== 'readable')
          throw new ERR_INVALID_STATE.TypeError('ReadableStream is already closed');
        readableByteStreamControllerClose(this);
      }

        enqueue(chunk) {
        if (!isReadableByteStreamController(this))
          throw new ERR_INVALID_THIS('ReadableByteStreamController');
        validateBuffer(chunk);
        const chunkByteLength = ArrayBufferViewGetByteLength(chunk);
        const chunkBuffer = ArrayBufferViewGetBuffer(chunk);

        if (isSharedArrayBuffer(chunkBuffer)) {
          throw new ERR_INVALID_ARG_VALUE(
            'chunk',
            chunk,
            'must not be backed by a SharedArrayBuffer',
          );
        }

        const chunkBufferByteLength = ArrayBufferPrototypeGetByteLength(chunkBuffer);
        if (chunkByteLength === 0 || chunkBufferByteLength === 0) {
          throw new ERR_INVALID_STATE.TypeError(
            'chunk ArrayBuffer is zero-length or detached');
        }
        if (this[kState].closeRequested)
          throw new ERR_INVALID_STATE.TypeError('Controller is already closed');
        if (this[kState].stream[kState].state !== 'readable')
          throw new ERR_INVALID_STATE.TypeError('ReadableStream is already closed');
        readableByteStreamControllerEnqueue(this, chunk);
      }

        error(error = undefined) {
        if (!isReadableByteStreamController(this))
          throw new ERR_INVALID_THIS('ReadableByteStreamController');
        readableByteStreamControllerError(this, error);
      }

      [kCancel](reason) {
        return readableByteStreamControllerCancelSteps(this, reason);
      }

      [kPull](readRequest) {
        readableByteStreamControllerPullSteps(this, readRequest);
      }

      [kRelease]() {
        const {
          pendingPullIntos,
        } = this[kState];
        if (pendingPullIntos.length > 0) {
          const firstPendingPullInto = pendingPullIntos.peek();
          firstPendingPullInto.type = 'none';
          const queue = new Queue();
          queue.push(firstPendingPullInto);
          this[kState].pendingPullIntos = queue;
        }
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], { });
      }
    }

    Object.defineProperties(ReadableByteStreamController.prototype, {
      byobRequest: kEnumerableProperty,
      desiredSize: kEnumerableProperty,
      close: kEnumerableProperty,
      enqueue: kEnumerableProperty,
      error: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(ReadableByteStreamController.name),
    });

    function createReadableStream(start, pull, cancel, highWaterMark = 1, size = defaultSizeAlgorithm) {
      const stream = new ReadableStream(kSkipThrow);
      setupReadableStreamDefaultController(
        stream,
        new ReadableStreamDefaultController(kSkipThrow),
        start,
        pull,
        cancel,
        highWaterMark,
        size);
      return stream;
    }

    function createReadableByteStream(start, pull, cancel) {
      const stream = new ReadableStream(kSkipThrow);
      setupReadableByteStreamController(
        stream,
        new ReadableByteStreamController(kSkipThrow),
        start,
        pull,
        cancel,
        0,
        undefined);
      return stream;
    }

    const isReadableStream =
      isBrandCheck('ReadableStream');
    const isReadableByteStreamController =
      isBrandCheck('ReadableByteStreamController');
    const isReadableStreamDefaultController =
      isBrandCheck('ReadableStreamDefaultController');
    const isReadableStreamBYOBRequest =
      isBrandCheck('ReadableStreamBYOBRequest');
    const isReadableStreamDefaultReader =
      isBrandCheck('ReadableStreamDefaultReader');
    const isReadableStreamBYOBReader =
      isBrandCheck('ReadableStreamBYOBReader');

    class ReadableStreamTransferState {
      writable = undefined;
      port1 = undefined;
      port2 = undefined;
      promise = undefined;
    }
    Object.setPrototypeOf(ReadableStreamTransferState.prototype, null);

    class ReadableStreamState {
      closedPromise = undefined;
      disturbed = false;
      reader = undefined;
      state = 'readable';
      storedError = undefined;
      controller = undefined;
      transfer = new ReadableStreamTransferState();
    }
    Object.setPrototypeOf(ReadableStreamState.prototype, null);

    function createReadableStreamState() {
      return new ReadableStreamState();
    }

    function readableStreamFromIterable(iterable) {
      let stream;
      const iteratorGetter = iterable[SymbolAsyncIterator] ?? iterable[SymbolIterator];
      if (iteratorGetter == null || typeof iteratorGetter !== 'function') {
        throw new ERR_ARG_NOT_ITERABLE(iterable);
      }
      const iterator = FunctionPrototypeCall(iteratorGetter, iterable);
      if (iterator === null || (typeof iterator !== 'object' && typeof iterator !== 'function')) {
        throw new ERR_INVALID_STATE.TypeError('The iterator method must return an object');
      }
      const nextMethod = iterator.next;
      const startAlgorithm = nonOpCallback;

      let pendingIterResult;

      function rejectPull(error) {
        readableStreamDefaultControllerError(stream[kState].controller, error);
      }

      function processIterResult(iterResult) {
        const controller = stream[kState].controller;
        if (typeof iterResult !== 'object' || iterResult === null) {
          rejectPull(new ERR_INVALID_STATE.TypeError(
            'The promise returned by the iterator.next() method must fulfill with an object'));
          return;
        }
        try {
          if (iterResult.done) {
            readableStreamDefaultControllerClose(controller);
          } else {
            const value = iterResult.value;
            if (value !== null &&
                (typeof value === 'object' || typeof value === 'function')) {
              PromisePrototypeThen(Promise.resolve(value), enqueueValue, rejectPull);
              return;
            }
            readableStreamDefaultControllerEnqueue(controller, value);
          }
        } catch (error) {
          rejectPull(error);
          return;
        }
        controller[kState].pullFulfilled();
      }

      function enqueueValue(value) {
        const controller = stream[kState].controller;
        try {
          readableStreamDefaultControllerEnqueue(controller, value);
        } catch (error) {
          rejectPull(error);
          return;
        }
        controller[kState].pullFulfilled();
      }

      function processPendingIterResult() {
        const iterResult = pendingIterResult;
        pendingIterResult = undefined;
        processIterResult(iterResult);
      }

      function pullAlgorithm() {
        let nextResult;
        try {
          nextResult = FunctionPrototypeCall(nextMethod, iterator);
        } catch (error) {
          return Promise.reject(error);
        }
        if (nextResult !== null &&
            (typeof nextResult === 'object' || typeof nextResult === 'function')) {
          PromisePrototypeThen(
            Promise.resolve(nextResult), processIterResult, rejectPull);
          return kParkedAlgorithmResult;
        }
        pendingIterResult = nextResult;
        PromisePrototypeThen(kResolvedPromise, processPendingIterResult);
        return kParkedAlgorithmResult;
      }

      async function cancelAlgorithm(reason) {
        const returnMethod = iterator.return;
        if (returnMethod === undefined) {
          return;
        }
        const iterResult = await FunctionPrototypeCall(returnMethod, iterator, reason);
        if (typeof iterResult !== 'object' || iterResult === null) {
          throw new ERR_INVALID_STATE.TypeError(
            'The promise returned by the iterator.return() method must fulfill with an object');
        }
      }

      stream = createReadableStream(
        startAlgorithm,
        pullAlgorithm,
        cancelAlgorithm,
        0,
      );

      return stream;
    }

    function readableStreamPipeTo(
      source,
      dest,
      preventClose,
      preventAbort,
      preventCancel,
      signal) {

      let reader;
      let writer;
      let disposable;
      try {
        reader = new ReadableStreamDefaultReader(source);
        writer = new WritableStreamDefaultWriter(dest);
      } catch (error) {
        return Promise.reject(error);
      }

      source[kState].disturbed = true;

      let shuttingDown = false;

      if (signal !== undefined) {
        try {
          validateAbortSignal(signal, 'options.signal');
        } catch (error) {
          return Promise.reject(error);
        }
      }

      const promise = PromiseWithResolvers();

      const writeTracker = {
        promise: null,
        pending: 0,
        failed: false,
        failure: undefined,
        stall: null,
        resolve() {
          if (--this.pending === 0 && this.stall !== null) {
            const stall = this.stall;
            this.stall = null;
            if (this.failed)
              stall.reject(this.failure);
            else
              stall.resolve();
          }
        },
        reject(error) {
          this.pending--;
          this.failed = true;
          this.failure = error;
          if (this.stall !== null) {
            const stall = this.stall;
            this.stall = null;
            stall.reject(error);
          }
        },
      };

      function finalize(rejected, error) {
        writer[kState].ready = undefined;
        writer[kState].close = undefined;
        reader[kState].close = undefined;
        writableStreamDefaultWriterRelease(writer);
        readableStreamReaderGenericRelease(reader);
        if (signal !== undefined)
          disposable?.[SymbolDispose]();
        if (rejected)
          promise.reject(error);
        else
          promise.resolve();
      }

      function waitForPendingWrites() {
        if (writeTracker.pending === 0) {
          return writeTracker.failed ?
            Promise.reject(writeTracker.failure) :
            Promise.resolve();
        }
        writeTracker.stall = PromiseWithResolvers();
        return writeTracker.stall.promise;
      }

      function shutdownWithAnAction(action, rejected, originalError) {
        if (shuttingDown) return;
        shuttingDown = true;
        if (dest[kState].state === 'writable' &&
            !writableStreamCloseQueuedOrInFlight(dest)) {
          PromisePrototypeThen(
            waitForPendingWrites(),
            complete,
            (error) => finalize(true, error));
          return;
        }
        complete();

        function complete() {
          PromisePrototypeThen(
            action(),
            () => finalize(rejected, originalError),
            (error) => finalize(true, error));
        }
      }

      function shutdown(rejected, error) {
        if (shuttingDown) return;
        shuttingDown = true;
        if (dest[kState].state === 'writable' &&
            !writableStreamCloseQueuedOrInFlight(dest)) {
          PromisePrototypeThen(
            waitForPendingWrites(),
            () => finalize(rejected, error),
            (error) => finalize(true, error));
          return;
        }
        finalize(rejected, error);
      }

      function abortAlgorithm() {
        let error;
        if (signal.reason instanceof AbortError) {
          error = new DOMException(signal.reason.message, 'AbortError');
        } else {
          error = signal.reason;
        }

        const actions = [];
        if (!preventAbort) {
          actions.push(() => {
              if (dest[kState].state === 'writable')
                return writableStreamAbort(dest, error);
              return Promise.resolve();
            });
        }
        if (!preventCancel) {
          actions.push(() => {
              if (source[kState].state === 'readable')
                return readableStreamCancel(source, error);
              return Promise.resolve();
            });
        }

        shutdownWithAnAction(
          () => SafePromiseAll(actions, (action) => action()),
          true,
          error);
      }

      function watchErrored(stream, promise, action) {
        if (stream[kState].state === 'errored')
          action(stream[kState].storedError);
        else
          PromisePrototypeThen(promise, undefined, action);
      }

      let pendingChunk;
      let readRequest;
      let readyHook;

      function ignoreReadyRejection() {}

      function parkOnReady() {
        readyHook ??= {
          promise: new Promise(nonOpCallback),
          resolve: pump,
          reject: ignoreReadyRejection,
        };
        writer[kState].ready = readyHook;
      }

      function forwardChunk() {
        const chunk = pendingChunk;
        pendingChunk = undefined;
        writableStreamDefaultWriterWriteWithRequest(writer, chunk, writeTracker);
        pump();
      }

      function pump() {
        if (shuttingDown) return;

        if (dest[kState].backpressure) {
          parkOnReady();
          return;
        }

        const controller = source[kState].controller;

        if (source[kState].state === 'readable' &&
            isReadableStreamDefaultController(controller) &&
            controller[kState].queue.length > 0) {

          while (controller[kState].queue.length > 0) {
            if (shuttingDown) return;

            const chunk = dequeueValue(controller);

            if (controller[kState].closeRequested && !controller[kState].queue.length) {
              readableStreamDefaultControllerClearAlgorithms(controller);
              readableStreamClose(source);
            }

            writableStreamDefaultWriterWriteWithRequest(writer, chunk, writeTracker);

            if (dest[kState].backpressure) {
              break;
            } else if (dest[kState].state !== 'writable' || writableStreamCloseQueuedOrInFlight(dest)) {
              break;
            }
          }

          readableStreamDefaultControllerCallPullIfNeeded(controller);

          if (source[kState].state === 'closed') {
            return;
          }

          if (dest[kState].backpressure) {
            parkOnReady();
            return;
          }

          PromisePrototypeThen(kResolvedPromise, pump);
          return;
        }

        readRequest ??= new StepsReadRequest(
          (chunk) => {
            pendingChunk = chunk;
            PromisePrototypeThen(kResolvedPromise, forwardChunk);
          },
          nonOpCallback,
          nonOpCallback);
        readableStreamDefaultReaderRead(reader, readRequest);
      }

      if (signal !== undefined) {
        if (signal.aborted) {
          abortAlgorithm();
          return promise.promise;
        }
        addAbortListener ??= require('internal/events/abort_listener').addAbortListener;
        disposable = addAbortListener(signal, abortAlgorithm);
      }

      pump();

      function onSourceErrored(error) {
        if (!preventAbort) {
          return shutdownWithAnAction(
            () => writableStreamAbort(dest, error),
            true,
            error);
        }
        shutdown(true, error);
      }

      function onSourceClosed() {
        if (!preventClose) {
          return shutdownWithAnAction(
            () => writableStreamDefaultWriterCloseWithErrorPropagation(writer));
        }
        shutdown();
      }

      if (source[kState].state === 'errored') {
        onSourceErrored(source[kState].storedError);
      } else if (source[kState].state !== 'closed') {
        PromisePrototypeThen(
          readerClosedPromise(reader).promise, onSourceClosed, onSourceErrored);
      }

      watchErrored(dest, writerClosedPromise(writer).promise, (error) => {
        if (!preventCancel) {
          return shutdownWithAnAction(
            () => readableStreamCancel(source, error),
            true,
            error);
        }
        shutdown(true, error);
      });

      if (source[kState].state === 'closed')
        onSourceClosed();

      if (writableStreamCloseQueuedOrInFlight(dest) ||
          dest[kState].state === 'closed') {
        const error = new ERR_INVALID_STATE.TypeError(
          'Destination WritableStream is closed');
        if (!preventCancel) {
          shutdownWithAnAction(
            () => readableStreamCancel(source, error), true, error);
        } else {
          shutdown(true, error);
        }
      }

      return promise.promise;
    }

    function readableStreamTee(stream, cloneForBranch2) {
      if (isReadableByteStreamController(stream[kState].controller)) {
        return readableByteStreamTee(stream);
      }
      return readableStreamDefaultTee(stream, cloneForBranch2);
    }

    function readableStreamDefaultTee(stream, cloneForBranch2) {
      const reader = new ReadableStreamDefaultReader(stream);
      let reading = false;
      let canceled1 = false;
      let canceled2 = false;
      let reason1;
      let reason2;
      let branch1;
      let branch2;
      const cancelPromise = PromiseWithResolvers();

      let pendingChunk;
      let readRequest;
      function forwardChunk() {
        reading = false;
        const value1 = pendingChunk;
        let value2 = pendingChunk;
        pendingChunk = undefined;
        if (!canceled2 && cloneForBranch2) {
          value2 = structuredClone(value2);
        }
        if (!canceled1) {
          readableStreamDefaultControllerEnqueue(
            branch1[kState].controller,
            value1);
        }
        if (!canceled2) {
          readableStreamDefaultControllerEnqueue(
            branch2[kState].controller,
            value2);
        }
      }

      function pullAlgorithm() {
        if (reading) return;
        reading = true;
        readRequest ??= new StepsReadRequest(
          (value) => {
            pendingChunk = value;
            PromisePrototypeThen(kResolvedPromise, forwardChunk);
          },
          () => {
            process.nextTick(() => {
              reading = false;
              if (!canceled1)
                readableStreamDefaultControllerClose(branch1[kState].controller);
              if (!canceled2)
                readableStreamDefaultControllerClose(branch2[kState].controller);
              if (!canceled1 || !canceled2)
                cancelPromise.resolve();
            });
          },
          () => {
            reading = false;
          });
        readableStreamDefaultReaderRead(reader, readRequest);
      }

      function cancel1Algorithm(reason) {
        canceled1 = true;
        reason1 = reason;
        if (canceled2) {
          const compositeReason = [reason1, reason2];
          cancelPromise.resolve(readableStreamCancel(stream, compositeReason));
        }
        return cancelPromise.promise;
      }

      function cancel2Algorithm(reason) {
        canceled2 = true;
        reason2 = reason;
        if (canceled1) {
          const compositeReason = [reason1, reason2];
          cancelPromise.resolve(readableStreamCancel(stream, compositeReason));
        }
        return cancelPromise.promise;
      }

      branch1 =
        createReadableStream(nonOpCallback, pullAlgorithm, cancel1Algorithm);
      branch2 =
        createReadableStream(nonOpCallback, pullAlgorithm, cancel2Algorithm);

      PromisePrototypeThen(
        readerClosedPromise(reader).promise,
        undefined,
        (error) => {
          readableStreamDefaultControllerError(branch1[kState].controller, error);
          readableStreamDefaultControllerError(branch2[kState].controller, error);
          if (!canceled1 || !canceled2)
            cancelPromise.resolve();
        });

      return [branch1, branch2];
    }

    function readableByteStreamTee(stream) {
      assert(isReadableStream(stream));
      assert(isReadableByteStreamController(stream[kState].controller));

      let reader = new ReadableStreamDefaultReader(stream);
      let reading = false;
      let readAgainForBranch1 = false;
      let readAgainForBranch2 = false;
      let canceled1 = false;
      let canceled2 = false;
      let reason1;
      let reason2;
      let branch1;
      let branch2;
      const cancelDeferred = PromiseWithResolvers();

      function forwardReaderError(thisReader) {
        PromisePrototypeThen(
          readerClosedPromise(thisReader).promise,
          undefined,
          (error) => {
            if (thisReader !== reader) {
              return;
            }
            readableStreamDefaultControllerError(branch1[kState].controller, error);
            readableStreamDefaultControllerError(branch2[kState].controller, error);
            if (!canceled1 || !canceled2) {
              cancelDeferred.resolve();
            }
          },
        );
      }

      let pendingChunk;
      function forwardChunk() {
        readAgainForBranch1 = false;
        readAgainForBranch2 = false;
        const chunk1 = pendingChunk;
        let chunk2 = pendingChunk;
        pendingChunk = undefined;

        if (!canceled1 && !canceled2) {
          try {
            chunk2 = cloneAsUint8Array(chunk1);
          } catch (error) {
            readableByteStreamControllerError(
              branch1[kState].controller,
              error,
            );
            readableByteStreamControllerError(
              branch2[kState].controller,
              error,
            );
            cancelDeferred.resolve(readableStreamCancel(stream, error));
            return;
          }
        }
        if (!canceled1) {
          readableByteStreamControllerEnqueue(
            branch1[kState].controller,
            chunk1,
          );
        }
        if (!canceled2) {
          readableByteStreamControllerEnqueue(
            branch2[kState].controller,
            chunk2,
          );
        }
        reading = false;

        if (readAgainForBranch1) {
          pull1Algorithm();
        } else if (readAgainForBranch2) {
          pull2Algorithm();
        }
      }

      let defaultReadRequest;
      function pullWithDefaultReader() {
        if (isReadableStreamBYOBReader(reader)) {
          readableStreamBYOBReaderRelease(reader);
          reader = new ReadableStreamDefaultReader(stream);
          forwardReaderError(reader);
        }

        defaultReadRequest ??= new StepsReadRequest(
          (chunk) => {
            pendingChunk = chunk;
            PromisePrototypeThen(kResolvedPromise, forwardChunk);
          },
          () => {
            reading = false;

            if (!canceled1) {
              readableByteStreamControllerClose(branch1[kState].controller);
            }
            if (!canceled2) {
              readableByteStreamControllerClose(branch2[kState].controller);
            }
            if (branch1[kState].controller[kState].pendingPullIntos.length > 0) {
              readableByteStreamControllerRespond(branch1[kState].controller, 0);
            }
            if (branch2[kState].controller[kState].pendingPullIntos.length > 0) {
              readableByteStreamControllerRespond(branch2[kState].controller, 0);
            }
            if (!canceled1 || !canceled2) {
              cancelDeferred.resolve();
            }
          },
          () => {
            reading = false;
          });
        readableStreamDefaultReaderRead(reader, defaultReadRequest);
      }

      function pullWithBYOBReader(view, forBranch2) {
        if (isReadableStreamDefaultReader(reader)) {
          readableStreamDefaultReaderRelease(reader);
          reader = new ReadableStreamBYOBReader(stream);
          forwardReaderError(reader);
        }

        const byobBranch = forBranch2 === true ? branch2 : branch1;
        const otherBranch = forBranch2 === false ? branch2 : branch1;
        const readIntoRequest = new StepsReadRequest(
          (chunk) => {
            queueMicrotask(() => {
              readAgainForBranch1 = false;
              readAgainForBranch2 = false;
              const byobCanceled = forBranch2 === true ? canceled2 : canceled1;
              const otherCanceled = forBranch2 === false ? canceled2 : canceled1;

              if (!otherCanceled) {
                let clonedChunk;

                try {
                  clonedChunk = cloneAsUint8Array(chunk);
                } catch (error) {
                  readableByteStreamControllerError(
                    byobBranch[kState].controller,
                    error,
                  );
                  readableByteStreamControllerError(
                    otherBranch[kState].controller,
                    error,
                  );
                  cancelDeferred.resolve(readableStreamCancel(stream, error));
                  return;
                }
                if (!byobCanceled) {
                  readableByteStreamControllerRespondWithNewView(
                    byobBranch[kState].controller,
                    chunk,
                  );
                }

                readableByteStreamControllerEnqueue(
                  otherBranch[kState].controller,
                  clonedChunk,
                );
              } else if (!byobCanceled) {
                readableByteStreamControllerRespondWithNewView(
                  byobBranch[kState].controller,
                  chunk,
                );
              }
              reading = false;

              if (readAgainForBranch1) {
                pull1Algorithm();
              } else if (readAgainForBranch2) {
                pull2Algorithm();
              }
            });
          },
          (chunk) => {
            reading = false;

            const byobCanceled = forBranch2 === true ? canceled2 : canceled1;
            const otherCanceled = forBranch2 === false ? canceled2 : canceled1;

            if (!byobCanceled) {
              readableByteStreamControllerClose(byobBranch[kState].controller);
            }
            if (!otherCanceled) {
              readableByteStreamControllerClose(otherBranch[kState].controller);
            }
            if (chunk !== undefined) {
              if (!byobCanceled) {
                readableByteStreamControllerRespondWithNewView(
                  byobBranch[kState].controller,
                  chunk,
                );
              }
              if (
                !otherCanceled &&
                otherBranch[kState].controller[kState].pendingPullIntos.length > 0
              ) {
                readableByteStreamControllerRespond(
                  otherBranch[kState].controller,
                  0,
                );
              }
            }
            if (!byobCanceled || !otherCanceled) {
              cancelDeferred.resolve();
            }
          },
          () => {
            reading = false;
          });
        readableStreamBYOBReaderRead(reader, view, 1, readIntoRequest);
      }

      function pull1Algorithm() {
        if (reading) {
          readAgainForBranch1 = true;
          return;
        }
        reading = true;

        const byobRequest = branch1[kState].controller.byobRequest;
        if (byobRequest === null) {
          pullWithDefaultReader();
        } else {
          pullWithBYOBReader(byobRequest[kState].view, false);
        }
      }

      function pull2Algorithm() {
        if (reading) {
          readAgainForBranch2 = true;
          return;
        }
        reading = true;

        const byobRequest = branch2[kState].controller.byobRequest;
        if (byobRequest === null) {
          pullWithDefaultReader();
        } else {
          pullWithBYOBReader(byobRequest[kState].view, true);
        }
      }

      function cancel1Algorithm(reason) {
        canceled1 = true;
        reason1 = reason;
        if (canceled2) {
          cancelDeferred.resolve(readableStreamCancel(stream, [reason1, reason2]));
        }
        return cancelDeferred.promise;
      }

      function cancel2Algorithm(reason) {
        canceled2 = true;
        reason2 = reason;
        if (canceled1) {
          cancelDeferred.resolve(readableStreamCancel(stream, [reason1, reason2]));
        }
        return cancelDeferred.promise;
      }

      branch1 =
        createReadableByteStream(nonOpCallback, pull1Algorithm, cancel1Algorithm);
      branch2 =
        createReadableByteStream(nonOpCallback, pull2Algorithm, cancel2Algorithm);

      forwardReaderError(reader);

      return [branch1, branch2];
    }

    function readableByteStreamControllerConvertPullIntoDescriptor(desc) {
      const {
        buffer,
        bytesFilled,
        byteLength,
        byteOffset,
        ctor,
        elementSize,
      } = desc;
      if (bytesFilled > byteLength)
        throw new ERR_INVALID_STATE.RangeError('The buffer size is invalid');
      assert(!(bytesFilled % elementSize));
      const transferredBuffer = ArrayBufferPrototypeTransfer(buffer);

      if (ctor === Buffer) {
        return Buffer.from(transferredBuffer, byteOffset, bytesFilled / elementSize);
      }

      return new ctor(transferredBuffer, byteOffset, bytesFilled / elementSize);
    }

    function isReadableStreamLocked(stream) {
      return stream[kState].reader !== undefined;
    }

    function readableStreamCancel(stream, reason) {
      stream[kState].disturbed = true;
      switch (stream[kState].state) {
        case 'closed':
          return Promise.resolve();
        case 'errored':
          return Promise.reject(stream[kState].storedError);
      }
      readableStreamClose(stream);
      const {
        reader,
      } = stream[kState];
      if (reader !== undefined && readableStreamHasBYOBReader(stream)) {
        const readIntoRequests = reader[kState].readIntoRequests;
        while (readIntoRequests.length)
          readIntoRequests.shift()[kClose]();
      }

      return PromisePrototypeThen(
        stream[kState].controller[kCancel](reason),
        () => {});
    }

    function readableStreamClose(stream) {
      assert(stream[kState].state === 'readable');
      stream[kState].state = 'closed';
      stream[kState].closedPromise?.resolve();
      const {
        reader,
      } = stream[kState];

      if (reader === undefined)
        return;

      reader[kState].close?.resolve();

      if (readableStreamHasDefaultReader(stream)) {
        const readRequests = reader[kState].readRequests;
        while (readRequests.length)
          readRequests.shift()[kClose]();
      }
    }

    function readableStreamError(stream, error) {
      assert(stream[kState].state === 'readable');
      stream[kState].state = 'errored';
      stream[kState].storedError = error;
      const closedPromiseCache = stream[kState].closedPromise;
      if (closedPromiseCache !== undefined) {
        setPromiseHandled(closedPromiseCache.promise);
        closedPromiseCache.reject(error);
      }

      const {
        reader,
      } = stream[kState];

      if (reader === undefined)
        return;

      const closeCache = reader[kState].close;
      if (closeCache !== undefined) {
        setPromiseHandled(closeCache.promise);
        closeCache.reject(error);
      }

      if (readableStreamHasDefaultReader(stream)) {
        const readRequests = reader[kState].readRequests;
        while (readRequests.length)
          readRequests.shift()[kError](error);
      } else {
        assert(readableStreamHasBYOBReader(stream));
        const readIntoRequests = reader[kState].readIntoRequests;
        while (readIntoRequests.length)
          readIntoRequests.shift()[kError](error);
      }
    }

    function readableStreamHasDefaultReader(stream) {
      const {
        reader,
      } = stream[kState];

      if (reader === undefined)
        return false;

      return reader[kState] !== undefined &&
             reader[kType] === 'ReadableStreamDefaultReader';
    }

    function readableStreamGetNumReadRequests(stream) {
      assert(readableStreamHasDefaultReader(stream));
      return stream[kState].reader[kState].readRequests.length;
    }

    function readableStreamHasBYOBReader(stream) {
      const {
        reader,
      } = stream[kState];

      if (reader === undefined)
        return false;

      return reader[kState] !== undefined &&
             reader[kType] === 'ReadableStreamBYOBReader';
    }

    function readableStreamGetNumReadIntoRequests(stream) {
      assert(readableStreamHasBYOBReader(stream));
      return stream[kState].reader[kState].readIntoRequests.length;
    }

    function readableStreamFulfillReadRequest(stream, chunk, done) {
      assert(readableStreamHasDefaultReader(stream));
      const {
        reader,
      } = stream[kState];
      assert(reader[kState].readRequests.length);
      const readRequest = reader[kState].readRequests.shift();

      if (done)
        readRequest[kClose]();
      else
        readRequest[kChunk](chunk);
    }

    function readableStreamFulfillReadIntoRequest(stream, chunk, done) {
      assert(readableStreamHasBYOBReader(stream));
      const {
        reader,
      } = stream[kState];
      assert(reader[kState].readIntoRequests.length);
      const readIntoRequest = reader[kState].readIntoRequests.shift();
      if (done)
        readIntoRequest[kClose](chunk);
      else
        readIntoRequest[kChunk](chunk);
    }

    function readableStreamAddReadRequest(stream, readRequest) {
      assert(readableStreamHasDefaultReader(stream));
      assert(stream[kState].state === 'readable');
      const readerState = stream[kState].reader[kState];
      let readRequests = readerState.readRequests;
      if (readRequests === kEmptyQueue)
        readRequests = readerState.readRequests = new Queue();
      readRequests.push(readRequest);
    }

    function readableStreamAddReadIntoRequest(stream, readIntoRequest) {
      assert(readableStreamHasBYOBReader(stream));
      assert(stream[kState].state !== 'errored');
      const readerState = stream[kState].reader[kState];
      let readIntoRequests = readerState.readIntoRequests;
      if (readIntoRequests === kEmptyQueue)
        readIntoRequests = readerState.readIntoRequests = new Queue();
      readIntoRequests.push(readIntoRequest);
    }

    function readableStreamReaderGenericCancel(reader, reason) {
      const {
        stream,
      } = reader[kState];
      assert(stream !== undefined);
      return readableStreamCancel(stream, reason);
    }

    function readableStreamReaderGenericInitialize(reader, stream) {
      reader[kState].stream = stream;
      stream[kState].reader = reader;
    }

    function readerClosedPromise(reader) {
      let close = reader[kState].close;
      if (close === undefined) {
        const stream = reader[kState].stream;
        if (stream === undefined) {
          close = {
            promise: Promise.reject(lazyReadableReleasedError()),
            resolve: undefined,
            reject: undefined,
          };
          setPromiseHandled(close.promise);
        } else {
          switch (stream[kState].state) {
            case 'readable':
              close = PromiseWithResolvers();
              break;
            case 'closed':
              close = {
                promise: Promise.resolve(),
                resolve: undefined,
                reject: undefined,
              };
              break;
            case 'errored':
              close = {
                promise: Promise.reject(stream[kState].storedError),
                resolve: undefined,
                reject: undefined,
              };
              setPromiseHandled(close.promise);
              break;
          }
        }
        reader[kState].close = close;
      }
      return close;
    }

    function readableStreamDefaultReaderRelease(reader) {
      readableStreamReaderGenericRelease(reader);
      readableStreamDefaultReaderErrorReadRequests(
        reader,
        lazyReadableReleasingError(),
      );
    }

    function readableStreamDefaultReaderErrorReadRequests(reader, e) {
      const readRequests = reader[kState].readRequests;
      while (readRequests.length)
        readRequests.shift()[kError](e);
    }

    function readableStreamBYOBReaderRelease(reader) {
      readableStreamReaderGenericRelease(reader);
      readableStreamBYOBReaderErrorReadIntoRequests(
        reader,
        lazyReadableReleasingError(),
      );
    }

    function readableStreamBYOBReaderErrorReadIntoRequests(reader, e) {
      const readIntoRequests = reader[kState].readIntoRequests;
      while (readIntoRequests.length)
        readIntoRequests.shift()[kError](e);
    }

    function readableStreamReaderGenericRelease(reader) {
      const {
        stream,
      } = reader[kState];
      assert(stream !== undefined);
      assert(stream[kState].reader === reader);

      const closeCache = reader[kState].close;
      if (stream[kState].state === 'readable') {
        if (closeCache !== undefined) {
          closeCache.reject(lazyReadableReleasedError());
          setPromiseHandled(closeCache.promise);
        }
      } else {
        reader[kState].close = undefined;
      }

      stream[kState].controller[kRelease]();

      stream[kState].reader = undefined;
      reader[kState].stream = undefined;
    }

    function readableStreamBYOBReaderReadView(reader, view, options) {
      if (!isReadableStreamBYOBReader(reader))
        throw new ERR_INVALID_THIS('ReadableStreamBYOBReader');
      validateBuffer(view, 'view');
      validateObject(options, 'options', kValidateObjectAllowObjectsAndNull);

      const viewByteLength = ArrayBufferViewGetByteLength(view);
      const viewBuffer = ArrayBufferViewGetBuffer(view);

      if (isSharedArrayBuffer(viewBuffer)) {
        throw new ERR_INVALID_ARG_VALUE(
          'view',
          view,
          'must not be backed by a SharedArrayBuffer',
        );
      }

      const viewBufferByteLength = ArrayBufferPrototypeGetByteLength(viewBuffer);

      if (viewByteLength === 0 || viewBufferByteLength === 0) {
        throw new ERR_INVALID_STATE.TypeError(
          'View or Viewed ArrayBuffer is zero-length or detached');
      }

      const min = options?.min ?? 1;
      validateNumber(min, 'options.min');
      if (!Number.isInteger(min))
        throw new ERR_INVALID_ARG_VALUE('options.min', min, 'must be an integer');
      if (min <= 0)
        throw new ERR_INVALID_ARG_VALUE('options.min', min, 'must be greater than 0');
      if (!isDataView(view)) {
        if (min > view.length) {
          throw new ERR_OUT_OF_RANGE('options.min', '<= view.length', min);
        }
      } else if (min > viewByteLength) {
        throw new ERR_OUT_OF_RANGE('options.min', '<= view.byteLength', min);
      }

      if (reader[kState].stream === undefined) {
        throw new ERR_INVALID_STATE.TypeError('The reader is not attached to a stream');
      }
      const readIntoRequest = new ReadIntoRequest();
      readableStreamBYOBReaderRead(reader, view, min, readIntoRequest);
      return readIntoRequest.promise;
    }

    function readableStreamBYOBReaderRead(reader, view, min, readIntoRequest) {
      const {
        stream,
      } = reader[kState];
      assert(stream !== undefined);
      stream[kState].disturbed = true;
      if (stream[kState].state === 'errored') {
        readIntoRequest[kError](stream[kState].storedError);
        return;
      }
      readableByteStreamControllerPullInto(
        stream[kState].controller,
        view,
        min,
        readIntoRequest);
    }

    function readableStreamDefaultReaderRead(reader, readRequest) {
      const {
        stream,
      } = reader[kState];
      assert(stream !== undefined);
      stream[kState].disturbed = true;
      switch (stream[kState].state) {
        case 'closed':
          readRequest[kClose]();
          break;
        case 'errored':
          readRequest[kError](stream[kState].storedError);
          break;
        case 'readable':
          stream[kState].controller[kPull](readRequest);
      }
    }

    function setupReadableStreamBYOBReader(reader, stream) {
      if (isReadableStreamLocked(stream))
        throw new ERR_INVALID_STATE.TypeError('ReadableStream is locked');
      const {
        controller,
      } = stream[kState];
      if (!isReadableByteStreamController(controller))
        throw new ERR_INVALID_ARG_VALUE('stream', stream, 'must be a byte stream');
      readableStreamReaderGenericInitialize(reader, stream);
      reader[kState].readIntoRequests = kEmptyQueue;
    }

    function setupReadableStreamDefaultReader(reader, stream) {
      if (isReadableStreamLocked(stream))
        throw new ERR_INVALID_STATE.TypeError('ReadableStream is locked');
      readableStreamReaderGenericInitialize(reader, stream);
      reader[kState].readRequests = kEmptyQueue;
    }

    function readableStreamDefaultControllerClose(controller) {
      if (!readableStreamDefaultControllerCanCloseOrEnqueue(controller))
        return;
      controller[kState].closeRequested = true;
      if (!controller[kState].queue.length) {
        readableStreamDefaultControllerClearAlgorithms(controller);
        readableStreamClose(controller[kState].stream);
      }
    }

    function readableStreamDefaultControllerEnqueue(controller, chunk) {
      const controllerState = controller[kState];
      const stream = controllerState.stream;
      if (controllerState.closeRequested || stream[kState].state !== 'readable')
        return;

      const reader = stream[kState].reader;
      if (reader !== undefined &&
          reader[kState] !== undefined &&
          reader[kType] === 'ReadableStreamDefaultReader' &&
          reader[kState].readRequests.length) {
        readableStreamFulfillReadRequest(stream, chunk, false);
      } else if (controllerState.sizeAlgorithm === defaultSizeAlgorithm) {
        materializeQueue(controllerState).pushPair(chunk, 1);
        controllerState.queueTotalSize++;
        if (controllerState.started &&
            controllerState.highWaterMark - controllerState.queueTotalSize > 0) {
          readableStreamDefaultControllerPull(controller);
        }
        return;
      } else {
        try {
          const chunkSize =
            FunctionPrototypeCall(
              controllerState.sizeAlgorithm,
              undefined,
              chunk);
          enqueueValueWithSize(controller, chunk, chunkSize);
        } catch (error) {
          readableStreamDefaultControllerError(controller, error);
          throw error;
        }
      }
      readableStreamDefaultControllerCallPullIfNeeded(controller);
    }

    function readableStreamDefaultControllerHasBackpressure(controller) {
      return !readableStreamDefaultControllerShouldCallPull(controller);
    }

    function readableStreamDefaultControllerCanCloseOrEnqueue(controller) {
      const {
        stream,
      } = controller[kState];
      return !controller[kState].closeRequested &&
             stream[kState].state === 'readable';
    }

    function readableStreamDefaultControllerGetDesiredSize(controller) {
      const {
        stream,
        highWaterMark,
        queueTotalSize,
      } = controller[kState];
      switch (stream[kState].state) {
        case 'errored': return null;
        case 'closed': return 0;
        default:
          return highWaterMark - queueTotalSize;
      }
    }

    function readableStreamDefaultControllerShouldCallPull(controller) {
      const controllerState = controller[kState];
      const stream = controllerState.stream;
      if (controllerState.closeRequested ||
          stream[kState].state !== 'readable' ||
          !controllerState.started)
        return false;

      const reader = stream[kState].reader;
      if (reader !== undefined &&
          reader[kState] !== undefined &&
          reader[kType] === 'ReadableStreamDefaultReader' &&
          reader[kState].readRequests.length) {
        return true;
      }

      return controllerState.highWaterMark - controllerState.queueTotalSize > 0;
    }

    function readableStreamDefaultControllerCallPullIfNeeded(controller) {
      if (!readableStreamDefaultControllerShouldCallPull(controller))
        return;
      readableStreamDefaultControllerPull(controller);
    }

    function readableStreamDefaultControllerPull(controller) {
      if (controller[kState].pullAlgorithm === nonOpCallback)
        return;
      if (controller[kState].pulling) {
        controller[kState].pullAgain = true;
        return;
      }
      assert(!controller[kState].pullAgain);
      controller[kState].pulling = true;
      if (controller[kState].pullFulfilled === undefined) {
        controller[kState].pullFulfilled = () => {
          controller[kState].pulling = false;
          if (controller[kState].pullAgain) {
            controller[kState].pullAgain = false;
            readableStreamDefaultControllerCallPullIfNeeded(controller);
          }
        };
        controller[kState].pullRejected =
          (error) => readableStreamDefaultControllerError(controller, error);
      }
      let result;
      try {
        result = controller[kState].pullAlgorithm(controller);
      } catch (error) {
        result = Promise.reject(error);
      }
      thenAlgorithmResult(
        result,
        controller[kState].pullFulfilled,
        controller[kState].pullRejected);
    }

    function readableStreamDefaultControllerClearAlgorithms(controller) {
      controller[kState].pullAlgorithm = undefined;
      controller[kState].cancelAlgorithm = undefined;
      controller[kState].sizeAlgorithm = undefined;
    }

    function readableStreamDefaultControllerError(controller, error) {
      const {
        stream,
      } = controller[kState];
      if (stream[kState].state === 'readable') {
        resetQueue(controller);
        readableStreamDefaultControllerClearAlgorithms(controller);
        readableStreamError(stream, error);
      }
    }

    function readableStreamDefaultControllerCancelSteps(controller, reason) {
      resetQueue(controller);
      const result = controller[kState].cancelAlgorithm(reason);
      readableStreamDefaultControllerClearAlgorithms(controller);
      return result;
    }

    function readableStreamDefaultControllerPullSteps(controller, readRequest) {
      const {
        stream,
        queue,
      } = controller[kState];
      if (queue.length) {
        const chunk = dequeueValue(controller);
        if (controller[kState].closeRequested && !queue.length) {
          readableStreamDefaultControllerClearAlgorithms(controller);
          readableStreamClose(stream);
        } else if (!controller[kState].closeRequested &&
                   controller[kState].started &&
                   controller[kState].highWaterMark -
                     controller[kState].queueTotalSize > 0) {
          readableStreamDefaultControllerPull(controller);
        }
        readRequest[kChunk](chunk);
        return;
      }
      readableStreamAddReadRequest(stream, readRequest);
      if (controller[kState].started)
        readableStreamDefaultControllerPull(controller);
    }

    function setupReadableStreamDefaultController(
      stream,
      controller,
      startAlgorithm,
      pullAlgorithm,
      cancelAlgorithm,
      highWaterMark,
      sizeAlgorithm) {
      assert(stream[kState].controller === undefined);
      controller[kState] = {
        cancelAlgorithm,
        closeRequested: false,
        highWaterMark,
        pullAgain: false,
        pullAlgorithm,
        pulling: false,
        pullFulfilled: undefined,
        pullRejected: undefined,
        queue: kEmptyQueue,
        queueTotalSize: 0,
        started: false,
        sizeAlgorithm,
        stream,
      };
      stream[kState].controller = controller;

      const startResult = startAlgorithm();

      const started = () => {
        controller[kState].started = true;
        assert(!controller[kState].pulling);
        assert(!controller[kState].pullAgain);
        readableStreamDefaultControllerCallPullIfNeeded(controller);
      };

      if (startResult === null ||
          (typeof startResult !== 'object' && typeof startResult !== 'function')) {
        queueMicrotask(started);
        return;
      }

      PromisePrototypeThen(
        new Promise((r) => r(startResult)),
        started,
        (error) => readableStreamDefaultControllerError(controller, error));
    }

    function setupReadableStreamDefaultControllerFromSource(
      stream,
      source,
      highWaterMark,
      sizeAlgorithm) {
      const controller = new ReadableStreamDefaultController(kSkipThrow);
      const start = source?.start;
      const pull = source?.pull;
      const cancel = source?.cancel;
      const startAlgorithm = start ?
        FunctionPrototypeBind(start, source, controller) :
        nonOpCallback;
      const pullAlgorithm = pull ?
        createRawCallback1Param('source.pull', pull, source) :
        nonOpCallback;
      const cancelAlgorithm = cancel ?
        createPromiseCallback1Param('source.cancel', cancel, source) :
        nonOpCancel;

      setupReadableStreamDefaultController(
        stream,
        controller,
        startAlgorithm,
        pullAlgorithm,
        cancelAlgorithm,
        highWaterMark,
        sizeAlgorithm);
    }

    function readableByteStreamControllerClose(controller) {
      const {
        closeRequested,
        pendingPullIntos,
        queueTotalSize,
        stream,
      } = controller[kState];

      if (closeRequested || stream[kState].state !== 'readable')
        return;

      if (queueTotalSize) {
        controller[kState].closeRequested = true;
        return;
      }

      if (pendingPullIntos.length) {
        const firstPendingPullInto = pendingPullIntos.peek();
        if (firstPendingPullInto.bytesFilled % firstPendingPullInto.elementSize !== 0) {
          const error = new ERR_INVALID_STATE.TypeError('Partial read');
          readableByteStreamControllerError(controller, error);
          throw error;
        }
      }

      readableByteStreamControllerClearAlgorithms(controller);
      readableStreamClose(stream);
    }

    function readableByteStreamControllerCommitPullIntoDescriptor(stream, desc) {
      assert(stream[kState].state !== 'errored');
      assert(desc.type !== 'none');

      let done = false;
      if (stream[kState].state === 'closed') {
        assert(desc.bytesFilled % desc.elementSize === 0);
        done = true;
      }

      const filledView =
        readableByteStreamControllerConvertPullIntoDescriptor(desc);

      if (desc.type === 'default') {
        readableStreamFulfillReadRequest(stream, filledView, done);
      } else {
        assert(desc.type === 'byob');
        readableStreamFulfillReadIntoRequest(stream, filledView, done);
      }
    }

    function readableByteStreamControllerCommitPullIntoDescriptors(stream, descriptors) {
      for (let i = 0; i < descriptors.length; ++i) {
        readableByteStreamControllerCommitPullIntoDescriptor(
          stream,
          descriptors[i],
        );
      }
    }

    function readableByteStreamControllerInvalidateBYOBRequest(controller) {
      if (controller[kState].byobRequest === null)
        return;
      controller[kState].byobRequest[kState].controller = undefined;
      controller[kState].byobRequest[kState].view = null;
      controller[kState].byobRequest = null;
    }

    function readableByteStreamControllerClearAlgorithms(controller) {
      controller[kState].pullAlgorithm = undefined;
      controller[kState].cancelAlgorithm = undefined;
    }

    function readableByteStreamControllerClearPendingPullIntos(controller) {
      readableByteStreamControllerInvalidateBYOBRequest(controller);
      controller[kState].pendingPullIntos = kEmptyQueue;
    }

    function readableByteStreamControllerGetDesiredSize(controller) {
      const {
        stream,
        highWaterMark,
        queueTotalSize,
      } = controller[kState];
      switch (stream[kState].state) {
        case 'errored': return null;
        case 'closed': return 0;
        default: return highWaterMark - queueTotalSize;
      }
    }

    function readableByteStreamControllerShouldCallPull(controller) {
      const controllerState = controller[kState];
      const stream = controllerState.stream;
      if (stream[kState].state !== 'readable' ||
          controllerState.closeRequested ||
          !controllerState.started) {
        return false;
      }
      const reader = stream[kState].reader;
      if (reader !== undefined && reader[kState] !== undefined) {
        const type = reader[kType];
        if (type === 'ReadableStreamDefaultReader') {
          if (reader[kState].readRequests.length) return true;
        } else if (type === 'ReadableStreamBYOBReader') {
          if (reader[kState].readIntoRequests.length) return true;
        }
      }

      return controllerState.highWaterMark - controllerState.queueTotalSize > 0;
    }

    function readableByteStreamControllerHandleQueueDrain(controller) {
      const {
        closeRequested,
        queueTotalSize,
        stream,
      } = controller[kState];
      assert(stream[kState].state === 'readable');
      if (!queueTotalSize && closeRequested) {
        readableByteStreamControllerClearAlgorithms(controller);
        readableStreamClose(stream);
        return;
      }
      readableByteStreamControllerCallPullIfNeeded(controller);
    }

    function readableByteStreamControllerPullInto(
      controller,
      view,
      min,
      readIntoRequest) {
      const {
        closeRequested,
        stream,
        pendingPullIntos,
      } = controller[kState];
      let elementSize = 1;
      let ctor = DataView;
      if (isArrayBufferView(view) && !isDataView(view)) {
        elementSize = view.constructor.BYTES_PER_ELEMENT;
        ctor = view.constructor;
      }

      const minimumFill = min * elementSize;
      assert(minimumFill >= elementSize && minimumFill <= view.byteLength);
      assert(minimumFill % elementSize === 0);

      const buffer = ArrayBufferViewGetBuffer(view);
      const byteOffset = ArrayBufferViewGetByteOffset(view);
      const byteLength = ArrayBufferViewGetByteLength(view);
      const bufferByteLength = ArrayBufferPrototypeGetByteLength(buffer);

      let transferredBuffer;
      try {
        transferredBuffer = ArrayBufferPrototypeTransfer(buffer);
      } catch (error) {
        readIntoRequest[kError](error);
        return;
      }
      const desc = {
        buffer: transferredBuffer,
        bufferByteLength,
        byteOffset,
        byteLength,
        bytesFilled: 0,
        minimumFill,
        elementSize,
        ctor,
        type: 'byob',
      };
      if (pendingPullIntos.length) {
        pendingPullIntos.push(desc);
        readableStreamAddReadIntoRequest(stream, readIntoRequest);
        return;
      }
      if (stream[kState].state === 'closed') {
        const emptyView = new ctor(desc.buffer, byteOffset, 0);
        readIntoRequest[kClose](emptyView);
        return;
      }
      if (controller[kState].queueTotalSize) {
        if (readableByteStreamControllerFillPullIntoDescriptorFromQueue(
          controller,
          desc)) {
          const filledView =
            readableByteStreamControllerConvertPullIntoDescriptor(desc);
          readableByteStreamControllerHandleQueueDrain(controller);
          readIntoRequest[kChunk](filledView);
          return;
        }
        if (closeRequested) {
          const error = new ERR_INVALID_STATE.TypeError('ReadableStream closed');
          readableByteStreamControllerError(controller, error);
          readIntoRequest[kError](error);
          return;
        }
      }
      materializePendingPullIntos(controller[kState]).push(desc);
      readableStreamAddReadIntoRequest(stream, readIntoRequest);
      readableByteStreamControllerCallPullIfNeeded(controller);
    }

    function materializePendingPullIntos(state) {
      const pendingPullIntos = state.pendingPullIntos;
      if (pendingPullIntos === kEmptyQueue)
        return state.pendingPullIntos = new Queue();
      return pendingPullIntos;
    }

    function readableByteStreamControllerRespondInternal(controller, bytesWritten) {
      const {
        stream,
        pendingPullIntos,
      } = controller[kState];
      const desc = pendingPullIntos.peek();
      readableByteStreamControllerInvalidateBYOBRequest(controller);
      if (stream[kState].state === 'closed') {
        if (bytesWritten)
          throw new ERR_INVALID_STATE.TypeError(
            'Controller is closed but view is not zero-length');
        readableByteStreamControllerRespondInClosedState(controller, desc);
      } else {
        assert(stream[kState].state === 'readable');
        if (!bytesWritten)
          throw new ERR_INVALID_STATE.TypeError('View cannot be zero-length');
        readableByteStreamControllerRespondInReadableState(
          controller,
          bytesWritten,
          desc);
      }
      readableByteStreamControllerCallPullIfNeeded(controller);
    }

    function readableByteStreamControllerRespond(controller, bytesWritten) {
      const {
        pendingPullIntos,
        stream,
      } = controller[kState];
      assert(pendingPullIntos.length);
      const desc = pendingPullIntos.peek();

      if (stream[kState].state === 'closed') {
        if (bytesWritten !== 0)
          throw new ERR_INVALID_ARG_VALUE('bytesWritten', bytesWritten);
      } else {
        assert(stream[kState].state === 'readable');

        if (!bytesWritten)
          throw new ERR_INVALID_ARG_VALUE('bytesWritten', bytesWritten);

        if ((desc.bytesFilled + bytesWritten) > desc.byteLength)
          throw new ERR_INVALID_ARG_VALUE.RangeError('bytesWritten', bytesWritten);
      }

      desc.buffer = ArrayBufferPrototypeTransfer(desc.buffer);

      readableByteStreamControllerRespondInternal(controller, bytesWritten);
    }

    function readableByteStreamControllerRespondInClosedState(controller, desc) {
      assert(desc.bytesFilled % desc.elementSize === 0);
      if (desc.type === 'none') {
        readableByteStreamControllerShiftPendingPullInto(controller);
      }
      const {
        stream,
      } = controller[kState];
      if (readableStreamHasBYOBReader(stream)) {
        const filledPullIntos = [];
        for (let i = 0; i < readableStreamGetNumReadIntoRequests(stream); ++i) {
          filledPullIntos.push(readableByteStreamControllerShiftPendingPullInto(controller));
        }
        readableByteStreamControllerCommitPullIntoDescriptors(stream, filledPullIntos);
      }
    }

    function readableByteStreamControllerFillHeadPullIntoDescriptor(
      controller,
      size,
      desc) {
      const {
        pendingPullIntos,
        byobRequest,
      } = controller[kState];
      assert(!pendingPullIntos.length || pendingPullIntos.peek() === desc);
      assert(byobRequest === null);
      desc.bytesFilled += size;
    }

    function readableByteStreamControllerEnqueue(controller, chunk) {
      const {
        closeRequested,
        pendingPullIntos,
        queue,
        stream,
      } = controller[kState];

      const buffer = ArrayBufferViewGetBuffer(chunk);
      const byteOffset = ArrayBufferViewGetByteOffset(chunk);
      const byteLength = ArrayBufferViewGetByteLength(chunk);

      if (closeRequested || stream[kState].state !== 'readable')
        return;

      const transferredBuffer = ArrayBufferPrototypeTransfer(buffer);

      if (pendingPullIntos.length) {
        const firstPendingPullInto = pendingPullIntos.peek();

        if (ArrayBufferPrototypeGetDetached(firstPendingPullInto.buffer)) {
          throw new ERR_INVALID_STATE.TypeError(
            'Destination ArrayBuffer is detached',
          );
        }

        readableByteStreamControllerInvalidateBYOBRequest(controller);

        firstPendingPullInto.buffer = ArrayBufferPrototypeTransfer(
          firstPendingPullInto.buffer,
        );

        if (firstPendingPullInto.type === 'none') {
          readableByteStreamControllerEnqueueDetachedPullIntoToQueue(
            controller,
            firstPendingPullInto,
          );
        }
      }

      const { reader } = stream[kState];
      if (reader !== undefined &&
          reader[kState] !== undefined &&
          reader[kType] === 'ReadableStreamDefaultReader') {
        const { readRequests } = reader[kState];
        if (readRequests.length && controller[kState].queueTotalSize > 0) {
          readableByteStreamControllerProcessReadRequestsUsingQueue(controller);
        }
        if (!readRequests.length) {
          readableByteStreamControllerEnqueueChunkToQueue(
            controller,
            transferredBuffer,
            byteOffset,
            byteLength);
        } else {
          assert(!queue.length);
          if (pendingPullIntos.length) {
            assert(pendingPullIntos.peek().type === 'default');
            readableByteStreamControllerShiftPendingPullInto(controller);
          }
          const transferredView =
            new Uint8Array(transferredBuffer, byteOffset, byteLength);
          const readRequest = readRequests.shift();
          readRequest[kChunk](transferredView);
        }
      } else if (readableStreamHasBYOBReader(stream)) {
        readableByteStreamControllerEnqueueChunkToQueue(
          controller,
          transferredBuffer,
          byteOffset,
          byteLength);
        const filledPullIntos = readableByteStreamControllerProcessPullIntoDescriptorsUsingQueue(
          controller);
        readableByteStreamControllerCommitPullIntoDescriptors(stream, filledPullIntos);
      } else {
        assert(!isReadableStreamLocked(stream));
        readableByteStreamControllerEnqueueChunkToQueue(
          controller,
          transferredBuffer,
          byteOffset,
          byteLength);
      }
      readableByteStreamControllerCallPullIfNeeded(controller);
    }

    function readableByteStreamControllerEnqueueClonedChunkToQueue(
      controller,
      buffer,
      byteOffset,
      byteLength,
    ) {
      let cloneResult;
      try {
        cloneResult = ArrayBufferPrototypeSlice(
          buffer,
          byteOffset,
          byteOffset + byteLength,
        );
      } catch (error) {
        readableByteStreamControllerError(controller, error);
        throw error;
      }
      readableByteStreamControllerEnqueueChunkToQueue(
        controller,
        cloneResult,
        0,
        byteLength,
      );
    }

    function readableByteStreamControllerEnqueueChunkToQueue(
      controller,
      buffer,
      byteOffset,
      byteLength) {
      const state = controller[kState];
      materializeQueue(state).push({
        buffer,
        byteOffset,
        byteLength,
      });
      state.queueTotalSize += byteLength;
    }

    function readableByteStreamControllerEnqueueDetachedPullIntoToQueue(
      controller,
      desc,
    ) {
      const {
        buffer,
        byteOffset,
        bytesFilled,
        type,
      } = desc;
      assert(type === 'none');

      if (bytesFilled > 0) {
        readableByteStreamControllerEnqueueClonedChunkToQueue(
          controller,
          buffer,
          byteOffset,
          bytesFilled,
        );
      }
      readableByteStreamControllerShiftPendingPullInto(controller);
    }

    function readableByteStreamControllerFillPullIntoDescriptorFromQueue(
      controller,
      desc) {
      const {
        buffer,
        byteLength,
        byteOffset,
        bytesFilled,
        minimumFill,
        elementSize,
      } = desc;
      const maxBytesToCopy = Math.min(
        controller[kState].queueTotalSize,
        byteLength - bytesFilled);
      const maxBytesFilled = bytesFilled + maxBytesToCopy;
      const maxAlignedBytes = maxBytesFilled - (maxBytesFilled % elementSize);
      let totalBytesToCopyRemaining = maxBytesToCopy;
      let ready = false;
      assert(!ArrayBufferPrototypeGetDetached(buffer));
      assert(bytesFilled < minimumFill);
      if (maxAlignedBytes >= minimumFill) {
        totalBytesToCopyRemaining = maxAlignedBytes - bytesFilled;
        ready = true;
      }
      const {
        queue,
      } = controller[kState];

      while (totalBytesToCopyRemaining) {
        const headOfQueue = queue.peek();
        const bytesToCopy = Math.min(
          totalBytesToCopyRemaining,
          headOfQueue.byteLength);
        const destStart = byteOffset + desc.bytesFilled;
        assert(canCopyArrayBuffer(
          buffer,
          destStart,
          headOfQueue.buffer,
          headOfQueue.byteOffset,
          bytesToCopy));
        copyArrayBuffer(
          buffer,
          destStart,
          headOfQueue.buffer,
          headOfQueue.byteOffset,
          bytesToCopy);
        if (headOfQueue.byteLength === bytesToCopy) {
          queue.shift();
        } else {
          headOfQueue.byteOffset += bytesToCopy;
          headOfQueue.byteLength -= bytesToCopy;
        }
        controller[kState].queueTotalSize -= bytesToCopy;
        readableByteStreamControllerFillHeadPullIntoDescriptor(
          controller,
          bytesToCopy,
          desc);
        totalBytesToCopyRemaining -= bytesToCopy;
      }

      if (!ready) {
        assert(!controller[kState].queueTotalSize);
        assert(desc.bytesFilled > 0);
        assert(desc.bytesFilled < minimumFill);
      }
      return ready;
    }

    function readableByteStreamControllerProcessPullIntoDescriptorsUsingQueue(
      controller) {
      const {
        closeRequested,
        pendingPullIntos,
      } = controller[kState];
      assert(!closeRequested);
      const filledPullIntos = [];
      while (pendingPullIntos.length) {
        if (!controller[kState].queueTotalSize)
          break;
        const desc = pendingPullIntos.peek();
        if (readableByteStreamControllerFillPullIntoDescriptorFromQueue(
          controller,
          desc)) {
          readableByteStreamControllerShiftPendingPullInto(controller);
          filledPullIntos.push(desc);
        }
      }
      return filledPullIntos;
    }

    function readableByteStreamControllerRespondInReadableState(
      controller,
      bytesWritten,
      desc) {
      const {
        stream,
      } = controller[kState];
      const {
        buffer,
        bytesFilled,
        byteLength,
        type,
      } = desc;

      if (bytesFilled + bytesWritten > byteLength)
        throw new ERR_INVALID_STATE.RangeError('The buffer size is invalid');

      readableByteStreamControllerFillHeadPullIntoDescriptor(
        controller,
        bytesWritten,
        desc);

      if (type === 'none') {
        readableByteStreamControllerEnqueueDetachedPullIntoToQueue(
          controller,
          desc,
        );
        const filledPullIntos = readableByteStreamControllerProcessPullIntoDescriptorsUsingQueue(
          controller,
        );
        readableByteStreamControllerCommitPullIntoDescriptors(stream, filledPullIntos);
        return;
      }

      if (desc.bytesFilled < desc.minimumFill)
        return;

      readableByteStreamControllerShiftPendingPullInto(controller);

      const remainderSize = desc.bytesFilled % desc.elementSize;

      if (remainderSize) {
        const end = desc.byteOffset + desc.bytesFilled;
        const start = end - remainderSize;
        const remainder =
          ArrayBufferPrototypeSlice(
            buffer,
            start,
            end);
        readableByteStreamControllerEnqueueChunkToQueue(
          controller,
          remainder,
          0,
          ArrayBufferPrototypeGetByteLength(remainder));
      }
      desc.bytesFilled -= remainderSize;
      const filledPullIntos = readableByteStreamControllerProcessPullIntoDescriptorsUsingQueue(controller);

      readableByteStreamControllerCommitPullIntoDescriptor(stream, desc);
      readableByteStreamControllerCommitPullIntoDescriptors(stream, filledPullIntos);
    }

    function readableByteStreamControllerRespondWithNewView(controller, view) {
      const {
        stream,
        pendingPullIntos,
      } = controller[kState];
      assert(pendingPullIntos.length);

      const desc = pendingPullIntos.peek();
      assert(stream[kState].state !== 'errored');

      const viewByteLength = ArrayBufferViewGetByteLength(view);
      const viewByteOffset = ArrayBufferViewGetByteOffset(view);
      const viewBuffer = ArrayBufferViewGetBuffer(view);
      const viewBufferByteLength = ArrayBufferPrototypeGetByteLength(viewBuffer);

      if (stream[kState].state === 'closed') {
        if (viewByteLength !== 0)
          throw new ERR_INVALID_STATE.TypeError('View is not zero-length');
      } else {
        assert(stream[kState].state === 'readable');
        if (viewByteLength === 0)
          throw new ERR_INVALID_STATE.TypeError('View is zero-length');
      }

      const {
        byteOffset,
        byteLength,
        bytesFilled,
        bufferByteLength,
      } = desc;

      if (byteOffset + bytesFilled !== viewByteOffset)
        throw new ERR_INVALID_ARG_VALUE.RangeError('view', view);

      if (bytesFilled + viewByteLength > byteLength)
        throw new ERR_INVALID_ARG_VALUE.RangeError('view', view);

      if (bufferByteLength !== viewBufferByteLength)
        throw new ERR_INVALID_ARG_VALUE.RangeError('view', view);

      desc.buffer = ArrayBufferPrototypeTransfer(viewBuffer);

      readableByteStreamControllerRespondInternal(controller, viewByteLength);
    }

    function readableByteStreamControllerShiftPendingPullInto(controller) {
      assert(controller[kState].byobRequest === null);
      return controller[kState].pendingPullIntos.shift();
    }

    function readableByteStreamControllerCallPullIfNeeded(controller) {
      if (!readableByteStreamControllerShouldCallPull(controller))
        return;
      if (controller[kState].pullAlgorithm === nonOpCallback)
        return;
      if (controller[kState].pulling) {
        controller[kState].pullAgain = true;
        return;
      }
      assert(!controller[kState].pullAgain);
      controller[kState].pulling = true;
      if (controller[kState].pullFulfilled === undefined) {
        controller[kState].pullFulfilled = () => {
          controller[kState].pulling = false;
          if (controller[kState].pullAgain) {
            controller[kState].pullAgain = false;
            readableByteStreamControllerCallPullIfNeeded(controller);
          }
        };
        controller[kState].pullRejected =
          (error) => readableByteStreamControllerError(controller, error);
      }
      let result;
      try {
        result = controller[kState].pullAlgorithm(controller);
      } catch (error) {
        result = Promise.reject(error);
      }
      thenAlgorithmResult(
        result,
        controller[kState].pullFulfilled,
        controller[kState].pullRejected);
    }

    function readableByteStreamControllerError(controller, error) {
      const {
        stream,
      } = controller[kState];
      if (stream[kState].state !== 'readable')
        return;
      readableByteStreamControllerClearPendingPullIntos(controller);
      resetQueue(controller);
      readableByteStreamControllerClearAlgorithms(controller);
      readableStreamError(stream, error);
    }

    function readableByteStreamControllerCancelSteps(controller, reason) {
      readableByteStreamControllerClearPendingPullIntos(controller);
      resetQueue(controller);
      const result = controller[kState].cancelAlgorithm(reason);
      readableByteStreamControllerClearAlgorithms(controller);
      return result;
    }

    function readableByteStreamControllerDequeueChunk(controller) {
      assert(controller[kState].queueTotalSize > 0);
      const {
        buffer,
        byteOffset,
        byteLength,
      } = controller[kState].queue.shift();

      controller[kState].queueTotalSize -= byteLength;
      readableByteStreamControllerHandleQueueDrain(controller);
      return new Uint8Array(buffer, byteOffset, byteLength);
    }

    function readableByteStreamControllerFillReadRequestFromQueue(controller, readRequest) {
      readRequest[kChunk](readableByteStreamControllerDequeueChunk(controller));
    }

    function readableByteStreamControllerProcessReadRequestsUsingQueue(controller) {
      const {
        stream,
        queueTotalSize,
      } = controller[kState];
      const { reader } = stream[kState];
      assert(isReadableStreamDefaultReader(reader));

      while (reader[kState].readRequests.length > 0) {
        if (queueTotalSize === 0) {
          return;
        }
        readableByteStreamControllerFillReadRequestFromQueue(
          controller,
          reader[kState].readRequests.shift(),
        );
      }
    }

    function readableByteStreamControllerPullSteps(controller, readRequest) {
      const {
        queueTotalSize,
        stream,
      } = controller[kState];
      assert(readableStreamHasDefaultReader(stream));
      if (queueTotalSize) {
        assert(!readableStreamGetNumReadRequests(stream));
        readableByteStreamControllerFillReadRequestFromQueue(
          controller,
          readRequest,
        );
        return;
      }
      const {
        autoAllocateChunkSize,
      } = controller[kState];
      if (autoAllocateChunkSize !== undefined) {
        try {
          const buffer = new ArrayBuffer(autoAllocateChunkSize);
          materializePendingPullIntos(controller[kState]).push(
            {
              buffer,
              bufferByteLength: autoAllocateChunkSize,
              byteOffset: 0,
              byteLength: autoAllocateChunkSize,
              bytesFilled: 0,
              minimumFill: 1,
              elementSize: 1,
              ctor: Uint8Array,
              type: 'default',
            });
        } catch (error) {
          readRequest[kError](error);
          return;
        }
      }

      readableStreamAddReadRequest(stream, readRequest);
      readableByteStreamControllerCallPullIfNeeded(controller);
    }

    function setupReadableByteStreamController(
      stream,
      controller,
      startAlgorithm,
      pullAlgorithm,
      cancelAlgorithm,
      highWaterMark,
      autoAllocateChunkSize) {
      assert(stream[kState].controller === undefined);
      if (autoAllocateChunkSize !== undefined) {
        assert(Number.isInteger(autoAllocateChunkSize));
        assert(autoAllocateChunkSize > 0);
      }
      controller[kState] = {
        byobRequest: null,
        closeRequested: false,
        pullAgain: false,
        pulling: false,
        pullFulfilled: undefined,
        pullRejected: undefined,
        started: false,
        stream,
        queue: kEmptyQueue,
        queueTotalSize: 0,
        highWaterMark,
        pullAlgorithm,
        cancelAlgorithm,
        autoAllocateChunkSize,
        pendingPullIntos: kEmptyQueue,
      };
      stream[kState].controller = controller;

      const startResult = startAlgorithm();

      const started = () => {
        controller[kState].started = true;
        assert(!controller[kState].pulling);
        assert(!controller[kState].pullAgain);
        readableByteStreamControllerCallPullIfNeeded(controller);
      };

      if (startResult === null ||
          (typeof startResult !== 'object' && typeof startResult !== 'function')) {
        queueMicrotask(started);
        return;
      }

      PromisePrototypeThen(
        new Promise((r) => r(startResult)),
        started,
        (error) => readableByteStreamControllerError(controller, error));
    }

    function setupReadableByteStreamControllerFromSource(
      stream,
      source,
      highWaterMark) {
      const controller = new ReadableByteStreamController(kSkipThrow);
      const start = source?.start;
      const pull = source?.pull;
      const cancel = source?.cancel;
      const autoAllocateChunkSize = source?.autoAllocateChunkSize;
      const startAlgorithm = start ?
        FunctionPrototypeBind(start, source, controller) :
        nonOpCallback;
      const pullAlgorithm = pull ?
        createRawCallback1Param('source.pull', pull, source) :
        nonOpCallback;
      const cancelAlgorithm = cancel ?
        createPromiseCallback1Param('source.cancel', cancel, source) :
        nonOpCancel;

      if (autoAllocateChunkSize === 0) {
        throw new ERR_INVALID_ARG_VALUE(
          'source.autoAllocateChunkSize',
          autoAllocateChunkSize);
      }
      setupReadableByteStreamController(
        stream,
        controller,
        startAlgorithm,
        pullAlgorithm,
        cancelAlgorithm,
        highWaterMark,
        autoAllocateChunkSize);
    }

    module.exports = {
      ReadableStream,
      ReadableStreamDefaultReader,
      ReadableStreamBYOBReader,
      ReadableStreamBYOBRequest,
      ReadableByteStreamController,
      ReadableStreamDefaultController,
      TransferredReadableStream,

      isReadableStream,
      isReadableByteStreamController,
      isReadableStreamBYOBRequest,
      isReadableStreamDefaultReader,
      isReadableStreamBYOBReader,
      isWritableStreamDefaultWriter,
      isWritableStreamDefaultController,

      readableStreamPipeTo,
      readableStreamTee,
      readableByteStreamControllerConvertPullIntoDescriptor,
      isReadableStreamLocked,
      readableStreamCancel,
      readableStreamClose,
      readableStreamError,
      readableStreamHasDefaultReader,
      readableStreamGetNumReadRequests,
      readableStreamHasBYOBReader,
      readableStreamGetNumReadIntoRequests,
      readableStreamFulfillReadRequest,
      readableStreamFulfillReadIntoRequest,
      readableStreamAddReadRequest,
      readableStreamAddReadIntoRequest,
      readableStreamReaderGenericCancel,
      readableStreamReaderGenericInitialize,
      readableStreamReaderGenericRelease,
      readableStreamBYOBReaderRead,
      readableStreamDefaultReaderRead,
      kChunk,
      kClose,
      kError,
      setupReadableStreamBYOBReader,
      setupReadableStreamDefaultReader,
      readableStreamDefaultControllerClose,
      readableStreamDefaultControllerEnqueue,
      readableStreamDefaultControllerHasBackpressure,
      readableStreamDefaultControllerCanCloseOrEnqueue,
      readableStreamDefaultControllerGetDesiredSize,
      readableStreamDefaultControllerShouldCallPull,
      readableStreamDefaultControllerCallPullIfNeeded,
      readableStreamDefaultControllerClearAlgorithms,
      readableStreamDefaultControllerError,
      readableStreamDefaultControllerCancelSteps,
      readableStreamDefaultControllerPullSteps,
      setupReadableStreamDefaultController,
      setupReadableStreamDefaultControllerFromSource,
      readableByteStreamControllerClose,
      readableByteStreamControllerCommitPullIntoDescriptor,
      readableByteStreamControllerInvalidateBYOBRequest,
      readableByteStreamControllerClearAlgorithms,
      readableByteStreamControllerClearPendingPullIntos,
      readableByteStreamControllerGetDesiredSize,
      readableByteStreamControllerShouldCallPull,
      readableByteStreamControllerHandleQueueDrain,
      readableByteStreamControllerPullInto,
      readableByteStreamControllerRespondInternal,
      readableByteStreamControllerRespond,
      readableByteStreamControllerRespondInClosedState,
      readableByteStreamControllerFillHeadPullIntoDescriptor,
      readableByteStreamControllerEnqueue,
      readableByteStreamControllerEnqueueChunkToQueue,
      readableByteStreamControllerFillPullIntoDescriptorFromQueue,
      readableByteStreamControllerProcessPullIntoDescriptorsUsingQueue,
      readableByteStreamControllerRespondInReadableState,
      readableByteStreamControllerRespondWithNewView,
      readableByteStreamControllerShiftPendingPullInto,
      readableByteStreamControllerCallPullIfNeeded,
      readableByteStreamControllerError,
      readableByteStreamControllerCancelSteps,
      readableByteStreamControllerPullSteps,
      setupReadableByteStreamController,
      setupReadableByteStreamControllerFromSource,
      createReadableStream,
      createReadableByteStream,
    };
  },
  "internal/webstreams/transfer": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      PromiseResolve,
      PromiseWithResolvers,
    } = primordials;

    const {
      kState,
      setPromiseHandled,
    } = require('internal/webstreams/util');

    const {
      DOMException,
    } = internalBinding('messaging');

    const {
      ReadableStream,
      readableStreamDefaultControllerEnqueue,
      readableStreamDefaultControllerClose,
      readableStreamDefaultControllerError,
      readableStreamPipeTo,
    } = require('internal/webstreams/readablestream');

    const {
      WritableStream,
      writableStreamDefaultControllerErrorIfNeeded,
    } = require('internal/webstreams/writablestream');

    const assert = require('internal/assert');

    class CrossRealmTransformReadableSource {
      constructor(port, unref) {
        this[kState] = {
          port,
          controller: undefined,
          unref,
        };

        port.onmessage = ({ data }) => {
          const {
            controller,
          } = this[kState];
          const {
            type,
            value,
          } = data;
          switch (type) {
            case 'chunk':
              readableStreamDefaultControllerEnqueue(
                controller,
                value);
              break;
            case 'close':
              readableStreamDefaultControllerClose(controller);
              port.close();
              break;
            case 'error':
              readableStreamDefaultControllerError(controller, value);
              port.close();
              break;
          }
        };

        port.onmessageerror = () => {
          const error = new DOMException(
            'Internal transferred ReadableStream error',
            'DataCloneError');
          port.postMessage({ type: 'error', value: error });
          readableStreamDefaultControllerError(
            this[kState].controller,
            error);
          port.close();
        };

        port.unref();
      }

      start(controller) {
        this[kState].controller = controller;
      }

      async pull() {
        if (this[kState].unref) {
          this[kState].unref = false;
          this[kState].port.ref();
        }
        this[kState].port.postMessage({ type: 'pull' });
      }

      async cancel(reason) {
        try {
          this[kState].port.postMessage({ type: 'error', value: reason });
        } catch (error) {
          this[kState].port.postMessage({ type: 'error', value: error });
          throw error;
        } finally {
          this[kState].port.close();
        }
      }
    }

    class CrossRealmTransformWritableSink {
      constructor(port, unref) {
        this[kState] = {
          port,
          controller: undefined,
          backpressurePromise: PromiseWithResolvers(),
          unref,
        };

        port.onmessage = ({ data }) => {
          assert(typeof data === 'object');
          const {
            type,
            value,
          } = { ...data };
          assert(typeof type === 'string');
          switch (type) {
            case 'pull':
              if (this[kState].backpressurePromise !== undefined)
                this[kState].backpressurePromise.resolve();
              this[kState].backpressurePromise = undefined;
              break;
            case 'error':
              writableStreamDefaultControllerErrorIfNeeded(
                this[kState].controller,
                value);
              if (this[kState].backpressurePromise !== undefined)
                this[kState].backpressurePromise.resolve();
              this[kState].backpressurePromise = undefined;
              break;
          }
        };
        port.onmessageerror = () => {
          const error = new DOMException(
            'Internal transferred ReadableStream error',
            'DataCloneError');
          port.postMessage({ type: 'error', value: error });
          writableStreamDefaultControllerErrorIfNeeded(
            this[kState].controller,
            error);
          port.close();
        };

        port.unref();
      }

      start(controller) {
        this[kState].controller = controller;
      }

      async write(chunk) {
        if (this[kState].unref) {
          this[kState].unref = false;
          this[kState].port.ref();
        }
        if (this[kState].backpressurePromise === undefined) {
          this[kState].backpressurePromise = {
            promise: Promise.resolve(),
            resolve: undefined,
            reject: undefined,
          };
        }
        await this[kState].backpressurePromise.promise;
        this[kState].backpressurePromise = PromiseWithResolvers();
        try {
          this[kState].port.postMessage({ type: 'chunk', value: chunk });
        } catch (error) {
          this[kState].port.postMessage({ type: 'error', value: error });
          this[kState].port.close();
          throw error;
        }
      }

      close() {
        this[kState].port.postMessage({ type: 'close' });
        this[kState].port.close();
      }

      abort(reason) {
        try {
          this[kState].port.postMessage({ type: 'error', value: reason });
        } catch (error) {
          this[kState].port.postMessage({ type: 'error', value: error });
          throw error;
        } finally {
          this[kState].port.close();
        }
      }
    }

    function newCrossRealmReadableStream(writable, port) {
      const readable = new ReadableStream(new CrossRealmTransformReadableSource(port, false));

      const promise = readableStreamPipeTo(readable, writable, false, false, false);

      setPromiseHandled(promise);

      return {
        readable,
        promise,
      };
    }

    function newCrossRealmWritableSink(readable, port) {
      const writable = new WritableStream(new CrossRealmTransformWritableSink(port, false));

      const promise = readableStreamPipeTo(readable, writable, false, false, false);

      setPromiseHandled(promise);

      return {
        writable,
        promise,
      };
    }

    module.exports = {
      newCrossRealmReadableStream,
      newCrossRealmWritableSink,
      CrossRealmTransformWritableSink,
      CrossRealmTransformReadableSource,
    };
  },
  "internal/webstreams/transformstream": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      FunctionPrototypeCall,
      ObjectDefineProperties,
      ObjectSetPrototypeOf,
      PromisePrototypeThen,
      PromiseReject,
      PromiseResolve,
      PromiseWithResolvers,
      Symbol,
      SymbolToStringTag,
    } = primordials;

    const {
      codes: {
        ERR_ILLEGAL_CONSTRUCTOR,
        ERR_INVALID_ARG_VALUE,
        ERR_INVALID_STATE,
        ERR_INVALID_THIS,
      },
    } = require('internal/errors');

    const {
      DOMException,
    } = internalBinding('messaging');

    const {
      customInspectSymbol: kInspect,
      kEmptyObject,
      kEnumerableProperty,
    } = require('internal/util');

    const {
      validateObject,
      kValidateObjectAllowObjects,
      kValidateObjectAllowObjectsAndNull,
    } = require('internal/validators');

    const {
      kDeserialize,
      kTransfer,
      kTransferList,
      markTransferMode,
    } = require('internal/worker/js_transferable');

    const {
      createPromiseCallback1Param,
      createRawCallback2Params,
      customInspect,
      extractHighWaterMark,
      extractSizeAlgorithm,
      getNonWritablePropertyDescriptor,
      isBrandCheck,
      kParkedAlgorithmResult,
      kResolvedPromise,
      kState,
      kType,
      nonOpCancel,
      nonOpFlush,
    } = require('internal/webstreams/util');

    const {
      createReadableStream,
      readableStreamDefaultControllerCanCloseOrEnqueue,
      readableStreamDefaultControllerClose,
      readableStreamDefaultControllerEnqueue,
      readableStreamDefaultControllerError,
      readableStreamDefaultControllerGetDesiredSize,
      readableStreamDefaultControllerHasBackpressure,
    } = require('internal/webstreams/readablestream');

    const {
      createWritableStream,
      writableStreamDefaultControllerErrorIfNeeded,
    } = require('internal/webstreams/writablestream');

    const assert = require('internal/assert');

    const kSkipThrow = Symbol('kSkipThrow');

    class TransformStream {
      [kType] = 'TransformStream';

        constructor(
        transformer = kEmptyObject,
        writableStrategy = kEmptyObject,
        readableStrategy = kEmptyObject) {
        markTransferMode(this, false, true);
        validateObject(transformer, 'transformer', kValidateObjectAllowObjects);
        validateObject(writableStrategy, 'writableStrategy', kValidateObjectAllowObjectsAndNull);
        validateObject(readableStrategy, 'readableStrategy', kValidateObjectAllowObjectsAndNull);
        const readableType = transformer?.readableType;
        const writableType = transformer?.writableType;
        const start = transformer?.start;

        if (readableType !== undefined) {
          throw new ERR_INVALID_ARG_VALUE.RangeError(
            'transformer.readableType',
            readableType);
        }
        if (writableType !== undefined) {
          throw new ERR_INVALID_ARG_VALUE.RangeError(
            'transformer.writableType',
            writableType);
        }

        const readableHighWaterMark = readableStrategy?.highWaterMark;
        const readableSize = readableStrategy?.size;

        const writableHighWaterMark = writableStrategy?.highWaterMark;
        const writableSize = writableStrategy?.size;

        const actualReadableHighWaterMark =
          extractHighWaterMark(readableHighWaterMark, 0);
        const actualReadableSize = extractSizeAlgorithm(readableSize);

        const actualWritableHighWaterMark =
          extractHighWaterMark(writableHighWaterMark, 1);
        const actualWritableSize = extractSizeAlgorithm(writableSize);

        const startPromise = start !== undefined ? PromiseWithResolvers() : undefined;

        initializeTransformStream(
          this,
          startPromise,
          actualWritableHighWaterMark,
          actualWritableSize,
          actualReadableHighWaterMark,
          actualReadableSize);

        setupTransformStreamDefaultControllerFromTransformer(this, transformer);

        if (start !== undefined) {
          startPromise.resolve(
            FunctionPrototypeCall(
              start,
              transformer,
              this[kState].controller));
        }
      }

        get readable() {
        if (!isTransformStream(this))
          throw new ERR_INVALID_THIS('TransformStream');
        return this[kState].readable;
      }

        get writable() {
        if (!isTransformStream(this))
          throw new ERR_INVALID_THIS('TransformStream');
        return this[kState].writable;
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          readable: this.readable,
          writable: this.writable,
          backpressure: this[kState].backpressure,
        });
      }

      [kTransfer]() {
        if (!isTransformStream(this))
          throw new ERR_INVALID_THIS('TransformStream');
        const {
          readable,
          writable,
        } = this[kState];
        if (readable.locked) {
          throw new DOMException(
            'Cannot transfer a locked ReadableStream',
            'DataCloneError');
        }
        if (writable.locked) {
          throw new DOMException(
            'Cannot transfer a locked WritableStream',
            'DataCloneError');
        }
        return {
          data: {
            readable,
            writable,
          },
          deserializeInfo:
            'internal/webstreams/transformstream:TransferredTransformStream',
        };
      }

      [kTransferList]() {
        return [ this[kState].readable, this[kState].writable ];
      }

      [kDeserialize]({ readable, writable }) {
        this[kState].readable = readable;
        this[kState].writable = writable;
      }
    }

    Object.defineProperties(TransformStream.prototype, {
      readable: kEnumerableProperty,
      writable: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(TransformStream.name),
    });

    class TransformStreamState {
      readable = undefined;
      writable = undefined;
      controller = undefined;
      backpressure = undefined;
      pullPending = false;
      pendingWriteParked = false;
      pendingWriteChunk = undefined;
      writeContinuation = undefined;
    }
    Object.setPrototypeOf(TransformStreamState.prototype, null);

    class TransformStreamControllerState {
      stream = undefined;
      transformAlgorithm = undefined;
      flushAlgorithm = undefined;
      cancelAlgorithm = undefined;
      performTransformRejected = undefined;
      finishPromise = undefined;
    }
    Object.setPrototypeOf(TransformStreamControllerState.prototype, null);

    function InternalTransferredTransformStream() {
      Object.setPrototypeOf(this, TransformStream.prototype);
      markTransferMode(this, false, true);
      this[kType] = 'TransformStream';
      this[kState] = new TransformStreamState();
    }

    Object.setPrototypeOf(InternalTransferredTransformStream.prototype, TransformStream.prototype);
    Object.setPrototypeOf(InternalTransferredTransformStream, TransformStream);

    function TransferredTransformStream() {
      const stream = new InternalTransferredTransformStream();

      stream.constructor = TransformStream;

      return stream;
    }

    TransferredTransformStream.prototype[kDeserialize] = () => {};

    class TransformStreamDefaultController {
      [kType] = 'TransformStreamDefaultController';
      [kState];

      constructor(skipThrowSymbol = undefined) {
        if (skipThrowSymbol !== kSkipThrow) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
      }

        get desiredSize() {
        if (!isTransformStreamDefaultController(this))
          throw new ERR_INVALID_THIS('TransformStreamDefaultController');
        const {
          stream,
        } = this[kState];
        const {
          readable,
        } = stream[kState];
        const {
          controller: readableController,
        } = readable[kState];
        return readableStreamDefaultControllerGetDesiredSize(readableController);
      }

        enqueue(chunk = undefined) {
        if (!isTransformStreamDefaultController(this))
          throw new ERR_INVALID_THIS('TransformStreamDefaultController');
        transformStreamDefaultControllerEnqueue(this, chunk);
      }

        error(reason = undefined) {
        if (!isTransformStreamDefaultController(this))
          throw new ERR_INVALID_THIS('TransformStreamDefaultController');
        transformStreamDefaultControllerError(this, reason);
      }

      terminate() {
        if (!isTransformStreamDefaultController(this))
          throw new ERR_INVALID_THIS('TransformStreamDefaultController');
        transformStreamDefaultControllerTerminate(this);
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          stream: this[kState].stream,
        });
      }
    }

    Object.defineProperties(TransformStreamDefaultController.prototype, {
      desiredSize: kEnumerableProperty,
      enqueue: kEnumerableProperty,
      error: kEnumerableProperty,
      terminate: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(TransformStreamDefaultController.name),
    });

    const isTransformStream =
      isBrandCheck('TransformStream');
    const isTransformStreamDefaultController =
      isBrandCheck('TransformStreamDefaultController');

    function defaultTransformAlgorithm(chunk, controller) {
      transformStreamDefaultControllerEnqueue(controller, chunk);
    }

    function resolvedStartAlgorithm() {
      return kResolvedPromise;
    }

    function initializeTransformStream(
      stream,
      startPromise,
      writableHighWaterMark,
      writableSizeAlgorithm,
      readableHighWaterMark,
      readableSizeAlgorithm) {

      const startAlgorithm = startPromise === undefined ?
        resolvedStartAlgorithm :
        () => startPromise.promise;

      const writable = createWritableStream(
        startAlgorithm,
        (chunk) => transformStreamDefaultSinkWriteAlgorithm(stream, chunk),
        () => transformStreamDefaultSinkCloseAlgorithm(stream),
        (reason) => transformStreamDefaultSinkAbortAlgorithm(stream, reason),
        writableHighWaterMark,
        writableSizeAlgorithm,
      );

      const readable = createReadableStream(
        startAlgorithm,
        () => transformStreamDefaultSourcePullAlgorithm(stream),
        (reason) => transformStreamDefaultSourceCancelAlgorithm(stream, reason),
        readableHighWaterMark,
        readableSizeAlgorithm,
      );

      const state = new TransformStreamState();
      state.readable = readable;
      state.writable = writable;
      stream[kState] = state;

      transformStreamSetBackpressure(stream, true);
    }

    function transformStreamError(stream, error) {
      const {
        readable,
      } = stream[kState];
      const {
        controller,
      } = readable[kState];
      readableStreamDefaultControllerError(controller, error);
      transformStreamErrorWritableAndUnblockWrite(stream, error);
    }

    function transformStreamErrorWritableAndUnblockWrite(stream, error) {
      const {
        controller,
        writable,
      } = stream[kState];
      transformStreamDefaultControllerClearAlgorithms(controller);
      writableStreamDefaultControllerErrorIfNeeded(
        writable[kState].controller,
        error);
      transformStreamUnblockWrite(stream);
    }

    function transformStreamUnblockWrite(stream) {
      if (stream[kState].backpressure)
        transformStreamSetBackpressure(stream, false);
    }

    function transformStreamSetBackpressure(stream, backpressure) {
      const state = stream[kState];
      assert(state.backpressure !== backpressure);
      state.backpressure = backpressure;
      if (backpressure) {
        if (state.pullPending) {
          state.pullPending = false;
          PromisePrototypeThen(
            kResolvedPromise,
            state.readable[kState].controller[kState].pullFulfilled);
        }
      } else if (state.pendingWriteParked) {
        PromisePrototypeThen(kResolvedPromise, state.writeContinuation);
      }
    }

    function setupTransformStreamDefaultController(
      stream,
      controller,
      transformAlgorithm,
      flushAlgorithm,
      cancelAlgorithm) {
      assert(isTransformStream(stream));
      assert(stream[kState].controller === undefined);
      const controllerState = new TransformStreamControllerState();
      controllerState.stream = stream;
      controllerState.transformAlgorithm = transformAlgorithm;
      controllerState.flushAlgorithm = flushAlgorithm;
      controllerState.cancelAlgorithm = cancelAlgorithm;
      controller[kState] = controllerState;
      stream[kState].controller = controller;
    }

    function setupTransformStreamDefaultControllerFromTransformer(
      stream,
      transformer) {
      const controller = new TransformStreamDefaultController(kSkipThrow);
      const transform = transformer?.transform;
      const flush = transformer?.flush;
      const cancel = transformer?.cancel;
      const transformAlgorithm = transform ?
        createRawCallback2Params('transformer.transform', transform, transformer) :
        defaultTransformAlgorithm;
      const flushAlgorithm = flush ?
        createPromiseCallback1Param('transformer.flush', flush, transformer) :
        nonOpFlush;
      const cancelAlgorithm = cancel ?
        createPromiseCallback1Param('transformer.cancel', cancel, transformer) :
        nonOpCancel;

      setupTransformStreamDefaultController(
        stream,
        controller,
        transformAlgorithm,
        flushAlgorithm,
        cancelAlgorithm);
    }

    function transformStreamDefaultControllerClearAlgorithms(controller) {
      controller[kState].transformAlgorithm = undefined;
      controller[kState].flushAlgorithm = undefined;
      controller[kState].cancelAlgorithm = undefined;
    }

    function transformStreamDefaultControllerEnqueue(controller, chunk) {
      const {
        stream,
      } = controller[kState];
      const {
        readable,
      } = stream[kState];
      const {
        controller: readableController,
      } = readable[kState];
      if (!readableStreamDefaultControllerCanCloseOrEnqueue(readableController))
        throw new ERR_INVALID_STATE.TypeError('Unable to enqueue');
      try {
        readableStreamDefaultControllerEnqueue(readableController, chunk);
      } catch (error) {
        transformStreamErrorWritableAndUnblockWrite(stream, error);
        throw readable[kState].storedError;
      }
      const backpressure =
        readableStreamDefaultControllerHasBackpressure(readableController);
      if (backpressure !== stream[kState].backpressure) {
        assert(backpressure);
        transformStreamSetBackpressure(stream, true);
      }
    }

    function transformStreamDefaultControllerError(controller, error) {
      transformStreamError(controller[kState].stream, error);
    }

    function transformStreamDefaultControllerPerformTransform(controller, chunk) {
      const controllerState = controller[kState];
      const transformAlgorithm = controllerState.transformAlgorithm;
      if (transformAlgorithm === undefined) {
        return kResolvedPromise;
      }
      let result;
      try {
        result = transformAlgorithm(chunk, controller);
      } catch (error) {
        result = Promise.reject(error);
      }
      if (result === null ||
          (typeof result !== 'object' && typeof result !== 'function')) {
        result = kResolvedPromise;
      } else {
        result = Promise.resolve(result);
      }
      controllerState.performTransformRejected ??= (error) => {
        transformStreamError(controller[kState].stream, error);
        throw error;
      };
      return PromisePrototypeThen(
        result,
        undefined,
        controllerState.performTransformRejected);
    }

    function transformStreamDefaultControllerTerminate(controller) {
      const {
        stream,
      } = controller[kState];
      const {
        readable,
      } = stream[kState];
      assert(readable !== undefined);
      const {
        controller: readableController,
      } = readable[kState];
      readableStreamDefaultControllerClose(readableController);
      transformStreamErrorWritableAndUnblockWrite(
        stream,
        new ERR_INVALID_STATE.TypeError('TransformStream has been terminated'));
    }

    function transformStreamDefaultSinkWriteAlgorithm(stream, chunk) {
      const state = stream[kState];
      const {
        writable,
        controller,
      } = state;
      assert(writable[kState].state === 'writable');
      if (state.backpressure) {
        assert(!state.pendingWriteParked);
        state.pendingWriteParked = true;
        state.pendingWriteChunk = chunk;
        state.writeContinuation ??= () => {
          const pendingChunk = state.pendingWriteChunk;
          state.pendingWriteParked = false;
          state.pendingWriteChunk = undefined;
          const writableState = state.writable[kState];
          const writableControllerState = writableState.controller[kState];
          if (writableState.state === 'erroring') {
            const error = writableState.storedError;
            PromisePrototypeThen(
              kResolvedPromise,
              () => writableControllerState.writeRejected(error));
            return;
          }
          assert(writableState.state === 'writable');
          PromisePrototypeThen(
            transformStreamDefaultControllerPerformTransform(
              controller,
              pendingChunk),
            writableControllerState.writeFulfilled,
            writableControllerState.writeRejected);
        };
        return kParkedAlgorithmResult;
      }
      return transformStreamDefaultControllerPerformTransform(controller, chunk);
    }

    async function transformStreamDefaultSinkAbortAlgorithm(stream, reason) {
      const {
        controller,
        readable,
      } = stream[kState];

      if (controller[kState].finishPromise !== undefined) {
        return controller[kState].finishPromise;
      }

      const { promise, resolve, reject } = PromiseWithResolvers();
      controller[kState].finishPromise = promise;
      const cancelPromise = controller[kState].cancelAlgorithm(reason);
      transformStreamDefaultControllerClearAlgorithms(controller);

      PromisePrototypeThen(
        cancelPromise,
        () => {
          if (readable[kState].state === 'errored')
            reject(readable[kState].storedError);
          else {
            readableStreamDefaultControllerError(readable[kState].controller, reason);
            resolve();
          }
        },
        (error) => {
          readableStreamDefaultControllerError(readable[kState].controller, error);
          reject(error);
        },
      );

      return controller[kState].finishPromise;
    }

    function transformStreamDefaultSinkCloseAlgorithm(stream) {
      const {
        readable,
        controller,
      } = stream[kState];

      if (controller[kState].finishPromise !== undefined) {
        return controller[kState].finishPromise;
      }
      const { promise, resolve, reject } = PromiseWithResolvers();
      controller[kState].finishPromise = promise;
      const flushPromise = controller[kState].flushAlgorithm(controller);
      transformStreamDefaultControllerClearAlgorithms(controller);
      PromisePrototypeThen(
        flushPromise,
        () => {
          if (readable[kState].state === 'errored')
            reject(readable[kState].storedError);
          else {
            readableStreamDefaultControllerClose(readable[kState].controller);
            resolve();
          }
        },
        (error) => {
          readableStreamDefaultControllerError(readable[kState].controller, error);
          reject(error);
        });
      return controller[kState].finishPromise;
    }

    function transformStreamDefaultSourcePullAlgorithm(stream) {
      const state = stream[kState];
      assert(state.backpressure);
      transformStreamSetBackpressure(stream, false);
      state.pullPending = true;
      return kParkedAlgorithmResult;
    }

    function transformStreamDefaultSourceCancelAlgorithm(stream, reason) {
      const {
        controller,
        writable,
      } = stream[kState];

      if (controller[kState].finishPromise !== undefined) {
        return controller[kState].finishPromise;
      }

      const { promise, resolve, reject } = PromiseWithResolvers();
      controller[kState].finishPromise = promise;
      const cancelPromise = controller[kState].cancelAlgorithm(reason);
      transformStreamDefaultControllerClearAlgorithms(controller);

      PromisePrototypeThen(
        cancelPromise,
        () => {
          if (writable[kState].state === 'errored')
            reject(writable[kState].storedError);
          else {
            writableStreamDefaultControllerErrorIfNeeded(
              writable[kState].controller,
              reason);
            transformStreamUnblockWrite(stream);
            resolve();
          }
        },
        (error) => {
          writableStreamDefaultControllerErrorIfNeeded(
            writable[kState].controller,
            error);
          transformStreamUnblockWrite(stream);
          reject(error);
        },
      );

      return controller[kState].finishPromise;
    }

    module.exports = {
      TransformStream,
      TransformStreamDefaultController,
      TransferredTransformStream,

      isTransformStream,
      isTransformStreamDefaultController,
    };
  },
  "internal/webstreams/util": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      Array,
      ArrayBufferPrototypeGetByteLength,
      ArrayBufferPrototypeGetDetached,
      ArrayBufferPrototypeSlice,
      DataViewPrototypeGetBuffer,
      DataViewPrototypeGetByteLength,
      DataViewPrototypeGetByteOffset,
      FunctionPrototypeCall,
      MathMax,
      NumberIsNaN,
      ObjectFreeze,
      PromisePrototypeThen,
      PromiseReject,
      PromiseResolve,
      Symbol,
      TypedArrayPrototypeGetBuffer,
      TypedArrayPrototypeGetByteLength,
      TypedArrayPrototypeGetByteOffset,
      Uint8Array,
    } = primordials;

    const {
      codes: {
        ERR_INVALID_ARG_VALUE,
      },
    } = require('internal/errors');

    const {
      copyArrayBuffer,
    } = internalBinding('buffer');

    const {
      inspect,
    } = require('util');

    const {
      constants: {
        kPending,
      },
      getPromiseDetails,
    } = internalBinding('util');

    const assert = require('internal/assert');

    const {
      isDataView,
    } = require('internal/util/types');

    const {
      validateFunction,
    } = require('internal/validators');

    const kState = Symbol('kState');
    const kType = Symbol('kType');

    const getNonWritablePropertyDescriptor = (value) => {
      return {
        __proto__: null,
        configurable: true,
        value,
      };
    };

    function extractHighWaterMark(value, defaultHWM) {
      if (value === undefined) return defaultHWM;
      const coercedValue = +value;
      if (Number.isNaN(coercedValue) ||
          coercedValue < 0)
        throw new ERR_INVALID_ARG_VALUE.RangeError('strategy.highWaterMark', value);
      return coercedValue;
    }

    const defaultSizeAlgorithm = () => 1;

    function extractSizeAlgorithm(size) {
      if (size === undefined) return defaultSizeAlgorithm;
      validateFunction(size, 'strategy.size');
      return size;
    }

    function customInspect(depth, options, name, data) {
      if (depth < 0)
        return this;

      const opts = {
        ...options,
        depth: options.depth == null ? null : options.depth - 1,
      };

      return `${name} ${inspect(data, opts)}`;
    }

    function ArrayBufferViewGetBuffer(view) {
      return isDataView(view) ?
        DataViewPrototypeGetBuffer(view) :
        view.buffer;
    }

    function ArrayBufferViewGetByteLength(view) {
      return isDataView(view) ?
        DataViewPrototypeGetByteLength(view) :
        view.byteLength;
    }

    function ArrayBufferViewGetByteOffset(view) {
      return isDataView(view) ?
        DataViewPrototypeGetByteOffset(view) :
        view.byteOffset;
    }

    function cloneAsUint8Array(view) {
      const buffer = ArrayBufferViewGetBuffer(view);
      const byteOffset = ArrayBufferViewGetByteOffset(view);
      const byteLength = ArrayBufferViewGetByteLength(view);
      return new Uint8Array(
        ArrayBufferPrototypeSlice(buffer, byteOffset, byteOffset + byteLength),
      );
    }

    function canCopyArrayBuffer(toBuffer, toIndex, fromBuffer, fromIndex, count) {
      return toBuffer !== fromBuffer &&
        !ArrayBufferPrototypeGetDetached(toBuffer) &&
        !ArrayBufferPrototypeGetDetached(fromBuffer) &&
        toIndex + count <= ArrayBufferPrototypeGetByteLength(toBuffer) &&
        fromIndex + count <= ArrayBufferPrototypeGetByteLength(fromBuffer);
    }

    function isBrandCheck(brand) {
      return (value) => {
        return value != null &&
               value[kState] !== undefined &&
               value[kType] === brand;
      };
    }

    class Queue {
      constructor(listLength = 8) {
        this.head = 0;
        this.tail = 0;
        this.length = 0;
        this.capacityMask = listLength - 1;
        this.list = new Array(listLength);
        this.dequeuedSize = 0;
      }

      push(entry) {
        if (this.length === this.list.length)
          this.grow();
        const tail = this.tail;
        this.list[tail] = entry;
        this.tail = (tail + 1) & this.capacityMask;
        this.length++;
      }

      shift() {
        const head = this.head;
        const list = this.list;
        const entry = list[head];
        list[head] = undefined;
        this.head = (head + 1) & this.capacityMask;
        if (--this.length === 0)
          this.rewind();
        return entry;
      }

      peek() {
        return this.list[this.head];
      }

      pushPair(value, size) {
        if (this.length * 2 === this.list.length)
          this.grow();
        const tail = this.tail;
        const list = this.list;
        list[tail] = value;
        list[tail + 1] = size;
        this.tail = (tail + 2) & this.capacityMask;
        this.length++;
      }

      shiftPair() {
        const head = this.head;
        const list = this.list;
        const value = list[head];
        this.dequeuedSize = list[head + 1];
        list[head] = undefined;
        list[head + 1] = undefined;
        this.head = (head + 2) & this.capacityMask;
        if (--this.length === 0)
          this.rewind();
        return value;
      }

      peekPairValue() {
        return this.list[this.head];
      }

      grow() {
        const list = this.list;
        const capacity = list.length;
        const head = this.head;
        if (head !== 0) {
          const relinearized = new Array(capacity * 2);
          let n = 0;
          for (let i = head; i < capacity; i++)
            relinearized[n++] = list[i];
          for (let i = 0; i < head; i++)
            relinearized[n++] = list[i];
          this.list = relinearized;
          this.head = 0;
        } else {
          list.length = capacity * 2;
        }
        this.tail = capacity;
        this.capacityMask = (capacity * 2) - 1;
      }

      rewind() {
        this.head = 0;
        this.tail = 0;
        if (this.list.length > 1024) {
          this.list.length = 8;
          this.capacityMask = 0b111;
        }
      }
    }

    const kEmptyQueue = Object.freeze(new Queue(0));

    function materializeQueue(state) {
      const queue = state.queue;
      if (queue === kEmptyQueue)
        return state.queue = new Queue();
      return queue;
    }

    function dequeueValue(controller) {
      const state = controller[kState];
      const queue = state.queue;
      assert(queue.length);
      const value = queue.shiftPair();
      state.queueTotalSize = Math.max(0, state.queueTotalSize - queue.dequeuedSize);
      return value;
    }

    function resetQueue(controller) {
      const state = controller[kState];
      state.queue = kEmptyQueue;
      state.queueTotalSize = 0;
    }

    function peekQueueValue(controller) {
      const state = controller[kState];
      assert(state.queue.length);
      return state.queue.peekPairValue();
    }

    function enqueueValueWithSize(controller, value, size) {
      const state = controller[kState];
      const coercedSize = +size;
      if (Number.isNaN(coercedSize) ||
          coercedSize < 0 ||
          coercedSize === Infinity) {
        throw new ERR_INVALID_ARG_VALUE.RangeError('size', size);
      }
      materializeQueue(state).pushPair(value, coercedSize);
      state.queueTotalSize += coercedSize;
    }

    function createPromiseCallbackNoParams(name, fn, thisArg) {
      validateFunction(fn, name);
      return async () => FunctionPrototypeCall(fn, thisArg);
    }

    function createRawCallback1Param(name, fn, thisArg) {
      validateFunction(fn, name);
      return (arg) => FunctionPrototypeCall(fn, thisArg, arg);
    }

    function createRawCallback2Params(name, fn, thisArg) {
      validateFunction(fn, name);
      return (arg1, arg2) => FunctionPrototypeCall(fn, thisArg, arg1, arg2);
    }

    const kResolvedPromise = Promise.resolve();

    const kParkedAlgorithmResult = Symbol('kParkedAlgorithmResult');

    function thenAlgorithmResult(result, onFulfilled, onRejected) {
      if (result === kParkedAlgorithmResult)
        return;
      if (result === null ||
          (typeof result !== 'object' && typeof result !== 'function')) {
        PromisePrototypeThen(kResolvedPromise, onFulfilled);
      } else {
        PromisePrototypeThen(Promise.resolve(result), onFulfilled, onRejected);
      }
    }

    function createPromiseCallback1Param(name, fn, thisArg) {
      validateFunction(fn, name);
      return async (arg) => FunctionPrototypeCall(fn, thisArg, arg);
    }

    function createPromiseCallback2Params(name, fn, thisArg) {
      validateFunction(fn, name);
      return async (arg1, arg2) => FunctionPrototypeCall(fn, thisArg, arg1, arg2);
    }

    function isPromisePending(promise) {
      if (promise === undefined) return false;
      const details = getPromiseDetails(promise);
      return details?.[0] === kPending;
    }

    function resolvedRecord() {
      return {
        promise: Promise.resolve(),
        resolve: undefined,
        reject: undefined,
      };
    }

    function rejectedHandledRecord(error) {
      const record = {
        promise: Promise.reject(error),
        resolve: undefined,
        reject: undefined,
      };
      setPromiseHandled(record.promise);
      return record;
    }

    function setPromiseHandled(promise) {
      PromisePrototypeThen(promise, undefined, () => {});
    }

    async function nonOpFlush() {}

    function nonOpCallback() {}

    async function nonOpCancel() {}

    let transfer;
    function lazyTransfer() {
      if (transfer === undefined)
        transfer = require('internal/webstreams/transfer');
      return transfer;
    }

    module.exports = {
      ArrayBufferViewGetBuffer,
      ArrayBufferViewGetByteLength,
      ArrayBufferViewGetByteOffset,
      Queue,
      canCopyArrayBuffer,
      cloneAsUint8Array,
      copyArrayBuffer,
      createPromiseCallbackNoParams,
      createPromiseCallback1Param,
      createPromiseCallback2Params,
      createRawCallback1Param,
      createRawCallback2Params,
      customInspect,
      defaultSizeAlgorithm,
      dequeueValue,
      enqueueValueWithSize,
      extractHighWaterMark,
      extractSizeAlgorithm,
      getNonWritablePropertyDescriptor,
      isBrandCheck,
      isPromisePending,
      kEmptyQueue,
      kParkedAlgorithmResult,
      kResolvedPromise,
      kState,
      kType,
      lazyTransfer,
      materializeQueue,
      nonOpCallback,
      nonOpCancel,
      nonOpFlush,

      peekQueueValue,
      rejectedHandledRecord,
      resetQueue,
      resolvedRecord,
      setPromiseHandled,
      thenAlgorithmResult,
    };
  },
  "internal/webstreams/writablestream": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      FunctionPrototypeBind,
      FunctionPrototypeCall,
      ObjectDefineProperties,
      ObjectSetPrototypeOf,
      Promise,
      PromisePrototypeThen,
      PromiseReject,
      PromiseResolve,
      PromiseWithResolvers,
      Symbol,
      SymbolToStringTag,
    } = primordials;

    const {
      codes: {
        ERR_ILLEGAL_CONSTRUCTOR,
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_ARG_VALUE,
        ERR_INVALID_STATE,
        ERR_INVALID_THIS,
      },
    } = require('internal/errors');

    const {
      DOMException,
    } = internalBinding('messaging');

    const {
      customInspectSymbol: kInspect,
      kEmptyObject,
      kEnumerableProperty,
      SideEffectFreeRegExpPrototypeSymbolReplace,
    } = require('internal/util');

    const {
      validateObject,
      kValidateObjectAllowObjects,
      kValidateObjectAllowObjectsAndNull,
    } = require('internal/validators');

    const {
      MessageChannel,
    } = require('internal/worker/io');

    const {
      kDeserialize,
      kTransfer,
      kTransferList,
      markTransferMode,
    } = require('internal/worker/js_transferable');

    const {
      Queue,
      createPromiseCallbackNoParams,
      createPromiseCallback1Param,
      createRawCallback2Params,
      customInspect,
      defaultSizeAlgorithm,
      dequeueValue,
      enqueueValueWithSize,
      extractHighWaterMark,
      extractSizeAlgorithm,
      getNonWritablePropertyDescriptor,
      isBrandCheck,
      isPromisePending,
      kEmptyQueue,
      kState,
      kType,
      lazyTransfer,
      nonOpCallback,
      nonOpCancel,
      peekQueueValue,
      rejectedHandledRecord,
      resetQueue,
      resolvedRecord,
      setPromiseHandled,
      thenAlgorithmResult,
    } = require('internal/webstreams/util');

    const {
      kIsClosedPromise,
      kIsErrored,
      kIsWritable,
      kControllerErrorFunction,
    } = require('internal/streams/utils');

    const {
      AbortController,
    } = require('internal/abort_controller');

    const {
      queueMicrotask,
    } = require('internal/process/task_queues');

    const assert = require('internal/assert');

    const kAbort = Symbol('kAbort');
    const kCloseSentinel = Symbol('kCloseSentinel');
    const kError = Symbol('kError');
    const kSkipThrow = Symbol('kSkipThrow');

    const kNilRequest = Object.setPrototypeOf({
      promise: undefined,
      resolve: undefined,
      reject: undefined,
    }, null);
    const kNilPendingAbortRequest = Object.setPrototypeOf({
      abort: kNilRequest,
      reason: undefined,
      wasAlreadyErroring: false,
    }, null);

    let releasedError;

    function lazyWritableReleasedError() {
      if (releasedError) {
        return releasedError;
      }
      const userModuleRegExp = /^ {4}at (?:[^/\\(]+ \()(?!node:(.+):\d+:\d+\)$).*/gm;

      releasedError = new ERR_INVALID_STATE.TypeError('Writer has been released');
      releasedError.stack = releasedError.stack.replace(userModuleRegExp, '');
      return releasedError;
    }

    class WritableStream {
      [kType] = 'WritableStream';

        constructor(sink = kEmptyObject, strategy = kEmptyObject) {
        markTransferMode(this, false, true);
        if (sink === kSkipThrow) {
          this[kState] = createWritableStreamState();
          return;
        }
        validateObject(sink, 'sink', kValidateObjectAllowObjects);
        validateObject(strategy, 'strategy', kValidateObjectAllowObjectsAndNull);
        const type = sink?.type;
        if (type !== undefined)
          throw new ERR_INVALID_ARG_VALUE.RangeError('type', type);

        this[kState] = createWritableStreamState();

        const size = extractSizeAlgorithm(strategy?.size);
        const highWaterMark = extractHighWaterMark(strategy?.highWaterMark, 1);

        setupWritableStreamDefaultControllerFromSink(
          this,
          sink,
          highWaterMark,
          size);
      }

      get [kIsErrored]() {
        return this[kState].state === 'errored';
      }

      get [kIsWritable]() {
        return this[kState].state === 'writable';
      }

      [kControllerErrorFunction](error) {
        this[kState].controller.error(error);
      }

      get [kIsClosedPromise]() {
        let cache = this[kState].closedPromise;
        if (cache === undefined) {
          switch (this[kState].state) {
            case 'writable':
            case 'erroring':
              cache = PromiseWithResolvers();
              break;
            case 'closed':
              cache = resolvedRecord();
              break;
            default:
              cache = rejectedHandledRecord(this[kState].storedError);
          }
          this[kState].closedPromise = cache;
        }
        return cache;
      }

        get locked() {
        if (!isWritableStream(this))
          throw new ERR_INVALID_THIS('WritableStream');
        return isWritableStreamLocked(this);
      }

        abort(reason = undefined) {
        if (!isWritableStream(this))
          return Promise.reject(new ERR_INVALID_THIS('WritableStream'));
        if (isWritableStreamLocked(this)) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError('WritableStream is locked'));
        }
        return writableStreamAbort(this, reason);
      }

        close() {
        if (!isWritableStream(this))
          return Promise.reject(new ERR_INVALID_THIS('WritableStream'));
        if (isWritableStreamLocked(this)) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError('WritableStream is locked'));
        }
        if (writableStreamCloseQueuedOrInFlight(this)) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError('Failure closing WritableStream'));
        }
        return writableStreamClose(this);
      }

        getWriter() {
        if (!isWritableStream(this))
          throw new ERR_INVALID_THIS('WritableStream');
        return new WritableStreamDefaultWriter(this);
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          locked: this.locked,
          state: this[kState].state,
        });
      }

      [kTransfer]() {
        if (!isWritableStream(this))
          throw new ERR_INVALID_THIS('WritableStream');
        if (this.locked) {
          this[kState].transfer.port1?.close();
          this[kState].transfer.port1 = undefined;
          this[kState].transfer.port2 = undefined;
          throw new DOMException(
            'Cannot transfer a locked WritableStream',
            'DataCloneError');
        }

        const {
          readable,
          promise,
        } = lazyTransfer().newCrossRealmReadableStream(
          this,
          this[kState].transfer.port1);

        this[kState].transfer.readable = readable;
        this[kState].transfer.promise = promise;

        return {
          data: { port: this[kState].transfer.port2 },
          deserializeInfo:
            'internal/webstreams/writablestream:TransferredWritableStream',
        };
      }

      [kTransferList]() {
        const { port1, port2 } = new MessageChannel();
        this[kState].transfer.port1 = port1;
        this[kState].transfer.port2 = port2;
        return [ port2 ];
      }

      [kDeserialize]({ port }) {
        const transfer = lazyTransfer();
        setupWritableStreamDefaultControllerFromSink(
          this,
          new transfer.CrossRealmTransformWritableSink(port, true),
          1,
          defaultSizeAlgorithm);
      }
    }

    Object.defineProperties(WritableStream.prototype, {
      locked: kEnumerableProperty,
      abort: kEnumerableProperty,
      close: kEnumerableProperty,
      getWriter: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(WritableStream.name),
    });

    function TransferredWritableStream() {
      return new WritableStream(kSkipThrow);
    }

    TransferredWritableStream.prototype[kDeserialize] = () => {};

    class WritableStreamDefaultWriter {
      [kType] = 'WritableStreamDefaultWriter';

        constructor(stream) {
        if (!isWritableStream(stream))
          throw new ERR_INVALID_ARG_TYPE('stream', 'WritableStream', stream);
        this[kState] = {
          stream: undefined,
          close: undefined,
          ready: undefined,
        };
        setupWritableStreamDefaultWriter(this, stream);
      }

        get closed() {
        if (!isWritableStreamDefaultWriter(this))
          return Promise.reject(new ERR_INVALID_THIS('WritableStreamDefaultWriter'));
        return writerClosedPromise(this).promise;
      }

        get desiredSize() {
        if (!isWritableStreamDefaultWriter(this))
          throw new ERR_INVALID_THIS('WritableStreamDefaultWriter');
        if (this[kState].stream === undefined) {
          throw new ERR_INVALID_STATE.TypeError(
            'Writer is not bound to a WritableStream');
        }
        return writableStreamDefaultWriterGetDesiredSize(this);
      }

        get ready() {
        if (!isWritableStreamDefaultWriter(this))
          return Promise.reject(new ERR_INVALID_THIS('WritableStreamDefaultWriter'));
        return writerReadyPromise(this).promise;
      }

        abort(reason = undefined) {
        if (!isWritableStreamDefaultWriter(this))
          return Promise.reject(new ERR_INVALID_THIS('WritableStreamDefaultWriter'));
        if (this[kState].stream === undefined) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError(
              'Writer is not bound to a WritableStream'));
        }
        return writableStreamDefaultWriterAbort(this, reason);
      }

        close() {
        if (!isWritableStreamDefaultWriter(this))
          return Promise.reject(new ERR_INVALID_THIS('WritableStreamDefaultWriter'));
        const {
          stream,
        } = this[kState];
        if (stream === undefined) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError(
              'Writer is not bound to a WritableStream'));
        }
        if (writableStreamCloseQueuedOrInFlight(stream)) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError('Failure to close WritableStream'));
        }
        return writableStreamDefaultWriterClose(this);
      }

      releaseLock() {
        if (!isWritableStreamDefaultWriter(this))
          throw new ERR_INVALID_THIS('WritableStreamDefaultWriter');
        const {
          stream,
        } = this[kState];
        if (stream === undefined)
          return;
        assert(stream[kState].writer !== undefined);
        writableStreamDefaultWriterRelease(this);
      }

        write(chunk = undefined) {
        if (!isWritableStreamDefaultWriter(this))
          return Promise.reject(new ERR_INVALID_THIS('WritableStreamDefaultWriter'));
        if (this[kState].stream === undefined) {
          return Promise.reject(
            new ERR_INVALID_STATE.TypeError(
              'Writer is not bound to a WritableStream'));
        }
        return writableStreamDefaultWriterWrite(this, chunk);
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          stream: this[kState].stream,
          close: writerClosedPromise(this).promise,
          ready: writerReadyPromise(this).promise,
          desiredSize: this.desiredSize,
        });
      }
    }

    Object.defineProperties(WritableStreamDefaultWriter.prototype, {
      closed: kEnumerableProperty,
      ready: kEnumerableProperty,
      desiredSize: kEnumerableProperty,
      abort: kEnumerableProperty,
      close: kEnumerableProperty,
      releaseLock: kEnumerableProperty,
      write: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(WritableStreamDefaultWriter.name),
    });

    class WritableStreamDefaultController {
      [kType] = 'WritableStreamDefaultController';
      [kState];

      constructor(skipThrowSymbol = undefined) {
        if (skipThrowSymbol !== kSkipThrow) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
      }

      [kAbort](reason) {
        const result = this[kState].abortAlgorithm(reason);
        writableStreamDefaultControllerClearAlgorithms(this);
        return result;
      }

      [kError]() {
        resetQueue(this);
      }

        get signal() {
        if (!isWritableStreamDefaultController(this))
          throw new ERR_INVALID_THIS('WritableStreamDefaultController');
        return this[kState].abortController.signal;
      }

        error(error = undefined) {
        if (!isWritableStreamDefaultController(this))
          throw new ERR_INVALID_THIS('WritableStreamDefaultController');
        if (this[kState].stream[kState].state !== 'writable')
          return;
        writableStreamDefaultControllerError(this, error);
      }

      [kInspect](depth, options) {
        return customInspect(depth, options, this[kType], {
          stream: this[kState].stream,
        });
      }
    }

    Object.defineProperties(WritableStreamDefaultController.prototype, {
      signal: kEnumerableProperty,
      error: kEnumerableProperty,
      [SymbolToStringTag]: getNonWritablePropertyDescriptor(WritableStreamDefaultController.name),
    });

    function createWritableStream(start, write, close, abort, highWaterMark = 1, size = defaultSizeAlgorithm) {
      const stream = new WritableStream(kSkipThrow);
      setupWritableStreamDefaultController(
        stream,
        new WritableStreamDefaultController(kSkipThrow),
        start,
        write,
        close,
        abort,
        highWaterMark,
        size,
      );
      return stream;
    }

    const isWritableStream =
      isBrandCheck('WritableStream');
    const isWritableStreamDefaultWriter =
      isBrandCheck('WritableStreamDefaultWriter');
    const isWritableStreamDefaultController =
      isBrandCheck('WritableStreamDefaultController');

    class WritableStreamTransferState {
      readable = undefined;
      port1 = undefined;
      port2 = undefined;
      promise = undefined;
    }
    Object.setPrototypeOf(WritableStreamTransferState.prototype, null);

    class WritableStreamState {
      closedPromise = undefined;
      closeRequest = kNilRequest;
      closeQueuedOrInFlight = false;
      inFlightWriteRequest = kNilRequest;
      inFlightCloseRequest = kNilRequest;
      pendingAbortRequest = kNilPendingAbortRequest;
      backpressure = false;
      controller = undefined;
      state = 'writable';
      storedError = undefined;
      writeRequests = kEmptyQueue;
      writer = undefined;
      transfer = new WritableStreamTransferState();
    }
    Object.setPrototypeOf(WritableStreamState.prototype, null);

    function createWritableStreamState() {
      return new WritableStreamState();
    }

    function isWritableStreamLocked(stream) {
      return stream[kState].writer !== undefined;
    }

    function setupWritableStreamDefaultWriter(writer, stream) {
      if (isWritableStreamLocked(stream))
        throw new ERR_INVALID_STATE.TypeError('WritableStream is locked');
      writer[kState].stream = stream;
      stream[kState].writer = writer;
    }

    function writerClosedPromise(writer) {
      let close = writer[kState].close;
      if (close === undefined) {
        const stream = writer[kState].stream;
        if (stream === undefined) {
          close = rejectedHandledRecord(lazyWritableReleasedError());
        } else {
          switch (stream[kState].state) {
            case 'writable':
            case 'erroring':
              close = PromiseWithResolvers();
              break;
            case 'closed':
              close = resolvedRecord();
              break;
            default:
              close = rejectedHandledRecord(stream[kState].storedError);
          }
        }
        writer[kState].close = close;
      }
      return close;
    }

    function writerReadyPromise(writer) {
      let ready = writer[kState].ready;
      if (ready === undefined) {
        const stream = writer[kState].stream;
        if (stream === undefined) {
          ready = rejectedHandledRecord(lazyWritableReleasedError());
        } else {
          switch (stream[kState].state) {
            case 'writable':
              ready = !writableStreamCloseQueuedOrInFlight(stream) &&
                      stream[kState].backpressure ?
                PromiseWithResolvers() :
                resolvedRecord();
              break;
            case 'closed':
              ready = resolvedRecord();
              break;
            default:
              ready = rejectedHandledRecord(stream[kState].storedError);
          }
        }
        writer[kState].ready = ready;
      }
      return ready;
    }

    function writableStreamAbort(stream, reason) {
      const { controller } = stream[kState];

      let state = stream[kState].state;
      if (state === 'closed' || state === 'errored')
        return Promise.resolve();

      controller[kState].abortController.abort(reason);

      state = stream[kState].state;
      if (state === 'closed' || state === 'errored')
        return Promise.resolve();

      if (stream[kState].pendingAbortRequest.abort.promise !== undefined)
        return stream[kState].pendingAbortRequest.abort.promise;

      assert(state === 'writable' || state === 'erroring');

      let wasAlreadyErroring = false;
      if (state === 'erroring') {
        wasAlreadyErroring = true;
        reason = undefined;
      }

      const abort = PromiseWithResolvers();

      stream[kState].pendingAbortRequest = {
        abort,
        reason,
        wasAlreadyErroring,
      };

      if (!wasAlreadyErroring)
        writableStreamStartErroring(stream, reason);

      return abort.promise;
    }

    function writableStreamClose(stream) {
      const {
        state,
        writer,
        backpressure,
        controller,
      } = stream[kState];
      if (state === 'closed' || state === 'errored') {
        return Promise.reject(
          new ERR_INVALID_STATE.TypeError('WritableStream is closed'));
      }
      assert(state === 'writable' || state === 'erroring');
      assert(!writableStreamCloseQueuedOrInFlight(stream));
      stream[kState].closeRequest = PromiseWithResolvers();
      stream[kState].closeQueuedOrInFlight = true;
      const { promise } = stream[kState].closeRequest;
      if (writer !== undefined && backpressure && state === 'writable')
        writer[kState].ready?.resolve();
      writableStreamDefaultControllerClose(controller);
      return promise;
    }

    function writableStreamUpdateBackpressure(controller, streamState) {
      const controllerState = controller[kState];
      const backpressure =
        controllerState.highWaterMark - controllerState.queueTotalSize <= 0;
      const writer = streamState.writer;
      const changed = streamState.backpressure !== backpressure;
      streamState.backpressure = backpressure;
      if (writer !== undefined && changed) {
        if (backpressure) {
          writer[kState].ready = undefined;
        } else {
          writer[kState].ready?.resolve();
        }
      }
    }

    function writableStreamStartErroring(stream, reason) {
      assert(stream[kState].storedError === undefined);
      assert(stream[kState].state === 'writable');
      const {
        controller,
        writer,
      } = stream[kState];
      assert(controller !== undefined);
      stream[kState].state = 'erroring';
      stream[kState].storedError = reason;
      if (writer !== undefined) {
        writableStreamDefaultWriterEnsureReadyPromiseRejected(writer, reason);
      }
      if (!writableStreamHasOperationMarkedInFlight(stream) &&
          controller[kState].started) {
        writableStreamFinishErroring(stream);
      }
    }

    function writableStreamRejectCloseAndClosedPromiseIfNeeded(stream) {
      assert(stream[kState].state === 'errored');
      if (stream[kState].closeRequest.promise !== undefined) {
        assert(stream[kState].inFlightCloseRequest.promise === undefined);
        stream[kState].closeRequest.reject(stream[kState].storedError);
        stream[kState].closeRequest = kNilRequest;
        stream[kState].closeQueuedOrInFlight = false;
      }

      const closedPromiseCache = stream[kState].closedPromise;
      if (closedPromiseCache !== undefined && isPromisePending(closedPromiseCache.promise)) {
        setPromiseHandled(closedPromiseCache.promise);
        closedPromiseCache.reject(stream[kState].storedError);
      }

      const {
        writer,
      } = stream[kState];
      if (writer !== undefined) {
        const closeCache = writer[kState].close;
        if (closeCache !== undefined && isPromisePending(closeCache.promise)) {
          setPromiseHandled(closeCache.promise);
          closeCache.reject(stream[kState].storedError);
        }
      }
    }

    function writableStreamMarkFirstWriteRequestInFlight(stream) {
      assert(stream[kState].inFlightWriteRequest.promise === undefined);
      assert(stream[kState].writeRequests.length);
      const writeRequest = stream[kState].writeRequests.shift();
      stream[kState].inFlightWriteRequest = writeRequest;
    }

    function writableStreamMarkCloseRequestInFlight(stream) {
      assert(stream[kState].inFlightWriteRequest.promise === undefined);
      assert(stream[kState].closeRequest.promise !== undefined);
      stream[kState].inFlightCloseRequest = stream[kState].closeRequest;
      stream[kState].closeRequest = kNilRequest;
    }

    function writableStreamHasOperationMarkedInFlight(stream) {
      const {
        inFlightWriteRequest,
        inFlightCloseRequest,
      } = stream[kState];
      if (inFlightWriteRequest.promise === undefined &&
          inFlightCloseRequest.promise === undefined) {
        return false;
      }
      return true;
    }

    function writableStreamFinishInFlightWriteWithError(stream, error) {
      assert(stream[kState].inFlightWriteRequest.promise !== undefined);
      stream[kState].inFlightWriteRequest.reject(error);
      stream[kState].inFlightWriteRequest = kNilRequest;
      assert(stream[kState].state === 'writable' ||
             stream[kState].state === 'erroring');
      writableStreamDealWithRejection(stream, error);
    }

    function writableStreamFinishInFlightWrite(stream) {
      assert(stream[kState].inFlightWriteRequest.promise !== undefined);
      stream[kState].inFlightWriteRequest.resolve();
      stream[kState].inFlightWriteRequest = kNilRequest;
    }

    function writableStreamFinishInFlightCloseWithError(stream, error) {
      assert(stream[kState].inFlightCloseRequest.promise !== undefined);
      stream[kState].inFlightCloseRequest.reject(error);
      stream[kState].inFlightCloseRequest = kNilRequest;
      stream[kState].closeQueuedOrInFlight = false;
      assert(stream[kState].state === 'writable' ||
             stream[kState].state === 'erroring');
      if (stream[kState].pendingAbortRequest.abort.promise !== undefined) {
        stream[kState].pendingAbortRequest.abort.reject(error);
        stream[kState].pendingAbortRequest = kNilPendingAbortRequest;
      }
      writableStreamDealWithRejection(stream, error);
    }

    function writableStreamFinishInFlightClose(stream) {
      assert(stream[kState].inFlightCloseRequest.promise !== undefined);
      stream[kState].inFlightCloseRequest.resolve();
      stream[kState].inFlightCloseRequest = kNilRequest;
      stream[kState].closeQueuedOrInFlight = false;
      if (stream[kState].state === 'erroring') {
        stream[kState].storedError = undefined;
        if (stream[kState].pendingAbortRequest.abort.promise !== undefined) {
          stream[kState].pendingAbortRequest.abort.resolve();
          stream[kState].pendingAbortRequest = kNilPendingAbortRequest;
        }
      }
      stream[kState].state = 'closed';
      if (stream[kState].writer !== undefined)
        stream[kState].writer[kState].close?.resolve();
      stream[kState].closedPromise?.resolve();
      assert(stream[kState].pendingAbortRequest.abort.promise === undefined);
      assert(stream[kState].storedError === undefined);
    }

    function writableStreamFinishErroring(stream) {
      assert(stream[kState].state === 'erroring');
      assert(!writableStreamHasOperationMarkedInFlight(stream));
      stream[kState].state = 'errored';
      stream[kState].controller[kError]();
      const storedError = stream[kState].storedError;
      const writeRequests = stream[kState].writeRequests;
      while (writeRequests.length)
        writeRequests.shift().reject(storedError);

      if (stream[kState].pendingAbortRequest.abort.promise === undefined) {
        writableStreamRejectCloseAndClosedPromiseIfNeeded(stream);
        return;
      }

      const abortRequest = stream[kState].pendingAbortRequest;
      stream[kState].pendingAbortRequest = kNilPendingAbortRequest;
      if (abortRequest.wasAlreadyErroring) {
        abortRequest.abort.reject(storedError);
        writableStreamRejectCloseAndClosedPromiseIfNeeded(stream);
        return;
      }
      stream[kState].closedPromise ??= PromiseWithResolvers();
      const writer = stream[kState].writer;
      if (writer !== undefined)
        writer[kState].close ??= PromiseWithResolvers();
      PromisePrototypeThen(
        stream[kState].controller[kAbort](abortRequest.reason),
        () => {
          abortRequest.abort.resolve();
          writableStreamRejectCloseAndClosedPromiseIfNeeded(stream);
        },
        (error) => {
          abortRequest.abort.reject(error);
          writableStreamRejectCloseAndClosedPromiseIfNeeded(stream);
        });
    }

    function writableStreamDealWithRejection(stream, error) {
      const {
        state,
      } = stream[kState];
      if (state === 'writable') {
        writableStreamStartErroring(stream, error);
        return;
      }

      assert(state === 'erroring');
      writableStreamFinishErroring(stream);
    }

    function writableStreamCloseQueuedOrInFlight(stream) {
      return stream[kState].closeQueuedOrInFlight;
    }

    function writableStreamAddWriteRequest(stream) {
      assert(isWritableStreamLocked(stream));
      assert(stream[kState].state === 'writable');
      const writeRequest = PromiseWithResolvers();
      const streamState = stream[kState];
      let writeRequests = streamState.writeRequests;
      if (writeRequests === kEmptyQueue)
        writeRequests = streamState.writeRequests = new Queue();
      writeRequests.push(writeRequest);
      return writeRequest.promise;
    }

    function writableStreamDefaultWriterWrite(writer, chunk) {
      const writerState = writer[kState];
      const stream = writerState.stream;
      assert(stream !== undefined);
      const streamState = stream[kState];
      const {
        controller,
      } = streamState;
      const chunkSize = writableStreamDefaultControllerGetChunkSize(
        controller,
        chunk);
      if (stream !== writerState.stream) {
        return Promise.reject(
          new ERR_INVALID_STATE.TypeError('Mismatched WritableStreams'));
      }
      const {
        state,
      } = streamState;

      if (state === 'errored')
        return Promise.reject(streamState.storedError);

      if (streamState.closeQueuedOrInFlight || state === 'closed') {
        return Promise.reject(
          new ERR_INVALID_STATE.TypeError('WritableStream is closed'));
      }

      if (state === 'erroring')
        return Promise.reject(streamState.storedError);

      assert(state === 'writable');

      const promise = writableStreamAddWriteRequest(stream);
      writableStreamDefaultControllerWrite(controller, chunk, chunkSize);
      return promise;
    }

    function writableStreamDefaultWriterWriteWithRequest(writer, chunk, request) {
      const writerState = writer[kState];
      const stream = writerState.stream;
      assert(stream !== undefined);
      const streamState = stream[kState];
      const {
        controller,
      } = streamState;
      const chunkSize = writableStreamDefaultControllerGetChunkSize(
        controller,
        chunk);
      if (stream !== writerState.stream) {
        request.failed = true;
        request.failure =
          new ERR_INVALID_STATE.TypeError('Mismatched WritableStreams');
        return;
      }
      const {
        state,
      } = streamState;

      if (state === 'errored') {
        request.failed = true;
        request.failure = streamState.storedError;
        return;
      }

      if (streamState.closeQueuedOrInFlight || state === 'closed') {
        request.failed = true;
        request.failure =
          new ERR_INVALID_STATE.TypeError('WritableStream is closed');
        return;
      }

      if (state === 'erroring') {
        request.failed = true;
        request.failure = streamState.storedError;
        return;
      }

      assert(state === 'writable');

      let writeRequests = streamState.writeRequests;
      if (writeRequests === kEmptyQueue)
        writeRequests = streamState.writeRequests = new Queue();
      writeRequests.push(request);
      request.pending++;
      writableStreamDefaultControllerWrite(controller, chunk, chunkSize);
    }

    function writableStreamDefaultWriterRelease(writer) {
      const {
        stream,
      } = writer[kState];
      assert(stream !== undefined);
      assert(stream[kState].writer === writer);
      const releasedStateError = lazyWritableReleasedError();
      writableStreamDefaultWriterEnsureReadyPromiseRejected(writer, releasedStateError);
      writableStreamDefaultWriterEnsureClosedPromiseRejected(writer, releasedStateError);
      stream[kState].writer = undefined;
      writer[kState].stream = undefined;
    }

    function writableStreamDefaultWriterGetDesiredSize(writer) {
      const {
        stream,
      } = writer[kState];
      switch (stream[kState].state) {
        case 'errored':
        case 'erroring':
          return null;
        case 'closed':
          return 0;
      }
      return writableStreamDefaultControllerGetDesiredSize(
        stream[kState].controller);
    }

    function writableStreamDefaultWriterEnsureReadyPromiseRejected(writer, error) {
      const ready = writer[kState].ready;
      if (ready !== undefined && isPromisePending(ready.promise)) {
        ready.reject(error);
        setPromiseHandled(ready.promise);
      } else {
        writer[kState].ready = undefined;
      }
    }

    function writableStreamDefaultWriterEnsureClosedPromiseRejected(writer, error) {
      const close = writer[kState].close;
      if (close !== undefined && isPromisePending(close.promise)) {
        close.reject(error);
        setPromiseHandled(close.promise);
      } else {
        writer[kState].close = undefined;
      }
    }

    function writableStreamDefaultWriterCloseWithErrorPropagation(writer) {
      const {
        stream,
      } = writer[kState];
      assert(stream !== undefined);
      const {
        state,
      } = stream[kState];
      if (writableStreamCloseQueuedOrInFlight(stream) || state === 'closed')
        return Promise.resolve();

      if (state === 'errored')
        return Promise.reject(stream[kState].storedError);

      assert(state === 'writable' || state === 'erroring');

      return writableStreamDefaultWriterClose(writer);
    }

    function writableStreamDefaultWriterClose(writer) {
      const {
        stream,
      } = writer[kState];
      assert(stream !== undefined);
      return writableStreamClose(stream);
    }

    function writableStreamDefaultWriterAbort(writer, reason) {
      const {
        stream,
      } = writer[kState];
      assert(stream !== undefined);
      return writableStreamAbort(stream, reason);
    }

    function writableStreamDefaultControllerWrite(controller, chunk, chunkSize) {
      try {
        enqueueValueWithSize(controller, chunk, chunkSize);
      } catch (error) {
        writableStreamDefaultControllerErrorIfNeeded(controller, error);
        return;
      }
      const {
        stream,
      } = controller[kState];
      const streamState = stream[kState];
      if (!streamState.closeQueuedOrInFlight &&
          streamState.state === 'writable') {
        writableStreamUpdateBackpressure(controller, streamState);
      }
      writableStreamDefaultControllerAdvanceQueueIfNeeded(controller);
    }

    function writableStreamDefaultControllerProcessWrite(controller, chunk) {
      const {
        stream,
        writeAlgorithm,
      } = controller[kState];
      writableStreamMarkFirstWriteRequestInFlight(stream);

      if (controller[kState].writeFulfilled === undefined) {
        controller[kState].writeFulfilled = () => {
          writableStreamFinishInFlightWrite(stream);
          const streamState = stream[kState];
          const {
            state,
          } = streamState;
          assert(state === 'writable' || state === 'erroring');
          dequeueValue(controller);
          if (!streamState.closeQueuedOrInFlight &&
              state === 'writable') {
            writableStreamUpdateBackpressure(controller, streamState);
          }
          writableStreamDefaultControllerAdvanceQueueIfNeeded(controller);
        };
        controller[kState].writeRejected = (error) => {
          if (stream[kState].state === 'writable')
            writableStreamDefaultControllerClearAlgorithms(controller);
          writableStreamFinishInFlightWriteWithError(stream, error);
        };
      }

      let result;
      try {
        result = writeAlgorithm(chunk, controller);
      } catch (error) {
        result = Promise.reject(error);
      }
      thenAlgorithmResult(
        result,
        controller[kState].writeFulfilled,
        controller[kState].writeRejected);
    }

    function writableStreamDefaultControllerProcessClose(controller) {
      const {
        closeAlgorithm,
        queue,
        stream,
      } = controller[kState];
      writableStreamMarkCloseRequestInFlight(stream);
      dequeueValue(controller);
      assert(!queue.length);
      const sinkClosePromise = closeAlgorithm();
      writableStreamDefaultControllerClearAlgorithms(controller);
      PromisePrototypeThen(
        sinkClosePromise,
        () => writableStreamFinishInFlightClose(stream),
        (error) => writableStreamFinishInFlightCloseWithError(stream, error));
    }

    function writableStreamDefaultControllerGetDesiredSize(controller) {
      const {
        highWaterMark,
        queueTotalSize,
      } = controller[kState];
      return highWaterMark - queueTotalSize;
    }

    function writableStreamDefaultControllerGetChunkSize(controller, chunk) {
      const {
        stream,
        sizeAlgorithm,
      } = controller[kState];
      if (sizeAlgorithm === undefined) {
        assert(stream[kState].state === 'closed' ||
               stream[kState].state === 'errored' ||
               stream[kState].state === 'erroring');
        return 1;
      }

      if (sizeAlgorithm === defaultSizeAlgorithm)
        return 1;

      try {
        return FunctionPrototypeCall(
          sizeAlgorithm,
          undefined,
          chunk);
      } catch (error) {
        writableStreamDefaultControllerErrorIfNeeded(controller, error);
        return 1;
      }
    }

    function writableStreamDefaultControllerErrorIfNeeded(controller, error) {
      const {
        stream,
      } = controller[kState];
      if (stream[kState].state === 'writable')
        writableStreamDefaultControllerError(controller, error);
    }

    function writableStreamDefaultControllerError(controller, error) {
      const {
        stream,
      } = controller[kState];
      assert(stream[kState].state === 'writable');
      writableStreamDefaultControllerClearAlgorithms(controller);
      writableStreamStartErroring(stream, error);
    }

    function writableStreamDefaultControllerClose(controller) {
      enqueueValueWithSize(controller, kCloseSentinel, 0);
      writableStreamDefaultControllerAdvanceQueueIfNeeded(controller);
    }

    function writableStreamDefaultControllerClearAlgorithms(controller) {
      controller[kState].writeAlgorithm = undefined;
      controller[kState].closeAlgorithm = undefined;
      controller[kState].abortAlgorithm = undefined;
      controller[kState].sizeAlgorithm = undefined;
    }

    function writableStreamDefaultControllerAdvanceQueueIfNeeded(controller) {
      const {
        queue,
        started,
        stream,
      } = controller[kState];
      if (!started || stream[kState].inFlightWriteRequest.promise !== undefined)
        return;

      if (stream[kState].state === 'erroring') {
        writableStreamFinishErroring(stream);
        return;
      }

      if (!queue.length)
        return;

      const value = peekQueueValue(controller);
      if (value === kCloseSentinel)
        writableStreamDefaultControllerProcessClose(controller);
      else
        writableStreamDefaultControllerProcessWrite(controller, value);
    }

    function setupWritableStreamDefaultControllerFromSink(
      stream,
      sink,
      highWaterMark,
      sizeAlgorithm) {
      const controller = new WritableStreamDefaultController(kSkipThrow);
      const start = sink?.start;
      const write = sink?.write;
      const close = sink?.close;
      const abort = sink?.abort;
      const startAlgorithm = start ?
        FunctionPrototypeBind(start, sink, controller) :
        nonOpCallback;
      const writeAlgorithm = write ?
        createRawCallback2Params('sink.write', write, sink) :
        nonOpCallback;
      const closeAlgorithm = close ?
        createPromiseCallbackNoParams('sink.close', close, sink) :
        nonOpCancel;
      const abortAlgorithm = abort ?
        createPromiseCallback1Param('sink.abort', abort, sink) :
        nonOpCancel;
      setupWritableStreamDefaultController(
        stream,
        controller,
        startAlgorithm,
        writeAlgorithm,
        closeAlgorithm,
        abortAlgorithm,
        highWaterMark,
        sizeAlgorithm);
    }

    function setupWritableStreamDefaultController(
      stream,
      controller,
      startAlgorithm,
      writeAlgorithm,
      closeAlgorithm,
      abortAlgorithm,
      highWaterMark,
      sizeAlgorithm) {
      assert(isWritableStream(stream));
      assert(stream[kState].controller === undefined);
      controller[kState] = {
        abortAlgorithm,
        closeAlgorithm,
        highWaterMark,
        queue: kEmptyQueue,
        queueTotalSize: 0,
        abortController: new AbortController(),
        sizeAlgorithm,
        started: false,
        stream,
        writeAlgorithm,
        writeFulfilled: undefined,
        writeRejected: undefined,
      };
      stream[kState].controller = controller;

      writableStreamUpdateBackpressure(controller, stream[kState]);

      const startResult = startAlgorithm();

      const started = () => {
        assert(stream[kState].state === 'writable' ||
               stream[kState].state === 'erroring');
        controller[kState].started = true;
        writableStreamDefaultControllerAdvanceQueueIfNeeded(controller);
      };

      if (startResult === null ||
          (typeof startResult !== 'object' && typeof startResult !== 'function')) {
        queueMicrotask(started);
        return;
      }

      PromisePrototypeThen(
        new Promise((r) => r(startResult)),
        started,
        (error) => {
          assert(stream[kState].state === 'writable' ||
                 stream[kState].state === 'erroring');
          controller[kState].started = true;
          writableStreamDealWithRejection(stream, error);
        });
    }

    module.exports = {
      WritableStream,
      WritableStreamDefaultWriter,
      WritableStreamDefaultController,
      TransferredWritableStream,

      isWritableStream,
      isWritableStreamDefaultController,
      isWritableStreamDefaultWriter,

      isWritableStreamLocked,
      setupWritableStreamDefaultWriter,
      writableStreamAbort,
      writableStreamClose,
      writableStreamUpdateBackpressure,
      writableStreamStartErroring,
      writableStreamRejectCloseAndClosedPromiseIfNeeded,
      writableStreamMarkFirstWriteRequestInFlight,
      writableStreamMarkCloseRequestInFlight,
      writableStreamHasOperationMarkedInFlight,
      writableStreamFinishInFlightWriteWithError,
      writableStreamFinishInFlightWrite,
      writableStreamFinishInFlightCloseWithError,
      writableStreamFinishInFlightClose,
      writableStreamFinishErroring,
      writableStreamDealWithRejection,
      writableStreamCloseQueuedOrInFlight,
      writableStreamAddWriteRequest,
      writableStreamDefaultWriterWrite,
      writableStreamDefaultWriterWriteWithRequest,
      writableStreamDefaultWriterRelease,
      writableStreamDefaultWriterGetDesiredSize,
      writableStreamDefaultWriterEnsureReadyPromiseRejected,
      writableStreamDefaultWriterEnsureClosedPromiseRejected,
      writerClosedPromise,
      writerReadyPromise,
      writableStreamDefaultWriterCloseWithErrorPropagation,
      writableStreamDefaultWriterClose,
      writableStreamDefaultWriterAbort,
      writableStreamDefaultControllerWrite,
      writableStreamDefaultControllerProcessWrite,
      writableStreamDefaultControllerProcessClose,
      writableStreamDefaultControllerGetDesiredSize,
      writableStreamDefaultControllerGetChunkSize,
      writableStreamDefaultControllerErrorIfNeeded,
      writableStreamDefaultControllerError,
      writableStreamDefaultControllerClose,
      writableStreamDefaultControllerClearAlgorithms,
      writableStreamDefaultControllerAdvanceQueueIfNeeded,
      setupWritableStreamDefaultControllerFromSink,
      setupWritableStreamDefaultController,
      createWritableStream,
    };
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

// what the other ports may have from here: the files, and not the stand-ins,
// which are this port's own answers to its own questions
function shared(name) {
  if (!Object.hasOwn(files, name)) {
    throw new Error("no module " + name + " in this port");
  }
  return own(name);
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

module.exports = load("stream/web");
