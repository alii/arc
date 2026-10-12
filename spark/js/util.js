"use strict";

// node's lib/util.js and the files only it uses: deep equality, parseArgs,
// MIMEType, debounce, throttle and diff. inspect, format, the type checks,
// promisify and deprecate come from the shared internals

const internal = require("spark:internal");

function optional(specifier) {
  try {
    return require(specifier);
  } catch {
    return undefined;
  }
}

internal.addLoader(own);

function missing(what) {
  return () => {
    throw new internal.load("internal/errors").codes.ERR_METHOD_NOT_IMPLEMENTED(what);
  };
}

const shims = {
  process: internal.process,
  primordials: internal.primordials,
  internalBinding: internal.internalBinding,
  fallback: internal.find,
  // until the stream module is here nothing is a stream
  "internal/streams/utils": () => ({
    isReadableStream: () => false,
    isWritableStream: () => false,
    isNodeStream: () => false,
  }),
  "internal/abort_controller": () => ({
    transferableAbortSignal: missing("transferableAbortSignal"),
    transferableAbortController: missing("transferableAbortController"),
    aborted: missing("aborted"),
  }),
  "internal/encoding": () => optional("spark:encoding") ?? {},
};

const files = {
  "util": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayIsArray,
      ArrayPrototypePop,
      ArrayPrototypePush,
      Error,
      ErrorCaptureStackTrace,
      FunctionPrototypeBind,
      NumberIsSafeInteger,
      ObjectDefineProperties,
      ObjectDefineProperty,
      ObjectGetOwnPropertyDescriptors,
      ObjectGetOwnPropertyNames,
      ObjectKeys,
      ObjectSetPrototypeOf,
      ObjectValues,
      ReflectApply,
      RegExpPrototypeExec,
      SafeMap,
      StringPrototypeSlice,
      StringPrototypeToWellFormed,
    } = primordials;

    const {
      ErrnoException,
      ExceptionWithHostPort,
      codes: {
        ERR_FALSY_VALUE_REJECTION,
        ERR_INVALID_ARG_TYPE,
        ERR_INVALID_ARG_VALUE,
        ERR_OUT_OF_RANGE,
      },
      isErrorStackTraceLimitWritable,
    } = require('internal/errors');
    const { Buffer } = require('buffer');
    const {
      format,
      formatWithOptions,
      inspect,
      stripVTControlCharacters,
    } = require('internal/util/inspect');
    const { debuglog } = require('internal/util/debuglog');
    const {
      validateBoolean,
      validateFunction,
      validateNumber,
      validateString,
      validateOneOf,
      validateObject,
      validateInteger,
    } = require('internal/validators');
    const {
      isReadableStream,
      isWritableStream,
      isNodeStream,
    } = require('internal/streams/utils');
    const types = require('internal/util/types');

    let utilColors;
    function lazyUtilColors() {
      utilColors ??= require('internal/util/colors');
      return utilColors;
    }
    const { getOptionValue } = require('internal/options');

    const binding = internalBinding('util');

    const {
      convertProcessSignalToExitCode,
      deprecate: internalDeprecate,
      getLazy,
      getSystemErrorMap,
      getSystemErrorName: internalErrorName,
      getSystemErrorMessage: internalErrorMessage,
      promisify,
      defineLazyProperties,
      kEmptyObject,
    } = require('internal/util');

    let abortController;

    function lazyAbortController() {
      abortController ??= require('internal/abort_controller');
      return abortController;
    }

    let internalDeepEqual;
    let internalPartialDeepEqual;

    const kEscape = '\u001b[';
    const kEscapeEnd = 'm';

    const kDimCode = 2;
    const kBoldCode = 1;

    const kHexCloseSeq = kEscape + '39' + kEscapeEnd;

    let styleCache;

    const kHexStyleCacheMax = 256;

    let hexStyleCache;

    function getHexStyleCache() {
      hexStyleCache ??= new SafeMap();
      return hexStyleCache;
    }

    function codesToStyle(codes) {
      const openNum = codes[0];
      return {
        __proto__: null,
        openSeq: kEscape + openNum + kEscapeEnd,
        closeSeq: kEscape + codes[1] + kEscapeEnd,
        keepClose: openNum === kDimCode || openNum === kBoldCode,
      };
    }

    function getStyleCache() {
      if (styleCache === undefined) {
        styleCache = { __proto__: null };
        const colors = inspect.colors;
        for (const key of Object.getOwnPropertyNames(colors)) {
          const codes = colors[key];
          if (codes) {
            styleCache[key] = codesToStyle(codes);
          }
        }
      }
      return styleCache;
    }

    function getHexStyle(hex) {
      const cache = getHexStyleCache();
      const cached = cache.get(hex);
      if (cached !== undefined) return cached;
      const { 0: r, 1: g, 2: b } = hexToRgb(hex);
      const style = {
        __proto__: null,
        openSeq: kEscape + rgbToAnsi24Bit(r, g, b) + kEscapeEnd,
        closeSeq: kHexCloseSeq,
      };
      if (cache.size >= kHexStyleCacheMax)
        cache.delete(cache.keys().next().value);
      cache.set(hex, style);
      return style;
    }

    function replaceCloseCode(str, closeSeq, openSeq, keepClose) {
      const closeLen = closeSeq.length;
      let index = str.indexOf(closeSeq);
      if (index === -1) return str;

      let result = '';
      let lastIndex = 0;
      const replacement = keepClose ? closeSeq + openSeq : openSeq;

      do {
        const afterClose = index + closeLen;
        if (afterClose < str.length) {
          result += str.slice(lastIndex, index) + replacement;
          lastIndex = afterClose;
        } else {
          break;
        }
        index = str.indexOf(closeSeq, lastIndex);
      } while (index !== -1);

      return result + str.slice(lastIndex);
    }

    const hexColorRegExp = /^#(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{6})$/;

    function hexToRgb(hex) {
      let hexStr;
      if (hex.length === 4) {
        hexStr = hex[1] + hex[1] + hex[2] + hex[2] + hex[3] + hex[3];
      } else if (hex.length === 7) {
        hexStr = hex.slice(1);
      } else {
        throw new ERR_OUT_OF_RANGE('hex', '#RGB or #RRGGBB', hex);
      }

      return Buffer.from(hexStr, 'hex');
    }

    function rgbToAnsi24Bit(r, g, b) {
      return `38;2;${r};${g};${b}`;
    }

    function styleText(format, text, options) {
      const validateStream = options?.validateStream ?? true;

      if (!validateStream && typeof format === 'string' && typeof text === 'string') {
        const cache = getStyleCache();
        if (format === 'none') return text;
        const style = cache[format];
        if (style !== undefined) {
          const processed = replaceCloseCode(text, style.closeSeq, style.openSeq, style.keepClose);
          return style.openSeq + processed + style.closeSeq;
        }

        if (format[0] === '#') {
          let hexStyle = getHexStyleCache().get(format);
          if (hexStyle === undefined && hexColorRegExp.exec(format) !== null) {
            hexStyle = getHexStyle(format);
          }
          if (hexStyle !== undefined) {
            const processed = replaceCloseCode(text, hexStyle.closeSeq, hexStyle.openSeq, false);
            return hexStyle.openSeq + processed + hexStyle.closeSeq;
          }
        }
      }

      validateString(text, 'text');
      if (options !== undefined) {
        validateObject(options, 'options');
      }
      validateBoolean(validateStream, 'options.validateStream');

      let skipColorize;
      if (validateStream) {
        const stream = options?.stream ?? process.stdout;
        if (
          !isReadableStream(stream) &&
          !isWritableStream(stream) &&
          !isNodeStream(stream)
        ) {
          throw new ERR_INVALID_ARG_TYPE('stream', ['ReadableStream', 'WritableStream', 'Stream'], stream);
        }
        skipColorize = !lazyUtilColors().shouldColorize(stream);
      }

      const formatArray = Array.isArray(format) ? format : [format];
      const colors = inspect.colors;

      let openCodes = '';
      let closeCodes = '';
      let processedText = text;

      for (const key of formatArray) {
        if (key === 'none') continue;

        if (typeof key === 'string' && key[0] === '#') {
          if (hexColorRegExp.exec(key) === null) {
            throw new ERR_INVALID_ARG_VALUE('format', key,
                                            'must be a valid hex color (#RGB or #RRGGBB)');
          }
          if (skipColorize) continue;
          const { 0: r, 1: g, 2: b } = hexToRgb(key);
          const hexOpenSeq = kEscape + rgbToAnsi24Bit(r, g, b) + kEscapeEnd;
          openCodes += hexOpenSeq;
          closeCodes = kHexCloseSeq + closeCodes;
          processedText = replaceCloseCode(processedText, kHexCloseSeq, hexOpenSeq, false);
          continue;
        }

        const codes = colors[key];
        if (!codes) {
          validateOneOf(key, 'format', Object.getOwnPropertyNames(inspect.colors));
        }
        const { openSeq, closeSeq, keepClose } = codesToStyle(codes);
        openCodes += openSeq;
        closeCodes = closeSeq + closeCodes;
        processedText = replaceCloseCode(processedText, closeSeq, openSeq, keepClose);
      }

      if (skipColorize) return text;

      return `${openCodes}${processedText}${closeCodes}`;
    }

    function inherits(ctor, superCtor) {

      if (ctor === undefined || ctor === null)
        throw new ERR_INVALID_ARG_TYPE('ctor', 'Function', ctor);

      if (superCtor === undefined || superCtor === null)
        throw new ERR_INVALID_ARG_TYPE('superCtor', 'Function', superCtor);

      if (superCtor.prototype === undefined) {
        throw new ERR_INVALID_ARG_TYPE('superCtor.prototype',
                                       'Object', superCtor.prototype);
      }
      Object.defineProperty(ctor, 'super_', {
        __proto__: null,
        value: superCtor,
        writable: true,
        configurable: true,
      });
      Object.setPrototypeOf(ctor.prototype, superCtor.prototype);
    }

    function _extend(target, source) {
      if (source === null || typeof source !== 'object') return target;

      const keys = Object.keys(source);
      let i = keys.length;
      while (i--) {
        target[keys[i]] = source[keys[i]];
      }
      return target;
    }

    const callbackifyOnRejected = (reason, cb) => {
      if (!reason) {
        reason = new ERR_FALSY_VALUE_REJECTION.HideStackFramesError(reason);
        Error.captureStackTrace(reason, callbackifyOnRejected);
      }
      return cb(reason);
    };

    function callbackify(original) {
      validateFunction(original, 'original');

      function callbackified(...args) {
        const maybeCb = args.pop();
        validateFunction(maybeCb, 'last argument');
        const cb = FunctionPrototypeBind(maybeCb, this);
        Reflect.apply(original, this, args)
          .then((ret) => process.nextTick(cb, null, ret),
                (rej) => process.nextTick(callbackifyOnRejected, rej, cb));
      }

      const descriptors = ObjectGetOwnPropertyDescriptors(original);
      if (typeof descriptors.length.value === 'number') {
        descriptors.length.value++;
      }
      if (typeof descriptors.name.value === 'string') {
        descriptors.name.value += 'Callbackified';
      }
      const propertiesValues = Object.values(descriptors);
      for (let i = 0; i < propertiesValues.length; i++) {
        Object.setPrototypeOf(propertiesValues[i], null);
      }
      Object.defineProperties(callbackified, descriptors);
      return callbackified;
    }

    function getSystemErrorMessage(err) {
      validateNumber(err, 'err');
      if (err >= 0 || !Number.isSafeInteger(err)) {
        throw new ERR_OUT_OF_RANGE('err', 'a negative integer', err);
      }
      return internalErrorMessage(err);
    }

    function getSystemErrorName(err) {
      validateNumber(err, 'err');
      if (err >= 0 || !Number.isSafeInteger(err)) {
        throw new ERR_OUT_OF_RANGE('err', 'a negative integer', err);
      }
      return internalErrorName(err);
    }

    function _errnoException(...args) {
      if (isErrorStackTraceLimitWritable()) {
        const limit = Error.stackTraceLimit;
        Error.stackTraceLimit = 0;
        const e = new ErrnoException(...args);
        Error.stackTraceLimit = limit;
        Error.captureStackTrace(e, _errnoException);
        return e;
      }
      return new ErrnoException(...args);
    }

    function _exceptionWithHostPort(...args) {
      if (isErrorStackTraceLimitWritable()) {
        const limit = Error.stackTraceLimit;
        Error.stackTraceLimit = 0;
        const e = new ExceptionWithHostPort(...args);
        Error.stackTraceLimit = limit;
        Error.captureStackTrace(e, _exceptionWithHostPort);
        return e;
      }
      return new ExceptionWithHostPort(...args);
    }

    function parseEnv(content) {
      validateString(content, 'content');
      return binding.parseEnv(content);
    }

    const lazySourceMap = getLazy(() => require('internal/source_map/source_map_cache'));

    function reconstructCallSite(callSite) {
      const { scriptName, lineNumber, columnNumber } = callSite;
      const sourceMap = lazySourceMap().findSourceMap(scriptName);
      if (!sourceMap) return;
      const entry = sourceMap.findEntry(lineNumber - 1, columnNumber - 1);
      if (!entry?.originalSource) return;
      return {
        __proto__: null,
        functionName: entry.name || callSite.functionName,
        scriptName: entry.originalSource,
        lineNumber: entry.originalLine + 1,
        column: entry.originalColumn + 1,
        columnNumber: entry.originalColumn + 1,
      };
    }

    function mapCallSite(callSites) {
      const result = [];
      for (let i = 0; i < callSites.length; ++i) {
        const callSite = callSites[i];
        const found = reconstructCallSite(callSite);
        result.push(found ?? callSite);
      }
      return result;
    }

    function getCallSites(frameCount = 10, options) {
      if (options === undefined) {
        if (typeof frameCount === 'object') {
          options = frameCount;
          validateObject(options, 'options');
          if (options.sourceMap !== undefined) {
            validateBoolean(options.sourceMap, 'options.sourceMap');
          }
          frameCount = 10;
        } else {
          options = {};
        };
      } else {
        validateObject(options, 'options');
        if (options.sourceMap !== undefined) {
          validateBoolean(options.sourceMap, 'options.sourceMap');
        }
      }

      validateInteger(frameCount, 'frameCount', 1, 200);
      if (options.sourceMap === true || (getOptionValue('--enable-source-maps') && options.sourceMap !== false)) {
        return mapCallSite(binding.getCallSites(frameCount));
      }
      return binding.getCallSites(frameCount);
    };

    function deprecate(fn, msg, code, { modifyPrototype } = kEmptyObject) {
      return internalDeprecate(fn, msg, code, undefined, modifyPrototype);
    }

    function markPromiseAsHandled(promise) {
      if (!types.isPromise(promise)) {
        throw new ERR_INVALID_ARG_TYPE('promise', 'Promise', promise);
      }
      binding.markPromiseAsHandled(promise);
    }

    module.exports = {
      _errnoException,
      _exceptionWithHostPort,
      _extend: internalDeprecate(_extend,
                                 'The `util._extend` API is deprecated. Please use Object.assign() instead.',
                                 'DEP0060'),
      callbackify,
      convertProcessSignalToExitCode,
      debug: debuglog,
      debuglog,
      deprecate,
      format,
      styleText,
      formatWithOptions,
      getCallSites,
      getSystemErrorMap,
      getSystemErrorName,
      getSystemErrorMessage,
      inherits,
      inspect,
      isArray: internalDeprecate(ArrayIsArray,
                                 'The `util.isArray` API is deprecated. Please use `Array.isArray()` instead.',
                                 'DEP0044'),
      isDeepStrictEqual(a, b, skipPrototype) {
        if (internalDeepEqual === undefined) {
          internalDeepEqual = require('internal/util/comparisons').isDeepStrictEqual;
        }
        return internalDeepEqual(a, b, skipPrototype);
      },
      isPartialDeepStrictEqual(a, b) {
        if (internalPartialDeepEqual === undefined) {
          internalPartialDeepEqual = require('internal/util/comparisons').isPartialStrictEqual;
        }
        return internalPartialDeepEqual(a, b);
      },
      markPromiseAsHandled,
      promisify,
      stripVTControlCharacters,
      toUSVString(input) {
        return `${input}`.toWellFormed();
      },
      get transferableAbortSignal() {
        return lazyAbortController().transferableAbortSignal;
      },
      get transferableAbortController() {
        return lazyAbortController().transferableAbortController;
      },
      get aborted() {
        return lazyAbortController().aborted;
      },
      types,
      parseEnv,
    };

    defineLazyProperties(
      module.exports,
      'internal/util/parse_args/parse_args',
      ['parseArgs'],
    );

    defineLazyProperties(
      module.exports,
      'internal/encoding',
      ['TextDecoder', 'TextEncoder'],
    );

    defineLazyProperties(
      module.exports,
      'internal/mime',
      ['MIMEType', 'MIMEParams'],
    );

    defineLazyProperties(
      module.exports,
      'internal/util/diff',
      ['diff'],
    );

    Object.defineProperties(module.exports, {
      debounce: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() { return require('internal/util/debounce'); },
      },
      throttle: {
        __proto__: null,
        configurable: true,
        enumerable: true,
        get() { return require('internal/util/throttle'); },
      },
    });

    defineLazyProperties(
      module.exports,
      'internal/util/trace_sigint',
      ['setTraceSigInt'],
    );
  },
  "internal/util/comparisons": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      Array,
      ArrayBuffer,
      ArrayIsArray,
      ArrayPrototypePush,
      BigInt,
      BigInt64Array,
      BigIntPrototypeValueOf,
      BigUint64Array,
      Boolean,
      BooleanPrototypeValueOf,
      DataView,
      Date,
      DatePrototypeGetTime,
      Error,
      Float16Array,
      Float32Array,
      Float64Array,
      Function,
      Int16Array,
      Int32Array,
      Int8Array,
      Map,
      Number,
      NumberPrototypeValueOf,
      Object,
      ObjectGetOwnPropertyDescriptor,
      ObjectGetOwnPropertySymbols: getOwnSymbols,
      ObjectGetPrototypeOf,
      ObjectIs,
      ObjectKeys,
      ObjectPrototypeHasOwnProperty: hasOwn,
      ObjectPrototypePropertyIsEnumerable: hasEnumerable,
      ObjectPrototypeToString,
      Promise,
      RegExp,
      SafeSet,
      Set,
      String,
      StringPrototypeValueOf,
      Symbol,
      SymbolPrototypeValueOf,
      SymbolToStringTag,
      TypedArrayPrototypeGetByteLength: getByteLength,
      TypedArrayPrototypeGetSymbolToStringTag,
      Uint16Array,
      Uint32Array,
      Uint8Array,
      Uint8ClampedArray,
      WeakMap,
      WeakSet,
    } = primordials;

    const { compare } = internalBinding('buffer');
    const assert = require('internal/assert');
    const { isURL } = require('internal/url');
    const { isError } = require('internal/util');
    const { Buffer } = require('buffer');

    const wellKnownConstructors = new SafeSet()
      .add(Array)
      .add(ArrayBuffer)
      .add(BigInt)
      .add(BigInt64Array)
      .add(BigUint64Array)
      .add(Boolean)
      .add(Buffer)
      .add(DataView)
      .add(Date)
      .add(Error)
      .add(Float16Array)
      .add(Float32Array)
      .add(Float64Array)
      .add(Function)
      .add(Int16Array)
      .add(Int32Array)
      .add(Int8Array)
      .add(Map)
      .add(Number)
      .add(Object)
      .add(Promise)
      .add(RegExp)
      .add(Set)
      .add(String)
      .add(Symbol)
      .add(Uint16Array)
      .add(Uint32Array)
      .add(Uint8Array)
      .add(Uint8ClampedArray)
      .add(WeakMap)
      .add(WeakSet);

    const types = require('internal/util/types');
    const {
      isAnyArrayBuffer,
      isArrayBufferView,
      isBigIntObject,
      isBooleanObject,
      isBoxedPrimitive,
      isCryptoKey,
      isDate,
      isFloat16Array,
      isFloat32Array,
      isFloat64Array,
      isKeyObject,
      isMap,
      isNativeError,
      isNumberObject,
      isPromise,
      isRegExp,
      isSet,
      isStringObject,
      isSymbolObject,
      isWeakMap,
      isWeakSet,
    } = types;
    const {
      constants: {
        ONLY_ENUMERABLE,
        SKIP_SYMBOLS,
      },
      getOwnNonIndexProperties,
    } = internalBinding('util');

    let getCryptoKeyHandle;
    let getCryptoKeyType;
    let getCryptoKeyExtractable;
    let getCryptoKeyAlgorithm;
    let getCryptoKeyUsagesMask;
    let getCryptoKeySecondaryHandle;
    let getCryptoKeySeedData;
    let getKeyObjectHandle;
    let getKeyObjectType;

    const kStrict = 2;
    const kStrictWithoutPrototypes = 3;
    const kLoose = 0;
    const kPartial = 1;

    const kNoIterator = 0;
    const kIsArray = 1;
    const kIsSet = 2;
    const kIsMap = 3;

    function areSimilarRegExps(a, b) {
      return a.source === b.source &&
             a.flags === b.flags &&
             a.lastIndex === b.lastIndex;
    }

    function isPartialUint8Array(a, b) {
      const lenA = getByteLength(a);
      const lenB = getByteLength(b);
      if (lenA < lenB) {
        return false;
      }
      let offsetA = 0;
      for (let offsetB = 0; offsetB < lenB; offsetB++) {
        while (a[offsetA] !== b[offsetB]) {
          offsetA++;
          if (offsetA > lenA - lenB + offsetB) {
            return false;
          }
        }
        offsetA++;
      }
      return true;
    }

    function isPartialArrayBufferView(a, b) {
      if (a.byteLength < b.byteLength) {
        return false;
      }
      return isPartialUint8Array(
        new Uint8Array(a.buffer, a.byteOffset, a.byteLength),
        new Uint8Array(b.buffer, b.byteOffset, b.byteLength),
      );
    }

    function areSimilarFloatArrays(a, b) {
      const len = getByteLength(a);
      if (len !== getByteLength(b)) {
        return false;
      }
      for (let offset = 0; offset < len; offset++) {
        if (a[offset] !== b[offset]) {
          return false;
        }
      }
      return true;
    }

    function areSimilarTypedArrays(a, b) {
      return a.byteLength === b.byteLength && compare(a, b) === 0;
    }

    function areEqualArrayBuffers(buf1, buf2) {
      return buf1.byteLength === buf2.byteLength &&
        compare(new Uint8Array(buf1), new Uint8Array(buf2)) === 0;
    }

    function isEqualBoxedPrimitive(val1, val2) {
      if (isNumberObject(val1)) {
        return isNumberObject(val2) &&
               Object.is(val1.valueOf(),
                        val2.valueOf());
      }
      if (isStringObject(val1)) {
        return isStringObject(val2) &&
               val1.valueOf() === val2.valueOf();
      }
      if (isBooleanObject(val1)) {
        return isBooleanObject(val2) &&
               BooleanPrototypeValueOf(val1) === BooleanPrototypeValueOf(val2);
      }
      if (isBigIntObject(val1)) {
        return isBigIntObject(val2) &&
               BigIntPrototypeValueOf(val1) === BigIntPrototypeValueOf(val2);
      }
      if (isSymbolObject(val1)) {
        return isSymbolObject(val2) &&
              SymbolPrototypeValueOf(val1) === SymbolPrototypeValueOf(val2);
      }
      /* c8 ignore next */
      assert.fail(`Unknown boxed type ${val1}`);
    }

    function isEnumerableOrIdentical(val1, val2, prop, mode, memos) {
      return hasEnumerable(val2, prop) || // This is handled by Object.keys()
          (mode === kPartial && (val2[prop] === undefined || (prop === 'message' && val2[prop] === ''))) ||
          innerDeepEqual(val1[prop], val2[prop], mode, memos);
    }

    function innerDeepEqual(val1, val2, mode, memos) {
      if (val1 === val2) {
        return val1 !== 0 || Object.is(val1, val2) || mode === kLoose;
      }

      if (mode !== kLoose) {
        if (typeof val1 === 'number') {
          return val1 !== val1 && val2 !== val2;
        }
        if (typeof val2 !== 'object' ||
            typeof val1 !== 'object' ||
            val1 === null ||
            val2 === null) {
          return false;
        }
      } else {
        if (val1 === null || typeof val1 !== 'object') {
          return (val2 === null || typeof val2 !== 'object') &&
                 (val1 == val2 || (val1 !== val1 && val2 !== val2));
        }
        if (val2 === null || typeof val2 !== 'object') {
          return false;
        }
      }
      return objectComparisonStart(val1, val2, mode, memos);
    }

    function hasUnequalTag(val1, val2) {
      return val1[SymbolToStringTag] !== val2[SymbolToStringTag];
    }

    function slowHasUnequalTag(val1Tag, val1, val2) {
      if (val1[SymbolToStringTag] !== undefined && val2[SymbolToStringTag] !== undefined) {
        return val1[SymbolToStringTag] !== val2[SymbolToStringTag];
      }
      return val1Tag !== ObjectPrototypeToString(val2);
    }

    function objectComparisonStart(val1, val2, mode, memos) {
      if (mode === kStrict) {
        if (wellKnownConstructors.has(val1.constructor) ||
            (val1.constructor !== undefined && !hasOwn(val1, 'constructor'))) {
          if (val1.constructor !== val2.constructor) {
            return false;
          }
        } else if (Object.getPrototypeOf(val1) !== Object.getPrototypeOf(val2)) {
          return false;
        }
      }

      if (Array.isArray(val1)) {
        if (!Array.isArray(val2) ||
            (val1.length !== val2.length && (mode !== kPartial || val1.length < val2.length)) ||
            hasUnequalTag(val1, val2)) {
          return false;
        }

        const filter = mode !== kLoose ? ONLY_ENUMERABLE : ONLY_ENUMERABLE | SKIP_SYMBOLS;
        const keys2 = getOwnNonIndexProperties(val2, filter);
        if (mode !== kPartial &&
            keys2.length !== getOwnNonIndexProperties(val1, filter).length) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kIsArray, keys2);
      }

      let val1Tag;
      if (val1[SymbolToStringTag] === undefined &&
          (val1Tag = ObjectPrototypeToString(val1)) === '[object Object]') {
        if (slowHasUnequalTag(val1Tag, val1, val2)) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kNoIterator);
      } else if (isSet(val1)) {
        if (!isSet(val2) ||
            (val1.size !== val2.size && (mode !== kPartial || val1.size < val2.size)) ||
            hasUnequalTag(val1, val2)) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kIsSet);
      } else if (isMap(val1)) {
        if (!isMap(val2) ||
            (val1.size !== val2.size && (mode !== kPartial || val1.size < val2.size)) ||
            hasUnequalTag(val1, val2)) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kIsMap);
      } else if (isArrayBufferView(val1)) {
        if (TypedArrayPrototypeGetSymbolToStringTag(val1) !==
            TypedArrayPrototypeGetSymbolToStringTag(val2)) {
          return false;
        }
        if (mode === kPartial && val1.byteLength !== val2.byteLength) {
          if (!isPartialArrayBufferView(val1, val2)) {
            return false;
          }
        } else if (mode === kLoose &&
                   (isFloat32Array(val1) || isFloat64Array(val1) || isFloat16Array(val1))) {
          if (!areSimilarFloatArrays(val1, val2)) {
            return false;
          }
        } else if (!areSimilarTypedArrays(val1, val2)) {
          return false;
        }
        const filter = mode !== kLoose ? ONLY_ENUMERABLE : ONLY_ENUMERABLE | SKIP_SYMBOLS;
        const keys2 = getOwnNonIndexProperties(val2, filter);
        if (mode !== kPartial &&
            keys2.length !== getOwnNonIndexProperties(val1, filter).length) {
          return false;
        }
        return keyCheck(val1, val2, mode, memos, kNoIterator, keys2);
      } else if (isDate(val1)) {
        if (!isDate(val2) || hasUnequalTag(val1, val2)) {
          return false;
        }
        const time1 = DatePrototypeGetTime(val1);
        const time2 = DatePrototypeGetTime(val2);
        if (time1 !== time2 && (time1 === time1 || time2 === time2)) {
          return false;
        }
      } else if (isRegExp(val1)) {
        if (!isRegExp(val2) || !areSimilarRegExps(val1, val2) || hasUnequalTag(val1, val2)) {
          return false;
        }
      } else if (isAnyArrayBuffer(val1)) {
        if (!isAnyArrayBuffer(val2) || hasUnequalTag(val1, val2)) {
          return false;
        }
        if (mode !== kPartial || val1.byteLength === val2.byteLength) {
          if (!areEqualArrayBuffers(val1, val2)) {
            return false;
          }
        } else if (!isPartialUint8Array(new Uint8Array(val1), new Uint8Array(val2))) {
          return false;
        }
      } else if (slowHasUnequalTag(val1Tag ?? ObjectPrototypeToString(val1), val1, val2) ||
                Array.isArray(val2) ||
                isArrayBufferView(val2) ||
                isSet(val2) ||
                isMap(val2) ||
                isDate(val2) ||
                isRegExp(val2) ||
                isAnyArrayBuffer(val2)) {
        return false;
      } else if (isError(val1)) {
        if (!isError(val2) ||
            !isEnumerableOrIdentical(val1, val2, 'message', mode, memos) ||
            !isEnumerableOrIdentical(val1, val2, 'name', mode, memos) ||
            !isEnumerableOrIdentical(val1, val2, 'cause', mode, memos) ||
            !isEnumerableOrIdentical(val1, val2, 'errors', mode, memos)) {
          return false;
        }
        const hasOwnVal2Cause = hasOwn(val2, 'cause');
        if ((hasOwnVal2Cause !== hasOwn(val1, 'cause') && (mode !== kPartial || hasOwnVal2Cause))) {
          return false;
        }
      } else if (isBoxedPrimitive(val1)) {
        if (!isEqualBoxedPrimitive(val1, val2)) {
          return false;
        }
      } else if (isURL(val1)) {
        if (!isURL(val2) || val1.href !== val2.href) {
          return false;
        }
      } else if (isKeyObject(val1)) {
        if (getKeyObjectHandle === undefined) {
          ({
            getKeyObjectHandle,
            getKeyObjectType,
          } = require('internal/crypto/keys'));
        }
        if (!isKeyObject(val2) ||
          getKeyObjectType(val1) !== getKeyObjectType(val2) ||
          !getKeyObjectHandle(val1).equals(getKeyObjectHandle(val2))
        ) {
          return false;
        }
      } else if (isCryptoKey(val1)) {
        if (getCryptoKeyHandle === undefined) {
          ({
            getCryptoKeyHandle,
            getCryptoKeyType,
            getCryptoKeyExtractable,
            getCryptoKeyAlgorithm,
            getCryptoKeyUsagesMask,
            getCryptoKeySecondaryHandle,
            getCryptoKeySeedData,
          } = require('internal/crypto/keys'));
        }
        const val1SecondaryHandle = getCryptoKeySecondaryHandle(val1);
        const val2SecondaryHandle = isCryptoKey(val2) ?
          getCryptoKeySecondaryHandle(val2) : undefined;
        const val1SeedData = getCryptoKeySeedData(val1);
        const val2SeedData = isCryptoKey(val2) ?
          getCryptoKeySeedData(val2) : undefined;
        if (!isCryptoKey(val2) ||
          getCryptoKeyType(val1) !== getCryptoKeyType(val2) ||
          getCryptoKeyExtractable(val1) !== getCryptoKeyExtractable(val2) ||
          !innerDeepEqual(getCryptoKeyAlgorithm(val1), getCryptoKeyAlgorithm(val2), mode, memos) ||
          getCryptoKeyUsagesMask(val1) !== getCryptoKeyUsagesMask(val2) ||
          !getCryptoKeyHandle(val1).equals(getCryptoKeyHandle(val2)) ||
          (val1SecondaryHandle === undefined) !== (val2SecondaryHandle === undefined) ||
          (val1SecondaryHandle !== undefined &&
            !val1SecondaryHandle.equals(val2SecondaryHandle)) ||
          (val1SeedData === undefined) !== (val2SeedData === undefined) ||
          (val1SeedData !== undefined &&
            !innerDeepEqual(val1SeedData, val2SeedData, mode, memos))
        ) {
          return false;
        }
      } else if (isBoxedPrimitive(val2) ||
          isNativeError(val2) ||
          val2 instanceof Error ||
          isWeakMap(val1) ||
          isWeakSet(val1) ||
          isPromise(val1)) {
        return false;
      }

      return keyCheck(val1, val2, mode, memos, kNoIterator);
    }

    function partialSymbolEquiv(val1, val2, keys2) {
      const symbolKeys = getOwnSymbols(val2);
      if (symbolKeys.length !== 0) {
        for (const key of symbolKeys) {
          if (hasEnumerable(val2, key)) {
            keys2.push(key);
          }
        }
      }
      return true;
    }

    function keyCheck(val1, val2, mode, memos, iterationType, keys2) {
      const isArrayLikeObject = keys2 !== undefined;

      if (keys2 === undefined) {
        keys2 = Object.keys(val2);
      }
      let keys1;

      if (!isArrayLikeObject) {
        if (mode === kPartial) {
          if (!partialSymbolEquiv(val1, val2, keys2)) {
            return false;
          }
        } else if (keys2.length !== (keys1 = Object.keys(val1)).length) {
          return false;
        } else if (mode === kStrict || mode === kStrictWithoutPrototypes) {
          for (const key of getOwnSymbols(val1)) {
            if (hasEnumerable(val1, key)) {
              keys1.push(key);
            }
          }
          for (const key of getOwnSymbols(val2)) {
            if (hasEnumerable(val2, key)) {
              keys2.push(key);
            }
          }
          if (keys1.length !== keys2.length) {
            return false;
          }
        }
      }

      if (keys2.length === 0 &&
          (iterationType === kNoIterator ||
            (iterationType === kIsArray && val2.length === 0) ||
            val2.size === 0)) {
        return true;
      }

      if (memos === null) {
        return objEquiv(val1, val2, mode, keys1, keys2, memos, iterationType);
      }
      return handleCycles(val1, val2, mode, keys1, keys2, memos, iterationType);
    }

    function handleCycles(val1, val2, mode, keys1, keys2, memos, iterationType) {
      if (memos === undefined) {
        memos = {
          set: undefined,
          a: val1,
          b: val2,
          c: undefined,
          d: undefined,
          deep: false,
        };
        return objEquiv(val1, val2, mode, keys1, keys2, memos, iterationType);
      }

      if (memos.set === undefined) {
        if (memos.deep === false) {
          if (memos.a === val1) {
            return memos.b === val2;
          }
          if (memos.b === val2) {
            return false;
          }
          memos.c = val1;
          memos.d = val2;
          memos.deep = true;
          const result = objEquiv(val1, val2, mode, keys1, keys2, memos, iterationType);
          memos.deep = false;
          if (memos.set !== undefined) {
            memos.set.delete(memos.c);
            memos.set.delete(memos.d);
          }
          return result;
        }
        memos.set = new SafeSet();
        memos.set.add(memos.a);
        memos.set.add(memos.b);
        memos.set.add(memos.c);
        memos.set.add(memos.d);
      }

      const { set } = memos;

      const originalSize = set.size;
      set.add(val1);
      set.add(val2);
      if (originalSize !== set.size - 2) {
        return originalSize === set.size;
      }

      const areEq = objEquiv(val1, val2, mode, keys1, keys2, memos, iterationType);

      set.delete(val1);
      set.delete(val2);

      return areEq;
    }

    function findLooseMatchingPrimitives(prim) {
      switch (typeof prim) {
        case 'undefined':
          return null;
        case 'object': // Only pass in null as object!
          return undefined;
        case 'symbol':
          return false;
        case 'string':
          prim = +prim;
        case 'number':
          if (prim !== prim) {
            return false;
          }
      }
      return true;
    }

    function setMightHaveLoosePrim(a, b, prim) {
      const altValue = findLooseMatchingPrimitives(prim);
      if (altValue != null)
        return altValue;

      return !b.has(altValue) && a.has(altValue);
    }

    function mapMightHaveLoosePrim(a, b, prim, item2, memo) {
      const altValue = findLooseMatchingPrimitives(prim);
      if (altValue != null) {
        return altValue;
      }
      const item1 = a.get(altValue);
      if ((item1 === undefined && !a.has(altValue)) ||
          !innerDeepEqual(item1, item2, kLoose, memo)) {
        return false;
      }
      return !b.has(altValue) && innerDeepEqual(item1, item2, kLoose, memo);
    }

    function partialObjectSetEquiv(array, a, b, mode, memo) {
      let aPos = 0;
      let direction = 1;
      let start = 0;
      let end = array.length - 1;
      for (const val1 of a) {
        aPos++;
        if (!b.has(val1)) {
          let innerStart = start;
          if (direction === 1) {
            if (innerDeepEqual(val1, array[start], mode, memo)) {
              if (start === end) {
                return true;
              }
              start += 1;
              continue;
            }
            if (start === end) {
              continue;
            }
            direction = -1;
            innerStart += 1;
          }
          let matched = true;
          if (!innerDeepEqual(val1, array[end], mode, memo)) {
            direction = 1;
            matched = arrayHasEqualElement(array, val1, mode, memo, innerDeepEqual, innerStart, end);
          }
          if (matched) {
            if (start === end) {
              return true;
            }
            end -= 1;
          }
        }
        if (a.size - aPos <= end - start) {
          return false;
        }
      }
      return false;
    }

    function arrayHasEqualElement(array, val1, mode, memo, comparator, start, end) {
      for (let i = end - 1; i >= start; i--) {
        if (comparator(val1, array[i], mode, memo)) {
          array[i] = array[end];
          return true;
        }
      }
      return false;
    }

    function setObjectEquiv(array, a, b, mode, memo) {
      let direction = 1;
      let start = 0;
      let end = array.length - 1;
      const comparator = mode !== kLoose ? objectComparisonStart : innerDeepEqual;
      const extraChecks = mode === kLoose || array.length !== a.size;
      for (const val1 of a) {
        if (typeof val1 !== 'object' || val1 === null) {
          if (b.has(val1)) {
            continue;
          }
          if (mode !== kLoose) {
            return false;
          }
        } else if (extraChecks && b.has(val1)) {
          continue;
        }

        let innerStart = start;
        if (direction === 1) {
          if (comparator(val1, array[start], mode, memo)) {
            start += 1;
            continue;
          }
          if (start === end) {
            return false;
          }
          direction = -1;
          innerStart += 1;
        }
        if (!comparator(val1, array[end], mode, memo)) {
          direction = 1;
          if (!arrayHasEqualElement(array, val1, mode, memo, comparator, innerStart, end)) {
            return false;
          }
        }
        end -= 1;
      }
      return true;
    }

    function compareSmallSets(a, b, val, iteratorB, mode, memo) {
      const iteratorA = a.values();
      const firstA = iteratorA.next().value;
      const first = innerDeepEqual(firstA, val, mode, memo);
      if (first) {
        if (b.size === 1) { // Partial mode && a.size === 1 || b.size === 1
          return true;
        }
        const secondA = iteratorA.next().value;
        return b.has(secondA) || innerDeepEqual(secondA, iteratorB.next().value, mode, memo);
      }
      return a.size !== 1 && innerDeepEqual(iteratorA.next().value, val, mode, memo) && (
        b.size === 1 || // Partial mode
        b.has(firstA) || // Primitive or reference equal
        innerDeepEqual(firstA, iteratorB.next().value, mode, memo)
      );
    }

    function setEquiv(a, b, mode, memo) {
      let array;

      const iteratorB = b.values();
      for (const val of iteratorB) {
        if (!a.has(val)) {
          if ((typeof val !== 'object' || val === null) &&
              (mode !== kLoose || !setMightHaveLoosePrim(a, b, val))) {
            return false;
          }

          if (array === undefined) {
            if (a.size < 3) {
              return compareSmallSets(a, b, val, iteratorB, mode, memo);
            }
            array = [];
          }
          array.push(val);
        }
      }

      if (array === undefined) {
        return true;
      }
      if (mode === kPartial) {
        return partialObjectSetEquiv(array, a, b, mode, memo);
      }
      return setObjectEquiv(array, a, b, mode, memo);
    }

    function partialObjectMapEquiv(array, a, b, mode, memo) {
      let aPos = 0;
      let direction = 1;
      let start = 0;
      let end = array.length - 1;
      for (const { 0: key1, 1: item1 } of a) {
        aPos++;
        if (typeof key1 === 'object' && key1 !== null) {
          let innerStart = start;
          if (direction === 1) {
            const key2 = array[start];
            if (objectComparisonStart(key1, key2, mode, memo) && innerDeepEqual(item1, b.get(key2), mode, memo)) {
              if (start === end) {
                return true;
              }
              start += 1;
              continue;
            }
            if (start === end) {
              continue;
            }
            direction = -1;
            innerStart += 1;
          }
          let matched = true;
          const key2 = array[end];
          if (!objectComparisonStart(key1, key2, mode, memo) || !innerDeepEqual(item1, b.get(key2), mode, memo)) {
            direction = 1;
            matched = arrayHasEqualMapElement(array, key1, item1, b, mode, memo, objectComparisonStart, innerStart, end);
          }
          if (matched) {
            if (start === end) {
              return true;
            }
            end -= 1;
          }
        }
        if (a.size - aPos <= end - start) {
          return false;
        }
      }
      return false;
    }

    function arrayHasEqualMapElement(array, key1, item1, b, mode, memo, comparator, start, end) {
      for (let i = end - 1; i >= start; i--) {
        const key2 = array[i];
        if (comparator(key1, key2, mode, memo) &&
            innerDeepEqual(item1, b.get(key2), mode, memo)) {
          array[i] = array[end];
          return true;
        }
      }
      return false;
    }

    function mapObjectEquiv(array, a, b, mode, memo) {
      let direction = 1;
      let start = 0;
      let end = array.length - 1;
      const comparator = mode !== kLoose ? objectComparisonStart : innerDeepEqual;

      for (const { 0: key1, 1: item1 } of a) {
        if (typeof key1 !== 'object' || key1 === null) {
          if (b.has(key1)) {
            if (mode !== kLoose || innerDeepEqual(item1, b.get(key1), mode, memo)) {
              continue;
            }
          } else if (mode !== kLoose) {
            return false;
          }
        }

        let innerStart = start;
        if (direction === 1) {
          const key2 = array[start];
          if (comparator(key1, key2, mode, memo) && innerDeepEqual(item1, b.get(key2), mode, memo)) {
            start += 1;
            continue;
          }
          if (start === end) {
            return false;
          }
          direction = -1;
          innerStart += 1;
        }
        const key2 = array[end];
        if ((!comparator(key1, key2, mode, memo) || !innerDeepEqual(item1, b.get(key2), mode, memo))) {
          direction = 1;
          if (!arrayHasEqualMapElement(array, key1, item1, b, mode, memo, comparator, innerStart, end)) {
            return false;
          }
        }
        end -= 1;
      }
      return true;
    }

    function mapEquiv(a, b, mode, memo) {
      let array;

      for (const { 0: key2, 1: item2 } of b) {
        if (typeof key2 === 'object' && key2 !== null) {
          if (array === undefined) {
            if (a.size === 1) {
              const { 0: key1, 1: item1 } = a.entries().next().value;
              return innerDeepEqual(key1, key2, mode, memo) &&
                      innerDeepEqual(item1, item2, mode, memo);
            }
            array = [];
          }
          array.push(key2);
        } else {
          const item1 = a.get(key2);
          if (((item1 === undefined && !a.has(key2)) ||
              !innerDeepEqual(item1, item2, mode, memo))) {
            if (mode !== kLoose)
              return false;
            if (!mapMightHaveLoosePrim(a, b, key2, item2, memo))
              return false;
            if (array === undefined) {
              array = [];
            }
            array.push(key2);
          }
        }
      }

      if (array === undefined) {
        return true;
      }

      if (mode === kPartial) {
        return partialObjectMapEquiv(array, a, b, mode, memo);
      }

      return mapObjectEquiv(array, a, b, mode, memo);
    }

    function partialSparseArrayEquiv(a, b, mode, memos, startA, startB) {
      let aPos = startA;
      const keysA = Object.keys(a);
      const keysB = Object.keys(b);
      const lenA = keysA.length - startA;
      const lenB = keysB.length - startB;
      if (lenA < lenB) {
        return false;
      }
      for (let i = 0; i < lenB; i++) {
        const keyB = keysB[startB + i];
        while (!innerDeepEqual(a[keysA[aPos]], b[keyB], mode, memos)) {
          aPos++;
          if (aPos > keysA.length - lenB + i) {
            return false;
          }
        }
        aPos++;
      }
      return true;
    }

    function partialArrayEquiv(a, b, mode, memos) {
      let aPos = 0;
      for (let i = 0; i < b.length; i++) {
        let isSparse = b[i] === undefined && !hasOwn(b, i);
        if (isSparse) {
          return partialSparseArrayEquiv(a, b, mode, memos, aPos, i);
        }
        while (!(isSparse = a[aPos] === undefined && !hasOwn(a, aPos)) &&
               !innerDeepEqual(a[aPos], b[i], mode, memos)) {
          aPos++;
          if (aPos > a.length - b.length + i) {
            return false;
          }
        }
        if (isSparse) {
          return partialSparseArrayEquiv(a, b, mode, memos, aPos, i);
        }
        aPos++;
      }
      return true;
    }

    function sparseArrayEquiv(a, b, mode, memos, i) {
      const keysA = Object.keys(a);
      const keysB = Object.keys(b);
      if (keysA.length !== keysB.length) {
        return false;
      }
      for (; i < keysB.length; i++) {
        const key = keysB[i];
        if ((a[key] === undefined && !hasOwn(a, key)) || !innerDeepEqual(a[key], b[key], mode, memos)) {
          return false;
        }
      }
      return true;
    }

    function objEquiv(a, b, mode, keys1, keys2, memos, iterationType) {
      if (keys2.length > 0) {
        let i = 0;
        if (keys1 !== undefined) {
          for (; i < keys2.length; i++) {
            const key = keys2[i];
            if (keys1[i] !== key) {
              break;
            }
            if (!innerDeepEqual(a[key], b[key], mode, memos)) {
              return false;
            }
          }
        }
        for (; i < keys2.length; i++) {
          const key = keys2[i];
          const descriptor = Object.getOwnPropertyDescriptor(a, key);
          if (descriptor === undefined || descriptor.enumerable !== true) {
            return false;
          }
          const value = descriptor.writable !== undefined ? descriptor.value : a[key];
          if (!innerDeepEqual(value, b[key], mode, memos)) {
            return false;
          }
        }
      }

      if (iterationType === kIsArray) {
        if (mode === kPartial) {
          return partialArrayEquiv(a, b, mode, memos);
        }
        for (let i = 0; i < a.length; i++) {
          if (b[i] === undefined) {
            if (!hasOwn(b, i))
              return sparseArrayEquiv(a, b, mode, memos, i);
            if ((a[i] !== undefined || !hasOwn(a, i)) && (mode !== kLoose || a[i] !== null))
              return false;
          } else if ((a[i] === undefined || !innerDeepEqual(a[i], b[i], mode, memos)) &&
                     (mode !== kLoose || b[i] !== null)) {
            return false;
          }
        }
      } else if (iterationType === kIsSet) {
        if (!setEquiv(a, b, mode, memos)) {
          return false;
        }
      } else if (iterationType === kIsMap) {
        if (!mapEquiv(a, b, mode, memos)) {
          return false;
        }
      }

      return true;
    }

    let detectCycles = function(val1, val2, mode) {
      try {
        return innerDeepEqual(val1, val2, mode, null);
      } catch {
        detectCycles = innerDeepEqual;
        return innerDeepEqual(val1, val2, mode, undefined);
      }
    };

    module.exports = {
      isDeepEqual(val1, val2) {
        return detectCycles(val1, val2, kLoose);
      },
      isDeepStrictEqual(val1, val2, skipPrototype) {
        return detectCycles(val1, val2, skipPrototype ? kStrictWithoutPrototypes : kStrict);
      },
      isPartialStrictEqual(val1, val2) {
        return detectCycles(val1, val2, kPartial);
      },
    };
  },
  "internal/util/parse_args/parse_args": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypeForEach,
      ArrayPrototypeMap,
      ArrayPrototypePush,
      ArrayPrototypePushApply,
      ArrayPrototypeShift,
      ArrayPrototypeSlice,
      ArrayPrototypeUnshiftApply,
      ObjectEntries,
      ObjectPrototypeHasOwnProperty: ObjectHasOwn,
      StringPrototypeCharAt,
      StringPrototypeIndexOf,
      StringPrototypeSlice,
      StringPrototypeStartsWith,
    } = primordials;

    const {
      validateArray,
      validateBoolean,
      validateBooleanArray,
      validateObject,
      validateString,
      validateStringArray,
      validateUnion,
    } = require('internal/validators');

    const {
      findLongOptionForShort,
      isLoneLongOption,
      isLoneShortOption,
      isLongOptionAndValue,
      isOptionValue,
      isOptionLikeValue,
      isShortOptionAndValue,
      isShortOptionGroup,
      useDefaultValueOption,
      objectGetOwn,
      optionsGetOwn,
    } = require('internal/util/parse_args/utils');

    const {
      codes: {
        ERR_INVALID_ARG_VALUE,
        ERR_PARSE_ARGS_INVALID_OPTION_VALUE,
        ERR_PARSE_ARGS_UNEXPECTED_POSITIONAL,
        ERR_PARSE_ARGS_UNKNOWN_OPTION,
      },
    } = require('internal/errors');

    const {
      kEmptyObject,
    } = require('internal/util');

    const { getOptionValue } = require('internal/options');

    function getMainArgs() {
      const evalValue = getOptionValue('--eval');

      if (evalValue.length !== 0) {
        return process.argv.slice(1);
      }

      return process.argv.slice(2);
    }

    function checkOptionLikeValue(token) {
      if (!token.inlineValue && isOptionLikeValue(token.value)) {
        const example = token.rawName.startsWith('--') ?
          `'${token.rawName}=-XYZ'` :
          `'--${token.name}=-XYZ' or '${token.rawName}-XYZ'`;
        const errorMessage = `Option '${token.rawName}' argument is ambiguous.
    Did you forget to specify the option argument for '${token.rawName}'?
    To specify an option argument starting with a dash use ${example}.`;
        throw new ERR_PARSE_ARGS_INVALID_OPTION_VALUE(errorMessage);
      }
    }

    function checkOptionUsage(config, token) {
      let tokenName = token.name;
      if (!ObjectHasOwn(config.options, tokenName)) {
        if (config.allowNegative && tokenName.startsWith('no-')) {
          tokenName = tokenName.slice(3);
          if (!ObjectHasOwn(config.options, tokenName) || optionsGetOwn(config.options, tokenName, 'type') !== 'boolean') {
            throw new ERR_PARSE_ARGS_UNKNOWN_OPTION(
              token.rawName, config.allowPositionals);
          }
        } else {
          throw new ERR_PARSE_ARGS_UNKNOWN_OPTION(
            token.rawName, config.allowPositionals);
        }
      }

      const short = optionsGetOwn(config.options, tokenName, 'short');
      const shortAndLong = `${short ? `-${short}, ` : ''}--${tokenName}`;
      const type = optionsGetOwn(config.options, tokenName, 'type');
      if (type === 'string' && typeof token.value !== 'string') {
        throw new ERR_PARSE_ARGS_INVALID_OPTION_VALUE(`Option '${shortAndLong} <value>' argument missing`);
      }
      if (type === 'boolean' && token.value != null) {
        throw new ERR_PARSE_ARGS_INVALID_OPTION_VALUE(`Option '${shortAndLong}' does not take an argument`);
      }
    }

    function storeOption(token, options, values, allowNegative) {
      let longOption = token.name;
      let optionValue = token.value;
      if (longOption === '__proto__') {
        return; // No. Just no.
      }

      if (allowNegative && longOption.startsWith('no-') && optionValue === undefined) {
        longOption = longOption.slice(3);
        token.name = longOption;
        optionValue = false;
      }

      const newValue = optionValue ?? true;
      if (optionsGetOwn(options, longOption, 'multiple')) {
        if (values[longOption]) {
          values[longOption].push(newValue);
        } else {
          values[longOption] = [newValue];
        }
      } else {
        values[longOption] = newValue;
      }
    }

    function storeDefaultOption(longOption, optionValue, values) {
      if (longOption === '__proto__') {
        return; // No. Just no.
      }

      values[longOption] = optionValue;
    }

    function argsToTokens(args, options) {
      const tokens = [];
      let index = -1;
      let groupCount = 0;

      const remainingArgs = args.slice();
      while (remainingArgs.length > 0) {
        const arg = remainingArgs.shift();
        const nextArg = remainingArgs[0];
        if (groupCount > 0)
          groupCount--;
        else
          index++;

        if (arg === '--') {
          tokens.push({ kind: 'option-terminator', index });
          ArrayPrototypePushApply(
            tokens, remainingArgs.map((arg) => {
              return { kind: 'positional', index: ++index, value: arg };
            }),
          );
          break; // Finished processing args, leave while loop.
        }

        if (isLoneShortOption(arg)) {
          const shortOption = arg.charAt(1);
          const longOption = findLongOptionForShort(shortOption, options);
          let value;
          let inlineValue;
          if (optionsGetOwn(options, longOption, 'type') === 'string' &&
              isOptionValue(nextArg)) {
            value = remainingArgs.shift();
            inlineValue = false;
          }
          tokens.push({ kind: 'option', name: longOption, rawName: arg,
              index, value, inlineValue });
          if (value != null) ++index;
          continue;
        }

        if (isShortOptionGroup(arg, options)) {
          const expanded = [];
          for (let index = 1; index < arg.length; index++) {
            const shortOption = arg.charAt(index);
            const longOption = findLongOptionForShort(shortOption, options);
            if (optionsGetOwn(options, longOption, 'type') !== 'string' ||
              index === arg.length - 1) {
              expanded.push(`-${shortOption}`);
            } else {
              expanded.push(`-${arg.slice(index)}`);
              break; // finished short group
            }
          }
          ArrayPrototypeUnshiftApply(remainingArgs, expanded);
          groupCount = expanded.length;
          continue;
        }

        if (isShortOptionAndValue(arg, options)) {
          const shortOption = arg.charAt(1);
          const longOption = findLongOptionForShort(shortOption, options);
          const value = arg.slice(2);
          tokens.push({ kind: 'option', name: longOption, rawName: `-${shortOption}`,
              index, value, inlineValue: true });
          continue;
        }

        if (isLoneLongOption(arg)) {
          const longOption = arg.slice(2);
          let value;
          let inlineValue;
          if (optionsGetOwn(options, longOption, 'type') === 'string' &&
              isOptionValue(nextArg)) {
            value = remainingArgs.shift();
            inlineValue = false;
          }
          tokens.push({ kind: 'option', name: longOption, rawName: arg,
              index, value, inlineValue });
          if (value != null) ++index;
          continue;
        }

        if (isLongOptionAndValue(arg)) {
          const equalIndex = arg.indexOf('=');
          const longOption = arg.slice(2, equalIndex);
          const value = arg.slice(equalIndex + 1);
          tokens.push({ kind: 'option', name: longOption, rawName: `--${longOption}`,
              index, value, inlineValue: true });
          continue;
        }

        tokens.push({ kind: 'positional', index, value: arg });
      }
      return tokens;
    }

    const parseArgs = (config = kEmptyObject) => {
      const args = objectGetOwn(config, 'args') ?? getMainArgs();
      const strict = objectGetOwn(config, 'strict') ?? true;
      const allowPositionals = objectGetOwn(config, 'allowPositionals') ?? !strict;
      const returnTokens = objectGetOwn(config, 'tokens') ?? false;
      const allowNegative = objectGetOwn(config, 'allowNegative') ?? false;
      const options = objectGetOwn(config, 'options') ?? { __proto__: null };
      const parseConfig = { args, strict, options, allowPositionals, allowNegative };

      validateArray(args, 'args');
      validateBoolean(strict, 'strict');
      validateBoolean(allowPositionals, 'allowPositionals');
      validateBoolean(returnTokens, 'tokens');
      validateBoolean(allowNegative, 'allowNegative');
      validateObject(options, 'options');
      Object.entries(options).forEach(({ 0: longOption, 1: optionConfig }) => {
          validateObject(optionConfig, `options.${longOption}`);

          const optionType = objectGetOwn(optionConfig, 'type');
          validateUnion(optionType, `options.${longOption}.type`, ['string', 'boolean']);

          if (ObjectHasOwn(optionConfig, 'short')) {
            const shortOption = optionConfig.short;
            validateString(shortOption, `options.${longOption}.short`);
            if (shortOption.length !== 1) {
              throw new ERR_INVALID_ARG_VALUE(
                `options.${longOption}.short`,
                shortOption,
                'must be a single character',
              );
            }
          }

          const multipleOption = objectGetOwn(optionConfig, 'multiple');
          if (ObjectHasOwn(optionConfig, 'multiple')) {
            validateBoolean(multipleOption, `options.${longOption}.multiple`);
          }

          const defaultValue = objectGetOwn(optionConfig, 'default');
          if (defaultValue !== undefined) {
            let validator;
            switch (optionType) {
              case 'string':
                validator = multipleOption ? validateStringArray : validateString;
                break;

              case 'boolean':
                validator = multipleOption ? validateBooleanArray : validateBoolean;
                break;
            }
            validator(defaultValue, `options.${longOption}.default`);
          }
        });

      const tokens = argsToTokens(args, options);

      const result = {
        values: { __proto__: null },
        positionals: [],
      };
      if (returnTokens) {
        result.tokens = tokens;
      }
      tokens.forEach((token) => {
        if (token.kind === 'option') {
          if (strict) {
            checkOptionUsage(parseConfig, token);
            checkOptionLikeValue(token);
          }
          storeOption(token, options, result.values, parseConfig.allowNegative);
        } else if (token.kind === 'positional') {
          if (!allowPositionals) {
            throw new ERR_PARSE_ARGS_UNEXPECTED_POSITIONAL(token.value);
          }
          result.positionals.push(token.value);
        }
      });

      Object.entries(options).forEach(({ 0: longOption,
                                                       1: optionConfig }) => {
        const mustSetDefault = useDefaultValueOption(longOption,
                                                     optionConfig,
                                                     result.values);
        if (mustSetDefault) {
          storeDefaultOption(longOption,
                             objectGetOwn(optionConfig, 'default'),
                             result.values);
        }
      });

      return result;
    };

    module.exports = {
      parseArgs,
    };
  },
  "internal/util/parse_args/utils": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypeFind,
      ObjectEntries,
      ObjectPrototypeHasOwnProperty: ObjectHasOwn,
      StringPrototypeCharAt,
      StringPrototypeIncludes,
      StringPrototypeStartsWith,
    } = primordials;

    const {
      validateObject,
    } = require('internal/validators');

    function objectGetOwn(obj, prop) {
      if (ObjectHasOwn(obj, prop))
        return obj[prop];
    }

    function optionsGetOwn(options, longOption, prop) {
      if (ObjectHasOwn(options, longOption))
        return objectGetOwn(options[longOption], prop);
    }

    function isOptionValue(value) {
      if (value == null) return false;

      return true; // greedy!
    }

    function isOptionLikeValue(value) {
      if (value == null) return false;

      return value.length > 1 && value.charAt(0) === '-';
    }

    function isLoneShortOption(arg) {
      return arg.length === 2 &&
        arg.charAt(0) === '-' &&
        arg.charAt(1) !== '-';
    }

    function isLoneLongOption(arg) {
      return arg.length > 2 &&
        arg.startsWith('--') &&
        !arg.includes('=', 3);
    }

    function isLongOptionAndValue(arg) {
      return arg.length > 2 &&
        arg.startsWith('--') &&
        arg.includes('=', 3);
    }

    function isShortOptionGroup(arg, options) {
      if (arg.length <= 2) return false;
      if (arg.charAt(0) !== '-') return false;
      if (arg.charAt(1) === '-') return false;

      const firstShort = arg.charAt(1);
      const longOption = findLongOptionForShort(firstShort, options);
      return optionsGetOwn(options, longOption, 'type') !== 'string';
    }

    function isShortOptionAndValue(arg, options) {
      validateObject(options, 'options');

      if (arg.length <= 2) return false;
      if (arg.charAt(0) !== '-') return false;
      if (arg.charAt(1) === '-') return false;

      const shortOption = arg.charAt(1);
      const longOption = findLongOptionForShort(shortOption, options);
      return optionsGetOwn(options, longOption, 'type') === 'string';
    }

    function findLongOptionForShort(shortOption, options) {
      validateObject(options, 'options');
      const longOptionEntry = Object.entries(options).find(({ 1: optionConfig }) => objectGetOwn(optionConfig, 'short') === shortOption);
      return longOptionEntry?.[0] ?? shortOption;
    }

    function useDefaultValueOption(longOption, optionConfig, values) {
      return objectGetOwn(optionConfig, 'default') !== undefined &&
        values[longOption] === undefined;
    }

    module.exports = {
      findLongOptionForShort,
      isLoneLongOption,
      isLoneShortOption,
      isLongOptionAndValue,
      isOptionValue,
      isOptionLikeValue,
      isShortOptionAndValue,
      isShortOptionGroup,
      useDefaultValueOption,
      objectGetOwn,
      optionsGetOwn,
    };
  },
  "internal/mime": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      FunctionPrototypeCall,
      ObjectDefineProperty,
      SafeMap,
      StringPrototypeCharCodeAt,
      StringPrototypeSlice,
      StringPrototypeToLowerCase,
      Symbol,
      SymbolIterator,
      Uint8Array,
    } = primordials;
    const {
      ERR_ILLEGAL_CONSTRUCTOR,
      ERR_INVALID_MIME_SYNTAX,
    } = require('internal/errors').codes;

    const kHTTPToken = 1;
    const kHTTPQuotedStringToken = 2;
    const codePointClass = new Uint8Array(256);
    {
      codePointClass[0x09] = kHTTPQuotedStringToken;
      for (let c = 0x20; c <= 0x7E; c++) codePointClass[c] = kHTTPQuotedStringToken;
      for (let c = 0x80; c <= 0xFF; c++) codePointClass[c] = kHTTPQuotedStringToken;
      const tokens = "!#$%&'*+-.^_`|~0123456789" +
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
      for (let i = 0; i < tokens.length; i++) {
        codePointClass[tokens.charCodeAt(i)] |= kHTTPToken;
      }
    }

    const kNoThrow = Symbol('kNoThrow');

    function isHTTPWhitespace(c) {
      return c === 0x20 || c === 0x09 || c === 0x0A || c === 0x0D;
    }

    function findInvalid(str, start, end, cls) {
      for (let i = start; i < end; i++) {
        const c = str.charCodeAt(i);
        if (c > 0xFF || (codePointClass[c] & cls) === 0) return i - start;
      }
      return -1;
    }

    function toASCIILower(str) {
      let hasUpper = false;
      for (let i = 0; i < str.length; i++) {
        const c = str.charCodeAt(i);
        if (c > 0x7F) {
          let result = '';
          for (let j = 0; j < str.length; j++) {
            const char = str[j];
            result += char >= 'A' && char <= 'Z' ?
              char.toLowerCase() :
              char;
          }
          return result;
        }
        if (c <= 0x5A && c >= 0x41) hasUpper = true;
      }
      return hasUpper ? str.toLowerCase() : str;
    }

    let parsedType = '';
    let parsedSubtype = '';
    let parsedParamsStart = 0;
    let parsedParamsEnd = 0;
    let failedProduction = '';
    let failedIndex = -1;

    function parseTypeAndSubtype(str) {
      const length = str.length;

      let position = 0;
      while (position < length &&
             isHTTPWhitespace(str.charCodeAt(position))) {
        position++;
      }

      const typeStart = position;
      let invalidTypeIndex = -1;
      let typeHasUpper = false;
      for (; position < length; position++) {
        const c = str.charCodeAt(position);
        if (c === 0x2F /* / */) break;
        if (c > 0xFF || (codePointClass[c] & kHTTPToken) === 0) {
          if (invalidTypeIndex === -1) invalidTypeIndex = position - typeStart;
        } else if (c <= 0x5A && c >= 0x41) {
          typeHasUpper = true;
        }
      }
      if (position === typeStart || invalidTypeIndex !== -1 || position >= length) {
        failedProduction = 'type';
        failedIndex = invalidTypeIndex;
        return false;
      }
      const typeEnd = position;

      position++;
      const subtypeStart = position;
      let subtypeEnd = position;
      let firstNonToken = -1;
      let subtypeHasUpper = false;
      for (; position < length; position++) {
        const c = str.charCodeAt(position);
        if (c === 0x3B /* ; */) break;
        if (c <= 0xFF && (codePointClass[c] & kHTTPToken) !== 0) {
          if (c <= 0x5A && c >= 0x41) subtypeHasUpper = true;
        } else if (firstNonToken === -1) {
          firstNonToken = position;
        }
        if (!isHTTPWhitespace(c)) subtypeEnd = position + 1;
      }
      const subtypeRawEnd = position;
      const invalidSubtypeIndex = firstNonToken !== -1 && firstNonToken < subtypeEnd ?
        firstNonToken - subtypeStart : -1;
      if (subtypeEnd === subtypeStart || invalidSubtypeIndex !== -1) {
        failedProduction = 'subtype';
        failedIndex = invalidSubtypeIndex;
        return false;
      }

      let paramsEnd = length;
      while (paramsEnd > subtypeRawEnd &&
             isHTTPWhitespace(str.charCodeAt(paramsEnd - 1))) {
        paramsEnd--;
      }

      const type = str.slice(typeStart, typeEnd);
      const subtype = str.slice(subtypeStart, subtypeEnd);
      parsedType = typeHasUpper ? type.toLowerCase() : type;
      parsedSubtype = subtypeHasUpper ? subtype.toLowerCase() : subtype;
      parsedParamsStart = subtypeRawEnd + 1;
      parsedParamsEnd = paramsEnd;
      return true;
    }

    function parseParameters(str, position, end, params) {
      while (position < end) {
        while (position < end &&
               isHTTPWhitespace(str.charCodeAt(position))) {
          position++;
        }

        const nameStart = position;
        let nameIsToken = true;
        let nameHasUpper = false;
        for (; position < end; position++) {
          const c = str.charCodeAt(position);
          if (c === 0x3B /* ; */ || c === 0x3D /* = */) break;
          if (c > 0xFF || (codePointClass[c] & kHTTPToken) === 0) {
            nameIsToken = false;
          } else if (c <= 0x5A && c >= 0x41) {
            nameHasUpper = true;
          }
        }
        const nameEnd = position;

        if (position < end) {
          if (str.charCodeAt(position) === 0x3B /* ; */) {
            position++;
            continue;
          }
          position++;
        }
        if (position >= end) break;

        let value;
        if (str.charCodeAt(position) === 0x22 /* " */) {
          position++;
          value = '';
          let chunkStart = position;
          while (true) {
            while (position < end) {
              const c = str.charCodeAt(position);
              if (c === 0x22 /* " */ || c === 0x5C /* \ */) break;
              position++;
            }
            if (position >= end) {
              value += str.slice(chunkStart, position);
              break;
            }
            const quoteOrBackslash = str.charCodeAt(position);
            value += str.slice(chunkStart, position);
            position++;
            if (quoteOrBackslash === 0x5C /* \ */) {
              if (position >= end) {
                value += '\\';
                break;
              }
              chunkStart = position;
              position++;
            } else {
              break;
            }
          }
          while (position < end &&
                 str.charCodeAt(position) !== 0x3B /* ; */) {
            position++;
          }
          if (findInvalid(value, 0, value.length, kHTTPQuotedStringToken) !== -1) {
            position++;
            continue;
          }
        } else {
          const valueStart = position;
          let valueEnd = position;
          let firstInvalid = -1;
          for (; position < end; position++) {
            const c = str.charCodeAt(position);
            if (c === 0x3B /* ; */) break;
            if (firstInvalid === -1 &&
                (c > 0xFF || (codePointClass[c] & kHTTPQuotedStringToken) === 0)) {
              firstInvalid = position;
            }
            if (!isHTTPWhitespace(c)) valueEnd = position + 1;
          }
          if (valueEnd === valueStart ||
              (firstInvalid !== -1 && firstInvalid < valueEnd)) {
            position++;
            continue;
          }
          value = str.slice(valueStart, valueEnd);
        }

        if (nameEnd !== nameStart && nameIsToken) {
          const name = nameHasUpper ?
            str.slice(nameStart, nameEnd).toLowerCase() :
            str.slice(nameStart, nameEnd);
          if (!params.has(name)) params.set(name, value);
        }
        position++;
      }
    }

    function escapeQuoteOrSolidus(str) {
      let result = '';
      for (let i = 0; i < str.length; i++) {
        const char = str[i];
        result += (char === '"' || char === '\\') ? `\\${char}` : char;
      }
      return result;
    }

    const encode = (value) => {
      if (value.length === 0) return '""';
      if (findInvalid(value, 0, value.length, kHTTPToken) === -1) return value;
      return `"${escapeQuoteOrSolidus(value)}"`;
    };

    class MIMEParams {
      #data = null;
      #string = null;
      #start = 0;
      #end = 0;

        static instantiateMimeParams(str, start, end) {
        const instance = new MIMEParams();
        instance.#string = str;
        instance.#start = start;
        instance.#end = end;
        return instance;
      }

        delete(name) {
        this.#parse().delete(toASCIILower(`${name}`));
      }

      get(name) {
        const value = this.#parse().get(toASCIILower(`${name}`));
        return value === undefined ? null : value;
      }

      has(name) {
        return this.#parse().has(toASCIILower(`${name}`));
      }

      set(name, value) {
        const data = this.#parse();
        name = toASCIILower(`${name}`);
        value = `${value}`;
        const invalidNameIndex = findInvalid(name, 0, name.length, kHTTPToken);
        if (name.length === 0 || invalidNameIndex !== -1) {
          throw new ERR_INVALID_MIME_SYNTAX(
            'parameter name',
            name,
            invalidNameIndex,
          );
        }
        const invalidValueIndex = findInvalid(value, 0, value.length,
                                              kHTTPQuotedStringToken);
        if (invalidValueIndex !== -1) {
          throw new ERR_INVALID_MIME_SYNTAX(
            'parameter value',
            value,
            invalidValueIndex,
          );
        }
        data.set(name, value);
      }

      *entries() {
        yield* this.#parse().entries();
      }

      *keys() {
        yield* this.#parse().keys();
      }

      *values() {
        yield* this.#parse().values();
      }

      toString() {
        let ret = '';
        for (const { 0: key, 1: value } of this.#parse()) {
          const encoded = encode(value);
          if (ret.length) ret += ';';
          ret += `${key}=${encoded}`;
        }
        return ret;
      }

        #parse() {
        let data = this.#data;
        if (data !== null) return data;
        data = this.#data = new SafeMap();
        const str = this.#string;
        if (str !== null) {
          this.#string = null;
          parseParameters(str, this.#start, this.#end, data);
        }
        return data;
      }
    }
    const MIMEParamsStringify = MIMEParams.prototype.toString;
    Object.defineProperty(MIMEParams.prototype, SymbolIterator, {
      __proto__: null,
      configurable: true,
      value: MIMEParams.prototype.entries,
      writable: true,
    });
    Object.defineProperty(MIMEParams.prototype, 'toJSON', {
      __proto__: null,
      configurable: true,
      value: MIMEParamsStringify,
      writable: true,
    });

    const { instantiateMimeParams } = MIMEParams;
    delete MIMEParams.instantiateMimeParams;

    class MIMEType {
      #type;
      #subtype;
      #parameters;
      constructor(string, noThrowSymbol = null) {
        if (noThrowSymbol === kNoThrow) {
          this.#init(string);
          return;
        }
        string = `${string}`;
        if (noThrowSymbol != null) {
          throw new ERR_ILLEGAL_CONSTRUCTOR();
        }
        if (!parseTypeAndSubtype(string)) {
          throw new ERR_INVALID_MIME_SYNTAX(failedProduction, string, failedIndex);
        }
        this.#init(string);
      }

      #init(string) {
        this.#type = parsedType;
        this.#subtype = parsedSubtype;
        this.#parameters = instantiateMimeParams(string, parsedParamsStart,
                                                 parsedParamsEnd);
      }

      static parse(string) {
        string = `${string}`;
        if (!parseTypeAndSubtype(string)) return null;
        return new MIMEType(string, kNoThrow);
      }

      get type() {
        return this.#type;
      }

      set type(v) {
        v = `${v}`;
        const invalidTypeIndex = findInvalid(v, 0, v.length, kHTTPToken);
        if (v.length === 0 || invalidTypeIndex !== -1) {
          throw new ERR_INVALID_MIME_SYNTAX('type', v, invalidTypeIndex);
        }
        this.#type = toASCIILower(v);
      }

      get subtype() {
        return this.#subtype;
      }

      set subtype(v) {
        v = `${v}`;
        const invalidSubtypeIndex = findInvalid(v, 0, v.length, kHTTPToken);
        if (v.length === 0 || invalidSubtypeIndex !== -1) {
          throw new ERR_INVALID_MIME_SYNTAX('subtype', v, invalidSubtypeIndex);
        }
        this.#subtype = toASCIILower(v);
      }

      get essence() {
        return `${this.#type}/${this.#subtype}`;
      }

      get params() {
        return this.#parameters;
      }

      toString() {
        let ret = `${this.#type}/${this.#subtype}`;
        const paramStr = FunctionPrototypeCall(MIMEParamsStringify, this.#parameters);
        if (paramStr.length) ret += `;${paramStr}`;
        return ret;
      }
    }
    Object.defineProperty(MIMEType.prototype, 'toJSON', {
      __proto__: null,
      configurable: true,
      value: MIMEType.prototype.toString,
      writable: true,
    });

    module.exports = {
      MIMEParams,
      MIMEType,
    };
  },
  "internal/util/debounce": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypePush,
      ObjectDefineProperties,
      PromiseReject,
      PromiseWithResolvers,
      ReflectApply,
    } = primordials;

    const {
      AbortError,
    } = require('internal/errors');
    const {
      validateAbortSignal,
      validateBoolean,
      validateFunction,
      validateInteger,
      validateObject,
    } = require('internal/validators');
    const { addAbortListener } = require('internal/events/abort_listener');
    const { kEmptyObject } = require('internal/util');
    const { TIMEOUT_MAX } = require('internal/timers');
    const { clearTimeout, setTimeout } = require('timers');
    const { markPromiseAsHandled } = internalBinding('util');

    function debounce(fn, wait, options = kEmptyObject) {
      validateFunction(fn, 'fn');
      validateInteger(wait, 'wait', 0, TIMEOUT_MAX);
      validateObject(options, 'options');

      const {
        leading = false,
        rejectOnCancel = false,
        signal,
      } = options;

      validateBoolean(leading, 'options.leading');
      validateBoolean(rejectOnCancel, 'options.rejectOnCancel');
      validateAbortSignal(signal, 'options.signal');

      if (signal?.aborted) {
        throw new AbortError(undefined, { __proto__: null, cause: signal.reason });
      }

      let args;
      let abortError;
      let pendingCalls = [];
      let refed = true;
      let timeout;

      function rejectPending(error) {
        const calls = pendingCalls;
        pendingCalls = [];
        for (let i = 0; i < calls.length; i++) {
          calls[i].reject(error);
        }
      }

      function cancelWithError(error) {
        if (timeout === undefined) return;
        clearTimeout(timeout);
        timeout = undefined;
        args = undefined;
        rejectPending(error);
      }

      function cancel(reason) {
        if (timeout === undefined) return;
        const error = reason === undefined ?
          new AbortError() :
          new AbortError(undefined, { __proto__: null, cause: reason });
        cancelWithError(error);
      }

      function abort() {
        if (abortError === undefined) {
          abortError = signal.reason === undefined ?
            new AbortError() :
            new AbortError(undefined, { __proto__: null, cause: signal.reason });
        }
        cancelWithError(abortError);
      }

      function ref() {
        refed = true;
        timeout?.ref();
        return debounced;
      }

      function unref() {
        refed = false;
        timeout?.unref();
        return debounced;
      }

      function invoke(callArgs, calls) {
        if (signal?.aborted) {
          abort();
          for (let i = 0; i < calls.length; i++) {
            calls[i].reject(abortError);
          }
          return;
        }

        let result;
        try {
          result = Reflect.apply(fn, debounced, callArgs);
        } catch (error) {
          for (let i = 0; i < calls.length; i++) {
            calls[i].reject(error);
          }
          return;
        }

        for (let i = 0; i < calls.length; i++) {
          calls[i].resolve(result);
        }
      }

      function invokePending() {
        const callArgs = args;
        args = undefined;
        const calls = pendingCalls;
        pendingCalls = [];
        if (calls.length !== 0) invoke(callArgs, calls);
      }

      function onTimeout() {
        timeout = undefined;
        invokePending();
      }

      function flush() {
        if (pendingCalls.length === 0) return;

        clearTimeout(timeout);
        timeout = undefined;
        invokePending();
      }

      function debounced(...callArgs) {
        if (signal?.aborted) abort();
        if (abortError !== undefined) return Promise.reject(abortError);

        const invokeNow = leading && timeout === undefined;
        if (timeout !== undefined) {
          if (pendingCalls.length !== 0) {
            markPromiseAsHandled(pendingCalls[pendingCalls.length - 1].promise);
            if (rejectOnCancel) {
              rejectPending(new AbortError('The debounced call was superseded'));
            }
          }
          timeout.refresh();
        } else {
          timeout = setTimeout(onTimeout, wait);
          if (!refed) timeout.unref();
        }

        if (signal?.aborted) {
          abort();
          return Promise.reject(abortError);
        }

        const call = PromiseWithResolvers();
        if (invokeNow) {
          invoke(callArgs, [call]);
        } else {
          pendingCalls.push(call);
          args = callArgs;
        }
        return call.promise;
      }

      function createFn(value) {
        return {
          __proto__: null,
          configurable: true,
          enumerable: true,
          writable: true,
          value,
        };
      }

      Object.defineProperties(debounced, {
        __proto__: null,
        cancel: createFn(cancel),
        flush: createFn(flush),
        ref: createFn(ref),
        unref: createFn(unref),
        pending: {
          __proto__: null,
          enumerable: false,
          get() {
            return pendingCalls.length === 0 ? null : pendingCalls[pendingCalls.length - 1].promise;
          },
        },
        pendingCount: {
          __proto__: null,
          enumerable: false,
          get() { return pendingCalls.length; },
        },
        length: {
          __proto__: null,
          configurable: true,
          value: fn.length,
        },
        name: {
          __proto__: null,
          configurable: true,
          value: fn.name,
        },
      });

      if (signal !== undefined) {
        addAbortListener(signal, abort);
      }

      return debounced;
    }

    module.exports = debounce;
  },
  "internal/util/throttle": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypePush,
      ArrayPrototypeSlice,
      ObjectDefineProperties,
      PromisePrototypeThen,
      PromiseReject,
      PromiseResolve,
      PromiseWithResolvers,
      ReflectApply,
    } = primordials;

    const {
      AbortError,
      codes: {
        ERR_THROTTLED,
      },
    } = require('internal/errors');
    const {
      validateAbortSignal,
      validateBoolean,
      validateFunction,
      validateInteger,
      validateObject,
      validateOneOf,
    } = require('internal/validators');
    const { addAbortListener } = require('internal/events/abort_listener');
    const { kEmptyObject } = require('internal/util');
    const { TIMEOUT_MAX } = require('internal/timers');
    const { clearTimeout, setTimeout } = require('timers');

    const timersBinding = internalBinding('timers');
    const { markPromiseAsHandled } = internalBinding('util');

    function throttle(fn, limit, interval, options = kEmptyObject) {
      validateFunction(fn, 'fn');
      validateInteger(limit, 'limit', 1);
      validateInteger(interval, 'interval', 0, TIMEOUT_MAX);
      validateObject(options, 'options');

      const {
        concurrency = Infinity,
        maxPending = Infinity,
        overflow = 'queue',
        signal,
        strict = false,
      } = options;

      if (concurrency !== Infinity) {
        validateInteger(concurrency, 'options.concurrency', 1);
      }
      if (maxPending !== Infinity) {
        validateInteger(maxPending, 'options.maxPending', 0);
      }
      validateOneOf(overflow, 'options.overflow', ['queue', 'drop']);
      validateAbortSignal(signal, 'options.signal');
      validateBoolean(strict, 'options.strict');

      if (signal?.aborted) {
        throw new AbortError(undefined, { __proto__: null, cause: signal.reason });
      }

      let abortError;
      let activeCount = 0;
      let pendingCalls = [];
      let pendingIndex = 0;
      let refed = true;
      let strictIndex = 0;
      let strictTicks = [];
      let timeout;
      let windowCount = 0;
      let windowStart;

      function pendingCount() {
        return pendingCalls.length - pendingIndex;
      }

      function dequeue() {
        const call = pendingCalls[pendingIndex];
        pendingCalls[pendingIndex++] = undefined;

        if (pendingIndex === pendingCalls.length) {
          pendingCalls = [];
          pendingIndex = 0;
        } else if (pendingIndex > 1024 && pendingIndex * 2 >= pendingCalls.length) {
          pendingCalls = pendingCalls.slice(pendingIndex);
          pendingIndex = 0;
        }

        return call;
      }

      function rejectPending(error) {
        const calls = pendingCalls;
        const start = pendingIndex;
        pendingCalls = [];
        pendingIndex = 0;
        for (let i = start; i < calls.length; i++) {
          calls[i].reject(error);
        }
      }

      function resetLimiter() {
        strictIndex = 0;
        strictTicks = [];
        windowCount = 0;
        windowStart = undefined;
      }

      function cancelWithError(error) {
        if (timeout !== undefined) {
          clearTimeout(timeout);
          timeout = undefined;
        }
        resetLimiter();
        rejectPending(error);
      }

      function cancel(reason) {
        const error = reason === undefined ?
          new AbortError() :
          new AbortError(undefined, { __proto__: null, cause: reason });
        cancelWithError(error);
      }

      function abort() {
        if (abortError === undefined) {
          abortError = signal.reason === undefined ?
            new AbortError() :
            new AbortError(undefined, { __proto__: null, cause: signal.reason });
        }
        cancelWithError(abortError);
      }

      function ref() {
        refed = true;
        timeout?.ref();
        return throttled;
      }

      function unref() {
        refed = false;
        timeout?.unref();
        return throttled;
      }

      function hasImmediateCapacity() {
        if (signal?.aborted ||
            pendingCount() !== 0 ||
            activeCount >= concurrency) {
          return false;
        }

        const now = timersBinding.getLibuvNow();
        if (strict) {
          let index = strictIndex;
          while (index < strictTicks.length &&
                 now - strictTicks[index] >= interval) {
            index++;
          }
          return strictTicks.length - index < limit;
        }

        return windowStart === undefined ||
          now - windowStart >= interval ||
          windowCount < limit;
      }

      function pruneStrictTicks(now) {
        while (strictIndex < strictTicks.length &&
               now - strictTicks[strictIndex] >= interval) {
          strictIndex++;
        }

        if (strictIndex === strictTicks.length) {
          strictIndex = 0;
          strictTicks = [];
        } else if (strictIndex > 1024 && strictIndex * 2 >= strictTicks.length) {
          strictTicks = strictTicks.slice(strictIndex);
          strictIndex = 0;
        }
      }

      function hasCapacity(now) {
        if (strict) {
          pruneStrictTicks(now);
          return strictTicks.length - strictIndex < limit;
        }

        if (windowStart === undefined || now - windowStart >= interval) {
          windowCount = 0;
          windowStart = now;
        }
        return windowCount < limit;
      }

      function reserve(now) {
        if (strict) {
          strictTicks.push(now);
        } else {
          windowCount++;
        }
      }

      function onSettled() {
        activeCount--;
        drain();
      }

      function invoke(call) {
        activeCount++;
        let resultPromise;
        try {
          resultPromise = Promise.resolve(Reflect.apply(fn, throttled, call.args));
        } catch (error) {
          resultPromise = Promise.reject(error);
        }
        call.resolve(resultPromise);
        PromisePrototypeThen(resultPromise, onSettled, onSettled);
      }

      function drop() {
        const promise = Promise.reject(new ERR_THROTTLED());
        markPromiseAsHandled(promise);
        return promise;
      }

      function schedule() {
        if (timeout !== undefined ||
            pendingCount() === 0 ||
            activeCount >= concurrency) {
          return;
        }

        const now = timersBinding.getLibuvNow();
        if (hasCapacity(now)) {
          drain();
          return;
        }

        const delay = strict ?
          strictTicks[strictIndex] + interval - now :
          windowStart + interval - now;
        timeout = setTimeout(onTimeout, delay);
        if (!refed) timeout.unref();
      }

      function drain() {
        if (signal?.aborted) {
          abort();
          return;
        }

        while (pendingCount() !== 0 && activeCount < concurrency) {
          const now = timersBinding.getLibuvNow();
          if (!hasCapacity(now)) break;
          const call = dequeue();
          reserve(now);
          invoke(call);
        }

        schedule();
      }

      function onTimeout() {
        timeout = undefined;
        drain();
      }

      function throttled(...callArgs) {
        if (signal?.aborted) abort();
        if (abortError !== undefined) return Promise.reject(abortError);

        const now = timersBinding.getLibuvNow();
        const canInvoke = pendingCount() === 0 &&
          activeCount < concurrency &&
          hasCapacity(now);
        if (!canInvoke &&
            (overflow === 'drop' || pendingCount() >= maxPending)) {
          return drop();
        }

        const call = PromiseWithResolvers();
        call.args = callArgs;

        if (canInvoke) {
          reserve(now);
          invoke(call);
        } else {
          pendingCalls.push(call);
          schedule();
        }

        return call.promise;
      }

      function createFn(value) {
        return {
          __proto__: null,
          configurable: true,
          enumerable: true,
          writable: true,
          value,
        };
      }

      Object.defineProperties(throttled, {
        __proto__: null,
        cancel: createFn(cancel),
        hasImmediateCapacity: createFn(hasImmediateCapacity),
        ref: createFn(ref),
        unref: createFn(unref),
        pending: {
          __proto__: null,
          enumerable: false,
          get() {
            return pendingCount() === 0 ? null :
              pendingCalls[pendingCalls.length - 1].promise;
          },
        },
        pendingCount: {
          __proto__: null,
          enumerable: false,
          get: pendingCount,
        },
        activeCount: {
          __proto__: null,
          enumerable: false,
          get() {
            return activeCount;
          },
        },
        length: {
          __proto__: null,
          configurable: true,
          value: fn.length,
        },
        name: {
          __proto__: null,
          configurable: true,
          value: fn.name,
        },
      });

      if (signal !== undefined) {
        addAbortListener(signal, abort);
      }

      return throttled;
    }

    module.exports = throttle;
  },
  "internal/util/diff": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayIsArray,
      ArrayPrototypeReverse,
    } = primordials;

    const { validateStringArray, validateString } = require('internal/validators');
    const { myersDiff } = require('internal/assert/myers_diff');

    function validateInput(value, name) {
      if (!Array.isArray(value)) {
        validateString(value, name);
        return;
      }

      validateStringArray(value, name);
    }

    function diff(actual, expected) {
      if (actual === expected) {
        return [];
      }

      validateInput(actual, 'actual');
      validateInput(expected, 'expected');

      return myersDiff(actual, expected).reverse();
    }

    module.exports = {
      diff,
    };
  },
  "internal/assert/myers_diff": function (module, exports, require, process, primordials, internalBinding) {
    'use strict';

    const {
      ArrayPrototypePush,
      Int32Array,
      StringPrototypeEndsWith,
    } = primordials;

    const {
      codes: {
        ERR_OUT_OF_RANGE,
      },
    } = require('internal/errors');

    const colors = require('internal/util/colors');

    const kNopLinesToCollapse = 5;
    const kOperations = {
      DELETE: -1,
      NOP: 0,
      INSERT: 1,
    };

    function areLinesEqual(actual, expected, checkCommaDisparity) {
      if (actual === expected) {
        return true;
      }
      if (checkCommaDisparity) {
        return (actual + ',') === expected || actual === (expected + ',');
      }
      return false;
    }

    function myersDiff(actual, expected, checkCommaDisparity = false) {
      const actualLength = actual.length;
      const expectedLength = expected.length;
      const max = actualLength + expectedLength;

      if (max > 2 ** 31 - 1) {
        throw new ERR_OUT_OF_RANGE(
          'myersDiff input size',
          '< 2^31',
          max,
        );
      }

      const v = new Int32Array(2 * max + 1);
      const trace = [];

      for (let diffLevel = 0; diffLevel <= max; diffLevel++) {
        trace.push(new Int32Array(v)); // Clone the current state of `v`

        for (let diagonalIndex = -diffLevel; diagonalIndex <= diffLevel; diagonalIndex += 2) {
          const offset = diagonalIndex + max;
          const previousOffset = v[offset - 1];
          const nextOffset = v[offset + 1];
          let x = diagonalIndex === -diffLevel || (diagonalIndex !== diffLevel && previousOffset < nextOffset) ?
            nextOffset :
            previousOffset + 1;
          let y = x - diagonalIndex;

          while (
            x < actualLength &&
            y < expectedLength &&
            areLinesEqual(actual[x], expected[y], checkCommaDisparity)
          ) {
            x++;
            y++;
          }

          v[offset] = x;

          if (x >= actualLength && y >= expectedLength) {
            return backtrack(trace, actual, expected, checkCommaDisparity);
          }
        }
      }
    }

    function backtrack(trace, actual, expected, checkCommaDisparity) {
      const actualLength = actual.length;
      const expectedLength = expected.length;
      const max = actualLength + expectedLength;

      let x = actualLength;
      let y = expectedLength;
      const result = [];

      for (let diffLevel = trace.length - 1; diffLevel >= 0; diffLevel--) {
        const v = trace[diffLevel];
        const diagonalIndex = x - y;
        const offset = diagonalIndex + max;

        let prevDiagonalIndex;
        if (
          diagonalIndex === -diffLevel ||
          (diagonalIndex !== diffLevel && v[offset - 1] < v[offset + 1])
        ) {
          prevDiagonalIndex = diagonalIndex + 1;
        } else {
          prevDiagonalIndex = diagonalIndex - 1;
        }

        const prevX = v[prevDiagonalIndex + max];
        const prevY = prevX - prevDiagonalIndex;

        while (x > prevX && y > prevY) {
          const actualItem = actual[x - 1];
          const value = checkCommaDisparity && !actualItem.endsWith(',') ? expected[y - 1] : actualItem;
          result.push([ kOperations.NOP, value ]);
          x--;
          y--;
        }

        if (diffLevel > 0) {
          if (x > prevX) {
            result.push([ kOperations.INSERT, actual[--x] ]);
          } else {
            result.push([ kOperations.DELETE, expected[--y] ]);
          }
        }
      }

      return result;
    }

    function printSimpleMyersDiff(diff) {
      let message = '';

      for (let diffIdx = diff.length - 1; diffIdx >= 0; diffIdx--) {
        const { 0: operation, 1: value } = diff[diffIdx];
        let color = colors.white;

        if (operation === kOperations.INSERT) {
          color = colors.green;
        } else if (operation === kOperations.DELETE) {
          color = colors.red;
        }

        message += `${color}${value}${colors.white}`;
      }

      return `\n${message}`;
    }

    function printMyersDiff(diff, operator) {
      let message = '';
      let skipped = false;
      let nopCount = 0;

      for (let diffIdx = diff.length - 1; diffIdx >= 0; diffIdx--) {
        const { 0: operation, 1: value } = diff[diffIdx];
        const previousOperation = diffIdx < diff.length - 1 ? diff[diffIdx + 1][0] : null;

        if (previousOperation === kOperations.NOP && operation !== previousOperation) {
          if (nopCount === kNopLinesToCollapse + 1) {
            message += `${colors.white}  ${diff[diffIdx + 1][1]}\n`;
          } else if (nopCount === kNopLinesToCollapse + 2) {
            message += `${colors.white}  ${diff[diffIdx + 2][1]}\n`;
            message += `${colors.white}  ${diff[diffIdx + 1][1]}\n`;
          } else if (nopCount >= kNopLinesToCollapse + 3) {
            message += `${colors.blue}...${colors.white}\n`;
            message += `${colors.white}  ${diff[diffIdx + 1][1]}\n`;
            skipped = true;
          }
          nopCount = 0;
        }

        if (operation === kOperations.INSERT) {
          if (operator === 'partialDeepStrictEqual') {
            message += `${colors.gray}${colors.hasColors ? ' ' : '+'} ${value}${colors.white}\n`;
          } else {
            message += `${colors.green}+${colors.white} ${value}\n`;
          }
        } else if (operation === kOperations.DELETE) {
          message += `${colors.red}-${colors.white} ${value}\n`;
        } else if (operation === kOperations.NOP) {
          if (nopCount < kNopLinesToCollapse) {
            message += `${colors.white}  ${value}\n`;
          }
          nopCount++;
        }
      }

      message = message.trimEnd();

      return { message: `\n${message}`, skipped };
    }

    module.exports = { myersDiff, printMyersDiff, printSimpleMyersDiff };
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

module.exports = load("util");
