"use strict";

// node's lib/events.js with the three internal files it pulls in. What it
// asks of the rest of node is answered by `shims` below

const errors = require("spark:errors");
const loop = require("spark:loop");

const AsyncIteratorPrototype = Object.getPrototypeOf(
  Object.getPrototypeOf(async function* () {}).prototype,
);

// a module the embedder may not have passed
function optional(specifier) {
  try {
    return require(specifier);
  } catch {
    return undefined;
  }
}

function spliceOne(list, index) {
  for (; index + 1 < list.length; index++) list[index] = list[index + 1];
  list.pop();
}

function identicalSequenceRange(a, b) {
  for (let i = 0; i < a.length - 3; i++) {
    const pos = b.indexOf(a[i]);
    if (pos !== -1) {
      const rest = b.length - pos;
      if (rest > 3) {
        let len = 1;
        const maxLen = Math.min(a.length - i, rest);
        while (maxLen > len && a[i + len] === b[pos + len]) {
          len++;
        }
        if (len > 3) {
          return [len, i];
        }
      }
    }
  }

  return [0, 0];
}

const shims = {
  // what the files see as `process`
  process: () => ({
    nextTick(callback, ...args) {
      loop.nextTick(() => callback(...args));
    },
    emitWarning(warning) {
      optional("node:process")?.emitWarning?.(warning);
    },
  }),
  "internal/errors": () => errors,
  "internal/validators": () => errors,
  "internal/util": () => ({
    kEmptyObject: Object.freeze({ __proto__: null }),
    spliceOne,
  }),
  "internal/util/inspect": () => ({
    inspect: (value, options) => errors.inspect(value, options),
    identicalSequenceRange,
  }),
  // without the dom events module there are no event targets to meet
  "internal/event_target": () => {
    const found = optional("spark:dom_events");
    if (found === undefined) {
      return {
        isEventTarget: () => false,
        kEvents: Symbol("events"),
        kResistStopPropagation: Symbol("resistStopPropagation"),
      };
    }
    return { Event: found.Event, ...found.internals };
  },
  "internal/process/task_queues": () => ({
    queueMicrotask: (callback) => {
      Promise.resolve().then(callback);
    },
  }),
  async_hooks: () => require("node:async_hooks"),
};

const files = {
  "events": function (module, exports, require, process, primordials, internalBinding) {
    "use strict";

    const kRejection = Symbol.for("nodejs.rejection");

    const { kEmptyObject, spliceOne } = require("internal/util");

    const {
      inspect,
      identicalSequenceRange,
    } = require("internal/util/inspect");

    let FixedQueue;
    let kFirstEventParam;
    let kResistStopPropagation;

    const {
      AbortError,
      codes: {
        ERR_INVALID_ARG_TYPE,
        ERR_UNHANDLED_ERROR,
      },
      genericNodeError,
      kEnhanceStackBeforeInspector,
    } = require("internal/errors");

    const {
      validateInteger,
      validateAbortSignal,
      validateBoolean,
      validateFunction,
      validateNumber,
      validateObject,
      validateString,
    } = require("internal/validators");
    const { addAbortListener } = require("internal/events/abort_listener");

    const kCapture = Symbol("kCapture");
    const kErrorMonitor = Symbol("events.errorMonitor");
    const kShapeMode = Symbol("shapeMode");
    const kEmitting = Symbol("events.emitting");
    const kMaxEventTargetListeners = Symbol("events.maxEventTargetListeners");
    const kMaxEventTargetListenersWarned =
      Symbol("events.maxEventTargetListenersWarned");
    const kWatermarkData = Symbol.for("nodejs.watermarkData");

    let EventEmitterAsyncResource;
    function lazyEventEmitterAsyncResource() {
      if (EventEmitterAsyncResource === undefined) {
        const {
          AsyncResource,
        } = require("async_hooks");

        class EventEmitterReferencingAsyncResource extends AsyncResource {
          #eventEmitter;

                constructor(ee, type, options) {
            super(type, options);
            this.#eventEmitter = ee;
          }

                get eventEmitter() {
            return this.#eventEmitter;
          }
        }

        EventEmitterAsyncResource =
          class EventEmitterAsyncResource extends EventEmitter {
            #asyncResource;

                    constructor(options = undefined) {
              let name;
              if (typeof options === "string") {
                name = options;
                options = undefined;
              } else {
                if (new.target === EventEmitterAsyncResource) {
                  validateString(options?.name, "options.name");
                }
                name = options?.name || new.target.name;
              }
              super(options);

              this.#asyncResource = new EventEmitterReferencingAsyncResource(this, name, options);
            }

                    emit(event, ...args) {
              const asyncResource = this.#asyncResource;
              args.unshift(super.emit, this, event);
              return Reflect.apply(asyncResource.runInAsyncScope, asyncResource,
                                  args);
            }

                    emitDestroy() {
              this.#asyncResource.emitDestroy();
            }

                    get asyncId() {
              return this.#asyncResource.asyncId();
            }

                    get triggerAsyncId() {
              return this.#asyncResource.triggerAsyncId();
            }

                    get asyncResource() {
              return this.#asyncResource;
            }
          };
      }
      return EventEmitterAsyncResource;
    }

    function EventEmitter(opts) {
      EventEmitter.init.call(this, opts);
    }
    module.exports = EventEmitter;
    module.exports.addAbortListener = addAbortListener;
    module.exports.once = once;
    module.exports.on = on;
    module.exports.getEventListeners = getEventListeners;
    module.exports.getMaxListeners = getMaxListeners;
    module.exports.listenerCount = listenerCount;
    EventEmitter.EventEmitter = EventEmitter;

    EventEmitter.usingDomains = false;

    EventEmitter.captureRejectionSymbol = kRejection;
    Object.defineProperty(EventEmitter, "captureRejections", {
      __proto__: null,
      get() {
        return EventEmitter.prototype[kCapture];
      },
      set(value) {
        validateBoolean(value, "EventEmitter.captureRejections");

        EventEmitter.prototype[kCapture] = value;
      },
      enumerable: true,
    });

    Object.defineProperty(EventEmitter, "EventEmitterAsyncResource", {
      __proto__: null,
      enumerable: true,
      get: lazyEventEmitterAsyncResource,
      set: undefined,
      configurable: true,
    });

    EventEmitter.errorMonitor = kErrorMonitor;

    Object.defineProperty(EventEmitter.prototype, kCapture, {
      __proto__: null,
      value: false,
      writable: true,
      enumerable: false,
    });

    EventEmitter.prototype._events = undefined;
    EventEmitter.prototype._eventsCount = 0;
    EventEmitter.prototype._maxListeners = undefined;

    let defaultMaxListeners = 10;
    let isEventTarget;

    function checkListener(listener) {
      validateFunction(listener, "listener");
    }

    Object.defineProperty(EventEmitter, "defaultMaxListeners", {
      __proto__: null,
      enumerable: true,
      get: function() {
        return defaultMaxListeners;
      },
      set: function(arg) {
        validateNumber(arg, "defaultMaxListeners", 0);
        defaultMaxListeners = arg;
      },
    });

    Object.defineProperties(EventEmitter, {
      kMaxEventTargetListeners: {
        __proto__: null,
        value: kMaxEventTargetListeners,
        enumerable: false,
        configurable: false,
        writable: false,
      },
      kMaxEventTargetListenersWarned: {
        __proto__: null,
        value: kMaxEventTargetListenersWarned,
        enumerable: false,
        configurable: false,
        writable: false,
      },
    });

    EventEmitter.setMaxListeners =
      function(n = defaultMaxListeners, ...eventTargets) {
        validateNumber(n, "setMaxListeners", 0);
        if (eventTargets.length === 0) {
          defaultMaxListeners = n;
        } else {
          if (isEventTarget === undefined)
            isEventTarget = require("internal/event_target").isEventTarget;

          for (let i = 0; i < eventTargets.length; i++) {
            const target = eventTargets[i];
            if (isEventTarget(target)) {
              target[kMaxEventTargetListeners] = n;
              target[kMaxEventTargetListenersWarned] = false;
            } else if (typeof target.setMaxListeners === "function") {
              target.setMaxListeners(n);
            } else {
              throw new ERR_INVALID_ARG_TYPE(
                "eventTargets",
                ["EventEmitter", "EventTarget"],
                target);
            }
          }
        }
      };

    EventEmitter.init = function(opts) {

      if (this._events === undefined ||
          this._events === Object.getPrototypeOf(this)._events) {
        this._events = { __proto__: null };
        this._eventsCount = 0;
        this[kShapeMode] = false;
      } else {
        this[kShapeMode] = true;
      }

      this._maxListeners ||= undefined;

      if (opts?.captureRejections) {
        validateBoolean(opts.captureRejections, "options.captureRejections");
        this[kCapture] = Boolean(opts.captureRejections);
      } else {
        this[kCapture] = EventEmitter.prototype[kCapture];
      }
    };

    function addCatch(that, promise, type, args) {
      if (!that[kCapture]) {
        return;
      }

      try {
        const then = promise.then;

        if (typeof then === "function") {
          then.call(promise, undefined, function(err) {
            process.nextTick(emitUnhandledRejectionOrErr, that, err, type, args);
          });
        }
      } catch (err) {
        that.emit("error", err);
      }
    }

    function emitUnhandledRejectionOrErr(ee, err, type, args) {
      if (typeof ee[kRejection] === "function") {
        ee[kRejection](err, type, ...args);
      } else {
        const prev = ee[kCapture];

        try {
          ee[kCapture] = false;
          ee.emit("error", err);
        } finally {
          ee[kCapture] = prev;
        }
      }
    }

    EventEmitter.prototype.setMaxListeners = function setMaxListeners(n) {
      validateNumber(n, "setMaxListeners", 0);
      this._maxListeners = n;
      return this;
    };

    function _getMaxListeners(that) {
      if (that._maxListeners === undefined)
        return EventEmitter.defaultMaxListeners;
      return that._maxListeners;
    }

    EventEmitter.prototype.getMaxListeners = function getMaxListeners() {
      return _getMaxListeners(this);
    };

    function enhanceStackTrace(err, own) {
      let ctorInfo = "";
      try {
        const { name } = this.constructor;
        if (name !== "EventEmitter")
          ctorInfo = ` on ${name} instance`;
      } catch {
      }
      const sep = `\nEmitted "error" event${ctorInfo} at:\n`;

      const errStack = err.stack.split("\n").slice(1);
      const ownStack = own.stack.split("\n").slice(1);

      const { len, offset } = identicalSequenceRange(ownStack, errStack);
      if (len > 0) {
        ownStack.splice(offset + 1, len - 2, "    [... lines matching original stack trace ...]");
      }

      return err.stack + sep + ownStack.join("\n");
    }

    function getUnhandledErrorException(ee, args) {
      let er;
      if (args.length > 0)
        er = args[0];
      if (er instanceof Error) {
        try {
          const capture = {};
          Error.captureStackTrace(capture, EventEmitter.prototype.emit);
          Object.defineProperty(er, kEnhanceStackBeforeInspector, {
            __proto__: null,
            value: enhanceStackTrace.bind(ee, er, capture),
            configurable: true,
          });
        } catch {
        }

        return er;
      }

      let stringifiedEr;
      try {
        stringifiedEr = inspect(er);
      } catch {
        stringifiedEr = er;
      }

      const err = new ERR_UNHANDLED_ERROR(stringifiedEr);
      err.context = er;
      return err;
    }

    EventEmitter.prototype.emit = function emit(type, ...args) {
      let doError = (type === "error");

      const events = this._events;
      if (events !== undefined) {
        if (doError && events[kErrorMonitor] !== undefined)
          this.emit(kErrorMonitor, ...args);
        doError &&= events.error === undefined;
      } else if (!doError)
        return false;

      if (doError) {
        const er = getUnhandledErrorException(this, args);
        throw er; // Unhandled "error" event
      }

      const handler = events[type];

      if (handler === undefined)
        return false;

      if (typeof handler === "function") {
        const result = Reflect.apply(handler, this, args);

        if (result !== undefined && result !== null) {
          addCatch(this, result, type, args);
        }
      } else {
        handler[kEmitting]++;
        try {
          for (let i = 0; i < handler.length; ++i) {
            const result = Reflect.apply(handler[i], this, args);

            if (result !== undefined && result !== null) {
              addCatch(this, result, type, args);
            }
          }
        } finally {
          handler[kEmitting]--;
        }
      }

      return true;
    };

    function _addListener(target, type, listener, prepend) {
      let m;
      let events;
      let existing;

      checkListener(listener);

      events = target._events;
      if (events === undefined) {
        events = target._events = { __proto__: null };
        target._eventsCount = 0;
      } else {
        if (events.newListener !== undefined) {
          target.emit("newListener", type,
                      listener.listener ?? listener);

          events = target._events;
        }
        existing = events[type];
      }

      if (existing === undefined) {
        events[type] = listener;
        ++target._eventsCount;
      } else {
        if (typeof existing === "function") {
          existing = prepend ? [listener, existing] : [existing, listener];
          existing[kEmitting] = 0;
          events[type] = existing;
        } else {
          existing = ensureMutableListenerArray(events, type, existing);
          if (prepend) {
            existing.unshift(listener);
          } else {
            existing.push(listener);
          }
        }

        m = _getMaxListeners(target);
        if (m > 0 && existing.length > m && !existing.warned)
          warnMaxListenersExceeded(target, type, existing, m);
      }

      return target;
    }

    function warnMaxListenersExceeded(target, type, existing, m) {
      existing.warned = true;
      const w = genericNodeError(
        `Possible EventEmitter memory leak detected. ${existing.length} ${String(type)} listeners ` +
        `added to ${inspect(target, { depth: -1 })}. MaxListeners is ${m}. Use emitter.setMaxListeners() to increase limit`,
        { name: "MaxListenersExceededWarning", emitter: target, type: type, count: existing.length });
      process.emitWarning(w);
    }

    EventEmitter.prototype.addListener = function addListener(type, listener) {
      return _addListener(this, type, listener, false);
    };

    EventEmitter.prototype.on = EventEmitter.prototype.addListener;

    EventEmitter.prototype.prependListener =
        function prependListener(type, listener) {
          return _addListener(this, type, listener, true);
        };

    function _onceWrap(target, type, listener) {
      let fired = false;
      function wrapper(...args) {
        if (fired) return;
        fired = true;
        target.removeListener(type, wrapper);
        return Reflect.apply(listener, target, args);
      }
      wrapper.listener = listener;
      return wrapper;
    }

    EventEmitter.prototype.once = function once(type, listener) {
      checkListener(listener);

      this.on(type, _onceWrap(this, type, listener));
      return this;
    };

    EventEmitter.prototype.prependOnceListener =
        function prependOnceListener(type, listener) {
          checkListener(listener);

          this.prependListener(type, _onceWrap(this, type, listener));
          return this;
        };

    EventEmitter.prototype.removeListener =
        function removeListener(type, listener) {
          checkListener(listener);

          const events = this._events;
          if (events === undefined)
            return this;

          let list = events[type];
          if (list === undefined)
            return this;

          if (list === listener || list.listener === listener) {
            this._eventsCount -= 1;

            if (this[kShapeMode]) {
              events[type] = undefined;
            } else if (this._eventsCount === 0) {
              this._events = { __proto__: null };
            } else {
              delete events[type];
            }

            if (events.removeListener !== undefined)
              this.emit("removeListener", type, list.listener || listener);
          } else if (typeof list !== "function") {
            list = ensureMutableListenerArray(events, type, list);
            let position = -1;

            for (let i = list.length - 1; i >= 0; i--) {
              if (list[i] === listener || list[i].listener === listener) {
                position = i;
                break;
              }
            }

            if (position < 0)
              return this;

            if (position === 0)
              list.shift();
            else {
              spliceOne(list, position);
            }

            if (list.length === 1)
              events[type] = list[0];

            if (events.removeListener !== undefined)
              this.emit("removeListener", type, listener);
          }

          return this;
        };

    EventEmitter.prototype.off = EventEmitter.prototype.removeListener;

    EventEmitter.prototype.removeAllListeners =
        function removeAllListeners(type) {
          const events = this._events;
          if (events === undefined)
            return this;

          if (events.removeListener === undefined) {
            if (arguments.length === 0) {
              this._events = { __proto__: null };
              this._eventsCount = 0;
            } else if (events[type] !== undefined) {
              if (--this._eventsCount === 0)
                this._events = { __proto__: null };
              else
                delete events[type];
            }
            this[kShapeMode] = false;
            return this;
          }

          if (arguments.length === 0) {
            for (const key of Reflect.ownKeys(events)) {
              if (key === "removeListener") continue;
              this.removeAllListeners(key);
            }
            this.removeAllListeners("removeListener");
            this._events = { __proto__: null };
            this._eventsCount = 0;
            this[kShapeMode] = false;
            return this;
          }

          const listeners = events[type];

          if (typeof listeners === "function") {
            this.removeListener(type, listeners);
          } else if (listeners !== undefined) {
            for (let i = listeners.length - 1; i >= 0; i--) {
              this.removeListener(type, listeners[i]);
            }
          }

          return this;
        };

    function _listeners(target, type, unwrap) {
      const events = target._events;

      if (events === undefined)
        return [];

      const evlistener = events[type];
      if (evlistener === undefined)
        return [];

      if (typeof evlistener === "function")
        return unwrap ? [evlistener.listener || evlistener] : [evlistener];

      return unwrap ?
        unwrapListeners(evlistener) : arrayClone(evlistener);
    }

    EventEmitter.prototype.listeners = function listeners(type) {
      return _listeners(this, type, true);
    };

    EventEmitter.prototype.rawListeners = function rawListeners(type) {
      return _listeners(this, type, false);
    };

    EventEmitter.prototype.listenerCount = function listenerCount(type, listener) {
      const events = this._events;

      if (events !== undefined) {
        const evlistener = events[type];

        if (typeof evlistener === "function") {
          if (listener != null) {
            return listener === evlistener || listener === evlistener.listener ? 1 : 0;
          }

          return 1;
        } else if (evlistener !== undefined) {
          if (listener != null) {
            let matching = 0;

            for (let i = 0, l = evlistener.length; i < l; i++) {
              if (evlistener[i] === listener || evlistener[i].listener === listener) {
                matching++;
              }
            }

            return matching;
          }

          return evlistener.length;
        }
      }

      return 0;
    };

    EventEmitter.prototype.eventNames = function eventNames() {
      if (this._eventsCount === 0)
        return [];
      const events = this._events;
      const names = [];
      for (const key of Reflect.ownKeys(events)) {
        if (events[key] !== undefined)
          names.push(key);
      }
      return names;
    };

    function arrayClone(arr) {
      switch (arr.length) {
        case 2: return [arr[0], arr[1]];
        case 3: return [arr[0], arr[1], arr[2]];
        case 4: return [arr[0], arr[1], arr[2], arr[3]];
        case 5: return [arr[0], arr[1], arr[2], arr[3], arr[4]];
        case 6: return [arr[0], arr[1], arr[2], arr[3], arr[4], arr[5]];
      }
      return arr.slice();
    }

    function cloneEventListenerArray(arr) {
      const copy = arrayClone(arr);
      copy[kEmitting] = 0;
      if (arr.warned) {
        copy.warned = true;
      }
      return copy;
    }

    function ensureMutableListenerArray(events, type, handler) {
      if (handler[kEmitting] > 0) {
        const copy = cloneEventListenerArray(handler);
        events[type] = copy;
        return copy;
      }
      return handler;
    }

    function unwrapListeners(arr) {
      const ret = arrayClone(arr);
      for (let i = 0; i < ret.length; ++i) {
        const orig = ret[i].listener;
        if (typeof orig === "function")
          ret[i] = orig;
      }
      return ret;
    }

    function getEventListeners(emitterOrTarget, type) {
      if (typeof emitterOrTarget.listeners === "function") {
        return emitterOrTarget.listeners(type);
      }
      const { isEventTarget, kEvents } = require("internal/event_target");
      if (isEventTarget(emitterOrTarget)) {
        const root = emitterOrTarget[kEvents]?.get(type);
        const listeners = [];
        let handler = root?.next;
        while (handler?.listener !== undefined) {
          const listener = handler.listener?.deref ?
            handler.listener.deref() : handler.listener;
          listeners.push(listener);
          handler = handler.next;
        }
        return listeners;
      }
      throw new ERR_INVALID_ARG_TYPE("emitter",
                                     ["EventEmitter", "EventTarget"],
                                     emitterOrTarget);
    }

    function getMaxListeners(emitterOrTarget) {
      if (typeof emitterOrTarget?.getMaxListeners === "function") {
        return _getMaxListeners(emitterOrTarget);
      } else if (typeof emitterOrTarget?.[kMaxEventTargetListeners] === "number") {
        return emitterOrTarget[kMaxEventTargetListeners];
      }

      throw new ERR_INVALID_ARG_TYPE("emitter",
                                     ["EventEmitter", "EventTarget"],
                                     emitterOrTarget);
    }

    function listenerCount(emitterOrTarget, type) {
      if (typeof emitterOrTarget.listenerCount === "function") {
        return emitterOrTarget.listenerCount(type);
      }
      const { isEventTarget, kEvents } = require("internal/event_target");
      if (isEventTarget(emitterOrTarget)) {
        return emitterOrTarget[kEvents]?.get(type)?.size ?? 0;
      }
      throw new ERR_INVALID_ARG_TYPE("emitter",
                                     ["EventEmitter", "EventTarget"],
                                     emitterOrTarget);
    }

    async function once(emitter, name, options = kEmptyObject) {
      validateObject(options, "options");
      const { signal } = options;
      validateAbortSignal(signal, "options.signal");
      if (signal?.aborted)
        throw new AbortError(undefined, { cause: signal.reason });
      return new Promise((resolve, reject) => {
        const errorListener = (err) => {
          emitter.removeListener(name, resolver);
          if (signal != null) {
            eventTargetAgnosticRemoveListener(signal, "abort", abortListener);
          }
          reject(err);
        };
        const resolver = (...args) => {
          if (typeof emitter.removeListener === "function") {
            emitter.removeListener("error", errorListener);
          }
          if (signal != null) {
            eventTargetAgnosticRemoveListener(signal, "abort", abortListener);
          }
          resolve(args);
        };

        kResistStopPropagation ??= require("internal/event_target").kResistStopPropagation;
        const opts = { __proto__: null, once: true, [kResistStopPropagation]: true };
        eventTargetAgnosticAddListener(emitter, name, resolver, opts);
        if (name !== "error" && typeof emitter.once === "function") {
          emitter.once("error", errorListener);
        }
        function abortListener() {
          eventTargetAgnosticRemoveListener(emitter, name, resolver);
          eventTargetAgnosticRemoveListener(emitter, "error", errorListener);
          reject(new AbortError(undefined, { cause: signal?.reason }));
        }
        if (signal != null) {
          eventTargetAgnosticAddListener(
            signal, "abort", abortListener, { __proto__: null, once: true, [kResistStopPropagation]: true });
        }
      });
    }

    function createIterResult(value, done) {
      return { done, value };
    }

    function eventTargetAgnosticRemoveListener(emitter, name, listener, flags) {
      if (typeof emitter.removeListener === "function") {
        emitter.removeListener(name, listener);
      } else if (typeof emitter.removeEventListener === "function") {
        emitter.removeEventListener(name, listener, flags);
      } else {
        throw new ERR_INVALID_ARG_TYPE("emitter", "EventEmitter", emitter);
      }
    }

    function eventTargetAgnosticAddListener(emitter, name, listener, flags) {
      if (typeof emitter.on === "function") {
        if (flags?.once) {
          emitter.once(name, listener);
        } else {
          emitter.on(name, listener);
        }
      } else if (typeof emitter.addEventListener === "function") {
        emitter.addEventListener(name, listener, flags);
      } else {
        throw new ERR_INVALID_ARG_TYPE("emitter", "EventEmitter", emitter);
      }
    }

    function on(emitter, event, options = kEmptyObject) {
      validateObject(options, "options");
      const signal = options.signal;
      validateAbortSignal(signal, "options.signal");
      if (signal?.aborted)
        throw new AbortError(undefined, { cause: signal.reason });
      const highWatermark = options.highWaterMark ?? options.highWatermark ?? Number.MAX_SAFE_INTEGER;
      validateInteger(highWatermark, "options.highWaterMark", 1);
      const lowWatermark = options.lowWaterMark ?? options.lowWatermark ?? 1;
      validateInteger(lowWatermark, "options.lowWaterMark", 1);

      FixedQueue ??= require("internal/fixed_queue");
      const unconsumedEvents = new FixedQueue();
      const unconsumedPromises = new FixedQueue();
      let paused = false;
      let error = null;
      let finished = false;
      let size = 0;

      const iterator = Object.setPrototypeOf({
        next() {
          if (size) {
            const value = unconsumedEvents.shift();
            size--;
            if (paused && size < lowWatermark) {
              emitter.resume(); // Can not be finished yet
              paused = false;
            }
            return Promise.resolve(createIterResult(value, false));
          }

          if (error) {
            const p = Promise.reject(error);
            error = null;
            return p;
          }

          if (finished) return closeHandler();

          return new Promise(function(resolve, reject) {
            unconsumedPromises.push({ resolve, reject });
          });
        },

        return() {
          return closeHandler();
        },

        throw(err) {
          if (!err || !(err instanceof Error)) {
            throw new ERR_INVALID_ARG_TYPE("EventEmitter.AsyncIterator",
                                           "Error", err);
          }
          errorHandler(err);
        },
        [Symbol.asyncIterator]() {
          return this;
        },
        [kWatermarkData]: {
                get size() {
            return size;
          },
                get low() {
            return lowWatermark;
          },
                get high() {
            return highWatermark;
          },
                get isPaused() {
            return paused;
          },
        },
      }, AsyncIteratorPrototype);

      const { addEventListener, removeAll } = listenersController();
      kFirstEventParam ??= require("internal/events/symbols").kFirstEventParam;
      addEventListener(emitter, event, options[kFirstEventParam] ? eventHandler : function(...args) {
        return eventHandler(args);
      });
      if (event !== "error" && typeof emitter.on === "function") {
        addEventListener(emitter, "error", errorHandler);
      }
      const closeEvents = options?.close;
      if (closeEvents?.length) {
        for (let i = 0; i < closeEvents.length; i++) {
          addEventListener(emitter, closeEvents[i], closeHandler);
        }
      }

      const abortListenerDisposable = signal ? addAbortListener(signal, abortListener) : null;

      return iterator;

      function abortListener() {
        errorHandler(new AbortError(undefined, { cause: signal?.reason }));
      }

      function eventHandler(value) {
        if (unconsumedPromises.isEmpty()) {
          size++;
          if (!paused && size > highWatermark) {
            paused = true;
            emitter.pause();
          }
          unconsumedEvents.push(value);
        } else unconsumedPromises.shift().resolve(createIterResult(value, false));
      }

      function errorHandler(err) {
        if (unconsumedPromises.isEmpty()) error = err;
        else unconsumedPromises.shift().reject(err);

        closeHandler();
      }

      function closeHandler() {
        abortListenerDisposable?.[Symbol.dispose]();
        removeAll();
        finished = true;
        paused = false;
        const doneResult = createIterResult(undefined, true);
        while (!unconsumedPromises.isEmpty()) {
          unconsumedPromises.shift().resolve(doneResult);
        }

        return Promise.resolve(doneResult);
      }
    }

    function listenersController() {
      const listeners = [];

      return {
        addEventListener(emitter, event, handler, flags) {
          eventTargetAgnosticAddListener(emitter, event, handler, flags);
          listeners.push([emitter, event, handler, flags]);
        },
        removeAll() {
          while (listeners.length > 0) {
            Reflect.apply(eventTargetAgnosticRemoveListener, undefined, listeners.pop());
          }
        },
      };
    }
  },
  "internal/events/abort_listener": function (module, exports, require, process, primordials, internalBinding) {
    "use strict";

    const {
      validateAbortSignal,
      validateFunction,
    } = require("internal/validators");
    const {
      codes: {
        ERR_INVALID_ARG_TYPE,
      },
    } = require("internal/errors");

    let queueMicrotask;
    let kResistStopPropagation;
    let abortListenerOptions;
    let eventTarget;

    function addAbortListener(signal, listener) {
      if (signal === undefined) {
        throw new ERR_INVALID_ARG_TYPE("signal", "AbortSignal", signal);
      }
      validateAbortSignal(signal, "signal");
      validateFunction(listener, "listener");

      let removeEventListener;
      if (signal.aborted) {
        queueMicrotask ??= require("internal/process/task_queues").queueMicrotask;
        eventTarget ??= require("internal/event_target");
        let disposed = false;
        queueMicrotask(() => {
          if (disposed) return;
          const { Event, kIsBeingDispatched, kTarget, kTrustEvent } = eventTarget;
          const event = new Event("abort", { [kTrustEvent]: true });
          event[kTarget] = signal;
          event[kIsBeingDispatched] = true;
          listener(event);
          event[kIsBeingDispatched] = false;
        });
        removeEventListener = () => {
          disposed = true;
        };
      } else {
        kResistStopPropagation ??= require("internal/event_target").kResistStopPropagation;
        abortListenerOptions ??= Object.freeze({ __proto__: null, once: true, [kResistStopPropagation]: true });
        signal.addEventListener("abort", listener, abortListenerOptions);
        removeEventListener = () => {
          signal.removeEventListener("abort", listener);
        };
      }
      return {
        __proto__: null,
        [Symbol.dispose]() {
          removeEventListener?.();
        },
      };
    }

    module.exports = {
      __proto__: null,
      addAbortListener,
    };
  },
  "internal/events/symbols": function (module, exports, require, process, primordials, internalBinding) {
    "use strict";

    const kFirstEventParam = Symbol("kFirstEventParam");

    module.exports = {
      kFirstEventParam,
    };
  },
  "internal/fixed_queue": function (module, exports, require, process, primordials, internalBinding) {
    "use strict";

    const kSize = 2048;
    const kMask = kSize - 1;

    class FixedCircularBuffer {
      constructor() {
        this.bottom = 0;
        this.top = 0;
        this.list = (new Array(kSize)).fill(undefined);
        this.next = null;
      }

      isEmpty() {
        return this.top === this.bottom;
      }

      isFull() {
        return ((this.top + 1) & kMask) === this.bottom;
      }

      push(data) {
        this.list[this.top] = data;
        this.top = (this.top + 1) & kMask;
      }

      shift() {
        const nextItem = this.list[this.bottom];
        if (nextItem === undefined)
          return null;
        this.list[this.bottom] = undefined;
        this.bottom = (this.bottom + 1) & kMask;
        return nextItem;
      }
    }

    module.exports = class FixedQueue {
      static #pool = [];

      constructor() {
        this.head = this.tail = FixedQueue.#pool.pop() ?? new FixedCircularBuffer();
      }

      isEmpty() {
        return this.head.isEmpty();
      }

      push(data) {
        if (this.head.isFull()) {
          this.head = this.head.next = FixedQueue.#pool.pop() ?? new FixedCircularBuffer();
        }
        this.head.push(data);
      }

      shift() {
        const tail = this.tail;
        const next = tail.shift();
        if (tail.isEmpty() && tail.next !== null) {
          this.tail = tail.next;
          tail.next = null;
          tail.bottom = 0;
          tail.top = 0;

          if (FixedQueue.#pool.length < 64) {
            FixedQueue.#pool.push(tail); // Recycle old tail
          }
        }
        return next;
      }
    };
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

module.exports = load("events");
