"use strict";

const {
  error,
  typeError,
  rangeError,
  invalidArgType,
  validateObject,
} = require("spark:errors");
const loop = require("spark:loop");
const { DOMException } = require("spark:domexception");
const { setTimeout, clearTimeout } = require("node:timers");

const startedAt = native.now();

const kType = Symbol("type");
const kTarget = Symbol("target");
const kIsBeingDispatched = Symbol("isBeingDispatched");
const kInPassiveListener = Symbol("inPassiveListener");
const kStop = Symbol("stop");
const kDetail = Symbol("detail");
const kTrustEvent = Symbol("trustEvent");
const kEvents = Symbol("events");
const kHandlers = Symbol("handlers");
const kIsEventTarget = Symbol("isEventTarget");
const kWeakHandler = Symbol("weakHandler");
const kResistStopPropagation = Symbol("resistStopPropagation");
const kNewListener = Symbol("newListener");
const kRemoveListener = Symbol("removeListener");
const kRemoveWeakListenerHelper = Symbol("removeWeakListenerHelper");

function missingArgs(...names) {
  const quoted = names.map((name) => `"${name}"`);
  const list =
    quoted.length === 1
      ? `${quoted[0]} argument`
      : `${quoted.slice(0, -1).join(", ")} and ${quoted[quoted.length - 1]} arguments`;
  return typeError("ERR_MISSING_ARGS", `The ${list} must be specified`);
}

function invalidThis(type) {
  return typeError("ERR_INVALID_THIS", `Value of "this" must be of type ${type}`);
}

// an error out of a listener never reaches the dispatcher; it is thrown later
function emitUncaughtException(thrown) {
  loop.nextTick(() => {
    throw thrown;
  });
}

function addCatch(promise) {
  const then = promise.then;
  if (typeof then === "function") {
    then.call(promise, undefined, emitUncaughtException);
  }
}

const isTrustedSet = new WeakSet();
const isTrusted = Object.getOwnPropertyDescriptor(
  {
    get isTrusted() {
      return isTrustedSet.has(this);
    },
  },
  "isTrusted",
).get;

function isEvent(value) {
  return typeof value?.[kType] === "string";
}

class Event {
  #cancelable = false;
  #bubbles = false;
  #composed = false;
  #defaultPrevented = false;
  #timestamp = native.now() - startedAt;
  #propagationStopped = false;

  constructor(type, options = undefined) {
    if (arguments.length === 0) throw missingArgs("type");
    if (options != null) validateObject(options, "options");
    this.#bubbles = !!options?.bubbles;
    this.#cancelable = !!options?.cancelable;
    this.#composed = !!options?.composed;
    this[kType] = `${type}`;
    if (options?.[kTrustEvent]) isTrustedSet.add(this);
    this[kTarget] = null;
    this[kIsBeingDispatched] = false;
    this[kInPassiveListener] = false;
  }

  initEvent(type, bubbles = false, cancelable = false) {
    if (arguments.length === 0) throw missingArgs("type");
    if (this[kIsBeingDispatched]) return;
    this[kType] = `${type}`;
    this.#bubbles = !!bubbles;
    this.#cancelable = !!cancelable;
  }

  stopImmediatePropagation() {
    if (!isEvent(this)) throw invalidThis("Event");
    this.stopPropagation();
    this[kStop] = true;
  }

  preventDefault() {
    if (!isEvent(this)) throw invalidThis("Event");
    if (!this.#cancelable || this[kInPassiveListener]) return;
    this.#defaultPrevented = true;
  }

  get target() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this[kTarget];
  }

  get currentTarget() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this[kIsBeingDispatched] ? this[kTarget] : null;
  }

  get srcElement() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this[kTarget];
  }

  get type() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this[kType];
  }

  get cancelable() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this.#cancelable;
  }

  get defaultPrevented() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this.#cancelable && this.#defaultPrevented;
  }

  get timeStamp() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this.#timestamp;
  }

  composedPath() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this[kIsBeingDispatched] ? [this[kTarget]] : [];
  }

  get returnValue() {
    if (!isEvent(this)) throw invalidThis("Event");
    return !this.#cancelable || !this.#defaultPrevented;
  }

  set returnValue(value) {
    if (!isEvent(this)) throw invalidThis("Event");
    if (!value) {
      if (!this.#cancelable || this[kInPassiveListener]) return;
      this.#defaultPrevented = true;
    }
  }

  get bubbles() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this.#bubbles;
  }

  get composed() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this.#composed;
  }

  get eventPhase() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this[kIsBeingDispatched] ? Event.AT_TARGET : Event.NONE;
  }

  get cancelBubble() {
    if (!isEvent(this)) throw invalidThis("Event");
    return this.#propagationStopped;
  }

  set cancelBubble(value) {
    if (!isEvent(this)) throw invalidThis("Event");
    if (value) this.#propagationStopped = true;
  }

  stopPropagation() {
    if (!isEvent(this)) throw invalidThis("Event");
    this.#propagationStopped = true;
  }
}

const enumerable = { enumerable: true };

Object.defineProperties(Event.prototype, {
  [Symbol.toStringTag]: {
    writable: false,
    enumerable: false,
    configurable: true,
    value: "Event",
  },
  initEvent: enumerable,
  stopImmediatePropagation: enumerable,
  preventDefault: enumerable,
  target: enumerable,
  currentTarget: enumerable,
  srcElement: enumerable,
  type: enumerable,
  cancelable: enumerable,
  defaultPrevented: enumerable,
  timeStamp: enumerable,
  composedPath: enumerable,
  returnValue: enumerable,
  bubbles: enumerable,
  composed: enumerable,
  eventPhase: enumerable,
  cancelBubble: enumerable,
  stopPropagation: enumerable,
  isTrusted: { configurable: false, enumerable: true, get: isTrusted },
});

["NONE", "CAPTURING_PHASE", "AT_TARGET", "BUBBLING_PHASE"].forEach(
  (name, index) => {
    Object.defineProperty(Event, name, {
      writable: false,
      configurable: false,
      enumerable: true,
      value: index,
    });
  },
);

function isCustomEvent(value) {
  return isEvent(value) && value?.[kDetail] !== undefined;
}

class CustomEvent extends Event {
  constructor(type, options = {}) {
    if (arguments.length === 0) throw missingArgs("type");
    super(type, options);
    this[kDetail] = options?.detail ?? null;
  }

  get detail() {
    if (!isCustomEvent(this)) throw invalidThis("CustomEvent");
    return this[kDetail];
  }
}

Object.defineProperties(CustomEvent.prototype, {
  [Symbol.toStringTag]: {
    writable: false,
    enumerable: false,
    configurable: true,
    value: "CustomEvent",
  },
  detail: enumerable,
});

// listeners held only weakly: kept alive by the object that asked for them
let weakListenersState;
let objectToWeakListenerMap;
function weakListeners() {
  weakListenersState ??= new FinalizationRegistry(
    ({ eventTarget, listener, eventType }) =>
      eventTarget.deref()?.[kRemoveWeakListenerHelper](eventType, listener),
  );
  objectToWeakListenerMap ??= new WeakMap();
  return { registry: weakListenersState, map: objectToWeakListenerMap };
}

class Listener {
  constructor(
    eventTarget,
    eventType,
    previous,
    listener,
    once,
    capture,
    passive,
    weak,
    resistStopPropagation,
  ) {
    this.next = undefined;
    if (previous !== undefined) previous.next = this;
    this.previous = previous;
    this.listener = listener;
    this.once = Boolean(once);
    this.capture = Boolean(capture);
    this.passive = Boolean(passive);
    this.weak = Boolean(weak);
    this.resistStopPropagation = Boolean(resistStopPropagation);
    this.removed = false;

    if (this.weak) {
      this.callback = new WeakRef(listener);
      weakListeners().registry.register(
        listener,
        { eventTarget: new WeakRef(eventTarget), listener: this, eventType },
        this,
      );
      this.weakKeyRef = new WeakRef(weak);
      let retained = weakListeners().map.get(weak);
      if (retained === undefined) {
        retained = new Set();
        weakListeners().map.set(weak, retained);
      }
      retained.add(listener);
      this.listener = this.callback;
    } else if (typeof listener === "function") {
      this.callback = listener;
    } else {
      this.callback = async (...args) => {
        if (listener.handleEvent) {
          await Reflect.apply(listener.handleEvent, listener, args);
        }
      };
    }
  }

  same(listener, capture) {
    const mine = this.weak ? this.listener.deref() : this.listener;
    return mine === listener && this.capture === capture;
  }

  remove() {
    if (this.previous !== undefined) this.previous.next = this.next;
    if (this.next !== undefined) this.next.previous = this.previous;
    this.removed = true;
    if (this.weak) {
      weakListeners().registry.unregister(this);
      const weakKey = this.weakKeyRef?.deref();
      const listener = this.callback?.deref();
      if (weakKey !== undefined && listener !== undefined) {
        const retained = weakListeners().map.get(weakKey);
        if (retained !== undefined) {
          retained.delete(listener);
          if (retained.size === 0) weakListeners().map.delete(weakKey);
        }
      }
    }
  }
}

function validateEventListener(listener) {
  if (
    typeof listener === "function" ||
    typeof listener?.handleEvent === "function"
  ) {
    return true;
  }
  if (listener == null) return false;
  if (typeof listener === "object") return true;
  throw invalidArgType("listener", "EventListener", listener);
}

function validateEventListenerOptions(options) {
  if (typeof options === "boolean") return { capture: options };
  if (options === null) return {};
  if (typeof options !== "object" && typeof options !== "function") {
    throw invalidArgType("options", "Object", options);
  }
  return {
    once: Boolean(options.once),
    capture: Boolean(options.capture),
    passive: Boolean(options.passive),
    signal: options.signal,
    weak: options[kWeakHandler],
    resistStopPropagation: options[kResistStopPropagation] ?? false,
  };
}

function validateAbortSignal(signal, name) {
  if (
    signal !== undefined &&
    (signal === null || typeof signal !== "object" || !("aborted" in signal))
  ) {
    throw invalidArgType(name, "AbortSignal", signal);
  }
}

function isEventTarget(value) {
  return value?.constructor?.[kIsEventTarget];
}

const noOptions = {};

class EventTarget {
  static [kIsEventTarget] = true;

  constructor() {
    this[kEvents] = undefined;
    this[kHandlers] = undefined;
  }

  [kNewListener](size, type, listener, once, capture, passive, weak) {}

  [kRemoveListener](size, type, listener, capture) {}

  addEventListener(type, listener, options = noOptions) {
    if (!isEventTarget(this)) throw invalidThis("EventTarget");
    if (arguments.length < 2) throw missingArgs("type", "listener");

    let once = false;
    let capture = false;
    let passive = false;
    let weak = false;
    let resistStopPropagation = false;

    if (options !== noOptions) {
      options = validateEventListenerOptions(options);
      once = options.once;
      capture = options.capture;
      passive = options.passive;
      weak = options.weak;
      resistStopPropagation = options.resistStopPropagation;

      const signal = options.signal;
      validateAbortSignal(signal, "options.signal");
      if (signal) {
        if (signal.aborted) return;
        // not prevented by stopImmediatePropagation, and gone when this is
        signal.addEventListener(
          "abort",
          () => {
            this.removeEventListener(type, listener, options);
          },
          { once: true, [kWeakHandler]: this, [kResistStopPropagation]: true },
        );
      }
    }

    if (!validateEventListener(listener)) return;
    type = `${type}`;

    this[kEvents] ??= new Map();
    let root = this[kEvents].get(type);

    if (root === undefined) {
      root = {
        size: 1,
        next: undefined,
        resistStopPropagation: Boolean(resistStopPropagation),
      };
      new Listener(
        this,
        type,
        root,
        listener,
        once,
        capture,
        passive,
        weak,
        resistStopPropagation,
      );
      this[kNewListener](root.size, type, listener, once, capture, passive, weak);
      this[kEvents].set(type, root);
      return;
    }

    let handler = root.next;
    let previous = root;
    while (handler !== undefined && !handler.same(listener, capture)) {
      previous = handler;
      handler = handler.next;
    }
    if (handler !== undefined) return;

    new Listener(
      this,
      type,
      previous,
      listener,
      once,
      capture,
      passive,
      weak,
      resistStopPropagation,
    );
    root.size++;
    root.resistStopPropagation ||= Boolean(resistStopPropagation);
    this[kNewListener](root.size, type, listener, once, capture, passive, weak);
  }

  removeEventListener(type, listener, options = noOptions) {
    if (!isEventTarget(this)) throw invalidThis("EventTarget");
    if (arguments.length < 2) throw missingArgs("type", "listener");
    if (!validateEventListener(listener)) return;

    type = `${type}`;
    const capture =
      typeof options === "boolean" ? options : options?.capture === true;

    if (this[kEvents] === undefined) return;
    const root = this[kEvents].get(type);
    if (root === undefined || root.next === undefined) return;

    let handler = root.next;
    while (handler !== undefined) {
      if (handler.same(listener, capture)) {
        handler.remove();
        root.size--;
        if (root.size === 0) this[kEvents].delete(type);
        this[kRemoveListener](root.size, type, listener, capture);
        break;
      }
      handler = handler.next;
    }
  }

  [kRemoveWeakListenerHelper](type, listener) {
    if (this[kEvents] === undefined) return;
    const root = this[kEvents].get(type);
    if (root === undefined || root.next === undefined) return;

    const capture = listener.capture === true;
    let handler = root.next;
    while (handler !== undefined) {
      if (handler === listener) {
        handler.remove();
        root.size--;
        if (root.size === 0) this[kEvents].delete(type);
        this[kRemoveListener](root.size, type, undefined, capture);
        break;
      }
      handler = handler.next;
    }
  }

  dispatchEvent(event) {
    if (!isEventTarget(this)) throw invalidThis("EventTarget");
    if (arguments.length < 1) throw missingArgs("event");
    if (!(event instanceof Event)) {
      throw invalidArgType("event", "Event", event);
    }
    if (event[kIsBeingDispatched]) {
      throw error(
        "ERR_EVENT_RECURSION",
        `The event "${event.type}" is already being dispatched`,
      );
    }

    const type = event.type;
    event[kTarget] = this;
    event[kIsBeingDispatched] = true;

    const root = this[kEvents]?.get(type);
    if (root === undefined || root.next === undefined) {
      event[kIsBeingDispatched] = false;
      return event.defaultPrevented !== true;
    }

    let handler = root.next;
    let next;
    while (
      handler !== undefined &&
      (root.resistStopPropagation || handler.passive || event[kStop] !== true)
    ) {
      next = handler.next;

      if (
        handler.removed ||
        (event[kStop] === true && !handler.resistStopPropagation)
      ) {
        handler = next;
        continue;
      }
      if (handler.once) {
        handler.remove();
        root.size--;
        const { listener, capture } = handler;
        this[kRemoveListener](root.size, type, listener, capture);
      }

      try {
        const callback = handler.weak
          ? handler.callback.deref()
          : handler.callback;
        let result;
        if (callback) {
          if (handler.passive) event[kInPassiveListener] = true;
          result = callback.call(this, event);
          event[kIsBeingDispatched] = false;
        }
        if (result !== undefined && result !== null) addCatch(result);
      } catch (thrown) {
        emitUncaughtException(thrown);
      } finally {
        if (event[kInPassiveListener]) event[kInPassiveListener] = false;
      }

      handler = next;
    }

    event[kIsBeingDispatched] = false;
    return event.defaultPrevented !== true;
  }
}

Object.defineProperties(EventTarget.prototype, {
  addEventListener: enumerable,
  removeEventListener: enumerable,
  dispatchEvent: enumerable,
  [Symbol.toStringTag]: {
    writable: false,
    enumerable: false,
    configurable: true,
    value: "EventTarget",
  },
});

function makeEventHandler(handler) {
  function eventHandler(...args) {
    if (typeof eventHandler.handler !== "function") return;
    return Reflect.apply(eventHandler.handler, this, args);
  }
  eventHandler.handler = handler;
  return eventHandler;
}

// the `on<name>` attribute, one handler slot kept among the listeners
function defineEventHandler(emitter, name) {
  const propName = `on${name}`;
  function get() {
    if (typeof this !== "object" || this === null || !(kHandlers in this)) {
      throw invalidThis("EventTarget");
    }
    return this[kHandlers]?.get(name)?.handler ?? null;
  }
  Object.defineProperty(get, "name", { value: `get ${propName}` });

  function set(value) {
    if (typeof this !== "object" || this === null || !(kHandlers in this)) {
      throw invalidThis("EventTarget");
    }
    // anything that is not callable or an object switches the handler off
    if (
      typeof value !== "function" &&
      (typeof value !== "object" || value === null)
    ) {
      value = null;
    }
    this[kHandlers] ??= new Map();
    let wrapped = this[kHandlers].get(name);
    if (wrapped) {
      this[kEvents] ??= new Map();
      if (typeof wrapped.handler === "function") {
        const root = this[kEvents].get(name);
        if (root !== undefined) {
          root.size--;
          this[kRemoveListener](root.size, name, wrapped.handler, false);
        }
      }
      wrapped.handler = value;
      if (typeof wrapped.handler === "function") {
        const root = this[kEvents].get(name);
        if (root !== undefined) {
          root.size++;
          this[kNewListener](root.size, name, value, false, false, false, false);
        } else {
          this.addEventListener(name, wrapped);
        }
      }
    } else {
      wrapped = makeEventHandler(value);
      this.addEventListener(name, wrapped);
    }
    this[kHandlers].set(name, wrapped);
  }
  Object.defineProperty(set, "name", { value: `set ${propName}` });

  Object.defineProperty(emitter, propName, {
    get,
    set,
    configurable: true,
    enumerable: true,
  });
}

const kDontThrowSymbol = Symbol("dontThrow");
const kAborted = Symbol("aborted");
const kReason = Symbol("reason");
const kTimeout = Symbol("timeout");
const kComposite = Symbol("composite");
const kFollowing = Symbol("following");
const kResultSignalWeakRef = Symbol("resultSignalWeakRef");
const kSourceSignals = Symbol("sourceSignals");
const kDependantSignals = Symbol("dependantSignals");

const clearTimeoutRegistry = new FinalizationRegistry(clearTimeout);
const dependantSignalsCleanupRegistry = new FinalizationRegistry(
  ({ sourceSignalRef, dependantSignalRef, sourceSignalsCleanupToken }) => {
    sourceSignalsCleanupRegistry.unregister(sourceSignalsCleanupToken);
    const sourceSignal = sourceSignalRef.deref();
    if (sourceSignal === undefined) return;
    sourceSignal[kDependantSignals].delete(dependantSignalRef);
  },
);
// signals something still listens to, held here so they outlive their makers
const gcPersistentSignals = new Set();
const sourceSignalsCleanupRegistry = new FinalizationRegistry(
  ({ sourceSignalRef, composedSignalRef }) => {
    const composedSignal = composedSignalRef.deref();
    if (composedSignal !== undefined) {
      composedSignal[kSourceSignals].delete(sourceSignalRef);
      if (composedSignal[kSourceSignals].size === 0) {
        gcPersistentSignals.delete(composedSignal);
      }
    }
  },
);

function validateThisAbortSignal(value) {
  if (value?.[kAborted] === undefined) throw invalidThis("AbortSignal");
}

function refreshCompositeSignal(signal) {
  if (!signal[kComposite] || signal[kAborted] || !signal[kSourceSignals]?.size) {
    return;
  }
  for (const sourceSignalWeakRef of signal[kSourceSignals]) {
    const sourceSignal = sourceSignalWeakRef.deref();
    if (sourceSignal === undefined) {
      signal[kSourceSignals].delete(sourceSignalWeakRef);
      continue;
    }
    if (sourceSignal[kAborted]) {
      abortSignal(signal, sourceSignal[kReason]);
      return;
    }
  }
}

function followCompositeSignal(signal) {
  if (signal[kFollowing] || signal[kAborted] || !signal[kSourceSignals]?.size) {
    return;
  }
  const resultSignalWeakRef = (signal[kResultSignalWeakRef] ??= new WeakRef(
    signal,
  ));
  for (const sourceSignalWeakRef of signal[kSourceSignals]) {
    const sourceSignal = sourceSignalWeakRef.deref();
    if (sourceSignal === undefined) {
      signal[kSourceSignals].delete(sourceSignalWeakRef);
      continue;
    }
    if (sourceSignal[kAborted]) {
      abortSignal(signal, sourceSignal[kReason]);
      return;
    }
    sourceSignal[kDependantSignals] ??= new Set();
    sourceSignal[kDependantSignals].add(resultSignalWeakRef);
    dependantSignalsCleanupRegistry.register(signal, {
      sourceSignalRef: sourceSignalWeakRef,
      dependantSignalRef: resultSignalWeakRef,
      sourceSignalsCleanupToken: sourceSignalWeakRef,
    });
    sourceSignalsCleanupRegistry.register(
      sourceSignal,
      { sourceSignalRef: sourceSignalWeakRef, composedSignalRef: resultSignalWeakRef },
      sourceSignalWeakRef,
    );
  }
  signal[kFollowing] = true;
}

function setWeakAbortSignalTimeout(weakRef, delay) {
  const timeout = setTimeout(() => {
    const signal = weakRef.deref();
    if (signal !== undefined) {
      clearTimeoutRegistry.unregister(signal);
      gcPersistentSignals.delete(signal);
      abortSignal(
        signal,
        new DOMException(
          "The operation was aborted due to timeout",
          "TimeoutError",
        ),
      );
    }
  }, delay);
  timeout.unref();
  return timeout;
}

function validateUint32(value, name) {
  if (typeof value !== "number") throw invalidArgType(name, "number", value);
  if (!Number.isInteger(value)) {
    throw rangeError(
      "ERR_OUT_OF_RANGE",
      `The value of "${name}" is out of range. It must be an integer. Received ${String(value)}`,
    );
  }
  if (value < 0 || value > 4294967295) {
    throw rangeError(
      "ERR_OUT_OF_RANGE",
      `The value of "${name}" is out of range. It must be >= 0 && <= 4294967295. Received ${String(value)}`,
    );
  }
}

let isSignal;

class AbortSignal extends EventTarget {
  #brand;

  static {
    isSignal = (value) =>
      typeof value === "object" && value !== null && #brand in value;
  }

  constructor(dontThrowSymbol = undefined, init = {}) {
    if (dontThrowSymbol !== kDontThrowSymbol) {
      throw typeError("ERR_ILLEGAL_CONSTRUCTOR", "Illegal constructor");
    }
    super();
    const { aborted = false, reason = undefined, composite = false } = init;
    this[kAborted] = aborted;
    this[kReason] = reason;
    this[kComposite] = composite;
  }

  get aborted() {
    validateThisAbortSignal(this);
    refreshCompositeSignal(this);
    return !!this[kAborted];
  }

  get reason() {
    validateThisAbortSignal(this);
    refreshCompositeSignal(this);
    return this[kReason];
  }

  throwIfAborted() {
    validateThisAbortSignal(this);
    refreshCompositeSignal(this);
    if (this[kAborted]) throw this[kReason];
  }

  static abort(
    reason = new DOMException("This operation was aborted", "AbortError"),
  ) {
    return new AbortSignal(kDontThrowSymbol, { aborted: true, reason });
  }

  static timeout(delay) {
    validateUint32(delay, "delay");
    const signal = new AbortSignal(kDontThrowSymbol);
    signal[kTimeout] = true;
    clearTimeoutRegistry.register(
      signal,
      setWeakAbortSignalTimeout(new WeakRef(signal), delay),
      signal,
    );
    return signal;
  }

  static any(signals) {
    const signalsArray = signalSequence(signals);
    const resultSignal = new AbortSignal(kDontThrowSymbol, { composite: true });
    if (!signalsArray.length) return resultSignal;

    resultSignal[kSourceSignals] = new Set();
    let hasTimeoutSignals = false;
    for (let i = 0; i < signalsArray.length; i++) {
      const signal = signalsArray[i];
      if (signal[kTimeout]) {
        hasTimeoutSignals = true;
        gcPersistentSignals.add(signal);
      }
      refreshCompositeSignal(signal);
      if (signal[kAborted]) {
        abortSignal(resultSignal, signal[kReason]);
        return resultSignal;
      }
      if (!signal[kComposite]) {
        resultSignal[kSourceSignals].add(new WeakRef(signal));
      } else if (!signal[kSourceSignals]) {
        continue;
      } else {
        for (const sourceSignalWeakRef of signal[kSourceSignals]) {
          const sourceSignal = sourceSignalWeakRef.deref();
          if (!sourceSignal) continue;
          if (sourceSignal[kAborted]) {
            abortSignal(resultSignal, sourceSignal[kReason]);
            return resultSignal;
          }
          if (resultSignal[kSourceSignals].has(sourceSignalWeakRef)) continue;
          resultSignal[kSourceSignals].add(sourceSignalWeakRef);
        }
      }
    }
    if (hasTimeoutSignals && resultSignal[kSourceSignals].size > 0) {
      resultSignal[kTimeout] = true;
    }
    // keeps the abort state even if the sources are collected before anyone
    // looks; following holds them weakly, so unused signals can still go
    followCompositeSignal(resultSignal);
    return resultSignal;
  }

  [kNewListener](size, type, listener, once, capture, passive, weak) {
    if (this[kComposite] && type === "abort" && !this.aborted && size === 1) {
      followCompositeSignal(this);
    }
    const keeps =
      this[kTimeout] || (this[kComposite] && this[kSourceSignals]?.size);
    if (keeps && type === "abort" && !this.aborted && !weak && size === 1) {
      gcPersistentSignals.add(this);
    }
  }

  [kRemoveListener](size, type, listener, capture) {
    const keeps =
      this[kTimeout] || (this[kComposite] && this[kSourceSignals]?.size);
    if (keeps && type === "abort" && size === 0) {
      gcPersistentSignals.delete(this);
    }
  }
}

// webidl sequence<AbortSignal>
function signalSequence(signals) {
  const notSequence = () =>
    typeError(
      "ERR_INVALID_ARG_TYPE",
      "signals cannot be converted to sequence.",
    );
  if (
    signals === null ||
    (typeof signals !== "object" && typeof signals !== "function")
  ) {
    throw notSequence();
  }
  const method = signals[Symbol.iterator];
  if (typeof method !== "function") throw notSequence();
  const iterator = method.call(signals);
  const nextMethod = iterator?.next;
  if (typeof nextMethod !== "function") throw notSequence();

  const list = [];
  while (true) {
    const next = nextMethod.call(iterator);
    if (next === null || (typeof next !== "object" && typeof next !== "function")) {
      throw notSequence();
    }
    if (next.done) break;
    if (!isSignal(next.value)) {
      throw typeError(
        "ERR_INVALID_ARG_TYPE",
        `signals[${list.length}] is not of type AbortSignal.`,
      );
    }
    list.push(next.value);
  }
  return list;
}

Object.defineProperties(AbortSignal.prototype, {
  aborted: enumerable,
});
Object.defineProperty(AbortSignal.prototype, Symbol.toStringTag, {
  writable: false,
  enumerable: false,
  configurable: true,
  value: "AbortSignal",
});
defineEventHandler(AbortSignal.prototype, "abort");

function abortSignal(signal, reason) {
  if (signal[kAborted]) return;
  signal[kAborted] = true;
  signal[kReason] = reason;

  const dependentSignalsToAbort = [];
  signal[kDependantSignals]?.forEach((weakRef) => {
    const dependentSignal = weakRef.deref();
    if (dependentSignal && !dependentSignal[kAborted]) {
      dependentSignal[kReason] = reason;
      dependentSignal[kAborted] = true;
      dependentSignalsToAbort.push(dependentSignal);
    }
  });

  runAbort(signal);
  for (let i = 0; i < dependentSignalsToAbort.length; i++) {
    const dependentSignal = dependentSignalsToAbort[i];
    runAbort(dependentSignal);
    gcPersistentSignals.delete(dependentSignal);
  }

  gcPersistentSignals.delete(signal);
  if (signal[kComposite] && signal[kSourceSignals]) {
    signal[kSourceSignals].forEach((sourceWeakRef) => {
      const sourceSignal = sourceWeakRef.deref();
      if (sourceSignal) gcPersistentSignals.delete(sourceSignal);
    });
  }
}

function runAbort(signal) {
  signal.dispatchEvent(new Event("abort", { [kTrustEvent]: true }));
}

class AbortController {
  #signal;

  get signal() {
    this.#signal ??= new AbortSignal(kDontThrowSymbol);
    return this.#signal;
  }

  abort(reason = new DOMException("This operation was aborted", "AbortError")) {
    abortSignal((this.#signal ??= new AbortSignal(kDontThrowSymbol)), reason);
  }
}

Object.defineProperties(AbortController.prototype, {
  signal: enumerable,
  abort: enumerable,
});
Object.defineProperty(AbortController.prototype, Symbol.toStringTag, {
  writable: false,
  enumerable: false,
  configurable: true,
  value: "AbortController",
});

module.exports = {
  Event,
  CustomEvent,
  EventTarget,
  AbortController,
  AbortSignal,
};

// for the events module; not an export
Object.defineProperty(module.exports, "internals", {
  value: {
    isEventTarget,
    kEvents,
    kIsBeingDispatched,
    kResistStopPropagation,
    kTarget,
    kTrustEvent,
  },
});
