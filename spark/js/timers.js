"use strict";

const { now, getContext, setContext } = native;
const { validateFunction } = require("spark:errors");

const TIMEOUT_MAX = 2 ** 31 - 1;

const active = new Map();
const heap = [];
const immediates = [];
let nextId = 1;
let nextSeq = 1;
let refCount = 0;

function before(a, b) {
  return a.deadline < b.deadline ||
    (a.deadline === b.deadline && a.seq < b.seq);
}

function heapPush(entry) {
  let i = heap.length;
  heap.push(entry);
  while (i > 0) {
    const parent = (i - 1) >> 1;
    if (!before(entry, heap[parent])) break;
    heap[i] = heap[parent];
    i = parent;
  }
  heap[i] = entry;
}

function heapPop() {
  const top = heap[0];
  const last = heap.pop();
  const size = heap.length;
  if (size === 0) return top;
  let i = 0;
  for (;;) {
    let child = 2 * i + 1;
    if (child >= size) break;
    if (child + 1 < size && before(heap[child + 1], heap[child])) child++;
    if (!before(heap[child], last)) break;
    heap[i] = heap[child];
    i = child;
  }
  heap[i] = last;
  return top;
}

class Timeout {
  #id = nextId++;
  #callback;
  #args;
  #delay;
  #repeat;
  #seq = 0;
  #refed = true;
  #active = false;
  #destroyed = false;
  #frame = getContext();

  constructor(callback, delay, args, repeat) {
    delay *= 1;
    if (!(delay >= 1 && delay <= TIMEOUT_MAX)) delay = 1;
    this.#callback = callback;
    this.#args = args;
    this.#delay = delay;
    this.#repeat = repeat;
    Timeout.#schedule(this);
  }

  static #schedule(timer) {
    timer.#seq = nextSeq++;
    if (!timer.#active) {
      timer.#active = true;
      active.set(timer.#id, timer);
      if (timer.#refed) refCount++;
    }
    heapPush({ deadline: now() + timer.#delay, seq: timer.#seq, timer });
  }

  static #cancel(timer) {
    if (!timer.#active) return;
    timer.#active = false;
    active.delete(timer.#id);
    if (timer.#refed) refCount--;
  }

  static isTimeout(value) {
    return typeof value === "object" && value !== null && #active in value;
  }

  static destroy(timer) {
    timer.#destroyed = true;
    Timeout.#cancel(timer);
  }

  // the earliest live entry, with stale ones dropped
  static peek() {
    while (heap.length > 0) {
      const entry = heap[0];
      if (entry.timer.#active && entry.timer.#seq === entry.seq) return entry;
      heapPop();
    }
    return undefined;
  }

  static fire(timer) {
    heapPop();
    if (timer.#repeat) {
      Timeout.#schedule(timer);
    } else {
      Timeout.#cancel(timer);
    }
    const prior = setContext(timer.#frame);
    try {
      Reflect.apply(timer.#callback, timer, timer.#args);
    } finally {
      setContext(prior);
    }
  }

  ref() {
    if (!this.#refed) {
      this.#refed = true;
      if (this.#active) refCount++;
    }
    return this;
  }

  unref() {
    if (this.#refed) {
      this.#refed = false;
      if (this.#active) refCount--;
    }
    return this;
  }

  hasRef() {
    return this.#refed;
  }

  refresh() {
    if (!this.#destroyed) Timeout.#schedule(this);
    return this;
  }

  close() {
    Timeout.destroy(this);
    return this;
  }

  [Symbol.toPrimitive]() {
    return this.#id;
  }

  [Symbol.dispose]() {
    Timeout.destroy(this);
  }
}

class Immediate {
  #callback;
  #args;
  #refed = true;
  #queued = true;
  #frame = getContext();

  constructor(callback, args) {
    this.#callback = callback;
    this.#args = args;
    refCount++;
    immediates.push(this);
  }

  static isImmediate(value) {
    return typeof value === "object" && value !== null && #queued in value;
  }

  static destroy(immediate) {
    if (!immediate.#queued) return;
    immediate.#queued = false;
    if (immediate.#refed) refCount--;
  }

  // false when it was cleared while queued
  static fire(immediate) {
    if (!immediate.#queued) return false;
    Immediate.destroy(immediate);
    const prior = setContext(immediate.#frame);
    try {
      Reflect.apply(immediate.#callback, immediate, immediate.#args);
    } finally {
      setContext(prior);
    }
    return true;
  }

  ref() {
    if (!this.#refed) {
      this.#refed = true;
      if (this.#queued) refCount++;
    }
    return this;
  }

  unref() {
    if (this.#refed) {
      this.#refed = false;
      if (this.#queued) refCount--;
    }
    return this;
  }

  hasRef() {
    return this.#refed;
  }

  [Symbol.dispose]() {
    Immediate.destroy(this);
  }
}

function setTimeout(callback, delay, ...args) {
  validateFunction(callback, "callback");
  return new Timeout(callback, delay, args, false);
}

function setInterval(callback, delay, ...args) {
  validateFunction(callback, "callback");
  return new Timeout(callback, delay, args, true);
}

function clearTimeout(timer) {
  if (Timeout.isTimeout(timer)) {
    Timeout.destroy(timer);
  } else if (typeof timer === "number" || typeof timer === "string") {
    const found = active.get(Number(timer));
    if (found !== undefined) Timeout.destroy(found);
  }
}

function setImmediate(callback, ...args) {
  validateFunction(callback, "callback");
  return new Immediate(callback, args);
}

function clearImmediate(immediate) {
  if (Immediate.isImmediate(immediate)) Immediate.destroy(immediate);
}

require("spark:loop").addSource({
  step() {
    const entry = Timeout.peek();
    if (entry !== undefined && entry.deadline <= now()) {
      Timeout.fire(entry.timer);
      return true;
    }
    while (immediates.length > 0) {
      if (Immediate.fire(immediates.shift())) return true;
    }
    return false;
  },
  wait() {
    if (immediates.length > 0) return 0;
    const entry = Timeout.peek();
    if (entry === undefined) return -1;
    return Math.max(entry.deadline - now(), 0);
  },
  alive() {
    return refCount > 0;
  },
});

module.exports = {
  setTimeout,
  clearTimeout,
  setInterval,
  clearInterval: clearTimeout,
  setImmediate,
  clearImmediate,
};
