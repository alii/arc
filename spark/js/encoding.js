"use strict";

const {
  typeError,
  rangeError,
  invalidArgType,
  validateString,
} = require("spark:errors");

const utf8Labels = new Set([
  "unicode-1-1-utf-8",
  "unicode11utf8",
  "unicode20utf8",
  "utf8",
  "utf-8",
  "x-unicode20utf8",
]);

function trimAsciiWhitespace(label) {
  let s = 0;
  let e = label.length;
  while (
    s < e &&
    (label[s] === "\u0009" ||
      label[s] === "\u000a" ||
      label[s] === "\u000c" ||
      label[s] === "\u000d" ||
      label[s] === " ")
  ) {
    s++;
  }
  while (
    e > s &&
    (label[e - 1] === "\u0009" ||
      label[e - 1] === "\u000a" ||
      label[e - 1] === "\u000c" ||
      label[e - 1] === "\u000d" ||
      label[e - 1] === " ")
  ) {
    e--;
  }
  return label.slice(s, e);
}

function invalidThis(type) {
  return typeError("ERR_INVALID_THIS", `Value of "this" must be of type ${type}`);
}

class TextEncoder {
  #brand = true;

  static #check(encoder) {
    if (encoder == null || !(#brand in encoder)) throw invalidThis("TextEncoder");
  }

  get encoding() {
    TextEncoder.#check(this);
    return "utf-8";
  }

  encode(input = "") {
    TextEncoder.#check(this);
    return new Uint8Array(native.encode(`${input}`));
  }

  // read counts string positions as this engine indexes them
  encodeInto(src, dest) {
    TextEncoder.#check(this);
    validateString(src, "src");
    if (!dest || !(dest instanceof Uint8Array)) {
      throw invalidArgType("dest", "Uint8Array", dest);
    }
    const whole = new Uint8Array(native.encode(src));
    if (whole.length <= dest.length) {
      dest.set(whole);
      return { read: src.length, written: whole.length };
    }
    let read = 0;
    let written = 0;
    for (const character of src) {
      const size = native.encode(character).byteLength;
      if (written + size > dest.length) break;
      written += size;
      read += character.length;
    }
    dest.set(whole.subarray(0, written));
    return { read, written };
  }
}

const empty = new Uint8Array(0);

class TextDecoder {
  #fatal;
  #ignoreBOM;
  #bomSeen = false;
  #pending = empty;

  constructor(encoding = "utf-8", options = {}) {
    encoding = `${encoding}`;
    if (options !== null && typeof options !== "object" && typeof options !== "function") {
      throw invalidArgType("options", "Object", options);
    }
    if (
      !utf8Labels.has(encoding) &&
      !utf8Labels.has(trimAsciiWhitespace(encoding.toLowerCase()))
    ) {
      throw rangeError(
        "ERR_ENCODING_NOT_SUPPORTED",
        `The "${encoding}" encoding is not supported`,
      );
    }
    this.#fatal = Boolean(options?.fatal);
    this.#ignoreBOM = Boolean(options?.ignoreBOM);
  }

  static #check(decoder) {
    if (decoder == null || !(#fatal in decoder)) throw invalidThis("TextDecoder");
  }

  get encoding() {
    TextDecoder.#check(this);
    return "utf-8";
  }

  get fatal() {
    TextDecoder.#check(this);
    return this.#fatal;
  }

  get ignoreBOM() {
    TextDecoder.#check(this);
    return this.#ignoreBOM;
  }

  decode(input = empty, options = {}) {
    TextDecoder.#check(this);
    let bytes;
    if (input instanceof ArrayBuffer || input instanceof SharedArrayBuffer) {
      bytes = new Uint8Array(input);
    } else if (ArrayBuffer.isView(input)) {
      bytes = new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
    } else {
      throw invalidArgType("input", ["ArrayBuffer", "ArrayBufferView"], input);
    }
    if (options !== null && typeof options !== "object" && typeof options !== "function") {
      throw invalidArgType("options", "Object", options);
    }
    const stream = Boolean(options?.stream);

    if (this.#pending.length > 0) {
      const joined = new Uint8Array(this.#pending.length + bytes.length);
      joined.set(this.#pending);
      joined.set(bytes, this.#pending.length);
      bytes = joined;
      this.#pending = empty;
    }

    const decoded = native.decode(bytes, this.#fatal, stream);
    if (decoded === undefined) {
      this.#bomSeen = false;
      const error = typeError(
        "ERR_ENCODING_INVALID_ENCODED_DATA",
        "The encoded data was not valid for encoding utf-8",
      );
      throw error;
    }

    let text = decoded[0];
    const leftover = decoded[1];
    if (leftover > 0) {
      this.#pending = bytes.slice(bytes.length - leftover);
    }

    if (!this.#bomSeen && text.length > 0) {
      if (!this.#ignoreBOM && text[0] === "﻿") text = text.slice(1);
      this.#bomSeen = true;
    }
    if (!stream) this.#bomSeen = false;
    return text;
  }
}

module.exports = { TextEncoder, TextDecoder };
