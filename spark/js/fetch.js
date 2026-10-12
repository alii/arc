"use strict";

// fetch, Headers, Request and Response, written to the fetch standard and
// worded the way node words its errors. The network is the embedder's: one
// want for a response's status and headers, then one for each piece of its
// body, so a body that never ends (a stream of events) costs nothing extra.
// Redirects, aborting and unpacking a compressed body are done here

const { AbortController } = require("spark:dom_events");
const { TextEncoder, TextDecoder } = require("spark:encoding");
const blobs = require("spark:blob");
const { Blob, File } = blobs;
const { URL, URLSearchParams } = require("node:url");
const { isDisturbed } = require("node:stream");
const {
  ReadableStream,
  TransformStream,
  TextDecoderStream,
  DecompressionStream,
} = require("node:stream/web");

const kInspect = Symbol.for("nodejs.util.inspect.custom");
// passed to a constructor to make an object that is filled in afterwards
const kMake = Symbol("make");
const encoder = new TextEncoder();

function illegalInvocation() {
  const error = new TypeError("Illegal invocation");
  error.code = "ERR_INVALID_THIS";
  return error;
}

function argumentsRequired(context, min, found) {
  if (found >= min) return;
  throw new TypeError(
    `${context}: ${min} argument${min === 1 ? "" : "s"} required, but${found ? " only" : ""} ${found} found.`,
  );
}

// a string whose every character fits in one byte
function byteString(value) {
  const string = `${value}`;
  for (let i = 0; i < string.length; i++) {
    if (string.charCodeAt(i) > 255) {
      throw new TypeError(
        `Cannot convert argument to a ByteString because the character at index ${i} has a value of ${string.charCodeAt(i)} which is greater than 255.`,
      );
    }
  }
  return string;
}

const TOKEN = /^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$/;

// lists these names when the object's keys are asked for, in this order
function enumerable(prototype, names) {
  for (const name of names) {
    const descriptor = Object.getOwnPropertyDescriptor(prototype, name);
    descriptor.enumerable = true;
    delete prototype[name];
    Object.defineProperty(prototype, name, descriptor);
  }
}

// ---- Headers

function normalizedValue(value) {
  return value.replace(/^[\t\n\r ]+|[\t\n\r ]+$/g, "");
}

function validValue(value) {
  return !/[\0\r\n]/.test(value);
}

class Headers {
  // [lower-case name, value] in the order they were added
  #list = [];
  #guard = "none";

  constructor(init = undefined) {
    if (init === kMake) return;
    if (init !== undefined) Headers.#fill(this, init);
  }

  static #fill(headers, init) {
    if (init !== null && (typeof init === "object" || typeof init === "function")) {
      if (typeof init[Symbol.iterator] === "function") {
        let at = 0;
        for (const pair of init) {
          if (
            pair === null ||
            typeof pair !== "object" ||
            typeof pair[Symbol.iterator] !== "function"
          ) {
            const shown = typeof pair === "string" ? `"${pair}"` : String(pair);
            throw new TypeError(
              `Headers constructor: init[${at}] (${shown}) is not iterable.`,
            );
          }
          const parts = [...pair];
          if (parts.length !== 2) {
            throw new TypeError(
              `Headers constructor: expected name/value pair to be length 2, found ${parts.length}.`,
            );
          }
          headers.#append("Headers.append", byteString(parts[0]), byteString(parts[1]));
          at++;
        }
        return;
      }
      // every key the object has itself, listed or not, as node takes them
      for (const key of Reflect.ownKeys(init)) {
        if (typeof key === "symbol") {
          throw new TypeError(
            `Headers constructor: Key ${key.toString()} in init is a symbol, which cannot be converted to a ByteString.`,
          );
        }
        headers.#append("Headers.append", byteString(key), byteString(init[key]));
      }
      return;
    }
    throw new TypeError(
      "Headers constructor: Argument 1 could not be converted to one of: sequence<sequence<ByteString>>, record<ByteString, ByteString>.",
    );
  }

  #append(context, name, value) {
    value = normalizedValue(value);
    if (!TOKEN.test(name)) {
      throw new TypeError(`${context}: "${name}" is an invalid header name.`);
    }
    if (!validValue(value)) {
      throw new TypeError(`${context}: "${value}" is an invalid header value.`);
    }
    if (this.#guard === "immutable") throw new TypeError("immutable");
    this.#list.push([name.toLowerCase(), value]);
  }

  static #name(context, name) {
    name = byteString(name);
    if (!TOKEN.test(name)) {
      throw new TypeError(`${context}: "${name}" is an invalid header name.`);
    }
    return name.toLowerCase();
  }

  append(name, value) {
    if (!(#list in this)) throw illegalInvocation();
    argumentsRequired("Headers.append", 2, arguments.length);
    this.#append("Headers.append", byteString(name), byteString(value));
  }

  delete(name) {
    if (!(#list in this)) throw illegalInvocation();
    argumentsRequired("Headers.delete", 1, arguments.length);
    name = Headers.#name("Headers.delete", name);
    if (this.#guard === "immutable") throw new TypeError("immutable");
    this.#list = this.#list.filter((entry) => entry[0] !== name);
  }

  get(name) {
    if (!(#list in this)) throw illegalInvocation();
    argumentsRequired("Headers.get", 1, arguments.length);
    name = Headers.#name("Headers.get", name);
    const values = [];
    for (const entry of this.#list) if (entry[0] === name) values.push(entry[1]);
    return values.length === 0 ? null : values.join(", ");
  }

  getSetCookie() {
    if (!(#list in this)) throw illegalInvocation();
    const values = [];
    for (const entry of this.#list) {
      if (entry[0] === "set-cookie") values.push(entry[1]);
    }
    return values;
  }

  has(name) {
    if (!(#list in this)) throw illegalInvocation();
    argumentsRequired("Headers.has", 1, arguments.length);
    name = Headers.#name("Headers.has", name);
    return this.#list.some((entry) => entry[0] === name);
  }

  set(name, value) {
    if (!(#list in this)) throw illegalInvocation();
    argumentsRequired("Headers.set", 2, arguments.length);
    value = normalizedValue(byteString(value));
    name = Headers.#name("Headers.set", name);
    if (!validValue(value)) {
      throw new TypeError(`Headers.set: "${value}" is an invalid header value.`);
    }
    if (this.#guard === "immutable") throw new TypeError("immutable");
    const at = this.#list.findIndex((entry) => entry[0] === name);
    if (at === -1) {
      this.#list.push([name, value]);
    } else {
      this.#list[at][1] = value;
      this.#list = this.#list.filter((entry, i) => i <= at || entry[0] !== name);
    }
  }

  // by name, a name's values joined with ", "; set-cookie is never joined
  #sorted() {
    const names = [...new Set(this.#list.map((entry) => entry[0]))].sort();
    const pairs = [];
    for (const name of names) {
      const values = [];
      for (const entry of this.#list) if (entry[0] === name) values.push(entry[1]);
      if (name === "set-cookie") {
        for (const value of values) pairs.push([name, value]);
      } else {
        pairs.push([name, values.join(", ")]);
      }
    }
    return pairs;
  }

  keys() {
    if (!(#list in this)) throw illegalInvocation();
    return iteratorOver(this.#sorted().map((pair) => pair[0]));
  }

  values() {
    if (!(#list in this)) throw illegalInvocation();
    return iteratorOver(this.#sorted().map((pair) => pair[1]));
  }

  entries() {
    if (!(#list in this)) throw illegalInvocation();
    return iteratorOver(this.#sorted());
  }

  forEach(callback, thisArg = undefined) {
    if (!(#list in this)) throw illegalInvocation();
    argumentsRequired("Headers.forEach", 1, arguments.length);
    if (typeof callback !== "function") {
      throw new TypeError(
        "Failed to execute 'forEach' on 'Headers': parameter 1 is not of type 'Function'.",
      );
    }
    for (const [name, value] of this.#sorted()) {
      callback.call(thisArg, value, name, this);
    }
  }

  [kInspect](depth, options, inspect) {
    const shown = {};
    for (const [name, value] of this.#list) {
      shown[name] = name in shown ? `${shown[name]}, ${value}` : value;
    }
    return `Headers ${inspect(shown, { ...options, depth: options.depth ?? depth })}`;
  }

  // for the classes below
  static made(pairs, guard) {
    const headers = new Headers(kMake);
    for (const [name, value] of pairs) headers.#list.push([name.toLowerCase(), value]);
    headers.#guard = guard;
    return headers;
  }

  static guard(headers, guard) {
    headers.#guard = guard;
    return headers;
  }

  static fill(headers, init) {
    Headers.#fill(headers, init);
  }

  static list(headers) {
    return headers.#list;
  }
}

Headers.prototype[Symbol.iterator] = Headers.prototype.entries;
Object.defineProperty(Headers.prototype, Symbol.toStringTag, {
  value: "Headers",
  configurable: true,
});
enumerable(Headers.prototype, [
  "append",
  "delete",
  "get",
  "has",
  "set",
  "getSetCookie",
  "keys",
  "values",
  "entries",
  "forEach",
]);

const headersIteratorPrototype = Object.setPrototypeOf(
  {
    next() {
      return this.inner.next();
    },
    [Symbol.toStringTag]: "Headers Iterator",
  },
  Object.getPrototypeOf(Object.getPrototypeOf([][Symbol.iterator]())),
);

function iteratorOver(items) {
  const iterator = Object.create(headersIteratorPrototype);
  Object.defineProperty(iterator, "inner", { value: items[Symbol.iterator]() });
  return iterator;
}

// ---- FormData

// text with any half of a surrogate pair replaced, as the web's strings are
function scalar(value) {
  return `${value}`.toWellFormed();
}

class FormData {
  // [name, text or File] in the order they were added
  #entries = [];

  constructor(form = undefined) {
    if (form !== undefined) {
      throw new TypeError(
        "FormData constructor: Argument 1 could not be converted to: undefined.",
      );
    }
  }

  static #entry(context, name, value, filename) {
    name = scalar(name);
    if (value instanceof Blob) {
      if (!(value instanceof File) || filename !== undefined) {
        value = new File([value], filename === undefined ? "blob" : scalar(filename), {
          type: value.type,
          lastModified: value instanceof File ? value.lastModified : undefined,
        });
      }
    } else {
      if (filename !== undefined) {
        const shown = typeof value === "string" ? `"${value}"` : String(value);
        throw new TypeError(
          `FormData.${context}: Expected value ("${shown}") to be an instance of Blob.`,
        );
      }
      value = scalar(value);
    }
    return [name, value];
  }

  append(name, value, filename = undefined) {
    if (!(#entries in this)) throw illegalInvocation();
    argumentsRequired("FormData.append", 2, arguments.length);
    this.#entries.push(FormData.#entry("append", name, value, filename));
  }

  delete(name) {
    if (!(#entries in this)) throw illegalInvocation();
    argumentsRequired("FormData.delete", 1, arguments.length);
    name = scalar(name);
    this.#entries = this.#entries.filter((entry) => entry[0] !== name);
  }

  get(name) {
    if (!(#entries in this)) throw illegalInvocation();
    argumentsRequired("FormData.get", 1, arguments.length);
    name = scalar(name);
    const found = this.#entries.find((entry) => entry[0] === name);
    return found === undefined ? null : found[1];
  }

  getAll(name) {
    if (!(#entries in this)) throw illegalInvocation();
    argumentsRequired("FormData.getAll", 1, arguments.length);
    name = scalar(name);
    return this.#entries.filter((entry) => entry[0] === name).map((entry) => entry[1]);
  }

  has(name) {
    if (!(#entries in this)) throw illegalInvocation();
    argumentsRequired("FormData.has", 1, arguments.length);
    name = scalar(name);
    return this.#entries.some((entry) => entry[0] === name);
  }

  set(name, value, filename = undefined) {
    if (!(#entries in this)) throw illegalInvocation();
    argumentsRequired("FormData.set", 2, arguments.length);
    const entry = FormData.#entry("set", name, value, filename);
    const at = this.#entries.findIndex((existing) => existing[0] === entry[0]);
    if (at === -1) {
      this.#entries.push(entry);
    } else {
      this.#entries[at] = entry;
      this.#entries = this.#entries.filter(
        (existing, i) => i <= at || existing[0] !== entry[0],
      );
    }
  }

  entries() {
    if (!(#entries in this)) throw illegalInvocation();
    return formIteratorOver(this.#entries.map((entry) => [...entry]));
  }

  keys() {
    if (!(#entries in this)) throw illegalInvocation();
    return formIteratorOver(this.#entries.map((entry) => entry[0]));
  }

  values() {
    if (!(#entries in this)) throw illegalInvocation();
    return formIteratorOver(this.#entries.map((entry) => entry[1]));
  }

  forEach(callback, thisArg = undefined) {
    if (!(#entries in this)) throw illegalInvocation();
    argumentsRequired("FormData.forEach", 1, arguments.length);
    if (typeof callback !== "function") {
      throw new TypeError(
        "Failed to execute 'forEach' on 'FormData': parameter 1 is not of type 'Function'.",
      );
    }
    for (const [name, value] of [...this.#entries]) {
      callback.call(thisArg, value, name, this);
    }
  }

  [kInspect](depth, options, inspect) {
    const shown = { __proto__: null };
    for (const [name, value] of this.#entries) {
      shown[name] = name in shown ? [].concat(shown[name], value) : value;
    }
    const text = inspect(shown, { ...options, depth: options.depth ?? depth });
    return `FormData ${text.slice(text.indexOf("]") + 2)}`;
  }

  static entriesOf(form) {
    return form.#entries;
  }

  static from(entries) {
    const form = new FormData();
    form.#entries = entries;
    return form;
  }
}

FormData.prototype[Symbol.iterator] = FormData.prototype.entries;
Object.defineProperty(FormData.prototype, Symbol.toStringTag, {
  value: "FormData",
  configurable: true,
});
enumerable(FormData.prototype, [
  "append",
  "delete",
  "get",
  "getAll",
  "has",
  "set",
  "keys",
  "values",
  "entries",
  "forEach",
]);

const formIteratorPrototype = Object.setPrototypeOf(
  {
    next() {
      return this.inner.next();
    },
    [Symbol.toStringTag]: "FormData Iterator",
  },
  Object.getPrototypeOf(Object.getPrototypeOf([][Symbol.iterator]())),
);

function formIteratorOver(items) {
  const iterator = Object.create(formIteratorPrototype);
  Object.defineProperty(iterator, "inner", { value: items[Symbol.iterator]() });
  return iterator;
}

// a form as the bytes of a multipart/form-data body, and the content type
// that names the line between its parts
function multipart(form) {
  const boundary = `----formdata-undici-0${`${Math.floor(Math.random() * 1e11)}`.padStart(11, "0")}`;
  const prefix = `--${boundary}\r\nContent-Disposition: form-data`;
  const escape = (text) =>
    text.replace(/\n/g, "%0A").replace(/\r/g, "%0D").replace(/"/g, "%22");
  const lineFeeds = (text) => text.replace(/\r?\n|\r/g, "\r\n");
  const parts = [];
  for (const [name, value] of FormData.entriesOf(form)) {
    if (typeof value === "string") {
      parts.push(
        encoder.encode(
          `${prefix}; name="${escape(lineFeeds(name))}"\r\n\r\n${lineFeeds(value)}\r\n`,
        ),
      );
    } else {
      parts.push(
        encoder.encode(
          `${prefix}; name="${escape(lineFeeds(name))}"` +
            (value.name ? `; filename="${escape(value.name)}"` : "") +
            `\r\nContent-Type: ${value.type || "application/octet-stream"}\r\n\r\n`,
        ),
        blobs.bytesOf(value),
        encoder.encode("\r\n"),
      );
    }
  }
  parts.push(encoder.encode(`--${boundary}--\r\n`));
  let size = 0;
  for (const part of parts) size += part.byteLength;
  const bytes = new Uint8Array(size);
  let at = 0;
  for (const part of parts) {
    bytes.set(part, at);
    at += part.byteLength;
  }
  return { bytes, type: `multipart/form-data; boundary=${boundary}` };
}

function indexOfBytes(bytes, needle, from) {
  outer: for (let i = from; i <= bytes.length - needle.length; i++) {
    for (let j = 0; j < needle.length; j++) {
      if (bytes[i + j] !== needle[j]) continue outer;
    }
    return i;
  }
  return -1;
}

// the form a body holds, going by its content type
function parsedForm(bytes, contentType) {
  const failed = () => new TypeError("Failed to parse body as FormData.");
  const type = (contentType ?? "").trim();
  if (/^application\/x-www-form-urlencoded\s*(;|$)/i.test(type)) {
    const entries = [];
    for (const [name, value] of new URLSearchParams(new TextDecoder().decode(bytes))) {
      entries.push([name, value]);
    }
    return FormData.from(entries);
  }
  if (!/^multipart\/form-data\s*(;|$)/i.test(type)) {
    throw new TypeError(
      'Content-Type was not one of "multipart/form-data" or "application/x-www-form-urlencoded".',
    );
  }
  const boundary = /;.*?boundary=(?:"([^"]+)"|([^;\s]+))/i.exec(type);
  if (boundary === null) throw failed();
  const line = encoder.encode(`--${boundary[1] ?? boundary[2]}`);
  const entries = [];
  let at = indexOfBytes(bytes, line, 0);
  if (at === -1) throw failed();
  for (;;) {
    at += line.length;
    // the last line between parts ends with two dashes
    if (bytes[at] === 0x2d && bytes[at + 1] === 0x2d) break;
    if (bytes[at] !== 0x0d || bytes[at + 1] !== 0x0a) throw failed();
    at += 2;
    const headEnd = indexOfBytes(bytes, encoder.encode("\r\n\r\n"), at);
    if (headEnd === -1) throw failed();
    const head = new TextDecoder().decode(bytes.subarray(at, headEnd));
    const next = indexOfBytes(bytes, encoder.encode(`\r\n--${boundary[1] ?? boundary[2]}`), headEnd + 4);
    if (next === -1) throw failed();
    const content = bytes.slice(headEnd + 4, next);
    const disposition = /^content-disposition:\s*form-data(.*)$/im.exec(head);
    if (disposition === null) throw failed();
    const name = /;\s*name="([^"]*)"/i.exec(disposition[1]);
    const filename = /;\s*filename="([^"]*)"/i.exec(disposition[1]);
    if (name === null) throw failed();
    const unescape = (text) =>
      text.replace(/%0A/g, "\n").replace(/%0D/g, "\r").replace(/%22/g, '"');
    if (filename === null) {
      entries.push([unescape(name[1]), new TextDecoder().decode(content)]);
    } else {
      const partType = /^content-type:\s*(.*)$/im.exec(head);
      entries.push([
        unescape(name[1]),
        new File([content], unescape(filename[1]), {
          type: partType === null ? "" : partType[1].trim(),
        }),
      ]);
    }
    at = next + 2;
  }
  return FormData.from(entries);
}

// ---- bodies

function bytesOf(view) {
  return new Uint8Array(view.buffer, view.byteOffset, view.byteLength).slice();
}

function streamOf(bytes) {
  return new ReadableStream({
    pull(controller) {
      if (bytes.byteLength > 0) controller.enqueue(bytes);
      controller.close();
    },
    type: "bytes",
  });
}

// what a request or response is given as its body: the stream to read it
// from, the bytes when they are known up front, and the content type it implies
function extractBody(object) {
  if (object instanceof ReadableStream) {
    if (isDisturbed(object) || object.locked) {
      throw new TypeError(
        "Response body object should not be disturbed or locked",
      );
    }
    return { stream: object, source: null, type: null };
  }
  let source;
  let type = null;
  if (object instanceof Blob) {
    source = blobs.bytesOf(object).slice();
    type = object.type === "" ? null : object.type;
  } else if (object instanceof FormData) {
    ({ bytes: source, type } = multipart(object));
  } else if (object instanceof URLSearchParams) {
    source = encoder.encode(object.toString());
    type = "application/x-www-form-urlencoded;charset=UTF-8";
  } else if (object instanceof ArrayBuffer) {
    source = new Uint8Array(object.slice(0));
  } else if (ArrayBuffer.isView(object)) {
    source = bytesOf(object);
  } else {
    source = encoder.encode(`${object}`);
    type = "text/plain;charset=UTF-8";
  }
  return { stream: streamOf(source), source, type };
}

function unusable(body) {
  return body !== null && (body.stream.locked || isDisturbed(body.stream));
}

async function allBytes(body) {
  if (body === null) return new Uint8Array(0);
  if (unusable(body)) {
    throw new TypeError("Body is unusable: Body has already been read");
  }
  const reader = body.stream.getReader();
  const chunks = [];
  let size = 0;
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    if (!(value instanceof Uint8Array)) {
      throw new TypeError("Received non-Uint8Array chunk");
    }
    chunks.push(value);
    size += value.byteLength;
  }
  const all = new Uint8Array(size);
  let at = 0;
  for (const chunk of chunks) {
    all.set(chunk, at);
    at += chunk.byteLength;
  }
  return all;
}

function clonedBody(body) {
  if (body === null) return null;
  const [mine, theirs] = body.stream.tee();
  body.stream = mine;
  return { stream: theirs, source: body.source, type: body.type };
}

// the body as text, a piece at a time
function textStream(body) {
  if (unusable(body)) {
    throw new TypeError("Body is unusable: Body has already been read");
  }
  const stream = body === null ? streamOf(new Uint8Array(0)) : body.stream;
  return stream.pipeThrough(new TextDecoderStream());
}

// a content type as the web writes one out: the kind in lower case, then each
// parameter after a semicolon with no space; nothing if it is not a type
function mimeOf(contentType) {
  if (contentType === null || contentType === undefined) return "";
  const [essence, ...parameters] = contentType.split(";");
  const kind = essence.trim().toLowerCase();
  if (!/^[!#$%&'*+\-.^_`|~0-9a-z]+\/[!#$%&'*+\-.^_`|~0-9a-z]+$/.test(kind)) return "";
  let text = kind;
  const seen = new Set();
  for (const parameter of parameters) {
    const at = parameter.indexOf("=");
    if (at === -1) continue;
    const name = parameter.slice(0, at).trim().toLowerCase();
    let value = parameter.slice(at + 1).trim();
    if (name === "" || seen.has(name)) continue;
    if (value.startsWith('"')) value = value.slice(1, value.endsWith('"') && value.length > 1 ? -1 : undefined);
    if (value === "" && !parameter.slice(at + 1).trim().startsWith('"')) continue;
    seen.add(name);
    text += /^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$/.test(value)
      ? `;${name}=${value}`
      : `;${name}="${value.replace(/(["\\])/g, "\\$1")}"`;
  }
  return text;
}

const bodyMethods = {
  async arrayBuffer(body) {
    return (await allBytes(body)).buffer;
  },
  async bytes(body) {
    return await allBytes(body);
  },
  async text(body) {
    return new TextDecoder().decode(await allBytes(body));
  },
  async json(body) {
    return JSON.parse(new TextDecoder().decode(await allBytes(body)));
  },
  async blob(body, contentType) {
    return new Blob([await allBytes(body)], { type: mimeOf(contentType) });
  },
  async formData(body, contentType) {
    return parsedForm(await allBytes(body), contentType);
  },
};

// ---- Response

const NULL_BODY_STATUS = [101, 103, 204, 205, 304];
const REDIRECT_STATUS = [301, 302, 303, 307, 308];

class Response {
  #type = "default";
  #status = 200;
  #statusText = "";
  #headers;
  #urls = [];
  #body = null;

  constructor(body = null, init = undefined) {
    if (body === kMake) return;
    if (init !== undefined && init !== null && typeof init !== "object" && typeof init !== "function") {
      throw new TypeError(
        `undefined: Expected ${String(init)} to be one of: Null, Undefined, Object.`,
      );
    }
    init ??= {};
    this.#headers = Headers.made([], "response");
    let status = 200;
    if (init.status !== undefined) {
      status = Number(init.status) & 0xffff;
      if (!Number.isFinite(Number(init.status))) status = 0;
      if (status < 200 || status > 599) {
        throw new RangeError(
          'init["status"] must be in the range of 200 to 599, inclusive.',
        );
      }
    }
    let statusText = "";
    if (init.statusText !== undefined) {
      statusText = byteString(init.statusText);
      if (!/^[\t\x20-\x7e\x80-\xff]*$/.test(statusText)) {
        throw new TypeError("Invalid statusText");
      }
    }
    this.#status = status;
    this.#statusText = statusText;
    if (init.headers !== undefined && init.headers !== null) {
      Headers.fill(this.#headers, init.headers);
    }
    if (body !== null && body !== undefined) {
      if (NULL_BODY_STATUS.includes(status)) {
        throw new TypeError(
          `Response constructor: Invalid response status code ${status}`,
        );
      }
      const extracted = extractBody(body);
      this.#body = extracted;
      if (extracted.type !== null && !this.#headers.has("content-type")) {
        this.#headers.append("content-type", extracted.type);
      }
    }
  }

  static error() {
    const response = new Response(kMake);
    response.#type = "error";
    response.#status = 0;
    response.#headers = Headers.made([], "immutable");
    return response;
  }

  static redirect(url, status = 302) {
    argumentsRequired("Response.redirect", 1, arguments.length);
    let parsed;
    try {
      parsed = new URL(`${url}`);
    } catch (cause) {
      throw new TypeError(`Failed to parse URL from ${url}`, { cause });
    }
    status = Number(status) & 0xffff;
    if (!REDIRECT_STATUS.includes(status)) {
      throw new RangeError(`Invalid status code ${status}`);
    }
    const response = new Response(kMake);
    response.#status = status;
    response.#headers = Headers.made([["location", parsed.href]], "immutable");
    return response;
  }

  static json(data, init = undefined) {
    argumentsRequired("Response.json", 1, arguments.length);
    const text = JSON.stringify(data);
    if (text === undefined) {
      throw new TypeError("Value is not JSON serializable");
    }
    const response = new Response(text, init);
    if (init?.headers === undefined || !new Headers(init.headers).has("content-type")) {
      response.#headers.set("content-type", "application/json");
    }
    return response;
  }

  // a response that came from the network
  static from(status, statusText, pairs, stream, urls) {
    const response = new Response(kMake);
    response.#type = "basic";
    response.#status = status;
    response.#statusText = statusText;
    response.#headers = Headers.made(pairs, "immutable");
    response.#urls = urls;
    response.#body = stream === null ? null : { stream, source: null, type: null };
    return response;
  }

  get type() {
    if (!(#status in this)) throw illegalInvocation();
    return this.#type;
  }

  get url() {
    if (!(#status in this)) throw illegalInvocation();
    const last = this.#urls.at(-1);
    if (last === undefined) return "";
    const url = new URL(last);
    url.hash = "";
    return url.href;
  }

  get redirected() {
    if (!(#status in this)) throw illegalInvocation();
    return this.#urls.length > 1;
  }

  get status() {
    if (!(#status in this)) throw illegalInvocation();
    return this.#status;
  }

  get ok() {
    if (!(#status in this)) throw illegalInvocation();
    return this.#status >= 200 && this.#status <= 299;
  }

  get statusText() {
    if (!(#status in this)) throw illegalInvocation();
    return this.#statusText;
  }

  get headers() {
    if (!(#status in this)) throw illegalInvocation();
    return this.#headers;
  }

  get body() {
    if (!(#status in this)) throw illegalInvocation();
    return this.#body === null ? null : this.#body.stream;
  }

  get bodyUsed() {
    if (!(#status in this)) throw illegalInvocation();
    return this.#body !== null && isDisturbed(this.#body.stream);
  }

  clone() {
    if (!(#status in this)) throw illegalInvocation();
    if (unusable(this.#body)) {
      throw new TypeError("Response.clone: Body has already been consumed.");
    }
    const response = new Response(kMake);
    response.#type = this.#type;
    response.#status = this.#status;
    response.#statusText = this.#statusText;
    response.#headers = Headers.made(Headers.list(this.#headers), "immutable");
    Headers.guard(response.#headers, this.#type === "default" ? "response" : "immutable");
    response.#urls = [...this.#urls];
    response.#body = clonedBody(this.#body);
    return response;
  }

  arrayBuffer() {
    if (!(#status in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.arrayBuffer(this.#body);
  }

  blob() {
    if (!(#status in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.blob(this.#body, this.#headers.get("content-type"));
  }

  bytes() {
    if (!(#status in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.bytes(this.#body);
  }

  formData() {
    if (!(#status in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.formData(this.#body, this.#headers.get("content-type"));
  }

  json() {
    if (!(#status in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.json(this.#body);
  }

  text() {
    if (!(#status in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.text(this.#body);
  }

  textStream() {
    if (!(#status in this)) throw illegalInvocation();
    return textStream(this.#body);
  }

  [kInspect](depth, options, inspect) {
    if (options.depth === null) options = { ...options, depth: 2 };
    const shown = {
      status: this.status,
      statusText: this.statusText,
      headers: this.headers,
      body: this.body,
      bodyUsed: this.bodyUsed,
      ok: this.ok,
      redirected: this.redirected,
      type: this.type,
      url: this.url,
    };
    return `Response ${inspect(shown, { ...options, depth: options.depth ?? depth })}`;
  }
}

Object.defineProperty(Response.prototype, Symbol.toStringTag, {
  value: "Response",
  configurable: true,
});
enumerable(Response.prototype, [
  "type",
  "url",
  "redirected",
  "status",
  "ok",
  "statusText",
  "headers",
  "body",
  "bodyUsed",
  "clone",
  "blob",
  "arrayBuffer",
  "text",
  "json",
  "formData",
  "bytes",
  "textStream",
]);
enumerable(Response, ["error", "json", "redirect"]);

// ---- Request

const NORMALIZED_METHODS = ["DELETE", "GET", "HEAD", "OPTIONS", "POST", "PUT"];
const FORBIDDEN_METHODS = ["CONNECT", "TRACE", "TRACK"];

function oneOf(value, allowed) {
  value = `${value}`;
  if (!allowed.includes(value)) {
    throw new TypeError(
      `undefined: ${value} is not an accepted type. Expected one of ${allowed.join(", ")}.`,
    );
  }
  return value;
}

class Request {
  #url;
  #method = "GET";
  #headers;
  #body = null;
  #signal;
  #mode = "cors";
  #credentials = "same-origin";
  #cache = "default";
  #redirect = "follow";
  #referrer = "about:client";
  #referrerPolicy = "";
  #integrity = "";
  #keepalive = false;

  constructor(input, init = undefined) {
    if (input === kMake) return;
    argumentsRequired("Request constructor", 1, arguments.length);
    if (init !== undefined && init !== null && typeof init !== "object" && typeof init !== "function") {
      throw new TypeError(
        `undefined: Expected ${String(init)} to be one of: Null, Undefined, Object.`,
      );
    }
    init ??= {};

    let signal = null;
    let inputBody = null;
    let headers = [];
    if (input instanceof Request) {
      this.#url = input.#url;
      this.#method = input.#method;
      headers = Headers.list(input.#headers).map((entry) => [...entry]);
      inputBody = input.#body;
      signal = input.#signal;
      this.#mode = input.#mode;
      this.#credentials = input.#credentials;
      this.#cache = input.#cache;
      this.#redirect = input.#redirect;
      this.#referrer = input.#referrer;
      this.#referrerPolicy = input.#referrerPolicy;
      this.#integrity = input.#integrity;
      this.#keepalive = input.#keepalive;
    } else {
      let parsed;
      try {
        parsed = new URL(`${input}`);
      } catch (cause) {
        throw new TypeError(`Failed to parse URL from ${input}`, { cause });
      }
      if (parsed.username !== "" || parsed.password !== "") {
        throw new TypeError(
          `Request cannot be constructed from a URL that includes credentials: ${input}`,
        );
      }
      this.#url = parsed.href;
    }

    if (init.window !== undefined && init.window !== null) {
      throw new TypeError("'window' option 'client' must be null");
    }
    if (init.referrer !== undefined) {
      const referrer = `${init.referrer}`;
      if (referrer === "") {
        this.#referrer = "no-referrer";
      } else {
        try {
          this.#referrer = new URL(referrer).href;
        } catch (cause) {
          throw new TypeError(`Referrer "${referrer}" is not a valid URL.`, {
            cause,
          });
        }
      }
    }
    if (init.referrerPolicy !== undefined) {
      this.#referrerPolicy = oneOf(init.referrerPolicy, [
        "",
        "no-referrer",
        "no-referrer-when-downgrade",
        "same-origin",
        "origin",
        "strict-origin",
        "origin-when-cross-origin",
        "strict-origin-when-cross-origin",
        "unsafe-url",
      ]);
    }
    if (init.mode !== undefined) {
      const mode = oneOf(init.mode, ["navigate", "same-origin", "no-cors", "cors"]);
      if (mode === "navigate") {
        throw new TypeError(
          "Request constructor: invalid request mode navigate.",
        );
      }
      this.#mode = mode;
    }
    if (init.credentials !== undefined) {
      this.#credentials = oneOf(init.credentials, ["omit", "same-origin", "include"]);
    }
    if (init.cache !== undefined) {
      this.#cache = oneOf(init.cache, [
        "default",
        "no-store",
        "reload",
        "no-cache",
        "force-cache",
        "only-if-cached",
      ]);
    }
    if (init.redirect !== undefined) {
      this.#redirect = oneOf(init.redirect, ["follow", "manual", "error"]);
    }
    if (init.integrity != null) this.#integrity = `${init.integrity}`;
    if (init.keepalive !== undefined) this.#keepalive = Boolean(init.keepalive);
    if (init.method !== undefined) {
      const method = byteString(init.method);
      if (!TOKEN.test(method)) {
        throw new TypeError(`'${method}' is not a valid HTTP method.`);
      }
      const upper = method.toUpperCase();
      if (FORBIDDEN_METHODS.includes(upper)) {
        throw new TypeError(`'${method}' HTTP method is unsupported.`);
      }
      this.#method = NORMALIZED_METHODS.includes(upper) ? upper : method;
    }
    if (init.signal !== undefined) signal = init.signal;
    if (
      signal != null &&
      (typeof signal !== "object" ||
        typeof signal.aborted !== "boolean" ||
        typeof signal.addEventListener !== "function")
    ) {
      throw new TypeError(
        `RequestInit: Expected signal ("${String(signal)}") to be an instance of AbortSignal.`,
      );
    }

    // a signal of its own, which follows the one given
    const controller = new AbortController();
    this.#signal = controller.signal;
    if (signal != null) {
      if (signal.aborted) {
        controller.abort(signal.reason);
      } else {
        signal.addEventListener("abort", () => controller.abort(signal.reason), {
          once: true,
        });
      }
    }

    this.#headers = Headers.made(headers, "request");
    if (init.headers !== undefined && init.headers !== null) {
      this.#headers = Headers.made([], "request");
      Headers.fill(this.#headers, init.headers);
    }

    const givesBody = init.body !== undefined && init.body !== null;
    if (
      (givesBody || inputBody !== null) &&
      (this.#method === "GET" || this.#method === "HEAD")
    ) {
      throw new TypeError("Request with GET/HEAD method cannot have body.");
    }
    if (givesBody) {
      const extracted = extractBody(init.body);
      if (extracted.source === null && init.duplex == null) {
        throw new TypeError(
          "RequestInit: duplex option is required when sending a body.",
        );
      }
      this.#body = extracted;
      if (extracted.type !== null && !this.#headers.has("content-type")) {
        this.#headers.append("content-type", extracted.type);
      }
    } else if (inputBody !== null) {
      if (unusable(inputBody)) {
        throw new TypeError(
          "Cannot construct a Request with a Request object that has already been used.",
        );
      }
      // the body moves to this request; the one it came from is left used
      const passed = inputBody.stream.pipeThrough(new TransformStream());
      this.#body = { stream: passed, source: inputBody.source, type: inputBody.type };
    }
  }

  get method() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#method;
  }

  get url() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#url;
  }

  get headers() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#headers;
  }

  get destination() {
    if (!(#url in this)) throw illegalInvocation();
    return "";
  }

  get referrer() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#referrer === "no-referrer" ? "" : this.#referrer;
  }

  get referrerPolicy() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#referrerPolicy;
  }

  get mode() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#mode;
  }

  get credentials() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#credentials;
  }

  get cache() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#cache;
  }

  get redirect() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#redirect;
  }

  get integrity() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#integrity;
  }

  get keepalive() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#keepalive;
  }

  get isReloadNavigation() {
    if (!(#url in this)) throw illegalInvocation();
    return false;
  }

  get isHistoryNavigation() {
    if (!(#url in this)) throw illegalInvocation();
    return false;
  }

  get signal() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#signal;
  }

  get body() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#body === null ? null : this.#body.stream;
  }

  get bodyUsed() {
    if (!(#url in this)) throw illegalInvocation();
    return this.#body !== null && isDisturbed(this.#body.stream);
  }

  get duplex() {
    if (!(#url in this)) throw illegalInvocation();
    return "half";
  }

  clone() {
    if (!(#url in this)) throw illegalInvocation();
    if (unusable(this.#body)) {
      throw new TypeError("unusable");
    }
    const request = new Request(this.#url, {
      method: this.#method,
      signal: this.#signal,
    });
    request.#headers = Headers.made(Headers.list(this.#headers), "request");
    request.#mode = this.#mode;
    request.#credentials = this.#credentials;
    request.#cache = this.#cache;
    request.#redirect = this.#redirect;
    request.#referrer = this.#referrer;
    request.#referrerPolicy = this.#referrerPolicy;
    request.#integrity = this.#integrity;
    request.#keepalive = this.#keepalive;
    request.#body = clonedBody(this.#body);
    return request;
  }

  arrayBuffer() {
    if (!(#url in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.arrayBuffer(this.#body);
  }

  blob() {
    if (!(#url in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.blob(this.#body, this.#headers.get("content-type"));
  }

  bytes() {
    if (!(#url in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.bytes(this.#body);
  }

  formData() {
    if (!(#url in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.formData(this.#body, this.#headers.get("content-type"));
  }

  json() {
    if (!(#url in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.json(this.#body);
  }

  text() {
    if (!(#url in this)) return Promise.reject(illegalInvocation());
    return bodyMethods.text(this.#body);
  }

  textStream() {
    if (!(#url in this)) throw illegalInvocation();
    return textStream(this.#body);
  }

  [kInspect](depth, options, inspect) {
    if (options.depth === null) options = { ...options, depth: 2 };
    const shown = {
      method: this.method,
      url: this.url,
      headers: this.headers,
      destination: this.destination,
      referrer: this.referrer,
      referrerPolicy: this.referrerPolicy,
      mode: this.mode,
      credentials: this.credentials,
      cache: this.cache,
      redirect: this.redirect,
      integrity: this.integrity,
      keepalive: this.keepalive,
      isReloadNavigation: this.isReloadNavigation,
      isHistoryNavigation: this.isHistoryNavigation,
      signal: this.signal,
    };
    return `Request ${inspect(shown, { ...options, depth: options.depth ?? depth })}`;
  }

  // for fetch
  static sent(request) {
    return { body: request.#body, headers: Headers.list(request.#headers) };
  }
}

Object.defineProperty(Request.prototype, Symbol.toStringTag, {
  value: "Request",
  configurable: true,
});
enumerable(Request.prototype, [
  "method",
  "url",
  "headers",
  "destination",
  "referrer",
  "referrerPolicy",
  "mode",
  "credentials",
  "cache",
  "redirect",
  "integrity",
  "keepalive",
  "isReloadNavigation",
  "isHistoryNavigation",
  "signal",
  "body",
  "bodyUsed",
  "duplex",
  "clone",
  "blob",
  "arrayBuffer",
  "text",
  "json",
  "formData",
  "bytes",
  "textStream",
]);
// node has this name on the prototype, holding nothing
Object.defineProperty(Request.prototype, "attribute", {
  value: undefined,
  writable: false,
  enumerable: true,
  configurable: false,
});

// ---- fetch

// ports a fetch may not go to, from the fetch standard
const BAD_PORTS = new Set([
  1, 7, 9, 11, 13, 15, 17, 19, 20, 21, 22, 23, 25, 37, 42, 43, 53, 69, 77, 79,
  87, 95, 101, 102, 103, 104, 109, 110, 111, 113, 115, 117, 119, 123, 135,
  137, 139, 143, 161, 179, 389, 427, 465, 512, 513, 514, 515, 526, 530, 531,
  532, 540, 548, 554, 556, 563, 587, 601, 636, 989, 990, 993, 995, 1719, 1720,
  1723, 2049, 3659, 4045, 4190, 5060, 5061, 6000, 6566, 6665, 6666, 6667,
  6668, 6669, 6679, 6697, 10080,
]);

function failed(message, properties) {
  const cause = Object.assign(new Error(message), properties);
  return new TypeError("fetch failed", { cause });
}

// a `data:` url is answered here; nothing is asked of the embedder
function dataResponse(url) {
  const text = url.href.slice(5);
  const comma = text.indexOf(",");
  if (comma === -1) throw failed("malformed data: URI");
  let type = text.slice(0, comma).trim();
  let body = decodeURIComponent(text.slice(comma + 1).replace(/#.*$/, ""));
  let bytes;
  if (/;\s*base64$/i.test(type)) {
    type = type.replace(/;\s*base64$/i, "");
    const { Buffer } = require("node:buffer");
    bytes = new Uint8Array(Buffer.from(body, "base64"));
  } else {
    bytes = Uint8Array.from(body, (c) => c.charCodeAt(0) & 0xff);
  }
  if (type === "" || type.startsWith(";")) type = `text/plain${type || ";charset=US-ASCII"}`;
  return Response.from(200, "OK", [["content-type", type]], streamOf(bytes), [url.href]);
}

// tells the embedder a body will be read no further; nothing waits on it
function drop(handle) {
  native.dropBody(handle).then(undefined, () => {});
}

// the body of a response, pulled from the embedder one piece at a time
function networkBody(handle, signal) {
  let finished = false;
  let cancel;
  const stream = new ReadableStream({
    start(controller) {
      cancel = () => {
        if (finished) return;
        finished = true;
        drop(handle);
        try {
          controller.error(signal.reason);
        } catch {
          // already closed or errored
        }
      };
      signal.addEventListener("abort", cancel, { once: true });
    },
    // an empty piece is not the end, and enqueuing nothing would leave a
    // reader waiting with no pull to come, so ask again
    async pull(controller) {
      for (;;) {
        let piece;
        try {
          piece = await native.readBodyLater(handle);
        } catch (thrown) {
          if (finished) return;
          finished = true;
          signal.removeEventListener("abort", cancel);
          controller.error(
            typeof thrown === "string"
              ? new TypeError("terminated", {
                  cause: Object.assign(new Error(thrown), { code: thrown }),
                })
              : thrown,
          );
          return;
        }
        if (finished) return;
        if (piece === undefined) {
          finished = true;
          signal.removeEventListener("abort", cancel);
          controller.close();
          return;
        }
        if (piece.byteLength > 0) {
          controller.enqueue(new Uint8Array(piece));
          return;
        }
      }
    },
    cancel() {
      if (finished) return;
      finished = true;
      signal.removeEventListener("abort", cancel);
      drop(handle);
    },
    type: "bytes",
  });
  return stream;
}

// a body sent packed is handed over unpacked, as the headers say it was packed
function unpacked(stream, pairs) {
  const packings = pairs
    .filter((pair) => pair[0].toLowerCase() === "content-encoding")
    .flatMap((pair) => pair[1].split(","))
    .map((name) => name.trim().toLowerCase())
    .filter((name) => name !== "" && name !== "identity");
  for (const packing of packings.reverse()) {
    if (packing === "gzip" || packing === "x-gzip") {
      stream = stream.pipeThrough(new DecompressionStream("gzip"));
    } else if (packing === "deflate") {
      stream = stream.pipeThrough(new DecompressionStream("deflate"));
    } else {
      // a packing not known here: the bytes are passed on as they came
      return stream;
    }
  }
  return stream;
}

const DEFAULT_HEADERS = [
  ["accept", "*/*"],
  ["accept-language", "*"],
  ["sec-fetch-mode", "cors"],
  ["user-agent", "node"],
  ["accept-encoding", "gzip, deflate"],
];

async function fetched(request) {
  const signal = request.signal;
  const sent = Request.sent(request);
  let url = new URL(request.url);
  let method = request.method;
  let headers = sent.headers.map((entry) => [...entry]);
  let body = null;
  if (sent.body !== null) {
    body = sent.body.source ?? (await allBytes(sent.body));
  }
  const urls = [url.href];

  for (let redirects = 0; ; redirects++) {
    if (url.protocol === "about:") throw failed("about scheme is not supported");
    if (url.protocol === "data:") return dataResponse(url);
    if (url.protocol !== "http:" && url.protocol !== "https:") {
      throw failed("unknown scheme");
    }
    if (url.port !== "" && BAD_PORTS.has(Number(url.port))) throw failed("bad port");

    const sending = headers.map((entry) => [...entry]);
    for (const [name, value] of DEFAULT_HEADERS) {
      if (!sending.some((entry) => entry[0] === name)) sending.push([name, value]);
    }
    const target = new URL(url.href);
    target.hash = "";

    let head;
    try {
      head = await native.fetchLater(
        method,
        target.href,
        body === null ? undefined : body,
        ...sending.flat(),
      );
    } catch (thrown) {
      if (typeof thrown !== "string") throw thrown;
      if (signal.aborted) throw signal.reason;
      throw failed(thrown, { code: thrown });
    }
    const [status, statusText, flat, handle] = head;
    if (signal.aborted) {
      drop(handle);
      throw signal.reason;
    }
    const pairs = [];
    for (let i = 0; i < flat.length; i += 2) pairs.push([flat[i], flat[i + 1]]);

    const location = pairs.find((pair) => pair[0].toLowerCase() === "location");
    if (
      REDIRECT_STATUS.includes(status) &&
      request.redirect !== "manual" &&
      (location !== undefined || request.redirect === "error")
    ) {
      drop(handle);
      if (request.redirect === "error") throw failed("unexpected redirect");
      {
        if (redirects >= 20) throw failed("redirect count exceeded");
        const next = new URL(location[1], url);
        if (next.protocol !== "http:" && next.protocol !== "https:") {
          throw failed("URL scheme must be a HTTP(S) scheme");
        }
        if (
          ((status === 301 || status === 302) && method === "POST") ||
          (status === 303 && method !== "GET" && method !== "HEAD")
        ) {
          method = "GET";
          body = null;
          headers = headers.filter((entry) => !entry[0].startsWith("content-"));
        }
        if (next.origin !== url.origin) {
          headers = headers.filter(
            (entry) =>
              !["authorization", "proxy-authorization", "cookie", "host"].includes(
                entry[0],
              ),
          );
        }
        url = next;
        urls.push(url.href);
        continue;
      }
    }

    const hasBody = method !== "HEAD" && !NULL_BODY_STATUS.includes(status);
    if (!hasBody) drop(handle);
    const stream = hasBody ? unpacked(networkBody(handle, signal), pairs) : null;
    return Response.from(status, statusText, pairs, stream, urls);
  }
}

function fetch(input, init = undefined) {
  let request;
  try {
    request = new Request(input, init);
  } catch (error) {
    return Promise.reject(error);
  }
  if (request.signal.aborted) return Promise.reject(request.signal.reason);
  return new Promise((resolve, reject) => {
    const aborted = () => reject(request.signal.reason);
    request.signal.addEventListener("abort", aborted, { once: true });
    fetched(request).then(
      (response) => {
        request.signal.removeEventListener("abort", aborted);
        resolve(response);
      },
      (error) => {
        request.signal.removeEventListener("abort", aborted);
        reject(error);
      },
    );
  });
}

module.exports = { fetch, Headers, Request, Response, FormData };
