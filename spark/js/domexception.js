"use strict";

const internals = new WeakMap();
const nameToCode = new Map();
const disusedNames = new Set([
  "DOMStringSizeError",
  "NoDataAllowedError",
  "ValidationError",
]);

function throwInvalidThis(type) {
  const error = new TypeError();
  Object.defineProperties(error, {
    message: {
      value: `Value of "this" must be of ${type}`,
      enumerable: false,
      writable: true,
      configurable: true,
    },
    toString: {
      value: function toString() {
        return `${this.name} [ERR_INVALID_THIS]: ${this.message}`;
      },
      enumerable: false,
      writable: true,
      configurable: true,
    },
  });
  error.code = "ERR_INVALID_THIS";
  throw error;
}

function internalsOf(exception) {
  const found = internals.get(exception);
  if (found === undefined) throwInvalidThis("DOMException");
  return found;
}

class DOMException {
  constructor(message = "", options = "Error") {
    const self = new Error();
    Object.setPrototypeOf(self, new.target.prototype);
    if (options && typeof options === "object") {
      const { name } = options;
      internals.set(self, { message: `${message}`, name: `${name}` });
      if ("cause" in options) {
        Object.defineProperty(self, "cause", {
          value: options.cause,
          configurable: true,
          writable: true,
          enumerable: false,
        });
      }
    } else {
      internals.set(self, { message: `${message}`, name: `${options}` });
    }
    return self;
  }

  get name() {
    return internalsOf(this).name;
  }

  get message() {
    return internalsOf(this).message;
  }

  get code() {
    const { name } = internalsOf(this);
    if (disusedNames.has(name)) return 0;
    const code = nameToCode.get(name);
    return code === undefined ? 0 : code;
  }
}

Object.setPrototypeOf(DOMException.prototype, Error.prototype);
Object.defineProperties(DOMException.prototype, {
  [Symbol.toStringTag]: { configurable: true, value: "DOMException" },
  name: { enumerable: true, configurable: true },
  message: { enumerable: true, configurable: true },
  code: { enumerable: true, configurable: true },
});

for (const [name, codeName, value] of [
  ["IndexSizeError", "INDEX_SIZE_ERR", 1],
  ["DOMStringSizeError", "DOMSTRING_SIZE_ERR", 2],
  ["HierarchyRequestError", "HIERARCHY_REQUEST_ERR", 3],
  ["WrongDocumentError", "WRONG_DOCUMENT_ERR", 4],
  ["InvalidCharacterError", "INVALID_CHARACTER_ERR", 5],
  ["NoDataAllowedError", "NO_DATA_ALLOWED_ERR", 6],
  ["NoModificationAllowedError", "NO_MODIFICATION_ALLOWED_ERR", 7],
  ["NotFoundError", "NOT_FOUND_ERR", 8],
  ["NotSupportedError", "NOT_SUPPORTED_ERR", 9],
  ["InUseAttributeError", "INUSE_ATTRIBUTE_ERR", 10],
  ["InvalidStateError", "INVALID_STATE_ERR", 11],
  ["SyntaxError", "SYNTAX_ERR", 12],
  ["InvalidModificationError", "INVALID_MODIFICATION_ERR", 13],
  ["NamespaceError", "NAMESPACE_ERR", 14],
  ["InvalidAccessError", "INVALID_ACCESS_ERR", 15],
  ["ValidationError", "VALIDATION_ERR", 16],
  ["TypeMismatchError", "TYPE_MISMATCH_ERR", 17],
  ["SecurityError", "SECURITY_ERR", 18],
  ["NetworkError", "NETWORK_ERR", 19],
  ["AbortError", "ABORT_ERR", 20],
  ["URLMismatchError", "URL_MISMATCH_ERR", 21],
  ["QuotaExceededError", "QUOTA_EXCEEDED_ERR", 22],
  ["TimeoutError", "TIMEOUT_ERR", 23],
  ["InvalidNodeTypeError", "INVALID_NODE_TYPE_ERR", 24],
  ["DataCloneError", "DATA_CLONE_ERR", 25],
]) {
  const descriptor = { enumerable: true, value };
  Object.defineProperty(DOMException, codeName, descriptor);
  Object.defineProperty(DOMException.prototype, codeName, descriptor);
  nameToCode.set(name, value);
}

class QuotaExceededError extends DOMException {
  #quota;
  #requested;

  constructor(message = "", options = {}) {
    super(message, "QuotaExceededError");

    let quota = null;
    let requested = null;

    if (options !== null && options !== undefined) {
      if ("quota" in options) {
        quota = +options.quota;
        if (!Number.isFinite(quota)) {
          throw new TypeError(
            `Cannot convert options.quota to a double: the value is ${Number.isNaN(quota) ? "NaN" : "Infinity"}`,
          );
        }
        if (quota < 0) {
          throw new RangeError("options.quota must not be negative");
        }
      }

      if ("requested" in options) {
        requested = +options.requested;
        if (!Number.isFinite(requested)) {
          throw new TypeError(
            `Cannot convert options.requested to a double: the value is ${Number.isNaN(requested) ? "NaN" : "Infinity"}`,
          );
        }
        if (requested < 0) {
          throw new RangeError("options.requested must not be negative");
        }
      }
    }

    if (quota !== null && requested !== null && requested < quota) {
      throw new RangeError(
        "options.requested must not be less than options.quota",
      );
    }

    this.#quota = quota;
    this.#requested = requested;
  }

  get quota() {
    if (!(#quota in this)) throwInvalidThis("QuotaExceededError");
    return this.#quota;
  }

  get requested() {
    if (!(#requested in this)) throwInvalidThis("QuotaExceededError");
    return this.#requested;
  }
}

Object.defineProperties(QuotaExceededError.prototype, {
  [Symbol.toStringTag]: { configurable: true, value: "QuotaExceededError" },
  quota: { enumerable: true, configurable: true },
  requested: { enumerable: true, configurable: true },
});

module.exports = { DOMException, QuotaExceededError };
