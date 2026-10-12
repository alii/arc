"use strict";

const { codes, invalidArgValue, validateString } = require("spark:errors");

const given = native.certificates();
const bundled = Object.freeze(given.bundled);
const system = Object.freeze(given.system);
const extra = Object.freeze(given.extra);
// what a connection trusts when it names no certificates of its own
const byDefault = Object.freeze([...given.bundled, ...given.extra]);

function getCACertificates(type = "default") {
  validateString(type, "type");
  switch (type) {
    case "default":
      return byDefault;
    case "bundled":
      return bundled;
    case "system":
      return system;
    case "extra":
      return extra;
    default:
      throw invalidArgValue("type", type);
  }
}

// connections are not here yet; each of these says so when called
function missing(name) {
  return function () {
    throw new codes.ERR_METHOD_NOT_IMPLEMENTED(`tls.${name}`);
  };
}

module.exports = {
  CLIENT_RENEG_LIMIT: 3,
  CLIENT_RENEG_WINDOW: 600,
  DEFAULT_ECDH_CURVE: "auto",
  DEFAULT_MIN_VERSION: "TLSv1.2",
  DEFAULT_MAX_VERSION: "TLSv1.3",
  rootCertificates: bundled,
  getCACertificates,
  setDefaultCACertificates: missing("setDefaultCACertificates"),
  checkServerIdentity: missing("checkServerIdentity"),
  createSecureContext: missing("createSecureContext"),
  SecureContext: missing("SecureContext"),
  TLSSocket: missing("TLSSocket"),
  Server: missing("Server"),
  createServer: missing("createServer"),
  connect: missing("connect"),
};
