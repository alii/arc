"use strict";

// the parts of node's net module that open nothing: isIP and friends,
// SocketAddress and BlockList. The address parsing and printing is libuv's
// inet_pton and inet_ntop; the matching rules are node's own

const {
  error,
  rangeError,
  invalidArgType,
  invalidArgValue,
  outOfRange,
  determineSpecificType,
  validateArray,
  validateObject,
  validateString,
} = require("spark:errors");

const v4Seg = "(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])";
const v4Str = `(?:${v4Seg}\\.){3}${v4Seg}`;
const IPv4Reg = new RegExp(`^${v4Str}$`);

const v6Seg = "(?:[0-9a-fA-F]{1,4})";
const IPv6Reg = new RegExp(
  "^(?:" +
    `(?:${v6Seg}:){7}(?:${v6Seg}|:)|` +
    `(?:${v6Seg}:){6}(?:${v4Str}|:${v6Seg}|:)|` +
    `(?:${v6Seg}:){5}(?::${v4Str}|(?::${v6Seg}){1,2}|:)|` +
    `(?:${v6Seg}:){4}(?:(?::${v6Seg}){0,1}:${v4Str}|(?::${v6Seg}){1,3}|:)|` +
    `(?:${v6Seg}:){3}(?:(?::${v6Seg}){0,2}:${v4Str}|(?::${v6Seg}){1,4}|:)|` +
    `(?:${v6Seg}:){2}(?:(?::${v6Seg}){0,3}:${v4Str}|(?::${v6Seg}){1,5}|:)|` +
    `(?:${v6Seg}:){1}(?:(?::${v6Seg}){0,4}:${v4Str}|(?::${v6Seg}){1,6}|:)|` +
    `(?::(?:(?::${v6Seg}){0,5}:${v4Str}|(?::${v6Seg}){1,7}|:))` +
    ")(?:%[0-9a-zA-Z-.:]{1,})?$",
);

function isIPv4(s) {
  return IPv4Reg.test(s);
}

function isIPv6(s) {
  if (typeof s === "string") {
    if (s.length < 2 || s.indexOf(":") === -1) return false;
  }
  return IPv6Reg.test(s);
}

function isIP(s) {
  if (isIPv4(s)) return 4;
  if (isIPv6(s)) return 6;
  return 0;
}

const AF_INET = 4;
const AF_INET6 = 6;

// four bytes, or undefined: no leading zeros, exactly four parts
function parseIPv4(text) {
  const bytes = [0];
  let sawDigit = false;
  let octets = 0;
  for (let i = 0; i < text.length; i++) {
    const ch = text.charCodeAt(i);
    if (ch >= 48 && ch <= 57) {
      const at = bytes.length - 1;
      const value = bytes[at] * 10 + (ch - 48);
      if (sawDigit && bytes[at] === 0) return undefined;
      if (value > 255) return undefined;
      bytes[at] = value;
      if (!sawDigit) {
        if (++octets > 4) return undefined;
        sawDigit = true;
      }
    } else if (ch === 46 && sawDigit) {
      if (octets === 4) return undefined;
      bytes.push(0);
      sawDigit = false;
    } else {
      return undefined;
    }
  }
  if (octets < 4) return undefined;
  return bytes;
}

function hexValue(ch) {
  if (ch >= 48 && ch <= 57) return ch - 48;
  if (ch >= 97 && ch <= 102) return ch - 87;
  if (ch >= 65 && ch <= 70) return ch - 55;
  return -1;
}

// sixteen bytes, or undefined; anything from a '%' on is a zone and ignored
function parseIPv6(text) {
  const zone = text.indexOf("%");
  if (zone !== -1) text = text.slice(0, zone);
  const bytes = [];
  let gap = -1;
  let i = 0;
  if (text.charCodeAt(0) === 58) {
    if (text.charCodeAt(1) !== 58) return undefined;
    i = 1;
  }
  let token = i;
  let digits = 0;
  let value = 0;
  let endedByQuad = false;
  while (i < text.length) {
    const ch = text.charCodeAt(i++);
    const digit = hexValue(ch);
    if (digit !== -1) {
      value = (value << 4) | digit;
      if (++digits > 4) return undefined;
      continue;
    }
    if (ch === 58) {
      token = i;
      if (digits === 0) {
        if (gap !== -1) return undefined;
        gap = bytes.length;
        continue;
      }
      if (i === text.length) return undefined;
      if (bytes.length + 2 > 16) return undefined;
      bytes.push((value >> 8) & 0xff, value & 0xff);
      digits = 0;
      value = 0;
      continue;
    }
    if (ch === 46 && bytes.length + 4 <= 16) {
      const quad = parseIPv4(text.slice(token));
      if (quad !== undefined) {
        bytes.push(...quad);
        digits = 0;
        endedByQuad = true;
        break;
      }
    }
    return undefined;
  }
  if (!endedByQuad && digits !== 0) {
    if (bytes.length + 2 > 16) return undefined;
    bytes.push((value >> 8) & 0xff, value & 0xff);
  }
  if (gap !== -1) {
    if (bytes.length === 16) return undefined;
    const tail = bytes.splice(gap);
    while (bytes.length + tail.length < 16) bytes.push(0);
    bytes.push(...tail);
  }
  if (bytes.length !== 16) return undefined;
  return bytes;
}

function formatIPv6(bytes) {
  const words = [];
  for (let i = 0; i < 16; i += 2) words.push((bytes[i] << 8) | bytes[i + 1]);
  // the longest run of zero words becomes "::"
  let bestBase = -1;
  let bestLen = 0;
  let curBase = -1;
  let curLen = 0;
  for (let i = 0; i < 8; i++) {
    if (words[i] === 0) {
      if (curBase === -1) {
        curBase = i;
        curLen = 1;
      } else {
        curLen++;
      }
    } else if (curBase !== -1) {
      if (bestBase === -1 || curLen > bestLen) {
        bestBase = curBase;
        bestLen = curLen;
      }
      curBase = -1;
    }
  }
  if (curBase !== -1 && (bestBase === -1 || curLen > bestLen)) {
    bestBase = curBase;
    bestLen = curLen;
  }
  if (bestBase !== -1 && bestLen < 2) bestBase = -1;

  let text = "";
  for (let i = 0; i < 8; i++) {
    if (bestBase !== -1 && i >= bestBase && i < bestBase + bestLen) {
      if (i === bestBase) text += ":";
      continue;
    }
    if (i !== 0) text += ":";
    // an address that wraps an ipv4 one ends in dotted form
    if (
      i === 6 &&
      bestBase === 0 &&
      (bestLen === 6 ||
        (bestLen === 7 && words[7] !== 0x0001) ||
        (bestLen === 5 && words[5] === 0xffff))
    ) {
      return text + bytes.slice(12).join(".");
    }
    text += words[i].toString(16);
  }
  if (bestBase !== -1 && bestBase + bestLen === 8) text += ":";
  return text;
}

function parseAddress(text, family) {
  return family === AF_INET ? parseIPv4(text) : parseIPv6(text);
}

function formatAddress(handle) {
  return handle.family === AF_INET
    ? handle.bytes.join(".")
    : formatIPv6(handle.bytes);
}

function familyName(handle) {
  return handle.family === AF_INET ? "IPv4" : "IPv6";
}

function sameBytes(a, b, count, aFrom = 0, bFrom = 0) {
  for (let i = 0; i < count; i++) {
    if (a[aFrom + i] !== b[bFrom + i]) return false;
  }
  return true;
}

// -1, 0 or 1 over `count` bytes
function orderBytes(a, b, count, aFrom = 0, bFrom = 0) {
  for (let i = 0; i < count; i++) {
    const d = a[aFrom + i] - b[bFrom + i];
    if (d !== 0) return d < 0 ? -1 : 1;
  }
  return 0;
}

const mappedPrefix = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff];

function isMapped(v6) {
  return sameBytes(v6, mappedPrefix, 12);
}

function isMatch(a, b) {
  if (a.family === b.family) {
    return sameBytes(a.bytes, b.bytes, a.bytes.length);
  }
  const v4 = a.family === AF_INET ? a : b;
  const v6 = a.family === AF_INET ? b : a;
  return isMapped(v6.bytes) && sameBytes(v6.bytes, v4.bytes, 4, 12, 0);
}

// undefined when an ipv4 address meets an ipv6 one that does not wrap one
function compare(a, b) {
  if (a.family === b.family) {
    return orderBytes(a.bytes, b.bytes, a.bytes.length);
  }
  if (a.family === AF_INET) {
    if (!isMapped(b.bytes)) return undefined;
    return orderBytes(a.bytes, b.bytes, 4, 0, 12);
  }
  if (!isMapped(a.bytes)) return undefined;
  return orderBytes(a.bytes, b.bytes, 4, 12, 0);
}

function prefixMatches(a, b, prefix) {
  const whole = prefix >> 3;
  const rest = prefix & 7;
  if (!sameBytes(a, b, whole)) return false;
  if (rest === 0) return true;
  const mask = (0xff << (8 - rest)) & 0xff;
  return (a[whole] & mask) === (b[whole] & mask);
}

function inNetwork(address, network, prefix) {
  if (address.family === network.family) {
    return prefixMatches(address.bytes, network.bytes, prefix);
  }
  if (address.family === AF_INET) {
    return prefixMatches(
      [...mappedPrefix, ...address.bytes],
      network.bytes,
      prefix,
    );
  }
  if (!isMapped(address.bytes)) return false;
  return prefixMatches(address.bytes.slice(12), network.bytes, prefix);
}

function invalidAddress() {
  return error("ERR_INVALID_ADDRESS", "Invalid socket address");
}

function validatePort(port, name = "Port", allowZero = true) {
  if (
    (typeof port !== "number" && typeof port !== "string") ||
    (typeof port === "string" && port.trim().length === 0) ||
    +port !== +port >>> 0 ||
    port > 0xffff ||
    (port === 0 && !allowZero)
  ) {
    const operator = allowZero ? ">=" : ">";
    throw rangeError(
      "ERR_SOCKET_BAD_PORT",
      `${name} should be ${operator} 0 and < 65536. Received ${determineSpecificType(port)}.`,
    );
  }
  return port | 0;
}

function validateUint32(value, name, positive = false) {
  if (typeof value !== "number") throw invalidArgType(name, "number", value);
  if (!Number.isInteger(value)) throw outOfRange(name, "an integer", value);
  const min = positive ? 1 : 0;
  const max = 4294967295;
  if (value < min || value > max) {
    throw outOfRange(name, `>= ${min} && <= ${max}`, value);
  }
}

function validateInt32(value, name, min = -2147483648, max = 2147483647) {
  if (typeof value !== "number") throw invalidArgType(name, "number", value);
  if (!Number.isInteger(value)) throw outOfRange(name, "an integer", value);
  if (value < min || value > max) {
    throw outOfRange(name, `>= ${min} && <= ${max}`, value);
  }
}

const kSocketAddressHandle = Symbol("kHandle");

class SocketAddress {
  static isSocketAddress(value) {
    return value?.[kSocketAddressHandle] !== undefined;
  }

  constructor(options = {}) {
    validateObject(options, "options");
    let { family = "ipv4" } = options;
    const {
      address = family === "ipv4" ? "127.0.0.1" : "::",
      port = 0,
      flowlabel = 0,
    } = options;

    let type;
    if (typeof family?.toLowerCase === "function") {
      family = family.toLowerCase();
    }
    switch (family) {
      case "ipv4":
        type = AF_INET;
        break;
      case "ipv6":
        type = AF_INET6;
        break;
      default:
        throw invalidArgValue("options.family", options.family);
    }

    validateString(address, "options.address");
    validatePort(port, "options.port");
    validateUint32(flowlabel, "options.flowlabel", false);

    const bytes = parseAddress(address, type);
    if (bytes === undefined) throw invalidAddress();
    this[kSocketAddressHandle] = {
      family: type,
      bytes,
      port: port | 0,
      flowlabel: type === AF_INET6 ? flowlabel | 0 : 0,
    };
  }

  get address() {
    return formatAddress(this[kSocketAddressHandle]);
  }

  get port() {
    return this[kSocketAddressHandle].port;
  }

  get family() {
    return this[kSocketAddressHandle].family === AF_INET ? "ipv4" : "ipv6";
  }

  get flowlabel() {
    return this[kSocketAddressHandle].flowlabel;
  }

  toJSON() {
    return {
      address: this.address,
      port: this.port,
      family: this.family,
      flowlabel: this.flowlabel,
    };
  }

  // node reads the input as a url, which is not here yet
  static parse(input) {
    validateString(input, "input");
    throw error(
      "ERR_METHOD_NOT_IMPLEMENTED",
      "The parse() method is not implemented",
    );
  }
}

function parseCIDR(cidr) {
  validateString(cidr, "cidr");
  const slash = cidr.lastIndexOf("/");
  if (slash === -1) {
    throw invalidArgValue(
      "cidr",
      cidr,
      'must contain a prefix length (e.g. "10.0.0.0/8")',
    );
  }
  const address = cidr.slice(0, slash);
  const prefixStr = cidr.slice(slash + 1);
  const prefix = Number.parseInt(prefixStr, 10);
  if (Number.isNaN(prefix) || `${prefix}` !== prefixStr) {
    throw invalidArgValue(
      "cidr",
      cidr,
      "prefix length must be a valid integer",
    );
  }
  const family = address.includes(":") ? "ipv6" : "ipv4";
  return { address, prefix, family };
}

function addressKey(handle) {
  return `${handle.family} ${handle.bytes.join(".")}`;
}

function toHandle(value, name, family) {
  if (SocketAddress.isSocketAddress(value)) return value;
  validateString(value, name);
  validateString(family, "family");
  return new SocketAddress({ address: value, family });
}

function validatePrefix(network, prefix) {
  switch (network.family) {
    case "ipv4":
      validateInt32(prefix, "prefix", 0, 32);
      break;
    case "ipv6":
      validateInt32(prefix, "prefix", 0, 128);
      break;
  }
}

const kHandle = Symbol("kHandle");

class BlockList {
  constructor() {
    // newest first in each: addresses by key, then subnets, then ranges
    this[kHandle] = { addresses: new Map(), subnets: [], ranges: [] };
  }

  static isBlockList(value) {
    return value?.[kHandle] !== undefined;
  }

  static PRIVATE_RANGES = Object.freeze([
    "10.0.0.0/8",
    "172.16.0.0/12",
    "192.168.0.0/16",
    "127.0.0.0/8",
    "::1/128",
    "169.254.0.0/16",
    "fe80::/10",
    "fc00::/7",
  ]);

  addAddress(address, family = "ipv4") {
    address = toHandle(address, "address", family);
    const handle = address[kSocketAddressHandle];
    const { addresses } = this[kHandle];
    const key = addressKey(handle);
    addresses.delete(key);
    addresses.set(key, handle);
  }

  addAddresses(addresses, family = "ipv4") {
    validateArray(addresses, "addresses");
    validateString(family, "family");
    const handles = [];
    for (let i = 0; i < addresses.length; i++) {
      let address = addresses[i];
      if (!SocketAddress.isSocketAddress(address)) {
        validateString(address, `addresses[${i}]`);
        address = new SocketAddress({ address, family });
      }
      handles.push(address[kSocketAddressHandle]);
    }
    for (const handle of handles) {
      const key = addressKey(handle);
      this[kHandle].addresses.delete(key);
      this[kHandle].addresses.set(key, handle);
    }
  }

  addRange(start, end, family = "ipv4") {
    start = toHandle(start, "start", family);
    end = toHandle(end, "end", family);
    const from = start[kSocketAddressHandle];
    const to = end[kSocketAddressHandle];
    if (compare(from, to) === 1) {
      throw invalidArgValue("start", start, "must come before end");
    }
    this[kHandle].ranges.unshift({ start: from, end: to });
  }

  addSubnet(network, prefix, family = "ipv4") {
    network = toHandle(network, "network", family);
    validatePrefix(network, prefix);
    prefix += 0;
    this[kHandle].subnets.unshift({
      network: network[kSocketAddressHandle],
      prefix,
    });
  }

  addCIDR(cidr) {
    const { address, prefix, family } = parseCIDR(cidr);
    this.addSubnet(address, prefix, family);
  }

  addCIDRs(cidrs) {
    validateArray(cidrs, "cidrs");
    const parsed = [];
    for (let i = 0; i < cidrs.length; i++) {
      validateString(cidrs[i], `cidrs[${i}]`);
      parsed.push(parseCIDR(cidrs[i]));
    }
    for (let i = 0; i < parsed.length; i++) {
      const { address, prefix, family } = parsed[i];
      this.addSubnet(address, prefix, family);
    }
  }

  removeAddress(address, family = "ipv4") {
    address = toHandle(address, "address", family);
    this[kHandle].addresses.delete(addressKey(address[kSocketAddressHandle]));
  }

  removeRange(start, end, family = "ipv4") {
    start = toHandle(start, "start", family);
    end = toHandle(end, "end", family);
    const from = addressKey(start[kSocketAddressHandle]);
    const to = addressKey(end[kSocketAddressHandle]);
    const { ranges } = this[kHandle];
    const at = ranges.findIndex(
      (range) => addressKey(range.start) === from && addressKey(range.end) === to,
    );
    if (at !== -1) ranges.splice(at, 1);
  }

  removeSubnet(network, prefix, family = "ipv4") {
    network = toHandle(network, "network", family);
    validatePrefix(network, prefix);
    prefix += 0;
    const key = addressKey(network[kSocketAddressHandle]);
    const { subnets } = this[kHandle];
    const at = subnets.findIndex(
      (subnet) => addressKey(subnet.network) === key && subnet.prefix === prefix,
    );
    if (at !== -1) subnets.splice(at, 1);
  }

  removeCIDR(cidr) {
    const { address, prefix, family } = parseCIDR(cidr);
    this.removeSubnet(address, prefix, family);
  }

  check(address, family = "ipv4") {
    let handle;
    if (!SocketAddress.isSocketAddress(address)) {
      validateString(address, "address");
      validateString(family, "family");
      let af;
      if (family === "ipv4" || family === "IPv4") {
        af = AF_INET;
      } else if (family === "ipv6" || family === "IPv6") {
        af = AF_INET6;
      } else {
        af = family.toLowerCase() === "ipv4" ? AF_INET : AF_INET6;
      }
      const bytes = parseAddress(address, af);
      if (bytes === undefined) return false;
      handle = { family: af, bytes };
    } else {
      handle = address[kSocketAddressHandle];
    }
    const { addresses, subnets, ranges } = this[kHandle];
    for (const rule of addresses.values()) {
      if (isMatch(rule, handle)) return true;
    }
    for (const { network, prefix } of subnets) {
      if (inNetwork(handle, network, prefix)) return true;
    }
    for (const { start, end } of ranges) {
      const low = compare(handle, start);
      const high = compare(handle, end);
      if (low !== undefined && high !== undefined && low >= 0 && high <= 0) {
        return true;
      }
    }
    return false;
  }

  clear() {
    this[kHandle] = { addresses: new Map(), subnets: [], ranges: [] };
  }

  #parseIPInfo(data) {
    for (const item of data) {
      if (item.includes("IPv4")) {
        const subnetMatch = item.match(
          /Subnet: IPv4 (\d{1,3}(?:\.\d{1,3}){3})\/(\d{1,2})/,
        );
        if (subnetMatch) {
          const { 1: network, 2: prefix } = subnetMatch;
          this.addSubnet(network, Number.parseInt(prefix));
          continue;
        }
        const addressMatch = item.match(
          /Address: IPv4 (\d{1,3}(?:\.\d{1,3}){3})/,
        );
        if (addressMatch) {
          const { 1: address } = addressMatch;
          this.addAddress(address);
          continue;
        }

        const rangeMatch = item.match(
          /Range: IPv4 (\d{1,3}(?:\.\d{1,3}){3})-(\d{1,3}(?:\.\d{1,3}){3})/,
        );
        if (rangeMatch) {
          const { 1: start, 2: end } = rangeMatch;
          this.addRange(start, end);
          continue;
        }
      }
      // IPv6 parsing with support for compressed addresses
      if (item.includes("IPv6")) {
        // IPv6 subnet pattern: supports both full and compressed formats
        const ipv6SubnetMatch = item.match(
          /Subnet: IPv6 ([0-9a-fA-F:.]{1,45})\/([0-9]{1,3})/i,
        );
        if (ipv6SubnetMatch) {
          const { 1: network, 2: prefix } = ipv6SubnetMatch;
          this.addSubnet(network, Number.parseInt(prefix), "ipv6");
          continue;
        }

        const ipv6AddressMatch = item.match(
          /Address: IPv6 ([0-9a-fA-F:.]{1,45})/i,
        );
        if (ipv6AddressMatch) {
          const { 1: address } = ipv6AddressMatch;
          this.addAddress(address, "ipv6");
          continue;
        }

        const ipv6RangeMatch = item.match(
          /Range: IPv6 ([0-9a-fA-F:.]{1,45})-([0-9a-fA-F:.]{1,45})/i,
        );
        if (ipv6RangeMatch) {
          const { 1: start, 2: end } = ipv6RangeMatch;
          this.addRange(start, end, "ipv6");
          continue;
        }
      }
    }
  }

  toJSON() {
    return this.rules;
  }

  fromJSON(data) {
    // The data argument must be a string, or an array of strings that
    // is JSON parseable.
    if (Array.isArray(data)) {
      for (const n of data) {
        if (typeof n !== "string") {
          throw invalidArgType("data", ["string", "string[]"], data);
        }
      }
    } else if (typeof data !== "string") {
      throw invalidArgType("data", ["string", "string[]"], data);
    } else {
      data = JSON.parse(data);
      if (!Array.isArray(data)) {
        throw invalidArgType("data", ["string", "string[]"], data);
      }
      for (const n of data) {
        if (typeof n !== "string") {
          throw invalidArgType("data", ["string", "string[]"], data);
        }
      }
    }

    this.#parseIPInfo(data);
  }

  get rules() {
    const { addresses, subnets, ranges } = this[kHandle];
    const rules = [];
    for (const handle of [...addresses.values()].reverse()) {
      rules.push(`Address: ${familyName(handle)} ${formatAddress(handle)}`);
    }
    for (const { network, prefix } of subnets) {
      rules.push(
        `Subnet: ${familyName(network)} ${formatAddress(network)}/${prefix}`,
      );
    }
    for (const { start, end } of ranges) {
      rules.push(
        `Range: ${familyName(start)} ${formatAddress(start)}-${formatAddress(end)}`,
      );
    }
    return rules;
  }

  get size() {
    const { addresses, subnets, ranges } = this[kHandle];
    return addresses.size + subnets.length + ranges.length;
  }
}

module.exports = { BlockList, SocketAddress, isIP, isIPv4, isIPv6 };
