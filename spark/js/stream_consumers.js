"use strict";

// the stream module holds this file, so we load it to make the file findable
require("node:stream");
const consumers = require("spark:internal").find("stream/consumers");

module.exports = {
  ...consumers,
  // needs a real Blob
  blob() {
    const { codes } = require("spark:internal").load("internal/errors");
    return Promise.reject(new codes.ERR_METHOD_NOT_IMPLEMENTED("blob"));
  },
};
