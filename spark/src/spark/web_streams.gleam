import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

/// the web's streams: ReadableStream, WritableStream, TransformStream, their
/// readers, writers, controllers and queuing strategies, the text encoder and
/// decoder streams, and the compression streams when zlib is passed too. Each
/// is a global and an export of node:stream/web
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:stream/web",
    source: js.web_streams,
    natives: fn(_io: Io) { [] },
    dependencies: [
      "spark:internal", "spark:errors", "spark:domexception", "spark:dom_events",
      "spark:encoding", "node:buffer", "node:events", "node:stream",
    ],
    optional: ["node:zlib", "spark:structured_clone"],
    exports: [
      "ReadableStream",
      "ReadableStreamTee",
      "ReadableStreamDefaultReader",
      "ReadableStreamBYOBReader",
      "ReadableStreamBYOBRequest",
      "ReadableByteStreamController",
      "ReadableStreamDefaultController",
      "TransformStream",
      "TransformStreamDefaultController",
      "WritableStream",
      "WritableStreamDefaultWriter",
      "WritableStreamDefaultController",
      "ByteLengthQueuingStrategy",
      "CountQueuingStrategy",
      "TextEncoderStream",
      "TextDecoderStream",
      "CompressionStream",
      "DecompressionStream",
    ],
    globals: [
      "ReadableStream",
      "ReadableStreamDefaultReader",
      "ReadableStreamBYOBReader",
      "ReadableStreamBYOBRequest",
      "ReadableByteStreamController",
      "ReadableStreamDefaultController",
      "TransformStream",
      "TransformStreamDefaultController",
      "WritableStream",
      "WritableStreamDefaultWriter",
      "WritableStreamDefaultController",
      "ByteLengthQueuingStrategy",
      "CountQueuingStrategy",
      "TextEncoderStream",
      "TextDecoderStream",
      "CompressionStream",
      "DecompressionStream",
    ],
  )
}
