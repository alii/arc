import arc/host.{type Context, HostMethod}
import arc/rt/types.{type JsVal, mk_bool, mk_int, mk_string}
import gleam/option.{type Option, None, Some}
import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

type Format {
  Deflate
  Gzip
  Raw
  Either
}

/// node's zlib: deflate, inflate, gzip, gunzip, their raw forms, unzip and
/// zstd, as one-shot calls and as streams. It is node's own javascript over
/// the compression the vm it runs on carries; there is no brotli yet. A
/// stream keeps what it was given and works it out again at each flush, so a
/// long stream with many flushes is slow
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:zlib",
    source: js.zlib,
    natives: fn(_io: Io) {
      [
        // format, level, windowBits, memLevel, strategy, dictionary, then
        // every write so far as bytes, flush, bytes, flush, ...
        HostMethod("deflate", 6, fn(ctx, args, _this) {
          case args {
            [format, level, bits, mem, strategy, dictionary, ..writes] -> {
              use format, ctx <- format_of(ctx, format)
              use level, ctx <- host.validate_integer(
                ctx,
                level,
                "level",
                -1,
                9,
              )
              use bits, ctx <- host.validate_integer(ctx, bits, "bits", 8, 15)
              use mem, ctx <- host.validate_integer(ctx, mem, "mem", 1, 9)
              use strategy, ctx <- host.validate_integer(
                ctx,
                strategy,
                "strategy",
                0,
                4,
              )
              case
                deflate(
                  ops(ctx, writes),
                  format,
                  level,
                  bits,
                  mem,
                  strategy,
                  host.bytes(ctx, dictionary),
                )
              {
                Ok(out) -> {
                  let #(buffer, ctx) = host.array_buffer(ctx, out)
                  #(Ok(buffer), ctx)
                }
                Error(code) -> #(Error(mk_string(code)), ctx)
              }
            }
            _ -> host.type_error(ctx, "deflate takes six arguments and writes")
          }
        }),
        // bytes, format, windowBits, dictionary -> [output, reached the end]
        HostMethod("inflate", 4, fn(ctx, args, _this) {
          use data, ctx <- bytes_of(ctx, host.arg_at(args, 0))
          use format, ctx <- format_of(ctx, host.arg_at(args, 1))
          use bits, ctx <- host.validate_integer(
            ctx,
            host.arg_at(args, 2),
            "bits",
            0,
            15,
          )
          partial(
            ctx,
            inflate(data, format, bits, host.bytes(ctx, host.arg_at(args, 3))),
          )
        }),
        HostMethod("zstdCompress", 2, fn(ctx, args, _this) {
          use data, ctx <- bytes_of(ctx, host.arg_at(args, 0))
          let level = case types.classify(host.arg_at(args, 1)) {
            types.KNum(types.JInt(n)) -> Some(n)
            _ -> None
          }
          case zstd_compress(data, level) {
            Ok(out) -> {
              let #(buffer, ctx) = host.array_buffer(ctx, out)
              #(Ok(buffer), ctx)
            }
            Error(message) -> #(Error(mk_string(message)), ctx)
          }
        }),
        // bytes -> [output, whether the frame was all there]
        HostMethod("zstdDecompress", 1, fn(ctx, args, _this) {
          use data, ctx <- bytes_of(ctx, host.arg_at(args, 0))
          partial(ctx, zstd_decompress(data))
        }),
        HostMethod("crc32", 2, fn(ctx, args, _this) {
          use data, ctx <- bytes_of(ctx, host.arg_at(args, 0))
          use initial, ctx <- host.validate_integer(
            ctx,
            host.arg_at(args, 1),
            "value",
            0,
            4_294_967_295,
          )
          #(Ok(mk_int(crc32(data, initial))), ctx)
        }),
      ]
    },
    dependencies: ["spark:internal", "spark:loop", "node:buffer", "node:stream"],
    optional: [],
    exports: [
      "crc32",
      "Deflate",
      "Inflate",
      "Gzip",
      "Gunzip",
      "DeflateRaw",
      "InflateRaw",
      "Unzip",
      "BrotliCompress",
      "BrotliDecompress",
      "ZstdCompress",
      "ZstdDecompress",
      "deflate",
      "deflateSync",
      "gzip",
      "gzipSync",
      "deflateRaw",
      "deflateRawSync",
      "unzip",
      "unzipSync",
      "inflate",
      "inflateSync",
      "gunzip",
      "gunzipSync",
      "inflateRaw",
      "inflateRawSync",
      "brotliCompress",
      "brotliCompressSync",
      "brotliDecompress",
      "brotliDecompressSync",
      "zstdCompress",
      "zstdCompressSync",
      "zstdDecompress",
      "zstdDecompressSync",
      "createDeflate",
      "createInflate",
      "createDeflateRaw",
      "createInflateRaw",
      "createGzip",
      "createGunzip",
      "createUnzip",
      "createBrotliCompress",
      "createBrotliDecompress",
      "createZstdCompress",
      "createZstdDecompress",
      "constants",
      "codes",
      "ZipEntry",
      "ZipFile",
      "ZipBuffer",
      "createZipArchive",
      "createZipArchiveSync",
      "zipFiles",
      "getMaxZipContentSize",
      "setMaxZipContentSize",
    ],
    globals: [],
  )
}

fn partial(
  ctx: Context(host),
  result: Result(#(BitArray, Bool), String),
) -> #(Result(JsVal, JsVal), Context(host)) {
  case result {
    Ok(#(out, finished)) -> {
      let #(buffer, ctx) = host.array_buffer(ctx, out)
      let #(pair, ctx) = host.array(ctx, [buffer, mk_bool(finished)])
      #(Ok(pair), ctx)
    }
    Error(code) -> #(Error(mk_string(code)), ctx)
  }
}

fn bytes_of(ctx: Context(host), value: JsVal, next) {
  case host.bytes(ctx, value) {
    Some(bytes) -> next(bytes, ctx)
    None -> host.type_error(ctx, "expected bytes")
  }
}

fn format_of(ctx: Context(host), value: JsVal, next) {
  case types.classify(value) {
    types.KStr("deflate") -> next(Deflate, ctx)
    types.KStr("gzip") -> next(Gzip, ctx)
    types.KStr("raw") -> next(Raw, ctx)
    types.KStr("either") -> next(Either, ctx)
    _ -> host.type_error(ctx, "unknown format")
  }
}

// bytes, flush, bytes, flush, ... as pairs; a pair that is not that is left out
fn ops(ctx: Context(host), writes: List(JsVal)) -> List(#(BitArray, Int)) {
  case writes {
    [bytes, flush, ..rest] ->
      case host.bytes(ctx, bytes), types.classify(flush) {
        Some(bytes), types.KNum(types.JInt(flush)) -> [
          #(bytes, flush),
          ..ops(ctx, rest)
        ]
        _, _ -> ops(ctx, rest)
      }
    _ -> []
  }
}

@external(erlang, "spark_zlib_ffi", "deflate")
fn deflate(
  ops: List(#(BitArray, Int)),
  format: Format,
  level: Int,
  window_bits: Int,
  mem_level: Int,
  strategy: Int,
  dictionary: Option(BitArray),
) -> Result(BitArray, String)

@external(erlang, "spark_zlib_ffi", "inflate")
fn inflate(
  data: BitArray,
  format: Format,
  window_bits: Int,
  dictionary: Option(BitArray),
) -> Result(#(BitArray, Bool), String)

@external(erlang, "spark_zlib_ffi", "zstd_compress")
fn zstd_compress(data: BitArray, level: Option(Int)) -> Result(BitArray, String)

@external(erlang, "spark_zlib_ffi", "zstd_decompress")
fn zstd_decompress(data: BitArray) -> Result(#(BitArray, Bool), String)

@external(erlang, "spark_zlib_ffi", "crc32")
fn crc32(data: BitArray, initial: Int) -> Int
