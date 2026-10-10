import arc/host.{type Context, HostMethod}
import arc/rt/types.{
  type JsVal, JInt, KNum, KStr, classify, mk_bool, mk_int, mk_string,
}
import gleam/bit_array
import gleam/option.{None, Some}
import spark/builtin.{type Builtin, Builtin}
import spark/js

// the names double as the atoms the erlang side matches on
type Encoding {
  Utf8
  Utf16le
  Latin1
  Ascii
  Base64
  Base64url
  Hex
}

type AtobError {
  InvalidCharacter
  BadLength
}

@external(erlang, "spark_buffer_ffi", "encode")
fn encode(text: String, encoding: Encoding, max: Int) -> BitArray

@external(erlang, "spark_buffer_ffi", "decode")
fn decode(bytes: BitArray, encoding: Encoding) -> String

@external(erlang, "spark_buffer_ffi", "compare")
fn compare(a: BitArray, b: BitArray) -> Int

@external(erlang, "spark_buffer_ffi", "index_of")
fn index_of(
  hay: BitArray,
  needle: BitArray,
  offset: Int,
  forward: Bool,
  step: Int,
) -> Int

@external(erlang, "spark_buffer_ffi", "swap")
fn swap(bytes: BitArray, bits: Int) -> BitArray

@external(erlang, "spark_buffer_ffi", "atob")
fn atob(text: String) -> Result(String, AtobError)

@external(erlang, "spark_buffer_ffi", "is_ascii")
fn is_ascii(bytes: BitArray) -> Bool

@external(erlang, "spark_buffer_ffi", "is_latin1")
fn is_latin1(text: String) -> Bool

@external(erlang, "spark_buffer_ffi", "utf16_units")
fn utf16_units(bytes: BitArray) -> Int

const no_limit = 9_007_199_254_740_991

type Reply(host) =
  #(Result(JsVal, JsVal), Context(host))

fn encoding_arg(
  ctx: Context(host),
  val: JsVal,
  cont: fn(Encoding, Context(host)) -> Reply(host),
) -> Reply(host) {
  case classify(val) {
    KStr("utf8") -> cont(Utf8, ctx)
    KStr("utf16le") -> cont(Utf16le, ctx)
    KStr("latin1") -> cont(Latin1, ctx)
    KStr("ascii") -> cont(Ascii, ctx)
    KStr("base64") -> cont(Base64, ctx)
    KStr("base64url") -> cont(Base64url, ctx)
    KStr("hex") -> cont(Hex, ctx)
    _ -> host.type_error(ctx, "buffer: not an encoding")
  }
}

fn bytes_arg(
  ctx: Context(host),
  val: JsVal,
  cont: fn(BitArray, Context(host)) -> Reply(host),
) -> Reply(host) {
  case host.bytes(ctx, val) {
    Some(bytes) -> cont(bytes, ctx)
    None -> host.type_error(ctx, "buffer: not a live buffer or view")
  }
}

fn int_arg(
  ctx: Context(host),
  val: JsVal,
  cont: fn(Int, Context(host)) -> Reply(host),
) -> Reply(host) {
  host.validate_integer(ctx, val, "index", -no_limit, no_limit, cont)
}

/// Buffer, atob and btoa. The byte work is native; the rest is node's own
/// javascript
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:buffer",
    source: js.buffer,
    natives: fn(_io) {
      [
        // (text, encoding, most bytes or undefined) -> ArrayBuffer
        HostMethod("encode", 3, fn(ctx, args, _this) {
          use text, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "string",
          )
          use encoding, ctx <- encoding_arg(ctx, host.arg_at(args, 1))
          let max = case classify(host.arg_at(args, 2)) {
            KNum(JInt(n)) -> n
            _ -> no_limit
          }
          let #(buffer, ctx) =
            host.array_buffer(ctx, encode(text, encoding, max))
          #(Ok(buffer), ctx)
        }),
        // (view, encoding, start, end) -> string
        HostMethod("decode", 4, fn(ctx, args, _this) {
          use bytes, ctx <- bytes_arg(ctx, host.first_arg(args))
          use encoding, ctx <- encoding_arg(ctx, host.arg_at(args, 1))
          use start, ctx <- int_arg(ctx, host.arg_at(args, 2))
          use end, ctx <- int_arg(ctx, host.arg_at(args, 3))
          case bit_array.slice(bytes, start, end - start) {
            Ok(part) -> #(Ok(mk_string(decode(part, encoding))), ctx)
            Error(Nil) -> host.range_error(ctx, "Index out of range")
          }
        }),
        HostMethod("byteLengthUtf8", 1, fn(ctx, args, _this) {
          use text, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "string",
          )
          #(Ok(mk_int(bit_array.byte_size(bit_array.from_string(text)))), ctx)
        }),
        HostMethod("compare", 2, fn(ctx, args, _this) {
          use a, ctx <- bytes_arg(ctx, host.first_arg(args))
          use b, ctx <- bytes_arg(ctx, host.arg_at(args, 1))
          #(Ok(mk_int(compare(a, b))), ctx)
        }),
        // (hay, needle, offset, forward, step) -> index or -1
        HostMethod("indexOf", 5, fn(ctx, args, _this) {
          use hay, ctx <- bytes_arg(ctx, host.first_arg(args))
          use needle, ctx <- bytes_arg(ctx, host.arg_at(args, 1))
          use offset, ctx <- int_arg(ctx, host.arg_at(args, 2))
          use forward, ctx <- host.validate_boolean(
            ctx,
            host.arg_at(args, 3),
            "forward",
          )
          use step, ctx <- int_arg(ctx, host.arg_at(args, 4))
          #(Ok(mk_int(index_of(hay, needle, offset, forward, step))), ctx)
        }),
        // (view, bits per unit) -> ArrayBuffer of the swapped bytes
        HostMethod("swap", 2, fn(ctx, args, _this) {
          use bytes, ctx <- bytes_arg(ctx, host.first_arg(args))
          use bits, ctx <- int_arg(ctx, host.arg_at(args, 1))
          let #(buffer, ctx) = host.array_buffer(ctx, swap(bytes, bits))
          #(Ok(buffer), ctx)
        }),
        // the decoded string, -1 for a bad length, -2 for a bad character
        HostMethod("atob", 1, fn(ctx, args, _this) {
          use text, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "input",
          )
          case atob(text) {
            Ok(decoded) -> #(Ok(mk_string(decoded)), ctx)
            Error(BadLength) -> #(Ok(mk_int(-1)), ctx)
            Error(InvalidCharacter) -> #(Ok(mk_int(-2)), ctx)
          }
        }),
        HostMethod("isAscii", 1, fn(ctx, args, _this) {
          use bytes, ctx <- bytes_arg(ctx, host.first_arg(args))
          #(Ok(mk_bool(is_ascii(bytes))), ctx)
        }),
        HostMethod("isUtf8", 1, fn(ctx, args, _this) {
          use bytes, ctx <- bytes_arg(ctx, host.first_arg(args))
          #(Ok(mk_bool(bit_array.is_utf8(bytes))), ctx)
        }),
        HostMethod("isLatin1", 1, fn(ctx, args, _this) {
          use text, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "input",
          )
          #(Ok(mk_bool(is_latin1(text))), ctx)
        }),
        HostMethod("utf16Units", 1, fn(ctx, args, _this) {
          use bytes, ctx <- bytes_arg(ctx, host.first_arg(args))
          #(Ok(mk_int(utf16_units(bytes))), ctx)
        }),
      ]
    },
    dependencies: ["spark:errors", "spark:domexception"],
    exports: [
      "Buffer", "INSPECT_MAX_BYTES", "atob", "btoa", "constants", "isAscii",
      "isLatin1", "isUtf8", "kMaxLength", "kStringMaxLength",
    ],
    globals: ["Buffer", "atob", "btoa"],
  )
}
