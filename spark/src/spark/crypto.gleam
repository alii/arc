import arc/host.{type Context, HostMethod}
import arc/rt/types.{type JsVal, mk_string}
import gleam/list
import gleam/option.{type Option, None, Some}
import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io, Random}
import spark/js

/// node:crypto, so far: the digests (createHash, createHmac, hash,
/// getHashes), random bytes (randomBytes, randomFill, randomInt, randomUUID,
/// getRandomValues) and timingSafeEqual; and `crypto`, the web's global,
/// with getRandomValues, randomUUID and subtle.digest. Digests are worked
/// out by the vm spark runs on. Random bytes are a want the embedder
/// answers (Random), so spark has no randomness of its own. No ciphers,
/// keys or signatures yet
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:crypto",
    source: js.crypto,
    natives: fn(io: Io) {
      [
        // name, bytes, and for a shake digest how many bytes to give
        HostMethod("hash", 3, fn(ctx, args, _this) {
          use name, ctx <- host.validate_string(
            ctx,
            host.arg_at(args, 0),
            "name",
          )
          use data, ctx <- bytes_of(ctx, host.arg_at(args, 1))
          let length = case types.classify(host.arg_at(args, 2)) {
            types.KNum(types.JInt(n)) -> Some(n)
            _ -> None
          }
          digest(ctx, hash(name, data, length))
        }),
        HostMethod("hmac", 3, fn(ctx, args, _this) {
          use name, ctx <- host.validate_string(
            ctx,
            host.arg_at(args, 0),
            "name",
          )
          use key, ctx <- bytes_of(ctx, host.arg_at(args, 1))
          use data, ctx <- bytes_of(ctx, host.arg_at(args, 2))
          digest(ctx, hmac(name, key, data))
        }),
        HostMethod("hashes", 0, fn(ctx, _args, _this) {
          let #(array, ctx) = host.array(ctx, list.map(names(), mk_string))
          #(Ok(array), ctx)
        }),
        HostMethod("random", 1, fn(ctx, args, _this) {
          use bytes, ctx <- host.validate_integer(
            ctx,
            host.first_arg(args),
            "size",
            0,
            2_147_483_647,
          )
          io.ask(io, ctx, Random(bytes:))
        }),
      ]
    },
    dependencies: [
      "spark:internal", "spark:loop", "spark:domexception", "node:buffer",
      "node:stream",
    ],
    optional: [],
    exports: [
      "createHash",
      "createHmac",
      "getHashes",
      "hash",
      "randomBytes",
      "randomFill",
      "randomFillSync",
      "randomInt",
      "randomUUID",
      "timingSafeEqual",
      "Hash",
      "Hmac",
      "getRandomValues",
      "webcrypto",
      "subtle",
    ],
    globals: ["crypto"],
  )
}

// undefined for a digest this build does not have
fn digest(
  ctx: Context(host),
  result: Result(BitArray, Nil),
) -> #(Result(JsVal, JsVal), Context(host)) {
  case result {
    Ok(bytes) -> {
      let #(buffer, ctx) = host.array_buffer(ctx, bytes)
      #(Ok(buffer), ctx)
    }
    Error(Nil) -> #(Ok(types.mk_undefined()), ctx)
  }
}

fn bytes_of(ctx: Context(host), value: JsVal, next) {
  case host.bytes(ctx, value) {
    Some(bytes) -> next(bytes, ctx)
    None -> host.type_error(ctx, "expected bytes")
  }
}

@external(erlang, "spark_crypto_ffi", "hash")
fn hash(
  name: String,
  data: BitArray,
  length: Option(Int),
) -> Result(BitArray, Nil)

@external(erlang, "spark_crypto_ffi", "hmac")
fn hmac(name: String, key: BitArray, data: BitArray) -> Result(BitArray, Nil)

@external(erlang, "spark_crypto_ffi", "names")
fn names() -> List(String)
