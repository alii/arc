import arc/host.{HostMethod}
import arc/rt/types.{KBool, classify, mk_int, mk_string, mk_undefined}
import gleam/bit_array
import gleam/option.{None, Some}
import spark/builtin.{type Builtin, Builtin}
import spark/js

@external(erlang, "spark_encoding_ffi", "decode_utf8")
fn decode_utf8(
  bytes: BitArray,
  fatal: Bool,
  stream: Bool,
) -> Result(#(String, Int), Nil)

/// TextEncoder and TextDecoder, utf-8 only, as globals
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:encoding",
    source: js.encoding,
    natives: fn(_io) {
      [
        HostMethod("encode", 1, fn(ctx, args, _this) {
          use text, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "input",
          )
          let #(buffer, ctx) =
            host.array_buffer(ctx, bit_array.from_string(text))
          #(Ok(buffer), ctx)
        }),
        // [text, bytes left undecoded], or undefined when fatal and invalid
        HostMethod("decode", 3, fn(ctx, args, _this) {
          let fatal = classify(host.arg_at(args, 1)) == KBool(True)
          let stream = classify(host.arg_at(args, 2)) == KBool(True)
          case host.bytes(ctx, host.first_arg(args)) {
            None -> host.type_error(ctx, "decode: not a buffer or a view")
            Some(bytes) ->
              case decode_utf8(bytes, fatal, stream) {
                Ok(#(text, leftover)) -> {
                  let #(pair, ctx) =
                    host.array(ctx, [mk_string(text), mk_int(leftover)])
                  #(Ok(pair), ctx)
                }
                Error(Nil) -> #(Ok(mk_undefined()), ctx)
              }
          }
        }),
      ]
    },
    dependencies: ["spark:errors"],
    exports: ["TextEncoder", "TextDecoder"],
    globals: ["TextEncoder", "TextDecoder"],
  )
}
