import arc/host.{type Context, HostMethod}
import arc/rt/types.{type JsVal}
import gleam/option.{None, Some}
import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io, DropBody, Fetch, ReadBody}
import spark/js

/// the web's fetch, with Headers, Request and Response, each a global. The
/// network is the embedder's: a request is a Fetch want, answered with the
/// status and headers, and the body then comes one ReadBody want at a time,
/// so a body that never ends costs nothing extra. Redirects, aborting and
/// unpacking a gzip or deflate body are done in the javascript. There is no
/// Blob or FormData yet, so blob() and formData() are refused
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "spark:fetch",
    source: js.fetch,
    natives: fn(io: Io) {
      [
        // method, url, body or undefined, then name, value, name, value, ...
        HostMethod("fetchLater", 3, fn(ctx, args, _this) {
          case args {
            [method, url, body, ..headers] -> {
              use method, ctx <- host.validate_string(ctx, method, "method")
              use url, ctx <- host.validate_string(ctx, url, "url")
              let body = case host.bytes(ctx, body) {
                Some(bytes) -> bytes
                None -> <<>>
              }
              io.ask_later(
                io,
                ctx,
                Fetch(method:, url:, headers: pairs(headers), body:),
              )
            }
            _ ->
              host.type_error(
                ctx,
                "fetchLater takes a method, a url and a body",
              )
          }
        }),
        HostMethod("readBodyLater", 1, fn(ctx, args, _this) {
          use body, ctx <- handle(ctx, args)
          io.ask_later(io, ctx, ReadBody(body:))
        }),
        HostMethod("dropBody", 1, fn(ctx, args, _this) {
          use body, ctx <- handle(ctx, args)
          io.ask_later(io, ctx, DropBody(body:))
        }),
      ]
    },
    dependencies: [
      "spark:errors", "spark:dom_events", "spark:encoding", "node:buffer",
      "node:url", "node:stream", "node:stream/web",
    ],
    optional: [],
    exports: ["fetch", "Headers", "Request", "Response"],
    globals: ["fetch", "Headers", "Request", "Response"],
  )
}

fn handle(ctx: Context(host), args: List(JsVal), next) {
  case types.classify(host.first_arg(args)) {
    types.KNum(types.JInt(body)) -> next(body, ctx)
    _ -> host.type_error(ctx, "expected the number of a body")
  }
}

// name, value, name, value, ... as pairs; what is not text is left out
fn pairs(flat: List(JsVal)) -> List(#(String, String)) {
  case flat {
    [name, value, ..rest] ->
      case types.classify(name), types.classify(value) {
        types.KStr(name), types.KStr(value) -> [#(name, value), ..pairs(rest)]
        _, _ -> pairs(rest)
      }
    _ -> []
  }
}
