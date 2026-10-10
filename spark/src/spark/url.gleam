import arc/host.{type Context, HostMethod}
import arc/rt/types.{
  type JsVal, JInt, KNum, KStr, KUndef, classify, mk_bool, mk_int, mk_string,
  mk_undefined,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import spark/builtin.{type Builtin, Builtin}
import spark/js

// the names double as the atoms the erlang side matches on
type Part {
  Protocol
  Host
  Hostname
  Port
  Username
  Password
  Pathname
  Search
  Hash
  Href
}

@external(erlang, "spark_url_ffi", "parse")
fn parse(
  input: String,
  base: Option(String),
) -> Result(#(String, List(Int)), Nil)

@external(erlang, "spark_url_ffi", "can_parse")
fn can_parse(input: String, base: Option(String)) -> Bool

@external(erlang, "spark_url_ffi", "update")
fn update(
  href: String,
  part: Part,
  value: String,
) -> Result(#(String, List(Int)), Nil)

@external(erlang, "spark_url_ffi", "domain_to_ascii")
fn domain_to_ascii(domain: String) -> String

@external(erlang, "spark_url_ffi", "domain_to_unicode")
fn domain_to_unicode(domain: String) -> String

@external(erlang, "spark_url_ffi", "idna_to_ascii")
fn idna_to_ascii(domain: String) -> String

type Reply(host) =
  #(Result(JsVal, JsVal), Context(host))

// node's numbering of the setters
fn part_arg(
  ctx: Context(host),
  val: JsVal,
  cont: fn(Part, Context(host)) -> Reply(host),
) -> Reply(host) {
  case classify(val) {
    KNum(JInt(0)) -> cont(Protocol, ctx)
    KNum(JInt(1)) -> cont(Host, ctx)
    KNum(JInt(2)) -> cont(Hostname, ctx)
    KNum(JInt(3)) -> cont(Port, ctx)
    KNum(JInt(4)) -> cont(Username, ctx)
    KNum(JInt(5)) -> cont(Password, ctx)
    KNum(JInt(6)) -> cont(Pathname, ctx)
    KNum(JInt(7)) -> cont(Search, ctx)
    KNum(JInt(8)) -> cont(Hash, ctx)
    KNum(JInt(9)) -> cont(Href, ctx)
    _ -> host.type_error(ctx, "url: not a part of a url")
  }
}

fn base_arg(
  ctx: Context(host),
  val: JsVal,
  cont: fn(Option(String), Context(host)) -> Reply(host),
) -> Reply(host) {
  case classify(val) {
    KStr(base) -> cont(Some(base), ctx)
    KUndef -> cont(None, ctx)
    _ -> host.type_error(ctx, "url: the base is neither a string nor undefined")
  }
}

// [href, [positions of its parts]], or undefined for no url
fn url_reply(
  ctx: Context(host),
  parsed: Result(#(String, List(Int)), Nil),
) -> Reply(host) {
  case parsed {
    Error(Nil) -> #(Ok(mk_undefined()), ctx)
    Ok(#(href, positions)) -> {
      let #(positions, ctx) = host.array(ctx, list.map(positions, mk_int))
      let #(pair, ctx) = host.array(ctx, [mk_string(href), positions])
      #(Ok(pair), ctx)
    }
  }
}

fn text_native(
  name: String,
  run: fn(String) -> String,
) -> host.HostMethod(host) {
  HostMethod(name, 1, fn(ctx, args, _this) {
    use text, ctx <- host.validate_string(ctx, host.first_arg(args), "domain")
    #(Ok(mk_string(run(text))), ctx)
  })
}

/// URL and URLSearchParams as globals, and the url module. The parsing is
/// native, to the whatwg url standard; the classes are node's javascript
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:url",
    source: js.url,
    natives: fn(_io) {
      [
        HostMethod("parse", 2, fn(ctx, args, _this) {
          use input, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "input",
          )
          use base, ctx <- base_arg(ctx, host.arg_at(args, 1))
          url_reply(ctx, parse(input, base))
        }),
        HostMethod("canParse", 2, fn(ctx, args, _this) {
          use input, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "input",
          )
          use base, ctx <- base_arg(ctx, host.arg_at(args, 1))
          #(Ok(mk_bool(can_parse(input, base))), ctx)
        }),
        HostMethod("update", 3, fn(ctx, args, _this) {
          use href, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "href",
          )
          use part, ctx <- part_arg(ctx, host.arg_at(args, 1))
          use value, ctx <- host.validate_string(
            ctx,
            host.arg_at(args, 2),
            "value",
          )
          url_reply(ctx, update(href, part, value))
        }),
        text_native("domainToASCII", domain_to_ascii),
        text_native("domainToUnicode", domain_to_unicode),
        text_native("toASCII", idna_to_ascii),
      ]
    },
    dependencies: [
      "spark:errors", "node:path", "node:buffer", "node:querystring",
      "spark:querystring", "node:process",
    ],
    optional: [],
    exports: [
      "URL", "URLSearchParams", "Url", "domainToASCII", "domainToUnicode",
      "fileURLToPath", "fileURLToPathBuffer", "format", "parse", "pathToFileURL",
      "resolve", "resolveObject", "urlToHttpOptions",
    ],
    globals: ["URL", "URLSearchParams"],
  )
}
