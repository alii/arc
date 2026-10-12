//// writes src/spark/js.gleam from js/*.js: `gleam run -m embed_js`

import gleam/list
import gleam/string
import simplifile

const names = [
  "bootstrap",
  "errors",
  "internal",
  "async_hooks",
  "timers",
  "fs",
  "process",
  "os",
  "encoding",
  "perf_hooks",
  "fs_promises",
  "global",
  "domexception",
  "dom_events",
  "buffer",
  "path",
  "path_posix",
  "path_win32",
  "net",
  "querystring_internal",
  "querystring",
  "url",
  "events",
  "string_decoder",
  "util",
  "stream",
  "stream_promises",
  "stream_consumers",
  "structured_clone",
  "zlib",
  "util_types",
  "tty",
  "tls",
  "web_streams",
  "fetch",
]

const target = "src/spark/js.gleam"

pub fn main() -> Nil {
  let assert Ok(Nil) = simplifile.write(target, generate())
  Nil
}

fn generate() -> String {
  let constants =
    list.map(names, fn(name) {
      let assert Ok(source) = simplifile.read("js/" <> name <> ".js")
      let literal =
        source
        |> string.replace("\\", "\\\\")
        |> string.replace("\"", "\\\"")
      "pub const " <> name <> " = \"" <> literal <> "\"\n"
    })
  "//// generated from js/*.js by `gleam run -m embed_js`; edit those instead\n\n"
  <> string.join(constants, "\n")
}

pub fn embedded_js_is_current_test() {
  let assert Ok(written) = simplifile.read(target)
  assert written == generate()
}
