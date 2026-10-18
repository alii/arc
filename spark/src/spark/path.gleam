import arc/host.{HostMethod}
import arc/rt/types.{mk_bool, mk_string}
import gleam/list
import gleam/string
import spark/builtin.{type Builtin, Builtin}
import spark/io.{type Io}
import spark/js

const exports = [
  "_makeLong", "basename", "delimiter", "dirname", "extname", "format",
  "isAbsolute", "join", "matchesGlob", "normalize", "parse", "posix", "relative",
  "resolve", "sep", "toNamespacedPath", "win32",
]

/// path, posix or windows flavoured by the platform the process builtin was
/// given; it also reads the working directory from there
pub fn builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:path",
    source: js.path,
    natives: fn(_io: Io) {
      [
        HostMethod("normalizeString", 3, fn(ctx, args, _this) {
          use path, ctx <- host.validate_string(
            ctx,
            host.first_arg(args),
            "path",
          )
          use separator, ctx <- host.validate_string(
            ctx,
            host.arg_at(args, 2),
            "separator",
          )
          let allow_above_root = host.arg_at(args, 1) == mk_bool(True)
          #(
            Ok(mk_string(normalize_string(path, allow_above_root, separator))),
            ctx,
          )
        }),
      ]
    },
    dependencies: ["spark:errors", "node:process"],
    optional: [],
    exports:,
    globals: [],
  )
}

pub fn posix_builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:path/posix",
    source: js.path_posix,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:path"],
    optional: [],
    exports:,
    globals: [],
  )
}

pub fn win32_builtin() -> Builtin(host) {
  Builtin(
    specifier: "node:path/win32",
    source: js.path_win32,
    natives: fn(_io: Io) { [] },
    dependencies: ["node:path"],
    optional: [],
    exports:,
    globals: [],
  )
}

/// node's normalizeString: drops empty and `.` segments and lets each `..`
/// take back the segment before it. Nothing leads or trails the result. A
/// backslash separator means windows, where a slash separates too
fn normalize_string(
  path: String,
  allow_above_root: Bool,
  separator: String,
) -> String {
  let path = case separator {
    "\\" -> string.replace(path, "/", "\\")
    _ -> path
  }
  string.split(path, separator)
  |> list.fold([], fn(kept, segment) {
    case segment, kept {
      "", _ | ".", _ -> kept
      "..", [] | "..", ["..", ..] ->
        case allow_above_root {
          True -> ["..", ..kept]
          False -> kept
        }
      "..", [_, ..before] -> before
      _, _ -> [segment, ..kept]
    }
  })
  |> list.reverse
  |> string.join(separator)
}
