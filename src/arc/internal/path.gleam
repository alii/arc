//// Pure path utilities shared by the CLI module loader and the test262
//// runner.

import gleam/list
import gleam/string

/// Resolve a module specifier relative to the parent module's path.
/// - Absolute paths are returned as-is
/// - Relative paths (./foo, ../bar) are resolved against the parent's directory
/// - Bare specifiers (no ./ or ../ prefix) are returned as-is (builtin/package)
pub fn resolve_specifier(raw: String, parent: String) -> String {
  case string.starts_with(raw, "./"), string.starts_with(raw, "../") {
    True, _ | _, True -> {
      let parent_dir = dirname(parent)
      normalize(parent_dir <> "/" <> raw)
    }
    _, _ -> raw
  }
}

/// Get the directory portion of a path (everything before the last /).
pub fn dirname(path: String) -> String {
  let parts = string.split(path, "/")
  case list.reverse(parts) {
    [_, ..rest] ->
      case list.reverse(rest) {
        [] -> "."
        dir_parts -> string.join(dir_parts, "/")
      }
    [] -> "."
  }
}

/// Normalize a path by resolving . and .. components.
pub fn normalize(path: String) -> String {
  let parts = string.split(path, "/")
  let resolved =
    list.fold(parts, [], fn(acc, part) {
      case part {
        "." -> acc
        ".." ->
          case acc {
            [_, ..rest] -> rest
            [] -> [".."]
          }
        "" ->
          case acc {
            [] -> [""]
            _ -> acc
          }
        _ -> [part, ..acc]
      }
    })
  list.reverse(resolved) |> string.join("/")
}
