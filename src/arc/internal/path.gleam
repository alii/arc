//// Pure path utilities shared by the CLI module loader and the test262
//// runner.

import gleam/list
import gleam/string

/// What a raw import specifier turned out to be, once resolved against its
/// referrer. The two arms are categorically different things and a loader has
/// to handle them differently, so they cannot be one `String`:
///
///  - `PathSpecifier` — a canonical, normalized path a loader may read.
///  - `BareSpecifier` — a builtin/package/URL name (`"fs"`, `"https://…"`).
///    Arc imposes NO path meaning on it; a filesystem loader must reject it
///    rather than treat it as a relative path. An embedder that registers host
///    modules under bare names (`engine.register_host_module("dance", …)`) must
///    consult its host-module registry on this arm and pass the name through —
///    the module graph resolves a specifier BEFORE it asks whether the result
///    is a host module, so rejecting every `BareSpecifier` outright makes such
///    imports unreachable.
pub type Specifier {
  PathSpecifier(path: String)
  BareSpecifier(text: String)
}

/// Resolve a module specifier relative to the parent module's path.
/// - Relative paths (./foo, ../bar) are resolved against the parent's directory
/// - Absolute paths (/foo) are normalized
/// - Bare specifiers (no ./, ../ or / prefix) come back as `BareSpecifier`
///   (builtin/package/URL — Arc imposes no path meaning on them)
///
/// The result is a module IDENTITY, so every path-shaped specifier goes through
/// `normalize`: `./a.js`, `a.js` and `x/../a.js` must not become three
/// modules for one file.
pub fn resolve_specifier(raw: String, parent: String) -> Specifier {
  case
    string.starts_with(raw, "./"),
    string.starts_with(raw, "../"),
    string.starts_with(raw, "/")
  {
    True, _, _ | _, True, _ -> {
      let parent_dir = dirname(parent)
      PathSpecifier(normalize(parent_dir <> "/" <> raw))
    }
    _, _, True -> PathSpecifier(normalize(raw))
    _, _, _ -> BareSpecifier(raw)
  }
}

/// Normalize a path by resolving `.` and `..` components — the canonical form
/// of a path-shaped specifier that is NOT relative to a referrer (the entry
/// specifier the CLI takes from argv, or an absolute path). Dropping the `.`
/// segments also drops a leading `./`, so `arc ./a.js` and `arc a.js` name the
/// same module as an `import "./a.js"` edge resolved against `.` does.
///
/// Rendering is TOTAL: a path that resolves to no segments still denotes a
/// directory, and WHICH directory is decided by the input, not by what the fold
/// left behind — `.`, `a/..` and `a/../` all normalize to `"."`, while `/` and
/// `/..` normalize to `"/"`. Never the empty string, which would be a module
/// identity naming nothing.
pub fn normalize(path: String) -> String {
  let parts = string.split(path, "/")
  let resolved =
    list.fold(parts, [], fn(acc, part) {
      case part {
        "." -> acc
        // Only a real directory segment can be popped by "..". `acc` is
        // built in reverse, so its head is the most recent segment:
        //  - ["", ..]    → at the root of an absolute path; ".." can't climb
        //                  above it, so it is dropped
        //  - [] / [".."] → nothing left to pop; the ".." must be preserved
        //                  (a leading run of ".."s escapes the base dir)
        //  - [seg, ..]   → pop it
        ".." ->
          case acc {
            ["", ..] -> acc
            [] | ["..", ..] -> ["..", ..acc]
            [_, ..rest] -> rest
          }
        "" ->
          case acc {
            [] -> [""]
            _ -> acc
          }
        _ -> [part, ..acc]
      }
    })
  case resolved {
    // Everything cancelled out. Which directory that leaves is a property of
    // the input, not of the accumulator: a trailing slash also parks a `""` in
    // `acc`, so `a/../` must not be mistaken for the root marker of `/..`.
    [] | [""] ->
      case string.starts_with(path, "/") {
        True -> "/"
        False -> "."
      }
    segments -> list.reverse(segments) |> string.join("/")
  }
}

/// Get the directory portion of a path (everything before the last /).
fn dirname(path: String) -> String {
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
