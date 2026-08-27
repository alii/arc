import arc/esm
import gleam/list
import gleam/string

pub type Specifier {
  PathSpecifier(path: esm.Resolved)
  BareSpecifier(text: esm.Raw)
}

pub fn resolve_specifier(raw: esm.Raw, parent: esm.Resolved) -> Specifier {
  let text = esm.raw_text(raw)
  case
    string.starts_with(text, "./"),
    string.starts_with(text, "../"),
    string.starts_with(text, "/")
  {
    True, _, _ | _, True, _ -> {
      let parent_dir = dirname(esm.resolved_text(parent))
      PathSpecifier(
        esm.resolved_unchecked(normalize(parent_dir <> "/" <> text)),
      )
    }
    _, _, True -> PathSpecifier(esm.resolved_unchecked(normalize(text)))
    _, _, _ -> BareSpecifier(raw)
  }
}

// never returns "": empty result is "." or "/"
pub fn normalize(path: String) -> String {
  let parts = string.split(path, "/")
  let resolved =
    list.fold(parts, [], fn(acc, part) {
      case part {
        "." -> acc
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
    [] | [""] ->
      case string.starts_with(path, "/") {
        True -> "/"
        False -> "."
      }
    segments -> list.reverse(segments) |> string.join("/")
  }
}

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
