import arc/parser/ast
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/string

// specifier as written in source; never a graph key
pub opaque type Raw {
  Raw(String)
}

// modulerequest record minus phase; attributes sorted by key
pub type Request {
  Request(specifier: Raw, attributes: List(ast.ImportAttribute))
}

// canonical module identity: host-resolved path plus import attributes
pub opaque type Resolved {
  Resolved(path: String, attributes: List(ast.ImportAttribute))
}

pub fn raw(text: String) -> Raw {
  Raw(text)
}

pub fn raw_text(r: Raw) -> String {
  let Raw(text) = r
  text
}

pub fn resolved(path: String) -> Resolved {
  Resolved(path:, attributes: [])
}

// modulerequestsequal: same path and attributes give the same module
pub fn resolved_with(
  path: String,
  attributes: List(ast.ImportAttribute),
) -> Resolved {
  Resolved(path:, attributes:)
}

pub fn resolved_path(r: Resolved) -> String {
  r.path
}

pub fn resolved_attributes(r: Resolved) -> List(ast.ImportAttribute) {
  r.attributes
}

// string key for the registry and bundle tables
pub fn registry_key(r: Resolved) -> String {
  case r.attributes {
    [] -> r.path
    attributes -> r.path <> " " <> with_clause(attributes)
  }
}

// 'path' plus its with clause, for messages
pub fn describe(r: Resolved) -> String {
  case r.attributes {
    [] -> "'" <> r.path <> "'"
    attributes -> "'" <> r.path <> "' " <> with_clause(attributes)
  }
}

fn with_clause(attributes: List(ast.ImportAttribute)) -> String {
  let entries =
    list.map(attributes, fn(a) { a.key <> ": " <> string.inspect(a.value) })
  "with { " <> string.join(entries, ", ") <> " }"
}

pub opaque type SpecifierMap {
  SpecifierMap(entries: Dict(Request, Resolved))
}

pub fn new_map() -> SpecifierMap {
  SpecifierMap(dict.new())
}

pub fn insert(map: SpecifierMap, from: Request, to: Resolved) -> SpecifierMap {
  let SpecifierMap(entries) = map
  SpecifierMap(dict.insert(entries, from, to))
}

// the only bridge from request to resolved
pub fn lookup(map: SpecifierMap, r: Request) -> Option(Resolved) {
  let SpecifierMap(entries) = map
  dict.get(entries, r) |> option.from_result
}

pub type Specifier {
  PathSpecifier(path: Resolved)
  BareSpecifier(text: Raw)
}

pub fn resolve_path(raw: Raw, parent: Resolved) -> Specifier {
  let text = raw_text(raw)
  case
    string.starts_with(text, "./"),
    string.starts_with(text, "../"),
    string.starts_with(text, "/")
  {
    True, _, _ | _, True, _ -> {
      let parent_dir = dirname(parent.path)
      PathSpecifier(resolved(normalize(parent_dir <> "/" <> text)))
    }
    _, _, True -> PathSpecifier(resolved(normalize(text)))
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
