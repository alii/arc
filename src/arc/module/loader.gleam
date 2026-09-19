import arc/module/specifier.{type Resolved}
import arc/parser/ast.{type ImportAttribute}
import gleam/list
import gleam/option.{type Option}

pub type ResolveError {
  ResolveNotFound
  // bare specifier this loader gives no meaning to; nothing was probed
  UnsupportedBareSpecifier
  ResolveRejected(reason: String)
  ResolveForbidden
}

pub type LoadError {
  LoadNotFound
  ReadFailed(reason: String)
  LoadForbidden
  // a `type` import attribute this loader cannot produce a module for
  UnsupportedModuleType(module_type: String)
}

// what the host loaded for a module request; the type attribute picks one
pub type ModuleSource {
  // source text module record
  SourceText(source: String)
  // parsejsonmodule input
  JsonSource(source: String)
  TextSource(text: String)
  BytesSource(bytes: BitArray)
}

// HostGetSupportedImportAttributes
pub const supported_import_attributes = ["type"]

// AllImportAttributesSupported, as the first unsupported key
pub fn unsupported_attribute(
  attributes: List(ImportAttribute),
) -> Option(String) {
  list.find(attributes, fn(attribute) {
    !list.contains(supported_import_attributes, attribute.key)
  })
  |> option.from_result
  |> option.map(fn(attribute) { attribute.key })
}

pub fn module_type(attributes: List(ImportAttribute)) -> Option(String) {
  list.find_map(attributes, fn(attribute) {
    case attribute {
      ast.ImportAttribute(key: "type", value:) -> Ok(value)
      ast.ImportAttribute(..) -> Error(Nil)
    }
  })
  |> option.from_result
}

pub fn resolve_failure_message(
  raw: String,
  referrer: String,
  error: ResolveError,
) -> String {
  "Cannot resolve module '"
  <> raw
  <> "' from '"
  <> referrer
  <> "': "
  <> resolve_reason(error)
}

fn resolve_reason(error: ResolveError) -> String {
  case error {
    ResolveNotFound -> "no module exists at that specifier"
    UnsupportedBareSpecifier ->
      "bare specifier: this loader resolves paths only"
    ResolveRejected(reason:) -> reason
    ResolveForbidden -> "imports are not allowed here"
  }
}

pub fn load_failure_message(module: Resolved, error: LoadError) -> String {
  "Cannot load module "
  <> specifier.describe(module)
  <> ": "
  <> load_reason(error)
}

fn load_reason(error: LoadError) -> String {
  case error {
    LoadNotFound -> "no module exists at that specifier"
    ReadFailed(reason:) -> reason
    LoadForbidden -> "imports are not allowed here"
    UnsupportedModuleType(module_type:) ->
      "unsupported module type '" <> module_type <> "'"
  }
}

pub type ResolveFn =
  fn(String, String) -> Result(String, ResolveError)

// called with the resolved specifier and the request's attributes
pub type LoadFn =
  fn(String, List(ImportAttribute)) -> Result(ModuleSource, LoadError)

// resolve/load pair that forbids every import
pub fn no_imports() -> #(ResolveFn, LoadFn) {
  #(forbid_resolve, forbid_load)
}

pub fn forbid_resolve(
  _raw_specifier: String,
  _referrer: String,
) -> Result(String, ResolveError) {
  Error(ResolveForbidden)
}

pub fn forbid_load(
  _resolved: String,
  _attributes: List(ImportAttribute),
) -> Result(ModuleSource, LoadError) {
  Error(LoadForbidden)
}
