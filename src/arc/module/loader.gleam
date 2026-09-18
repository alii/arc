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

pub fn load_failure_message(specifier: String, error: LoadError) -> String {
  "Cannot load module '" <> specifier <> "': " <> load_reason(error)
}

fn load_reason(error: LoadError) -> String {
  case error {
    LoadNotFound -> "no module exists at that specifier"
    ReadFailed(reason:) -> reason
    LoadForbidden -> "imports are not allowed here"
  }
}

pub type ResolveFn =
  fn(String, String) -> Result(String, ResolveError)

pub type LoadFn =
  fn(String) -> Result(String, LoadError)

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

pub fn forbid_load(_resolved: String) -> Result(String, LoadError) {
  Error(LoadForbidden)
}
