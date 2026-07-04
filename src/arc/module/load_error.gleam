//// Why an embedder's module loader could not produce a module.
////
//// This is the error type of `graph.Resolve` / `graph.Load` (and therefore of
//// `module.compile_bundle` and `module_host`'s `ResolveFn` / `LoadFn`): the
//// ONE typed channel a host loader reports failure on. It lives in its own
//// leaf module so every layer of the module pipeline — the runtime-free graph
//// walk, the AOT compiler, the dynamic-import host hook, and the embedder
//// itself — can name the same categories without importing each other.

/// A CATEGORY of loader failure, never a pre-worded message: the wording lives
/// in `message`, and because failure has its own type a loader can no longer
/// return its error text as an `Ok` value, nor can a caller downstream of the
/// graph walk be handed a rendered string it can only print.
pub type ModuleLoadError {
  /// Nothing exists at this resolved specifier.
  NotFound(specifier: String)
  /// This graph is not allowed to import anything.
  ImportsForbidden(specifier: String)
  /// The module exists but its source could not be read (permissions, it is a
  /// directory, an I/O error). `reason` is the host's own description.
  ReadFailed(specifier: String, reason: String)
  /// A raw specifier could not be mapped to a module identity.
  ResolveFailed(raw: String, referrer: String)
  /// A bare specifier ("fs", a URL) this loader gives no meaning to — NOT a
  /// missing file: no path was ever probed.
  UnsupportedBareSpecifier(specifier: String)
}

/// The ONE place a `ModuleLoadError` is worded. Every JS-visible resolve/load
/// rejection message is rendered here, so a loader can no longer invent a
/// category — a self-contradicting "file not found: ./lib (is a directory)"
/// is unrepresentable.
pub fn message(error: ModuleLoadError) -> String {
  case error {
    NotFound(specifier:) -> "Cannot find module '" <> specifier <> "'"
    ImportsForbidden(specifier:) ->
      "Cannot import '" <> specifier <> "': imports are not allowed here"
    ReadFailed(specifier:, reason:) ->
      "Cannot read module '" <> specifier <> "': " <> reason
    ResolveFailed(raw:, referrer:) ->
      "Cannot resolve module '" <> raw <> "' from '" <> referrer <> "'"
    UnsupportedBareSpecifier(specifier:) ->
      "Cannot resolve bare specifier '"
      <> specifier
      <> "': this loader resolves paths only"
  }
}
