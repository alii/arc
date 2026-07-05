//// Why an embedder's module loader could not produce a module.
////
//// The module pipeline has TWO loader steps, and each gets its own error type:
//// `graph.Resolve` (specifier → module identity) fails with a `ResolveError`,
//// `graph.Load` (identity → source text) fails with a `LoadError`. Splitting
//// them keeps a resolve-only category (a bare specifier was rejected) out of
//// the load channel and vice versa, and neither type carries the specifier or
//// referrer it failed on: those live on `graph.ResolveFailed` /
//// `graph.LoadFailed`, which is the ONE place they are recorded, so a
//// stuttering or self-contradicting pair is unrepresentable.
////
//// It lives in its own leaf module so every layer of the module pipeline —
//// the runtime-free graph walk, the AOT compiler, the dynamic-import host
//// hook, and the embedder itself — can name the same categories without
//// importing each other.

/// Why a raw specifier could not be mapped to a module identity. A CATEGORY,
/// never a pre-worded message: the wording lives in `resolve_failure_message`,
/// and because failure has its own type a resolver can no longer return its
/// error text as an `Ok` value.
pub type ResolveError {
  /// The specifier was probed and nothing exists there.
  ResolveNotFound
  /// A bare specifier ("fs", a URL) this loader gives no meaning to — NOT a
  /// missing file: no path was ever probed.
  UnsupportedBareSpecifier
  /// The resolver refused for its own reason (a policy, a malformed
  /// specifier). `reason` is the host's own description.
  ResolveRejected(reason: String)
  /// This graph is not allowed to import anything (see `module_host.no_imports`).
  ResolveForbidden
}

/// Why a resolved specifier's source could not be read.
pub type LoadError {
  /// Nothing exists at this resolved specifier.
  LoadNotFound
  /// The module exists but its source could not be read (permissions, it is a
  /// directory, an I/O error). `reason` is the host's own description.
  ReadFailed(reason: String)
  /// This graph is not allowed to import anything (see `module_host.no_imports`).
  LoadForbidden
}

/// The ONE place a resolve failure is worded — the raw specifier and its
/// referrer come from the CALLER (which is the only holder of them), so the
/// message can never disagree with the request that actually failed.
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

/// The ONE place a load failure is worded. `specifier` is the resolved
/// specifier the caller asked for, so a "file not found: ./lib (is a
/// directory)" mismatch is unrepresentable.
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
