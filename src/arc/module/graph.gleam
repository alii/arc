//// Source-level module graph loading: the resolve/parse/analyze walk shared
//// by everything that consumes a module graph. Given an entry module and a
//// host resolver callback, produces the full transitive graph with each
//// module's source text, AST, and static import/export summary intact.
////
//// This is deliberately runtime-free — no compiler, no VM. Build bundlers
//// and dev tooling directly on `SourceGraph`; the runtime's
//// `arc/module.compile_bundle` is itself `load` followed by a per-module
//// compile, so both consumers walk the graph with exactly the same
//// resolution semantics.
////
//// Each module is resolved, parsed, and analyzed exactly once per walk.
//// `prepare` is the per-module parse+analyze step exposed directly, for
//// single-file tooling that doesn't need a graph — it yields a `ParsedModule`,
//// which only the walk can turn into a `SourceModule` (a parsed module whose
//// every request has a resolved specifier).

import arc/compiler/scope
import arc/esm.{type Raw, type Resolved}
import arc/parser
import arc/parser/ast
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set.{type Set}

/// Resolve a module request against its referrer to the module's canonical
/// specifier. The request carries the raw specifier as written
/// (`request.specifier`) and its phase; future import attributes will live
/// here too, since they're part of module identity. The resolved specifier
/// IS the module's identity — two requests resolving to the same string are
/// the same module. Called once per import edge, so keep it to specifier
/// math and existence probing; reading source belongs in `Load`. Arc
/// imposes no meaning on specifiers; paths, URLs, and synthetic names all
/// work. Per HostLoadImportedModule, only successful resolutions need to be
/// stable — a failed resolution may succeed on a later walk.
pub type Resolve =
  fn(esm.ModuleRequest, Resolved) -> Result(Resolved, String)

/// Fetch the source text of a resolved specifier. Called exactly once per
/// unique module per walk, however many edges point at it.
pub type Load =
  fn(Resolved) -> Result(String, String)

/// One parsed-and-analyzed module, BEFORE any of its requests are resolved.
/// This is exactly what `prepare` can know from a single file: no edges yet.
pub type ParsedModule {
  ParsedModule(
    specifier: Resolved,
    source: String,
    /// The module body — exactly what `compiler.compile_module` and
    /// `esm.analyze` consume. No `ast.Program` wrapper: this record can only
    /// ever hold a module, so there is no script variant for a consumer to
    /// mis-handle.
    items: List(ast.ModuleItem),
    /// Scope tree the parser accumulated alongside the AST, ready for
    /// `scope.finalize`. Stored here so compile can run without re-walking
    /// the AST.
    sb: scope.ScopeBuilder,
    summary: esm.ModuleSummary,
  )
}

/// One loaded module: a `ParsedModule` whose every request has been resolved.
/// A value of this type can only be produced by the walk, so "parsed but not
/// yet resolved" and "resolved" are different types — no half-filled record.
pub type SourceModule {
  SourceModule(
    parsed: ParsedModule,
    /// The module's import edges, in `parsed.summary.requested` order: each
    /// merged `ModuleRequest` paired with the specifier the host resolved it
    /// to. TOTAL — one entry per request, so a raw→resolved projection over
    /// this list never has to invent a fallback.
    resolved: List(#(esm.ModuleRequest, Resolved)),
  )
}

/// The module's raw specifier → resolved specifier map, projected out of its
/// (total) `resolved` edge list. This is what the export resolver (`arc/link`)
/// consumes, since export/import entries carry raw specifiers as written in
/// source.
pub fn specifier_map(m: SourceModule) -> esm.SpecifierMap {
  use acc, #(request, resolved) <- list.fold(
    m.resolved,
    esm.new_specifier_map(),
  )
  esm.insert_specifier(acc, request.specifier, resolved)
}

pub type SourceGraph {
  SourceGraph(
    entry: Resolved,
    modules: Dict(Resolved, SourceModule),
    /// Specifiers in dependencies-first order (the entry module is last).
    /// For cyclic graphs this is the DFS post-order, the same order module
    /// evaluation uses (§16.2.1.5.3).
    order: List(Resolved),
  )
}

pub type GraphError {
  /// A module failed to parse. `error` is the structured parser error;
  /// render it with `parser.parse_error_to_string` / `parser.parse_error_pos`.
  ParseFailed(specifier: Resolved, error: parser.ParseError)
  /// The host resolver rejected a request. `raw` is quoted as the source wrote
  /// it; `referrer` is the module that wrote it.
  ResolveFailed(raw: Raw, referrer: Resolved, message: String)
  /// The host loader could not read a resolved specifier's source.
  LoadFailed(specifier: Resolved, message: String)
  /// The module contains a static source-phase import
  /// (`import source x from "m"`), which neither the runtime nor graph
  /// tooling supports yet. Checked after the module's requests resolve, so
  /// resolution failures win — matching the spec, where this error arises
  /// at linking (GetModuleSource, §16.2.1.7.2) while resolution failures
  /// arise at loading.
  SourcePhaseUnsupported(specifier: Resolved)
}

/// Parse and analyze one module — the per-module unit of work `load`
/// performs, exposed for single-file tooling. Resolves nothing, so it yields
/// a `ParsedModule`; only the walk can turn one into a `SourceModule`.
pub fn prepare(
  specifier: Resolved,
  source: String,
) -> Result(ParsedModule, GraphError) {
  use #(items, sb) <- result.map(
    parser.parse_module(source)
    |> result.map_error(ParseFailed(specifier, _)),
  )
  ParsedModule(specifier:, source:, items:, sb:, summary: esm.analyze(items))
}

type Walk {
  Walk(
    /// Specifiers whose visit has STARTED — the cycle guard. A module lands
    /// in `modules` only once every one of its edges is resolved, so no
    /// half-resolved `SourceModule` is ever observable.
    visiting: Set(Resolved),
    modules: Dict(Resolved, SourceModule),
    order: List(Resolved),
  )
}

/// Load the full module graph reachable from an already-loaded entry module.
///
/// `is_host` marks specifiers provided natively by the embedder (host /
/// synthetic modules): they are resolved (so the referrer records the edge)
/// but their source is never loaded, parsed, or recursed — they are leaves,
/// injected into the bundle separately by the linker.
pub fn load(
  entry_specifier: Resolved,
  entry_source: String,
  resolve: Resolve,
  load_source: Load,
  is_host: fn(Resolved) -> Bool,
) -> Result(SourceGraph, GraphError) {
  use entry <- result.try(prepare(entry_specifier, entry_source))
  use walk <- result.map(visit(
    entry,
    resolve,
    load_source,
    is_host,
    Walk(visiting: set.new(), modules: dict.new(), order: []),
  ))
  SourceGraph(
    entry: entry_specifier,
    modules: walk.modules,
    order: list.reverse(walk.order),
  )
}

fn visit(
  node: ParsedModule,
  resolve: Resolve,
  load_source: Load,
  is_host: fn(Resolved) -> Bool,
  walk: Walk,
) -> Result(Walk, GraphError) {
  let specifier = node.specifier
  // Mark before walking dependencies so cycles terminate.
  let walk = Walk(..walk, visiting: set.insert(walk.visiting, specifier))
  use #(walk, edges) <- result.try(
    list.try_fold(node.summary.requested, #(walk, []), fn(acc, request) {
      let #(walk, edges) = acc
      let raw = request.specifier
      use resolved <- result.try(
        resolve(request, specifier)
        |> result.map_error(ResolveFailed(raw, specifier, _)),
      )
      let edges = [#(request, resolved), ..edges]
      // A host module is a leaf: the edge is recorded above, but there is no
      // source to load or dependencies to walk.
      use <- bool.guard(is_host(resolved), Ok(#(walk, edges)))
      use <- bool.guard(
        set.contains(walk.visiting, resolved),
        Ok(#(walk, edges)),
      )
      use source <- result.try(
        load_source(resolved) |> result.map_error(LoadFailed(resolved, _)),
      )
      use dep <- result.try(prepare(resolved, source))
      use walk <- result.map(visit(dep, resolve, load_source, is_host, walk))
      #(walk, edges)
    }),
  )
  // §16.2.1.7.2: GetModuleSource always throws for a Source Text Module
  // Record, so a static `import source` fails at LINKING. Checked only after
  // the module's requests resolve, so a resolution failure — which arises at
  // LOADING — wins.
  use <- bool.guard(
    node.summary.has_source_phase,
    Error(SourcePhaseUnsupported(specifier)),
  )
  Ok(
    Walk(
      ..walk,
      modules: dict.insert(
        walk.modules,
        specifier,
        SourceModule(parsed: node, resolved: list.reverse(edges)),
      ),
      order: [specifier, ..walk.order],
    ),
  )
}
