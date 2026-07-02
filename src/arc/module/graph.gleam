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
//// single-file tooling that doesn't need a graph.

import arc/compiler/scope
import arc/esm
import arc/parser
import arc/parser/ast
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/result

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
  fn(esm.ModuleRequest, String) -> Result(String, String)

/// Fetch the source text of a resolved specifier. Called exactly once per
/// unique module per walk, however many edges point at it.
pub type Load =
  fn(String) -> Result(String, String)

/// One loaded module: everything the walk learned about it.
pub type SourceModule {
  SourceModule(
    specifier: String,
    source: String,
    program: ast.Program,
    /// Scope tree the parser accumulated alongside the AST, ready for
    /// `scope.finalize`. Stored here so compile can run without re-walking
    /// the AST.
    sb: scope.ScopeBuilder,
    summary: esm.ModuleSummary,
    /// raw specifier as written in source → resolved specifier.
    specifier_map: Dict(String, String),
  )
}

pub type SourceGraph {
  SourceGraph(
    entry: String,
    modules: Dict(String, SourceModule),
    /// Specifiers in dependencies-first order (the entry module is last).
    /// For cyclic graphs this is the DFS post-order, the same order module
    /// evaluation uses (§16.2.1.5.3).
    order: List(String),
  )
}

pub type GraphError {
  /// A module failed to parse. `error` is the structured parser error;
  /// render it with `parser.parse_error_to_string` / `parser.parse_error_pos`.
  ParseFailed(specifier: String, error: parser.ParseError)
  /// The host resolver rejected a request.
  ResolveFailed(raw: String, referrer: String, message: String)
  /// The host loader could not read a resolved specifier's source.
  LoadFailed(specifier: String, message: String)
  /// The module contains a static source-phase import
  /// (`import source x from "m"`), which neither the runtime nor graph
  /// tooling supports yet. Checked after the module's requests resolve, so
  /// resolution failures win — matching the spec, where this error arises
  /// at linking (GetModuleSource, §16.2.1.7.2) while resolution failures
  /// arise at loading.
  SourcePhaseUnsupported(specifier: String)
}

/// Parse and analyze one module — the per-module unit of work `load`
/// performs, exposed for single-file tooling.
pub fn prepare(
  specifier: String,
  source: String,
) -> Result(SourceModule, GraphError) {
  use #(program, sb) <- result.map(
    parser.parse(source, parser.Module)
    |> result.map_error(ParseFailed(specifier, _)),
  )
  SourceModule(
    specifier:,
    source:,
    program:,
    sb:,
    summary: esm.analyze(program),
    specifier_map: dict.new(),
  )
}

type Walk {
  Walk(modules: Dict(String, SourceModule), order: List(String))
}

/// Load the full module graph reachable from an already-loaded entry module.
///
/// `is_host` marks specifiers provided natively by the embedder (host /
/// synthetic modules): they are resolved (so the referrer's specifier_map
/// records the mapping) but their source is never loaded, parsed, or recursed —
/// they are leaves, injected into the bundle separately by the linker.
pub fn load(
  entry_specifier: String,
  entry_source: String,
  resolve: Resolve,
  load_source: Load,
  is_host: fn(String) -> Bool,
) -> Result(SourceGraph, GraphError) {
  use entry <- result.try(prepare(entry_specifier, entry_source))
  use walk <- result.map(visit(
    entry,
    resolve,
    load_source,
    is_host,
    Walk(modules: dict.new(), order: []),
  ))
  SourceGraph(
    entry: entry_specifier,
    modules: walk.modules,
    order: list.reverse(walk.order),
  )
}

fn visit(
  node: SourceModule,
  resolve: Resolve,
  load_source: Load,
  is_host: fn(String) -> Bool,
  walk: Walk,
) -> Result(Walk, GraphError) {
  let specifier = node.specifier
  // Insert before walking dependencies so cycles terminate. The raw→resolved
  // specifier_map is accumulated through the fold below and written back in
  // one final insert — dependencies never look their referrer up by
  // specifier, so the placeholder's empty map is never observed.
  let walk = Walk(..walk, modules: dict.insert(walk.modules, specifier, node))
  use #(walk, specifier_map) <- result.try(
    list.try_fold(
      node.summary.requested,
      #(walk, node.specifier_map),
      fn(acc, request) {
        let #(walk, specifier_map) = acc
        let raw = request.specifier
        use resolved <- result.try(
          resolve(request, specifier)
          |> result.map_error(ResolveFailed(raw, specifier, _)),
        )
        let specifier_map = dict.insert(specifier_map, raw, resolved)
        // A host module is a leaf: resolution is recorded above, but there is
        // no source to load or dependencies to walk.
        use <- bool.guard(is_host(resolved), Ok(#(walk, specifier_map)))
        case dict.has_key(walk.modules, resolved) {
          True -> Ok(#(walk, specifier_map))
          False -> {
            use source <- result.try(
              load_source(resolved)
              |> result.map_error(LoadFailed(resolved, _)),
            )
            use dep <- result.try(prepare(resolved, source))
            use walk <- result.map(visit(
              dep,
              resolve,
              load_source,
              is_host,
              walk,
            ))
            #(walk, specifier_map)
          }
        }
      },
    ),
  )
  let walk =
    Walk(
      ..walk,
      modules: dict.insert(
        walk.modules,
        specifier,
        SourceModule(..node, specifier_map:),
      ),
    )
  case node.summary.has_source_phase {
    True -> Error(SourcePhaseUnsupported(specifier))
    False -> Ok(Walk(..walk, order: [specifier, ..walk.order]))
  }
}

/// A module's dependencies as resolved specifiers, in request order.
pub fn dependencies(module: SourceModule) -> List(String) {
  list.filter_map(module.summary.requested, fn(request) {
    dict.get(module.specifier_map, request.specifier)
  })
}
