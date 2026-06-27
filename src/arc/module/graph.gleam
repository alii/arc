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
pub fn load(
  entry_specifier: String,
  entry_source: String,
  resolve: Resolve,
  load_source: Load,
) -> Result(SourceGraph, GraphError) {
  use entry <- result.try(prepare(entry_specifier, entry_source))
  use walk <- result.map(visit(
    entry,
    resolve,
    load_source,
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
  walk: Walk,
) -> Result(Walk, GraphError) {
  let specifier = node.specifier
  // Insert before walking dependencies so cycles terminate.
  let walk = Walk(..walk, modules: dict.insert(walk.modules, specifier, node))
  use walk <- result.try(
    list.try_fold(node.summary.requested, walk, fn(walk, request) {
      let raw = request.specifier
      use resolved <- result.try(
        resolve(request, specifier)
        |> result.map_error(ResolveFailed(raw, specifier, _)),
      )
      let walk = record_resolution(walk, specifier, raw, resolved)
      case dict.has_key(walk.modules, resolved) {
        True -> Ok(walk)
        False -> {
          use source <- result.try(
            load_source(resolved) |> result.map_error(LoadFailed(resolved, _)),
          )
          use dep <- result.try(prepare(resolved, source))
          visit(dep, resolve, load_source, walk)
        }
      }
    }),
  )
  case node.summary.has_source_phase {
    True -> Error(SourcePhaseUnsupported(specifier))
    False -> Ok(Walk(..walk, order: [specifier, ..walk.order]))
  }
}

/// Record a raw→resolved mapping in the referrer's specifier_map.
fn record_resolution(
  walk: Walk,
  referrer: String,
  raw: String,
  resolved: String,
) -> Walk {
  case dict.get(walk.modules, referrer) {
    Ok(node) ->
      Walk(
        ..walk,
        modules: dict.insert(
          walk.modules,
          referrer,
          SourceModule(
            ..node,
            specifier_map: dict.insert(node.specifier_map, raw, resolved),
          ),
        ),
      )
    Error(Nil) -> walk
  }
}

/// A module's dependencies as resolved specifiers, in request order.
pub fn dependencies(module: SourceModule) -> List(String) {
  list.filter_map(module.summary.requested, fn(request) {
    dict.get(module.specifier_map, request.specifier)
  })
}
