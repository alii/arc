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
//// Each module is parsed exactly once and analyzed exactly once per walk.
//// Hosts that cache (watch mode, dev servers) can skip even that: `prepare`
//// is the per-module parse+analyze step exposed directly, and a resolver
//// may answer `Prepared` with a cached result instead of `Source`.

import arc/esm
import arc/parser
import arc/parser/ast
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result

/// What a resolver may answer: source text for Arc to prepare, or an
/// already-prepared module (from `prepare`, cached by the host — key it on
/// content, not just path, and it composes with any invalidation scheme).
///
/// `Prepared` is a parse artifact, NOT a module-map entry: a SourceModule
/// carries no evaluation state, so `Prepared(m)` is exactly equivalent to
/// `Source(m.specifier, m.source)` minus the re-parse. The spec's module
/// map — where an evaluation throw is recorded in [[EvaluationError]] and
/// rethrown on every later import (§16.2.1.5) — lives in the realm's
/// heap-resident registry and never passes through this layer. Load
/// failures, by contrast, are retryable per HostLoadImportedModule, which
/// is why resolvers run fresh each walk.
pub type Resolution {
  /// The resolved module's canonical specifier and source text.
  Source(specifier: String, source: String)
  /// A cached `prepare` result. Its `specifier` is the module's identity;
  /// its specifier_map is rebuilt during the walk, so stale maps in cached
  /// values are harmless.
  Prepared(module: SourceModule)
}

/// The host resolver: (module_request, referrer_specifier) → where and what
/// the module is. The request carries the raw specifier as written
/// (`request.specifier`) and its phase; future import attributes will live
/// here too, since they're part of module identity. The resolved specifier
/// is the module's identity — two requests resolving to the same string are
/// the same module. Arc imposes no meaning on specifiers; paths, URLs, and
/// synthetic names all work.
pub type ResolveAndLoad =
  fn(esm.ModuleRequest, String) -> Result(Resolution, String)

/// One loaded module: everything the walk learned about it.
pub type SourceModule {
  SourceModule(
    specifier: String,
    source: String,
    program: ast.Program,
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
  /// The module contains a static source-phase import
  /// (`import source x from "m"`), which neither the runtime nor graph
  /// tooling supports yet. Checked after the module's requests resolve, so
  /// resolution failures win — matching the spec, where this error arises
  /// at linking (GetModuleSource, §16.2.1.7.2) while resolution failures
  /// arise at loading.
  SourcePhaseUnsupported(specifier: String)
}

/// Parse and analyze one module — the per-module unit of work `load`
/// performs, exposed so hosts can do it ahead of time and cache the result
/// (answer `Prepared` from the resolver to skip the re-parse).
pub fn prepare(
  specifier: String,
  source: String,
) -> Result(SourceModule, GraphError) {
  use program <- result.map(
    parser.parse(source, parser.Module)
    |> result.map_error(ParseFailed(specifier, _)),
  )
  SourceModule(
    specifier:,
    source:,
    program:,
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
  resolve_and_load: ResolveAndLoad,
) -> Result(SourceGraph, GraphError) {
  use entry <- result.try(prepare(entry_specifier, entry_source))
  use walk <- result.map(visit(
    entry,
    resolve_and_load,
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
  resolve_and_load: ResolveAndLoad,
  walk: Walk,
) -> Result(Walk, GraphError) {
  let specifier = node.specifier
  // Insert before walking dependencies so cycles terminate. The
  // specifier_map is rebuilt from this walk's resolutions, so cached
  // `Prepared` nodes can't leak stale mappings.
  let node = SourceModule(..node, specifier_map: dict.new())
  let walk = Walk(..walk, modules: dict.insert(walk.modules, specifier, node))
  use walk <- result.try(
    list.try_fold(node.summary.requested, walk, fn(walk, request) {
      let raw = request.specifier
      use resolution <- result.try(
        resolve_and_load(request, specifier)
        |> result.map_error(ResolveFailed(raw, specifier, _)),
      )
      use #(resolved, unvisited) <- result.try(case resolution {
        Source(specifier: resolved, source:) ->
          case dict.has_key(walk.modules, resolved) {
            // Already visited — skip the re-parse entirely.
            True -> Ok(#(resolved, None))
            False -> {
              use dep <- result.map(prepare(resolved, source))
              #(resolved, Some(dep))
            }
          }
        Prepared(module:) ->
          case dict.has_key(walk.modules, module.specifier) {
            True -> Ok(#(module.specifier, None))
            False -> Ok(#(module.specifier, Some(module)))
          }
      })
      let walk = record_resolution(walk, specifier, raw, resolved)
      case unvisited {
        None -> Ok(walk)
        Some(dep) -> visit(dep, resolve_and_load, walk)
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
