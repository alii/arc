// runtime-free resolve/parse/analyze walk over a module graph

import arc/compiler/scope_builder
import arc/module/loader.{type LoadError, type ResolveError}
import arc/module/specifier.{type Raw, type Resolved}
import arc/module/summary
import arc/parser
import arc/parser/ast
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set.{type Set}

pub type Resolve =
  fn(summary.ModuleRequest, Resolved) -> Result(Resolved, ResolveError)

pub type Load =
  fn(Resolved) -> Result(String, LoadError)

pub type ParsedModule {
  ParsedModule(
    specifier: Resolved,
    source: String,
    items: List(ast.ModuleItem),
    scopes: scope_builder.ScopeBuilder,
    summary: summary.ModuleSummary,
  )
}

pub type SourceModule {
  SourceModule(
    parsed: ParsedModule,
    edges: List(#(summary.ModuleRequest, Resolved)),
  )
}

pub fn specifier_map(m: SourceModule) -> specifier.SpecifierMap {
  use acc, #(request, resolved) <- list.fold(m.edges, specifier.new_map())
  specifier.insert(acc, request.specifier, resolved)
}

pub type SourceGraph {
  SourceGraph(
    entry: Resolved,
    modules: Dict(Resolved, SourceModule),
    // dependencies first, entry last (dfs post-order)
    order: List(Resolved),
  )
}

pub type GraphError {
  ParseFailed(specifier: Resolved, error: parser.ParseError)
  ResolveFailed(raw: Raw, referrer: Resolved, error: ResolveError)
  LoadFailed(specifier: Resolved, error: LoadError)
  SourcePhaseUnsupported(specifier: Resolved)
}

fn parse_and_analyze(
  specifier: Resolved,
  source: String,
) -> Result(ParsedModule, GraphError) {
  use #(items, scopes) <- result.map(
    parser.parse_module(source)
    |> result.map_error(ParseFailed(specifier, _)),
  )
  ParsedModule(
    specifier:,
    source:,
    items:,
    scopes:,
    summary: summary.analyze(items),
  )
}

type Walk {
  Walk(
    // never removed, handles both cycles and diamonds
    started: Set(Resolved),
    modules: Dict(Resolved, SourceModule),
    order: List(Resolved),
  )
}

// is_host specifiers are leaves, never loaded or parsed
pub fn load(
  entry_specifier: Resolved,
  entry_source: String,
  resolve: Resolve,
  load_source: Load,
  is_host: fn(Resolved) -> Bool,
) -> Result(SourceGraph, GraphError) {
  use entry <- result.try(parse_and_analyze(entry_specifier, entry_source))
  use walk <- result.map(visit(
    entry,
    resolve,
    load_source,
    is_host,
    Walk(started: set.new(), modules: dict.new(), order: []),
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
  // mark before walking deps so cycles terminate
  let walk = Walk(..walk, started: set.insert(walk.started, specifier))
  use #(edges, walk) <- result.try(
    list.try_fold(node.summary.requested, #([], walk), fn(acc, request) {
      let #(edges, walk) = acc
      let raw = request.specifier
      use resolved <- result.try(
        resolve(request, specifier)
        |> result.map_error(ResolveFailed(raw, specifier, _)),
      )
      let edges = [#(request, resolved), ..edges]
      use <- bool.guard(is_host(resolved), Ok(#(edges, walk)))
      use <- bool.guard(
        set.contains(walk.started, resolved),
        Ok(#(edges, walk)),
      )
      use source <- result.try(
        load_source(resolved) |> result.map_error(LoadFailed(resolved, _)),
      )
      use dep <- result.try(parse_and_analyze(resolved, source))
      use walk <- result.map(visit(dep, resolve, load_source, is_host, walk))
      #(edges, walk)
    }),
  )
  // §16.2.1.7.2 checked after resolve so resolve errors win
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
        SourceModule(parsed: node, edges: list.reverse(edges)),
      ),
      order: [specifier, ..walk.order],
    ),
  )
}
