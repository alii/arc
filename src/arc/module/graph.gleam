// runtime-free resolve/parse/analyze walk over a module graph

import arc/compiler/scope_builder
import arc/module/loader.{type LoadError, type ModuleSource, type ResolveError}
import arc/module/specifier.{type Raw, type Resolved}
import arc/module/summary
import arc/parser
import arc/parser/ast
import arc/rt/builtins/json
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}

// host resolve gives a path; identity adds the request's attributes
type Resolve =
  fn(summary.ModuleRequest, Resolved) -> Result(String, ResolveError)

type Load =
  fn(Resolved) -> Result(ModuleSource, LoadError)

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

// createdefaultexportsyntheticmodule, value built at link
pub type DefaultExport {
  JsonExport(value: json.JsonValue)
  TextExport(text: String)
  BytesExport(bytes: BitArray)
}

pub type LoadedModule {
  SourceTextModule(module: SourceModule)
  DefaultExportModule(default_export: DefaultExport)
}

pub fn specifier_map(m: SourceModule) -> specifier.SpecifierMap {
  use acc, #(request, resolved) <- list.fold(m.edges, specifier.new_map())
  specifier.insert(acc, request.request, resolved)
}

pub type SourceGraph {
  SourceGraph(
    entry: Resolved,
    modules: Dict(Resolved, LoadedModule),
    // dependencies first, entry last (dfs post-order)
    order: List(Resolved),
  )
}

pub type GraphError {
  ParseFailed(specifier: Resolved, error: parser.ParseError)
  JsonParseFailed(specifier: Resolved, message: String)
  UnsupportedImportAttribute(raw: Raw, referrer: Resolved, key: String)
  ResolveFailed(raw: Raw, referrer: Resolved, error: ResolveError)
  LoadFailed(specifier: Resolved, error: LoadError)
  SourcePhaseUnsupported(specifier: Resolved)
}

type Analyzed {
  AnalyzedSource(ParsedModule)
  AnalyzedDefaultExport(DefaultExport)
}

fn analyze(
  specifier: Resolved,
  source: ModuleSource,
) -> Result(Analyzed, GraphError) {
  case source {
    loader.SourceText(source:) ->
      parse_and_analyze(specifier, source) |> result.map(AnalyzedSource)
    // parsejsonmodule
    loader.JsonSource(source:) ->
      json.parse_module_source(source)
      |> result.map(fn(value) { AnalyzedDefaultExport(JsonExport(value)) })
      |> result.map_error(JsonParseFailed(specifier, _))
    loader.TextSource(text:) -> Ok(AnalyzedDefaultExport(TextExport(text)))
    loader.BytesSource(bytes:) -> Ok(AnalyzedDefaultExport(BytesExport(bytes)))
  }
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
    modules: Dict(Resolved, LoadedModule),
    order: List(Resolved),
  )
}

// is_host specifiers are leaves, never loaded or parsed
pub fn load(
  entry_specifier: Resolved,
  entry_source: ModuleSource,
  resolve: Resolve,
  load_source: Load,
  is_host: fn(Resolved) -> Bool,
) -> Result(SourceGraph, GraphError) {
  use entry <- result.try(analyze(entry_specifier, entry_source))
  use walk <- result.map(visit(
    entry_specifier,
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
  key: Resolved,
  node: Analyzed,
  resolve: Resolve,
  load_source: Load,
  is_host: fn(Resolved) -> Bool,
  walk: Walk,
) -> Result(Walk, GraphError) {
  // mark before walking deps so cycles terminate
  let walk = Walk(..walk, started: set.insert(walk.started, key))
  case node {
    AnalyzedDefaultExport(default_export) ->
      Ok(finish(walk, key, DefaultExportModule(default_export)))
    AnalyzedSource(parsed) ->
      visit_source(parsed, resolve, load_source, is_host, walk)
  }
}

fn finish(walk: Walk, specifier: Resolved, module: LoadedModule) -> Walk {
  Walk(..walk, modules: dict.insert(walk.modules, specifier, module), order: [
    specifier,
    ..walk.order
  ])
}

// innermoduleloading: attributes checked before the host resolves
fn visit_source(
  node: ParsedModule,
  resolve: Resolve,
  load_source: Load,
  is_host: fn(Resolved) -> Bool,
  walk: Walk,
) -> Result(Walk, GraphError) {
  let referrer = node.specifier
  use #(edges, walk) <- result.try(
    list.try_fold(node.summary.requested, #([], walk), fn(acc, request) {
      let #(edges, walk) = acc
      let specifier.Request(specifier: raw, attributes:) = request.request
      use Nil <- result.try(case loader.unsupported_attribute(attributes) {
        Some(key) -> Error(UnsupportedImportAttribute(raw, referrer, key))
        None -> Ok(Nil)
      })
      use path <- result.try(
        resolve(request, referrer)
        |> result.map_error(ResolveFailed(raw, referrer, _)),
      )
      let resolved = specifier.resolved_with(path, attributes)
      let edges = [#(request, resolved), ..edges]
      use <- bool.guard(is_host(resolved), Ok(#(edges, walk)))
      use <- bool.guard(
        set.contains(walk.started, resolved),
        Ok(#(edges, walk)),
      )
      use source <- result.try(
        load_source(resolved)
        |> result.map_error(LoadFailed(resolved, _)),
      )
      use dep <- result.try(analyze(resolved, source))
      use walk <- result.map(visit(
        resolved,
        dep,
        resolve,
        load_source,
        is_host,
        walk,
      ))
      #(edges, walk)
    }),
  )
  // §16.2.1.7.2 checked after resolve so resolve errors win
  use <- bool.guard(
    node.summary.has_source_phase,
    Error(SourcePhaseUnsupported(referrer)),
  )
  let module = SourceModule(parsed: node, edges: list.reverse(edges))
  Ok(finish(walk, referrer, SourceTextModule(module)))
}
