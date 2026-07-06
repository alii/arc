//// Static semantics of ES modules, computed from the AST alone — the spec's
//// ImportEntries / ExportEntries / ModuleRequests (§16.2.1.2), with no
//// dependency on the compiler or VM. This is the layer to build module-graph
//// tooling on (bundlers, dev servers, linters): parse with `arc/parser`,
//// then `analyze` for the metadata.
//// `arc/module/graph` composes this with a host resolver into a full graph
//// walk, and the runtime's `arc/module.compile_bundle` builds on the same
//// layers.
////
//// `analyze` makes exactly one pass over the top-level items and never
//// descends into statement or expression bodies — O(declarations), not
//// O(AST). Deeper analyses (top-level await, dynamic imports) will be
//// separate opt-in functions so consumers only pay for what they use.

import arc/parser/ast
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

// =============================================================================
// Specifiers: raw (as written) vs resolved (module identity)
// =============================================================================
//
// A module specifier exists in two incompatible flavours, and mixing them is
// the module system's classic silent bug: `./m` (what the source wrote) is
// meaningless without a referrer, while `m` (what the host resolved it to) IS
// the module's identity — the key of every graph, bundle and cache. They are
// distinct types so a swap is a compile error, and `resolve/2` is the ONLY
// bridge between them.

/// A module specifier exactly as written in source: `"./m"`, `"../lib/x.js"`,
/// `"node:fs"`. Only meaningful relative to the module that wrote it, so it
/// can never be a graph key.
pub opaque type Raw {
  Raw(String)
}

/// A specifier the host resolved to a canonical module identity — the key
/// every module graph, bundle and registry is keyed by. Two requests that
/// resolve to the same `Resolved` are the same module.
pub opaque type Resolved {
  Resolved(String)
}

/// The specifier as written in source. Free: source text is raw by definition.
pub fn raw(text: String) -> Raw {
  Raw(text)
}

/// The source text of a raw specifier — for error messages, which quote what
/// the programmer wrote, and for handing to a host resolver.
pub fn raw_text(r: Raw) -> String {
  let Raw(text) = r
  text
}

/// Assert `text` is already a canonical module identity. The only way to mint
/// a `Resolved` outside `resolve/2`, and deliberately named so its callers
/// stand out: the value a host resolver returned, the entry specifier the
/// embedder named, and `arc/internal/path.resolve_specifier`'s normalized path
/// (which is what such a resolver returns).
pub fn resolved_unchecked(text: String) -> Resolved {
  Resolved(text)
}

/// The identity string of a resolved specifier — for keying the heap-resident
/// caches, and for error messages naming a concrete module.
pub fn resolved_text(r: Resolved) -> String {
  let Resolved(text) = r
  text
}

/// One module's raw → resolved projection. TOTAL over that module's own
/// requests: `arc/module/graph` builds it with one entry per `ModuleRequest`,
/// so a raw specifier taken from the same module's import/export entries always
/// has an entry.
pub opaque type SpecifierMap {
  SpecifierMap(entries: Dict(Raw, Resolved))
}

/// A module with no dependencies (a host/synthetic module) resolves nothing.
pub fn new_specifier_map() -> SpecifierMap {
  SpecifierMap(dict.new())
}

pub fn insert_specifier(
  map: SpecifierMap,
  from: Raw,
  to: Resolved,
) -> SpecifierMap {
  let SpecifierMap(entries) = map
  SpecifierMap(dict.insert(entries, from, to))
}

/// The one bridge from `Raw` to `Resolved`. `None` means `r` is not one of this
/// module's requests — unreachable for a map projected off the module's own
/// graph node, so callers treat it as "no such dependency" rather than falling
/// back to the raw text (which would silently look the WRONG key up in the
/// graph, and occasionally hit).
pub fn resolve(map: SpecifierMap, r: Raw) -> Option(Resolved) {
  let SpecifierMap(entries) = map
  dict.get(entries, r) |> option.from_result
}

/// The phase of a namespace import. Deliberately narrower than the AST's
/// ImportPhase: source-phase declarations (`import source x from "m"`) parse,
/// but neither the runtime nor graph tooling supports them yet — the graph
/// walk rejects them (`graph.SourcePhaseUnsupported`), so they never reach
/// these bindings.
pub type Phase {
  /// The ~evaluation~ phase (`ast.PhaseEvaluation`): `import * as ns from
  /// 'mod'`, and every non-defer form — evaluated eagerly with the importer.
  /// Named for the PHASE, not for `import foo from 'mod'` (that's
  /// `DefaultImport`).
  Evaluation
  /// `import defer * as ns from 'mod'` (defer-import-eval proposal) — the
  /// module's evaluation is deferred until its namespace is first accessed.
  Deferred
}

/// One binding introduced by an import declaration (an ImportEntry, §16.2.1.2).
pub type ImportBinding {
  /// import { foo } from 'mod'  or  import { foo as bar } from 'mod'
  NamedImport(imported: String, local: String)
  /// import foo from 'mod'
  DefaultImport(local: String)
  /// import * as ns from 'mod', at the declaration's phase.
  NamespaceImport(local: String, phase: Phase)
}

/// An export entry maps an exported name to how to find its value
/// (an ExportEntry, §16.2.1.2).
pub type ExportEntry {
  /// Export a local variable: `export let x = 42` or `export { x }`
  /// For default exports, export_name is "default" and local_name is "*default*".
  LocalExport(export_name: String, local_name: String)
  /// Re-export a named binding: `export { x } from 'mod'` or `export { x as y } from 'mod'`
  ReExport(export_name: String, imported_name: String, source_specifier: Raw)
  /// Re-export everything: `export * from 'mod'`
  ReExportAll(source_specifier: Raw)
  /// Re-export everything under a namespace: `export * as ns from 'mod'`
  ReExportNamespace(export_name: String, source_specifier: Raw)
}

/// A ModuleRequest Record (§16.2.1.2): one unique specifier this module
/// references, with the phases of all its declarations merged.
pub type ModuleRequest {
  ModuleRequest(
    specifier: Raw,
    /// ~evaluation~ unless EVERY reference is `import defer * as ns` — a
    /// single eager declaration or re-export of the same specifier forces
    /// evaluation, so eager wins the merge. InnerModuleEvaluation skips
    /// Deferred requests (§16.2.1.5.3.1), deferring the dependency's
    /// evaluation to first namespace access.
    phase: Phase,
  )
}

/// Everything statically knowable about a module's imports and exports.
pub type ModuleSummary {
  ModuleSummary(
    /// Import declarations in source order: (specifier as written, bindings).
    /// A bare `import "m"` contributes an entry with no bindings. Un-merged:
    /// the same specifier may appear in several declarations. The specifier is
    /// `Raw` — meaningless without a referrer, so a consumer must `resolve` it
    /// through the module's `SpecifierMap` before it can index a graph with it
    /// (see `link.project_module`).
    imports: List(#(Raw, List(ImportBinding))),
    exports: List(ExportEntry),
    /// [[RequestedModules]]: the unique specifiers this module references
    /// via import or re-export, in source order, with merged phases.
    requested: List(ModuleRequest),
    /// Whether the module contains a static source-phase import declaration
    /// (`import source x from "m"`). A host with no source-phase module
    /// representation must reject such modules with a SyntaxError
    /// (GetModuleSource always throws for Source Text Module Records,
    /// §16.2.1.7.2) — `arc/module/graph` does so during the walk.
    has_source_phase: Bool,
  )
}

/// The local binding names introduced by a module's import declarations, in
/// declaration order. This is the canonical order of the compiler's capture
/// slots 0..N-1; link-time import seeding must produce box refs in exactly this
/// order.
pub fn import_local_names(summary: ModuleSummary) -> List(String) {
  binding_local_names(summary.imports)
}

/// `import_local_names` off any per-declaration binding list — the compiled
/// module's `import_bindings` carries the same declarations in the same order,
/// keyed by whatever the consumer keyed them by.
pub fn binding_local_names(
  imports: List(#(a, List(ImportBinding))),
) -> List(String) {
  list.flat_map(imports, fn(entry) {
    list.map(entry.1, fn(binding) {
      case binding {
        NamedImport(local:, ..) -> local
        DefaultImport(local:) -> local
        NamespaceImport(local:, ..) -> local
      }
    })
  })
}

type Analysis {
  Analysis(
    imports: List(#(Raw, List(ImportBinding))),
    exports: List(ExportEntry),
    requests: List(ModuleRequest),
    has_source_phase: Bool,
  )
}

/// Full static analysis of a parsed module, in one pass over the top-level
/// items. Takes the module ITEMS (`parser.parse_module`'s first element), not
/// an `ast.Program`: there is no such thing as a script's import/export
/// summary, so there is no empty-summary branch to fall into by mistake.
pub fn analyze(items: List(ast.ModuleItem)) -> ModuleSummary {
  let empty =
    Analysis(imports: [], exports: [], requests: [], has_source_phase: False)
  let analysis = list.fold(items, empty, analyze_item)
  ModuleSummary(
    imports: list.reverse(analysis.imports),
    exports: list.reverse(analysis.exports),
    requested: merge_requests(list.reverse(analysis.requests)),
    has_source_phase: analysis.has_source_phase,
  )
}

/// One top-level item into the accumulator. Lists are built reversed.
fn analyze_item(acc: Analysis, item: ast.ModuleItem) -> Analysis {
  case item {
    ast.ImportDeclaration(
      specifiers:,
      source: ast.StringLit(source),
      phase:,
      ..,
    ) -> {
      // The request's phase comes straight off the declaration: only
      // `import defer * as ns` defers; bare and binding-carrying forms are
      // eager. (PhaseSource requests are recorded eagerly — the graph walk
      // rejects the whole module before the phase could matter.)
      let request_phase = case phase {
        ast.PhaseDefer -> Deferred
        ast.PhaseEvaluation | ast.PhaseSource -> Evaluation
      }
      Analysis(
        imports: [
          #(Raw(source), declaration_bindings(specifiers, phase)),
          ..acc.imports
        ],
        exports: acc.exports,
        requests: [
          ModuleRequest(specifier: Raw(source), phase: request_phase),
          ..acc.requests
        ],
        has_source_phase: acc.has_source_phase || phase == ast.PhaseSource,
      )
    }
    ast.StatementItem(_) -> acc
    ast.ExportDeclaration(..)
    | ast.ExportNamed(..)
    | ast.ExportDefaultDeclaration(..)
    | ast.ExportAllDeclaration(..) -> {
      let entries = export_entries(item)
      let exports =
        list.fold(entries, acc.exports, fn(exports, entry) {
          [entry, ..exports]
        })
      // §16.2.1.3 ModuleRequests: a `from` clause requests its module even
      // when it re-exports NOTHING (`export {} from "m"` — the module is
      // still fetched, linked and evaluated for its side effects). That case
      // yields zero export entries, so the request cannot be derived from
      // them; record it off the declaration itself. `merge_requests` dedups
      // it against the per-specifier requests below.
      let requests = case item {
        ast.ExportNamed(source: Some(ast.StringLit(source)), ..) -> [
          ModuleRequest(specifier: Raw(source), phase: Evaluation),
          ..acc.requests
        ]
        ast.ExportNamed(source: None, ..)
        | ast.ExportDeclaration(..)
        | ast.ExportDefaultDeclaration(..)
        | ast.ExportAllDeclaration(..)
        | ast.ImportDeclaration(..)
        | ast.StatementItem(_) -> acc.requests
      }
      let requests =
        list.fold(entries, requests, fn(requests, entry) {
          case request_of_entry(entry) {
            Ok(request) -> [request, ..requests]
            Error(Nil) -> requests
          }
        })
      Analysis(..acc, exports:, requests:)
    }
  }
}

/// Re-exports request their source module at phase ~evaluation~.
fn request_of_entry(entry: ExportEntry) -> Result(ModuleRequest, Nil) {
  case entry {
    ReExport(source_specifier:, ..)
    | ReExportAll(source_specifier:)
    | ReExportNamespace(source_specifier:, ..) ->
      Ok(ModuleRequest(specifier: source_specifier, phase: Evaluation))
    LocalExport(..) -> Error(Nil)
  }
}

/// Deduplicate requests by specifier, keeping source order. Eager wins the
/// PHASE (a specifier is Deferred only if EVERY request for it is
/// `import defer * as ns`) — and the first eager request also wins the
/// POSITION: §16.2.1.5.3.1 InnerModuleEvaluation walks the spec's
/// per-(specifier, phase) request list in order, SKIPPING ~defer~ entries,
/// so a module imported `defer` first and eagerly later evaluates at the
/// LATER, eager position (test262 import-defer/evaluation-sync/
/// module-imported-defer-and-eager.js). The bundle's flat per-specifier
/// lists cannot carry two entries for one specifier, so the merged entry
/// takes the first eager occurrence's slot. Residual (unobservable without
/// top-level await in the deferred subgraph): such a module's async
/// transitive dependencies are gathered at the eager position instead of
/// the earlier defer position.
fn merge_requests(requests: List(ModuleRequest)) -> List(ModuleRequest) {
  let merged: List(ModuleRequest) = []
  list.fold(requests, merged, fn(merged, request) {
    let seen =
      list.find(merged, fn(existing) { existing.specifier == request.specifier })
    case seen, request.phase {
      Error(Nil), _ -> list.append(merged, [request])
      // Already eager: the earliest eager occurrence keeps the slot.
      Ok(ModuleRequest(phase: Evaluation, ..)), _ -> merged
      // Deferred so far, another deferred request: nothing changes.
      Ok(ModuleRequest(phase: Deferred, ..)), Deferred -> merged
      // Deferred so far, FIRST eager request: eager phase + this position.
      Ok(ModuleRequest(phase: Deferred, ..)), Evaluation ->
        list.append(
          list.filter(merged, fn(e) { e.specifier != request.specifier }),
          [request],
        )
    }
  })
}

/// The bindings of one import declaration, at the declaration's phase.
fn declaration_bindings(
  specifiers: List(ast.ImportSpecifier),
  declaration_phase: ast.ImportPhase,
) -> List(ImportBinding) {
  let phase = case declaration_phase {
    ast.PhaseDefer -> Deferred
    // A PhaseSource declaration binds no namespace (the grammar only allows
    // `import source x`), so this arm only ever fires for PhaseEvaluation.
    ast.PhaseEvaluation | ast.PhaseSource -> Evaluation
  }
  list.map(specifiers, fn(spec) {
    case spec {
      ast.ImportNamedSpecifier(imported:, local:, ..) ->
        NamedImport(imported:, local:)
      ast.ImportDefaultSpecifier(local:, ..) -> DefaultImport(local:)
      ast.ImportNamespaceSpecifier(local:, ..) ->
        NamespaceImport(local:, phase:)
    }
  })
}

/// The export entries of one top-level item ([] for non-exports).
fn export_entries(item: ast.ModuleItem) -> List(ExportEntry) {
  case item {
    ast.ExportDeclaration(declaration:, ..) -> declaration_exports(declaration)
    ast.ExportNamed(specifiers:, source: None, ..) ->
      list.map(specifiers, fn(spec) {
        case spec {
          ast.ExportSpecifier(local:, exported:, ..) ->
            LocalExport(export_name: exported, local_name: local)
        }
      })
    // §16.2.3.7: `export default function fn() {}` / `class fn {}` bind
    // the NAME (BoundNames = « fn »); only anonymous defaults use the
    // synthetic *default* binding.
    ast.ExportDefaultDeclaration(
      declaration: ast.FunctionExpression(
        name: Some(ast.NamedBinding(name:, ..)),
        ..,
      ),
      ..,
    ) -> [LocalExport(export_name: "default", local_name: name)]
    ast.ExportDefaultDeclaration(
      declaration: ast.ClassExpression(
        name: Some(ast.NamedBinding(name:, ..)),
        ..,
      ),
      ..,
    ) -> [LocalExport(export_name: "default", local_name: name)]
    ast.ExportDefaultDeclaration(..) -> [
      LocalExport(export_name: "default", local_name: "*default*"),
    ]
    // Re-exports from other modules
    ast.ExportNamed(specifiers:, source: Some(ast.StringLit(source)), ..) ->
      list.map(specifiers, fn(spec) {
        case spec {
          ast.ExportSpecifier(local:, exported:, ..) ->
            ReExport(
              export_name: exported,
              imported_name: local,
              source_specifier: Raw(source),
            )
        }
      })
    ast.ExportAllDeclaration(
      exported: Some(name),
      source: ast.StringLit(source),
      ..,
    ) -> [
      ReExportNamespace(export_name: name, source_specifier: Raw(source)),
    ]
    ast.ExportAllDeclaration(exported: None, source: ast.StringLit(source), ..) -> [
      ReExportAll(source_specifier: Raw(source)),
    ]
    ast.StatementItem(_) | ast.ImportDeclaration(..) -> []
  }
}

/// Exported names of `export let x = 42` / `export const { a, b } = o` /
/// `export function f() {}` / `export class C {}`. §16.2.3.3: the
/// ExportedNames of an exported VariableDeclaration are the BoundNames of
/// every declarator's binding target — including everything a destructuring
/// pattern binds. `ast.Declaration` has exactly these three variants, so
/// there is no "some other statement got exported" arm to write.
fn declaration_exports(declaration: ast.Declaration) -> List(ExportEntry) {
  case declaration {
    ast.DeclVariable(declarations:, ..) ->
      list.flat_map(declarations, fn(decl) {
        ast.pattern_bound_names(decl.id) |> list.map(self_export)
      })
    // `export function`/`export class` require a name (§16.2.1 — only
    // `export default` admits the anonymous forms), so `None` is unreachable.
    ast.DeclFunction(function:) -> binding_exports(function.name)
    ast.DeclClass(name:, ..) -> binding_exports(name)
  }
}

/// `export function f() {}` — the local binding IS the exported name.
fn binding_exports(name: Option(ast.NamedBinding)) -> List(ExportEntry) {
  ast.binding_name(name)
  |> option.map(fn(n) { [self_export(n)] })
  |> option.unwrap([])
}

fn self_export(name: String) -> ExportEntry {
  LocalExport(export_name: name, local_name: name)
}
