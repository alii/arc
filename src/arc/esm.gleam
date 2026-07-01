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
import gleam/list
import gleam/option.{None, Some}

/// The phase of a namespace import. Deliberately narrower than the AST's
/// ImportPhase: source-phase declarations (`import source x from "m"`) parse,
/// but neither the runtime nor graph tooling supports them yet — the graph
/// walk rejects them (`graph.SourcePhaseUnsupported`), so they never reach
/// these bindings.
pub type Phase {
  /// `import * as ns from 'mod'` — evaluated eagerly with the importer.
  Default
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
  ReExport(export_name: String, imported_name: String, source_specifier: String)
  /// Re-export everything: `export * from 'mod'`
  ReExportAll(source_specifier: String)
  /// Re-export everything under a namespace: `export * as ns from 'mod'`
  ReExportNamespace(export_name: String, source_specifier: String)
}

/// A ModuleRequest Record (§16.2.1.2): one unique specifier this module
/// references, with the phases of all its declarations merged.
pub type ModuleRequest {
  ModuleRequest(
    specifier: String,
    /// Default (~evaluation~) unless EVERY reference is `import defer * as
    /// ns` — a single eager declaration or re-export of the same specifier
    /// forces evaluation, so eager wins the merge. InnerModuleEvaluation
    /// skips Deferred requests (§16.2.1.5.3.1), deferring the dependency's
    /// evaluation to first namespace access.
    phase: Phase,
  )
}

/// Everything statically knowable about a module's imports and exports.
pub type ModuleSummary {
  ModuleSummary(
    /// Import declarations in source order: (raw_specifier, bindings).
    /// A bare `import "m"` contributes an entry with no bindings. Un-merged:
    /// the same specifier may appear in several declarations.
    imports: List(#(String, List(ImportBinding))),
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

type Analysis {
  Analysis(
    imports: List(#(String, List(ImportBinding))),
    exports: List(ExportEntry),
    requests: List(ModuleRequest),
    has_source_phase: Bool,
  )
}

/// Full static analysis of a parsed module, in one pass over the top-level
/// items. For a Script, everything is empty.
pub fn analyze(program: ast.Program) -> ModuleSummary {
  case program {
    ast.Script(_) ->
      ModuleSummary(
        imports: [],
        exports: [],
        requested: [],
        has_source_phase: False,
      )
    ast.Module(body) -> {
      let empty =
        Analysis(
          imports: [],
          exports: [],
          requests: [],
          has_source_phase: False,
        )
      let analysis = list.fold(body, empty, analyze_item)
      ModuleSummary(
        imports: list.reverse(analysis.imports),
        exports: list.reverse(analysis.exports),
        requested: merge_requests(list.reverse(analysis.requests)),
        has_source_phase: analysis.has_source_phase,
      )
    }
  }
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
        ast.PhaseEvaluation | ast.PhaseSource -> Default
      }
      Analysis(
        imports: [
          #(source, declaration_bindings(specifiers, phase)),
          ..acc.imports
        ],
        exports: acc.exports,
        requests: [
          ModuleRequest(specifier: source, phase: request_phase),
          ..acc.requests
        ],
        has_source_phase: acc.has_source_phase || phase == ast.PhaseSource,
      )
    }
    ast.StatementItem(_) -> acc
    _ -> {
      let entries = export_entries(item)
      let exports =
        list.fold(entries, acc.exports, fn(exports, entry) {
          [entry, ..exports]
        })
      let requests =
        list.fold(entries, acc.requests, fn(requests, entry) {
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
      Ok(ModuleRequest(specifier: source_specifier, phase: Default))
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
      Ok(ModuleRequest(phase: Default, ..)), _ -> merged
      // Deferred so far, another deferred request: nothing changes.
      Ok(ModuleRequest(phase: Deferred, ..)), Deferred -> merged
      // Deferred so far, FIRST eager request: eager phase + this position.
      Ok(ModuleRequest(phase: Deferred, ..)), Default ->
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
    ast.PhaseEvaluation | ast.PhaseSource -> Default
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
    ast.ExportNamedDeclaration(declaration:, specifiers:, source: None, ..) ->
      named_exports(declaration, specifiers)
    // §16.2.3.7: `export default function fn() {}` / `class fn {}` bind
    // the NAME (BoundNames = « fn »); only anonymous defaults use the
    // synthetic *default* binding.
    ast.ExportDefaultDeclaration(
      declaration: ast.FunctionExpression(name: Some(name), ..),
      ..,
    ) -> [LocalExport(export_name: "default", local_name: name)]
    ast.ExportDefaultDeclaration(
      declaration: ast.ClassExpression(name: Some(name), ..),
      ..,
    ) -> [LocalExport(export_name: "default", local_name: name)]
    ast.ExportDefaultDeclaration(..) -> [
      LocalExport(export_name: "default", local_name: "*default*"),
    ]
    // Re-exports from other modules
    ast.ExportNamedDeclaration(
      declaration: _,
      specifiers:,
      source: Some(ast.StringLit(source)),
      ..,
    ) ->
      list.map(specifiers, fn(spec) {
        case spec {
          ast.ExportSpecifier(local:, exported:, ..) ->
            ReExport(
              export_name: exported,
              imported_name: local,
              source_specifier: source,
            )
        }
      })
    ast.ExportAllDeclaration(
      exported: Some(name),
      source: ast.StringLit(source),
      ..,
    ) -> [
      ReExportNamespace(export_name: name, source_specifier: source),
    ]
    ast.ExportAllDeclaration(exported: None, source: ast.StringLit(source), ..) -> [
      ReExportAll(source_specifier: source),
    ]
    _ -> []
  }
}

/// Exported names from a named export declaration.
fn named_exports(
  declaration: option.Option(ast.Statement),
  specifiers: List(ast.ExportSpecifier),
) -> List(ExportEntry) {
  // From specifiers: `export { a, b as c }`
  let spec_exports =
    list.map(specifiers, fn(spec) {
      case spec {
        ast.ExportSpecifier(local:, exported:, ..) ->
          LocalExport(export_name: exported, local_name: local)
      }
    })

  // From declaration: `export let x = 42`, `export function f() {}`
  let decl_exports = case declaration {
    Some(ast.VariableDeclaration(declarations:, ..)) ->
      list.filter_map(declarations, fn(decl) {
        case decl {
          ast.VariableDeclarator(id: ast.IdentifierPattern(name:, ..), ..) ->
            Ok(LocalExport(export_name: name, local_name: name))
          _ -> Error(Nil)
        }
      })
    Some(ast.FunctionDeclaration(name: Some(name), ..)) -> [
      LocalExport(export_name: name, local_name: name),
    ]
    Some(ast.ClassDeclaration(name: Some(name), ..)) -> [
      LocalExport(export_name: name, local_name: name),
    ]
    _ -> []
  }

  list.append(spec_exports, decl_exports)
}
