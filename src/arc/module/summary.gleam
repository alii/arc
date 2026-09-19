// §16.2.1.2 static import/export entries from the ast alone

import arc/module/specifier.{type Raw}
import arc/parser/ast
import gleam/list
import gleam/option.{type Option, None, Some}

pub type Phase {
  Evaluation
  Deferred
}

pub type ImportBinding {
  NamedImport(imported: String, local: String)
  DefaultImport(local: String)
  NamespaceImport(local: String, phase: Phase)
}

// the spec local name of an anonymous export default
pub const default_export_local_name = "*default*"

pub type ExportEntry {
  LocalExport(export_name: String, local_name: String)
  ReExport(export_name: String, imported_name: String, source_specifier: Raw)
  ReExportAll(source_specifier: Raw)
  ReExportNamespace(export_name: String, source_specifier: Raw)
}

pub type ModuleRequest {
  ModuleRequest(
    specifier: Raw,
    // deferred only if every reference is import defer
    phase: Phase,
  )
}

pub type ModuleSummary {
  ModuleSummary(
    imports: List(#(Raw, List(ImportBinding))),
    exports: List(ExportEntry),
    requested: List(ModuleRequest),
    has_source_phase: Bool,
  )
}

// declaration order = compiler capture slot order
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
    requested: List(ModuleRequest),
    has_source_phase: Bool,
  )
}

pub fn analyze(items: List(ast.ModuleItem)) -> ModuleSummary {
  let empty =
    Analysis(imports: [], exports: [], requested: [], has_source_phase: False)
  let analysis = list.fold(items, empty, analyze_item)
  ModuleSummary(
    imports: list.reverse(analysis.imports),
    exports: list.reverse(analysis.exports),
    requested: merge_requests(list.reverse(analysis.requested)),
    has_source_phase: analysis.has_source_phase,
  )
}

fn analyze_item(acc: Analysis, item: ast.ModuleItem) -> Analysis {
  case item {
    ast.ImportDeclaration(specifiers:, source:, phase:, ..) -> {
      let request_phase = case phase {
        ast.PhaseDefer -> Deferred
        ast.PhaseEvaluation | ast.PhaseSource -> Evaluation
      }
      Analysis(
        imports: [
          #(specifier.raw(source), declaration_bindings(specifiers, phase)),
          ..acc.imports
        ],
        exports: acc.exports,
        requested: [
          ModuleRequest(specifier: specifier.raw(source), phase: request_phase),
          ..acc.requested
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
      // §16.2.1.3: `export {} from "m"` still requested m
      let requested = case item {
        ast.ExportNamed(source: Some(source), ..) -> [
          ModuleRequest(specifier: specifier.raw(source), phase: Evaluation),
          ..acc.requested
        ]
        ast.ExportNamed(source: None, ..)
        | ast.ExportDeclaration(..)
        | ast.ExportDefaultDeclaration(..)
        | ast.ExportAllDeclaration(..)
        | ast.ImportDeclaration(..)
        | ast.StatementItem(_) -> acc.requested
      }
      let requested =
        list.fold(entries, requested, fn(requested, entry) {
          case request_of_entry(entry) {
            Ok(request) -> [request, ..requested]
            Error(Nil) -> requested
          }
        })
      Analysis(..acc, exports:, requested:)
    }
  }
}

fn request_of_entry(entry: ExportEntry) -> Result(ModuleRequest, Nil) {
  case entry {
    ReExport(source_specifier:, ..)
    | ReExportAll(source_specifier:)
    | ReExportNamespace(source_specifier:, ..) ->
      Ok(ModuleRequest(specifier: source_specifier, phase: Evaluation))
    LocalExport(..) -> Error(Nil)
  }
}

// dedup by specifier; first eager request wins phase and position
fn merge_requests(requested: List(ModuleRequest)) -> List(ModuleRequest) {
  let merged: List(ModuleRequest) = []
  list.fold(requested, merged, fn(merged, request) {
    let seen =
      list.find(merged, fn(existing) { existing.specifier == request.specifier })
    case seen, request.phase {
      Error(Nil), _ -> list.append(merged, [request])
      Ok(ModuleRequest(phase: Evaluation, ..)), _ -> merged
      Ok(ModuleRequest(phase: Deferred, ..)), Deferred -> merged
      Ok(ModuleRequest(phase: Deferred, ..)), Evaluation ->
        list.append(
          list.filter(merged, fn(e) { e.specifier != request.specifier }),
          [request],
        )
    }
  })
}

fn declaration_bindings(
  specifiers: List(ast.ImportSpecifier),
  declaration_phase: ast.ImportPhase,
) -> List(ImportBinding) {
  let phase = case declaration_phase {
    ast.PhaseDefer -> Deferred
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
    // §16.2.3.7 named default decls bind their own name
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
      LocalExport(export_name: "default", local_name: default_export_local_name),
    ]
    ast.ExportNamed(specifiers:, source: Some(source), ..) ->
      list.map(specifiers, fn(spec) {
        case spec {
          ast.ExportSpecifier(local:, exported:, ..) ->
            ReExport(
              export_name: exported,
              imported_name: local,
              source_specifier: specifier.raw(source),
            )
        }
      })
    ast.ExportAllDeclaration(exported: Some(name), source:, ..) -> [
      ReExportNamespace(
        export_name: name,
        source_specifier: specifier.raw(source),
      ),
    ]
    ast.ExportAllDeclaration(exported: None, source:, ..) -> [
      ReExportAll(source_specifier: specifier.raw(source)),
    ]
    ast.StatementItem(_) | ast.ImportDeclaration(..) -> []
  }
}

fn declaration_exports(declaration: ast.Declaration) -> List(ExportEntry) {
  case declaration {
    ast.DeclareVariable(declarations:, ..) ->
      list.flat_map(declarations, fn(decl) {
        ast.pattern_bound_names(decl.id) |> list.map(self_export)
      })
    ast.DeclareFunction(function:) -> binding_exports(function.name)
    ast.DeclareClass(name:, ..) -> binding_exports(name)
  }
}

fn binding_exports(name: Option(ast.NamedBinding)) -> List(ExportEntry) {
  ast.binding_name(name)
  |> option.map(fn(n) { [self_export(n)] })
  |> option.unwrap([])
}

fn self_export(name: String) -> ExportEntry {
  LocalExport(export_name: name, local_name: name)
}
