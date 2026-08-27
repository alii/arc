// §16.2.1.6.3 resolveexport over a runtime-free module view

import arc/esm.{type Raw, type Resolved}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/result
import gleam/set.{type Set}

pub type LinkableModule {
  LinkableModule(
    import_bindings: List(#(Raw, Resolved, List(esm.ImportBinding))),
    export_entries: List(LinkableExport),
    star_exports: List(Resolved),
  )
}

pub type LinkableExport {
  LocalExport(export_name: String, local_name: String)
  ReExport(
    export_name: String,
    imported_name: String,
    source: Raw,
    dep: Resolved,
  )
  ReExportNamespace(export_name: String, dep: Resolved)
}

pub type LinkableGraph =
  Dict(Resolved, LinkableModule)

// error(raw) means a broken specifier map, never guest code
pub fn project_module(
  import_bindings: List(#(Raw, List(esm.ImportBinding))),
  export_entries: List(esm.ExportEntry),
  specifier_map: esm.SpecifierMap,
) -> Result(LinkableModule, Raw) {
  let resolve = fn(raw: Raw) {
    esm.resolve(specifier_map, raw) |> option.to_result(raw)
  }
  use imports <- result.try(
    list.try_map(import_bindings, fn(entry) {
      let #(raw_dep, bindings) = entry
      use dep <- result.map(resolve(raw_dep))
      #(raw_dep, dep, bindings)
    }),
  )
  use #(exports, stars) <- result.map(
    list.try_fold(export_entries, #([], []), fn(acc, e) {
      let #(exports, stars) = acc
      case e {
        esm.LocalExport(export_name:, local_name:) ->
          Ok(#([LocalExport(export_name:, local_name:), ..exports], stars))
        esm.ReExport(export_name:, imported_name:, source_specifier:) -> {
          use dep <- result.map(resolve(source_specifier))
          let entry =
            ReExport(
              export_name:,
              imported_name:,
              source: source_specifier,
              dep:,
            )
          #([entry, ..exports], stars)
        }
        esm.ReExportNamespace(export_name:, source_specifier:) -> {
          use dep <- result.map(resolve(source_specifier))
          #([ReExportNamespace(export_name:, dep:), ..exports], stars)
        }
        esm.ReExportAll(source_specifier:) -> {
          use dep <- result.map(resolve(source_specifier))
          #(exports, [dep, ..stars])
        }
      }
    }),
  )
  LinkableModule(
    import_bindings: imports,
    export_entries: list.reverse(exports),
    star_exports: list.reverse(stars),
  )
}

pub type ExportResolution {
  ResolvedTo(module: Resolved, binding: String)
  ResolvedNamespace(module: Resolved)
  ResolvedDeferredNamespace(module: Resolved)
  Unresolvable
  Ambiguous
}

// §16.2.1.6.3 resolveexport
pub fn resolve_export(
  graph: LinkableGraph,
  specifier: Resolved,
  name: String,
) -> ExportResolution {
  resolve_export_set(graph, specifier, name, set.new())
}

fn resolve_export_set(
  graph: LinkableGraph,
  specifier: Resolved,
  name: String,
  resolve_set: Set(#(Resolved, String)),
) -> ExportResolution {
  case set.contains(resolve_set, #(specifier, name)) {
    True -> Unresolvable
    False ->
      case dict.get(graph, specifier) {
        Error(Nil) -> Unresolvable
        Ok(m) ->
          resolve_export_in(
            graph,
            m,
            specifier,
            name,
            set.insert(resolve_set, #(specifier, name)),
          )
      }
  }
}

fn resolve_export_in(
  graph: LinkableGraph,
  m: LinkableModule,
  specifier: Resolved,
  name: String,
  resolve_set: Set(#(Resolved, String)),
) -> ExportResolution {
  // local and named re-exports before export * (steps 4-6, 7)
  let direct =
    list.find_map(m.export_entries, fn(e) {
      case e {
        LocalExport(export_name:, local_name:) if export_name == name ->
          Ok(resolve_local_export(graph, m, specifier, local_name, resolve_set))
        ReExport(export_name:, imported_name:, dep:, ..)
          if export_name == name
        -> Ok(resolve_export_set(graph, dep, imported_name, resolve_set))
        ReExportNamespace(export_name:, dep:) if export_name == name ->
          Ok(ResolvedNamespace(dep))
        _ -> Error(Nil)
      }
    })
  case direct {
    Ok(resolution) -> resolution
    Error(Nil) ->
      case name {
        // export * never provides default (step 6)
        "default" -> Unresolvable
        _ -> resolve_star_exports(graph, m, name, resolve_set)
      }
  }
}

// a local export of an import binding resolves through the import
fn resolve_local_export(
  graph: LinkableGraph,
  m: LinkableModule,
  specifier: Resolved,
  local_name: String,
  resolve_set: Set(#(Resolved, String)),
) -> ExportResolution {
  let import_binding =
    list.find_map(m.import_bindings, fn(entry) {
      let #(_raw_dep, dep, bindings) = entry
      list.find_map(bindings, fn(binding) {
        case binding {
          esm.NamedImport(local:, ..) if local == local_name ->
            Ok(#(dep, binding))
          esm.DefaultImport(local:) if local == local_name -> Ok(#(dep, binding))
          esm.NamespaceImport(local:, ..) if local == local_name ->
            Ok(#(dep, binding))
          _ -> Error(Nil)
        }
      })
    })
  case import_binding {
    Error(Nil) -> ResolvedTo(specifier, local_name)
    Ok(#(dep, binding)) ->
      case binding {
        esm.NamedImport(imported:, ..) ->
          resolve_export_set(graph, dep, imported, resolve_set)
        esm.DefaultImport(..) ->
          resolve_export_set(graph, dep, "default", resolve_set)
        esm.NamespaceImport(phase: esm.Deferred, ..) ->
          ResolvedDeferredNamespace(dep)
        esm.NamespaceImport(phase: esm.Evaluation, ..) -> ResolvedNamespace(dep)
      }
  }
}

// §16.2.1.6.3 step 7
fn resolve_star_exports(
  graph: LinkableGraph,
  m: LinkableModule,
  name: String,
  resolve_set: Set(#(Resolved, String)),
) -> ExportResolution {
  list.fold(m.star_exports, Unresolvable, fn(acc, src) {
    case acc {
      Ambiguous -> Ambiguous
      _ ->
        case resolve_export_set(graph, src, name, resolve_set), acc {
          Ambiguous, _ -> Ambiguous
          Unresolvable, _ -> acc
          found, Unresolvable -> found
          found, _ ->
            case found == acc {
              True -> acc
              False -> Ambiguous
            }
        }
    }
  })
}

// §16.2.1.6.2 getexportednames
pub fn exported_names(
  graph: LinkableGraph,
  specifier: Resolved,
) -> List(String) {
  exported_names_with(graph, specifier, set.new())
}

fn exported_names_with(
  graph: LinkableGraph,
  spec: Resolved,
  star_set: Set(Resolved),
) -> List(String) {
  case set.contains(star_set, spec), dict.get(graph, spec) {
    True, _ | _, Error(Nil) -> []
    False, Ok(m) -> {
      let star_set = set.insert(star_set, spec)
      let direct = list.map(m.export_entries, fn(e) { e.export_name })
      let star =
        list.flat_map(m.star_exports, fn(src) {
          exported_names_with(graph, src, star_set)
          |> list.filter(fn(n) { n != "default" })
        })
      list.append(direct, star) |> list.unique
    }
  }
}

// §16.2.1.6.4, both surface as syntaxerror
pub type LinkError {
  UnresolvedExport(requested_module: Raw, export_name: String)
  AmbiguousExport(requested_module: Raw, export_name: String)
}

pub fn link_error_message(e: LinkError) -> String {
  case e {
    UnresolvedExport(requested_module:, export_name:) ->
      "The requested module '"
      <> esm.raw_text(requested_module)
      <> "' does not provide an export named '"
      <> export_name
      <> "'"
    AmbiguousExport(requested_module:, export_name:) ->
      "The requested module '"
      <> esm.raw_text(requested_module)
      <> "' provides an ambiguous export named '"
      <> export_name
      <> "'"
  }
}

pub fn validate(graph: LinkableGraph) -> Result(Nil, LinkError) {
  list.try_each(dict.values(graph), fn(m) {
    use Nil <- result.try(check_imports(graph, m))
    check_indirect_exports(graph, m)
  })
}

fn check_imports(
  graph: LinkableGraph,
  m: LinkableModule,
) -> Result(Nil, LinkError) {
  list.try_each(m.import_bindings, fn(entry) {
    let #(raw_dep, dep, bindings) = entry
    list.try_each(bindings, fn(binding) {
      case binding {
        esm.NamespaceImport(..) -> Ok(Nil)
        esm.NamedImport(imported:, ..) ->
          check_dep(graph, dep, raw_dep, imported)
        esm.DefaultImport(..) -> check_dep(graph, dep, raw_dep, "default")
      }
    })
  })
}

// look up the source-side name so the reported name is right
fn check_indirect_exports(
  graph: LinkableGraph,
  m: LinkableModule,
) -> Result(Nil, LinkError) {
  list.try_each(m.export_entries, fn(e) {
    case e {
      ReExport(export_name: _, imported_name:, source:, dep:) ->
        check_dep(graph, dep, source, imported_name)
      LocalExport(..) | ReExportNamespace(..) -> Ok(Nil)
    }
  })
}

fn check_dep(
  graph: LinkableGraph,
  dep: Resolved,
  raw_dep: Raw,
  imported_name: String,
) -> Result(Nil, LinkError) {
  case resolve_export(graph, dep, imported_name) {
    ResolvedTo(..) | ResolvedNamespace(..) | ResolvedDeferredNamespace(..) ->
      Ok(Nil)
    Unresolvable ->
      Error(UnresolvedExport(
        requested_module: raw_dep,
        export_name: imported_name,
      ))
    Ambiguous ->
      Error(AmbiguousExport(
        requested_module: raw_dep,
        export_name: imported_name,
      ))
  }
}
