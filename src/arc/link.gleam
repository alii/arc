//// The shared ESM ResolveExport core (§16.2.1.6.3).
////
//// A string-level `ResolveExport` operating on a minimal `LinkableModule`
//// view — just the three fields the algorithm reads (`import_bindings`,
//// `export_entries`, `specifier_map`); a module's identity is its
//// `LinkableGraph` key, so the record carries no specifier of its own.
//// The runtime (`arc/module`) projects each `CompiledModule` onto a
//// `LinkableModule`, builds a `LinkableGraph`, and calls
//// `resolve_export`/`exported_names`/`validate` — so there is one source of
//// truth for export resolution and the runtime SyntaxError messages.
////
//// This module is runtime-free: it consumes only the linkable projection, so
//// any caller that can build a `LinkableGraph` (the VM, a bundler, a linter)
//// reuses the same resolve body.

import arc/esm
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set.{type Set}

// =============================================================================
// The minimal linkable view
// =============================================================================

/// The string-level projection of a module that ResolveExport needs: exactly
/// the three fields the algorithm reads. The module's own resolved specifier
/// is the `LinkableGraph` key it is stored under, not a field. The VM
/// projects each `CompiledModule` onto this so the resolve body is decoupled
/// from bytecode/heap state.
pub type LinkableModule {
  LinkableModule(
    import_bindings: List(#(String, List(esm.ImportBinding))),
    export_entries: List(esm.ExportEntry),
    specifier_map: Dict(String, String),
  )
}

/// A module graph reduced to the linkable view, keyed by resolved specifier.
pub type LinkableGraph =
  Dict(String, LinkableModule)

// =============================================================================
// ResolveExport (§16.2.1.6.3) — the shared string-level core
// =============================================================================

/// Result of resolving an export name through a linkable graph. The VM consumes
/// these variant names directly (`ResolvedTo`/`ResolvedNamespace`/
/// `ResolvedDeferredNamespace`/`Unresolvable`/`Ambiguous`), so they are public.
pub type ExportResolution {
  /// Resolves to a concrete binding `binding` owned by module `module`.
  ResolvedTo(module: String, binding: String)
  /// Resolves to a module namespace object (`export * as ns from`, or
  /// `export { ns }` of an `import * as ns` binding).
  ResolvedNamespace(module: String)
  /// Resolves to a DEFERRED module namespace (`export { ns }` of an
  /// `import defer * as ns` binding) — importers receive the deferred proxy.
  ResolvedDeferredNamespace(module: String)
  /// No export of this name exists (or only via a circular path).
  Unresolvable
  /// Two distinct `export *` sources provide the name — ambiguous.
  Ambiguous
}

/// §16.2.1.6.3 ResolveExport. Resolve `name` exported from `specifier`.
pub fn resolve_export(
  graph: LinkableGraph,
  specifier: String,
  name: String,
) -> ExportResolution {
  resolve_export_set(graph, specifier, name, set.new())
}

/// §16.2.1.6.3 ResolveExport. `resolve_set` guards circular re-exports.
fn resolve_export_set(
  graph: LinkableGraph,
  specifier: String,
  name: String,
  resolve_set: Set(#(String, String)),
) -> ExportResolution {
  case set.contains(resolve_set, #(specifier, name)) {
    // Already resolving this exact export → circular request, not resolvable.
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
  specifier: String,
  name: String,
  resolve_set: Set(#(String, String)),
) -> ExportResolution {
  // Direct local export, then named/namespace re-export, take priority over
  // `export *` (§16.2.1.6.3 steps 4–6 before step 7).
  let direct =
    list.find_map(m.export_entries, fn(e) {
      case e {
        esm.LocalExport(export_name:, local_name:) if export_name == name ->
          Ok(resolve_local_export(graph, m, specifier, local_name, resolve_set))
        esm.ReExport(export_name:, imported_name:, source_specifier:)
          if export_name == name
        -> {
          let src = resolved_specifier(m, source_specifier)
          Ok(resolve_export_set(graph, src, imported_name, resolve_set))
        }
        esm.ReExportNamespace(export_name:, source_specifier:)
          if export_name == name
        -> Ok(ResolvedNamespace(resolved_specifier(m, source_specifier)))
        _ -> Error(Nil)
      }
    })
  case direct {
    Ok(resolution) -> resolution
    // `default` is never provided by `export *` (step 6).
    Error(Nil) ->
      case name {
        "default" -> Unresolvable
        _ -> resolve_star_exports(graph, m, name, resolve_set)
      }
  }
}

/// A LocalExport (`export { x }` / `export let x`) whose local name is an
/// IMPORT binding is really a re-export: `import { a } from "m"; export { a }`
/// resolves through "m" (§16.2.1.6.3 — the binding is the imported one), and
/// `import [defer] * as ns from "m"; export { ns }` resolves to "m"'s
/// (deferred) namespace. Genuine local bindings resolve to their own cell.
fn resolve_local_export(
  graph: LinkableGraph,
  m: LinkableModule,
  specifier: String,
  local_name: String,
  resolve_set: Set(#(String, String)),
) -> ExportResolution {
  let import_binding =
    list.find_map(m.import_bindings, fn(entry) {
      let #(raw_dep, bindings) = entry
      list.find_map(bindings, fn(binding) {
        case binding {
          esm.NamedImport(local:, ..) if local == local_name ->
            Ok(#(raw_dep, binding))
          esm.DefaultImport(local:) if local == local_name ->
            Ok(#(raw_dep, binding))
          esm.NamespaceImport(local:, ..) if local == local_name ->
            Ok(#(raw_dep, binding))
          _ -> Error(Nil)
        }
      })
    })
  case import_binding {
    Error(Nil) -> ResolvedTo(specifier, local_name)
    Ok(#(raw_dep, binding)) -> {
      let dep = resolved_specifier(m, raw_dep)
      case binding {
        esm.NamedImport(imported:, ..) ->
          resolve_export_set(graph, dep, imported, resolve_set)
        esm.DefaultImport(..) ->
          resolve_export_set(graph, dep, "default", resolve_set)
        esm.NamespaceImport(phase: esm.Deferred, ..) ->
          ResolvedDeferredNamespace(dep)
        esm.NamespaceImport(phase: esm.Default, ..) -> ResolvedNamespace(dep)
      }
    }
  }
}

/// §16.2.1.6.3 step 7: gather across `export *` sources, flagging ambiguity.
fn resolve_star_exports(
  graph: LinkableGraph,
  m: LinkableModule,
  name: String,
  resolve_set: Set(#(String, String)),
) -> ExportResolution {
  let star_sources = star_sources(m)
  list.fold(star_sources, Unresolvable, fn(acc, src) {
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

/// Resolved specifiers of every `export *` source of `m`.
fn star_sources(m: LinkableModule) -> List(String) {
  list.filter_map(m.export_entries, fn(e) {
    case e {
      esm.ReExportAll(source_specifier:) ->
        Ok(resolved_specifier(m, source_specifier))
      _ -> Error(Nil)
    }
  })
}

fn resolved_specifier(m: LinkableModule, raw: String) -> String {
  dict.get(m.specifier_map, raw) |> result.unwrap(raw)
}

// =============================================================================
// GetExportedNames (§16.2.1.6.2) — shared
// =============================================================================

/// §16.2.1.6.2 GetExportedNames — local + indirect names, plus `export *` names
/// (excluding `default`), de-duplicated. Used by the VM's export-cell builder.
pub fn exported_names(graph: LinkableGraph, specifier: String) -> List(String) {
  exported_names_with(graph, specifier, set.new())
}

/// `star_set` guards `export *` cycles.
fn exported_names_with(
  graph: LinkableGraph,
  spec: String,
  star_set: Set(String),
) -> List(String) {
  case set.contains(star_set, spec), dict.get(graph, spec) {
    True, _ | _, Error(Nil) -> []
    False, Ok(m) -> {
      let star_set = set.insert(star_set, spec)
      let direct =
        list.filter_map(m.export_entries, fn(e) {
          case e {
            esm.LocalExport(export_name:, ..) -> Ok(export_name)
            esm.ReExport(export_name:, ..) -> Ok(export_name)
            esm.ReExportNamespace(export_name:, ..) -> Ok(export_name)
            esm.ReExportAll(..) -> Error(Nil)
          }
        })
      let star =
        list.flat_map(m.export_entries, fn(e) {
          case e {
            esm.ReExportAll(source_specifier:) ->
              exported_names_with(
                graph,
                resolved_specifier(m, source_specifier),
                star_set,
              )
              |> list.filter(fn(n) { n != "default" })
            _ -> []
          }
        })
      list.append(direct, star) |> list.unique
    }
  }
}

// =============================================================================
// Validation (§16.2.1.6.4) — the VM's link path; exact SyntaxError strings
// =============================================================================

/// A link-time validation failure (§16.2.1.6.4). Both variants surface to JS
/// as a SyntaxError whose message is `link_error_message`; callers that need
/// to distinguish the two cases match the variant instead of the prose.
pub type LinkError {
  /// An import or indirect re-export names an export the requested module
  /// does not provide. `requested_module` is the RAW specifier as written in
  /// source and `export_name` the name that failed to resolve — both appear
  /// verbatim in the SyntaxError message. `imported_name` is the name looked
  /// up in the source module; it differs from `export_name` only for a
  /// renaming indirect re-export (`export { imported_name as export_name }
  /// from "requested_module"`).
  UnresolvedExport(
    requested_module: String,
    export_name: String,
    imported_name: String,
  )
  /// Two distinct `export *` sources provide `export_name` (§16.2.1.6.3
  /// step 7) — the binding is ambiguous.
  AmbiguousExport(requested_module: String, export_name: String)
}

/// The exact JS-visible SyntaxError message for a link failure. This is the
/// only place the prose lives — the runtime, tests, and any tooling all render
/// through it.
pub fn link_error_message(e: LinkError) -> String {
  case e {
    UnresolvedExport(requested_module:, export_name:, imported_name: _) ->
      "The requested module '"
      <> requested_module
      <> "' does not provide an export named '"
      <> export_name
      <> "'"
    AmbiguousExport(requested_module:, export_name:) ->
      "The requested module '"
      <> requested_module
      <> "' provides an ambiguous export named '"
      <> export_name
      <> "'"
  }
}

/// Verify every import and indirect re-export in the graph resolves to a
/// unique binding. Returns the first failure as a `LinkError`.
pub fn validate(graph: LinkableGraph) -> Result(Nil, LinkError) {
  list.try_each(dict.to_list(graph), fn(entry) {
    let #(specifier, m) = entry
    use Nil <- result.try(check_imports(graph, m))
    check_indirect_exports(graph, specifier, m)
  })
}

fn check_imports(
  graph: LinkableGraph,
  m: LinkableModule,
) -> Result(Nil, LinkError) {
  list.try_each(m.import_bindings, fn(entry) {
    let #(raw_dep, bindings) = entry
    let dep = resolved_specifier(m, raw_dep)
    list.try_each(bindings, fn(binding) {
      case binding {
        // `import * as ns` always resolves (the namespace gathers names).
        esm.NamespaceImport(..) -> Ok(Nil)
        esm.NamedImport(imported:, ..) ->
          check_resolves(graph, dep, imported, raw_dep, imported)
        esm.DefaultImport(..) ->
          check_resolves(graph, dep, "default", raw_dep, "default")
      }
    })
  })
}

fn check_indirect_exports(
  graph: LinkableGraph,
  specifier: String,
  m: LinkableModule,
) -> Result(Nil, LinkError) {
  list.try_each(m.export_entries, fn(e) {
    case e {
      // `export { x } from 'mod'` — resolve THIS module's export name, which
      // recurses into the source (§16.2.1.6.4 step 1).
      esm.ReExport(export_name:, imported_name:, source_specifier:) ->
        check_resolves(
          graph,
          specifier,
          export_name,
          source_specifier,
          imported_name,
        )
      _ -> Ok(Nil)
    }
  })
}

fn check_resolves(
  graph: LinkableGraph,
  specifier: String,
  name: String,
  raw_dep: String,
  imported_name: String,
) -> Result(Nil, LinkError) {
  case resolve_export(graph, specifier, name) {
    ResolvedTo(..) | ResolvedNamespace(..) | ResolvedDeferredNamespace(..) ->
      Ok(Nil)
    Unresolvable ->
      Error(UnresolvedExport(
        requested_module: raw_dep,
        export_name: name,
        imported_name:,
      ))
    Ambiguous ->
      Error(AmbiguousExport(requested_module: raw_dep, export_name: name))
  }
}
