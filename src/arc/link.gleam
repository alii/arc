//// The shared ESM ResolveExport core (§16.2.1.6.3).
////
//// A `ResolveExport` operating on a minimal `LinkableModule` view — just the
//// fields the algorithm reads (`import_bindings`, `export_entries`,
//// `star_exports`); a module's identity is its `LinkableGraph` key, so the
//// record carries no specifier of its own. The runtime (`arc/module`) projects
//// each `CompiledModule` onto a `LinkableModule`, builds a `LinkableGraph`, and
//// calls `resolve_export`/`exported_names`/`validate` — so there is one source
//// of truth for export resolution and the runtime SyntaxError messages.
////
//// Every specifier here is either an `esm.Raw` (as written in the source that
//// mentions it) or an `esm.Resolved` (a graph key). They are distinct types, so
//// the two can never be swapped: the graph is only ever indexed by `Resolved`,
//// error messages only ever quote a `Raw`, and `esm.resolve` is the sole bridge.
//// That bridge is crossed exactly ONCE, in `project_module` — every dependency
//// a linkable module names arrives already resolved, so nothing downstream can
//// meet an unresolvable specifier and have to invent an answer for it.
////
//// This module is runtime-free: it consumes only the linkable projection, so
//// any caller that can build a `LinkableGraph` (the VM, a bundler, a linter)
//// reuses the same resolve body.

import arc/esm.{type Raw, type Resolved}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/result
import gleam/set.{type Set}

// =============================================================================
// The minimal linkable view
// =============================================================================

/// The projection of a module that ResolveExport needs: exactly the fields the
/// algorithm reads, with every dependency ALREADY resolved. The module's own
/// resolved specifier is the `LinkableGraph` key it is stored under, not a
/// field. The VM projects each `CompiledModule` onto this so the resolve body is
/// decoupled from bytecode/heap state.
///
/// Each dependency appears as a `#(Raw, Resolved)` pair: the `Resolved` is what
/// every lookup uses, the `Raw` is retained only so an error message can quote
/// the specifier the source actually wrote. Build one with `project_module`.
pub type LinkableModule {
  LinkableModule(
    /// One entry per import declaration: (as written, resolved, its bindings).
    import_bindings: List(#(Raw, Resolved, List(esm.ImportBinding))),
    /// Every export entry that names an export: local, `export {x} from`,
    /// `export * as ns from`. `export *` lives in `star_exports` — it names no
    /// export, so it cannot appear where a name is looked up.
    export_entries: List(LinkableExport),
    /// `export * from 'm'` sources (§16.2.1.6.3 step 7).
    star_exports: List(Resolved),
  )
}

/// `esm.ExportEntry` with the source specifier resolved. `esm.ReExportAll` has
/// no counterpart: it becomes a `star_exports` entry, so the "does this export
/// entry provide a name?" question no longer has an arm that answers "no".
pub type LinkableExport {
  /// `export let x` / `export { x }` — may still be an import binding, see
  /// `resolve_local_export`.
  LocalExport(export_name: String, local_name: String)
  /// `export { imported_name as export_name } from source` (resolving to `dep`).
  ReExport(
    export_name: String,
    imported_name: String,
    source: Raw,
    dep: Resolved,
  )
  /// `export * as export_name from` the module `dep`.
  ReExportNamespace(export_name: String, dep: Resolved)
}

/// A module graph reduced to the linkable view, keyed by resolved specifier.
pub type LinkableGraph =
  Dict(Resolved, LinkableModule)

/// Project one module's raw import/export entries onto the linkable view,
/// resolving every specifier it mentions through its own (TOTAL) `SpecifierMap`.
/// This is the ONLY `esm.resolve` on the linking path.
///
/// `Error(raw)` means that map does not cover `raw` — never a guest program's
/// fault, always a broken builder (a module whose specifier map disagrees with
/// its own import/export entries). Reporting it here rather than downstream is
/// what lets every consumer of a `LinkableModule` take resolution for granted.
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
  // One pass over the export entries: each becomes either an export the
  // resolver can look a name up in, or an `export *` source. Lists build
  // reversed.
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

// =============================================================================
// ResolveExport (§16.2.1.6.3) — the shared string-level core
// =============================================================================

/// Result of resolving an export name through a linkable graph. The VM consumes
/// these variant names directly (`ResolvedTo`/`ResolvedNamespace`/
/// `ResolvedDeferredNamespace`/`Unresolvable`/`Ambiguous`), so they are public.
pub type ExportResolution {
  /// Resolves to a concrete binding `binding` owned by module `module`.
  ResolvedTo(module: Resolved, binding: String)
  /// Resolves to a module namespace object (`export * as ns from`, or
  /// `export { ns }` of an `import * as ns` binding).
  ResolvedNamespace(module: Resolved)
  /// Resolves to a DEFERRED module namespace (`export { ns }` of an
  /// `import defer * as ns` binding) — importers receive the deferred proxy.
  ResolvedDeferredNamespace(module: Resolved)
  /// No export of this name exists (or only via a circular path).
  Unresolvable
  /// Two distinct `export *` sources provide the name — ambiguous.
  Ambiguous
}

/// §16.2.1.6.3 ResolveExport. Resolve `name` exported from `specifier`.
pub fn resolve_export(
  graph: LinkableGraph,
  specifier: Resolved,
  name: String,
) -> ExportResolution {
  resolve_export_set(graph, specifier, name, set.new())
}

/// §16.2.1.6.3 ResolveExport. `resolve_set` guards circular re-exports.
fn resolve_export_set(
  graph: LinkableGraph,
  specifier: Resolved,
  name: String,
  resolve_set: Set(#(Resolved, String)),
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
  specifier: Resolved,
  name: String,
  resolve_set: Set(#(Resolved, String)),
) -> ExportResolution {
  // Direct local export, then named/namespace re-export, take priority over
  // `export *` (§16.2.1.6.3 steps 4–6 before step 7).
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
          esm.DefaultImport(local:) if local == local_name ->
            Ok(#(dep, binding))
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

/// §16.2.1.6.3 step 7: gather across `export *` sources, flagging ambiguity.
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

// =============================================================================
// GetExportedNames (§16.2.1.6.2) — shared
// =============================================================================

/// §16.2.1.6.2 GetExportedNames — local + indirect names, plus `export *` names
/// (excluding `default`), de-duplicated. Used by the VM's export-cell builder.
pub fn exported_names(
  graph: LinkableGraph,
  specifier: Resolved,
) -> List(String) {
  exported_names_with(graph, specifier, set.new())
}

/// `star_set` guards `export *` cycles.
fn exported_names_with(
  graph: LinkableGraph,
  spec: Resolved,
  star_set: Set(Resolved),
) -> List(String) {
  case set.contains(star_set, spec), dict.get(graph, spec) {
    True, _ | _, Error(Nil) -> []
    False, Ok(m) -> {
      let star_set = set.insert(star_set, spec)
      // Every `LinkableExport` names an export (`export *` is not one of them),
      // so this is a total map — no entry can be silently skipped.
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

// =============================================================================
// Validation (§16.2.1.6.4) — the VM's link path; exact SyntaxError strings
// =============================================================================

/// A link-time validation failure (§16.2.1.6.4). The two guest-visible
/// variants surface to JS as a SyntaxError whose message is
/// `link_error_message`; callers that need to distinguish the cases match the
/// variant instead of the prose. `UnresolvedDependency` is NOT guest-visible —
/// it reports a broken caller invariant, and `arc/module` panics on it.
pub type LinkError {
  /// An import or indirect re-export names an export the requested module does
  /// not provide. `requested_module` is the specifier as WRITTEN in the failing
  /// source line and `export_name` the name that module failed to provide —
  /// both appear verbatim in the SyntaxError message. For a renaming re-export
  /// (`export { orig as renamed } from './m'`) that name is `orig`, the one
  /// `./m` was asked for, not this module's alias.
  UnresolvedExport(requested_module: Raw, export_name: String)
  /// Two distinct `export *` sources provide `export_name` (§16.2.1.6.3
  /// step 7) — the binding is ambiguous.
  AmbiguousExport(requested_module: Raw, export_name: String)
}

/// The exact JS-visible SyntaxError message for a link failure. This is the
/// only place the prose lives — the runtime, tests, and any tooling all render
/// through it. The specifier is quoted exactly as the source wrote it (`./m`,
/// never the resolved `m`), which the `Raw` type guarantees.
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

/// Verify every import and indirect re-export in the graph resolves to a
/// unique binding. Returns the first failure as a `LinkError`.
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
        // `import * as ns` requests no particular export name — the namespace
        // gathers whatever the source provides — so nothing can fail.
        esm.NamespaceImport(..) -> Ok(Nil)
        esm.NamedImport(imported:, ..) ->
          check_dep(graph, dep, raw_dep, imported)
        esm.DefaultImport(..) -> check_dep(graph, dep, raw_dep, "default")
      }
    })
  })
}

/// §16.2.1.6.4 step 1 for `export { orig as renamed } from './m'`: resolve
/// `orig` in `./m`. Resolving THIS module's `renamed` would terminate at the
/// same place (it is a one-hop re-export), but would report `renamed` — a name
/// `./m` was never asked for. So look the source-side name up in the source
/// module, exactly as `check_imports` does; the reported name is then the one
/// that actually failed.
///
/// `export *` and `export * as ns` name no export, so there is nothing to
/// check. A `LocalExport` names no dependency at all — and if its local name
/// is an import binding, `check_imports` already covered that dependency.
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

/// Check that the dependency `dep` provides `imported_name`.
///
/// `raw_dep` and `dep` are the same dependency in its two flavours: the text
/// the source wrote (for the message) and the graph key (for the lookup). They
/// are different types, so they cannot be transposed.
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
