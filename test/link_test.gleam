//// Tests for `arc/link` — the shared ResolveExport core
//// (`resolve_export`/`exported_names`/`validate`) over a `LinkableGraph`.

import arc/esm
import arc/link
import arc/module/graph
import gleam/dict.{type Dict}
import gleam/list

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Build a graph from an in-memory map of specifier → source. Specifiers are
/// the dict keys, so resolution is the identity (the raw specifier IS identity).
fn graph_of(
  entry: String,
  files: List(#(String, String)),
) -> graph.SourceGraph {
  let sources: Dict(String, String) = dict.from_list(files)
  let assert Ok(entry_source) = dict.get(sources, entry)
  // Canonicalize `./b` → `b` so the dict keys stay simple. The resolved
  // specifier IS module identity, so a self-import `./a` and the entry `a`
  // collapse to the same module.
  let resolve = fn(request: esm.ModuleRequest, _referrer: String) {
    case request.specifier {
      "./" <> rest -> Ok(rest)
      other -> Ok(other)
    }
  }
  let load = fn(specifier: String) {
    case dict.get(sources, specifier) {
      Ok(src) -> Ok(src)
      Error(Nil) -> Error("no such module: " <> specifier)
    }
  }
  let assert Ok(g) =
    graph.load(entry, entry_source, resolve, load, fn(_) { False })
  g
}

/// Project parsed sources straight onto the `LinkableGraph` view that the
/// runtime resolve core consumes — the same projection `arc/module` does for
/// `CompiledModule`s, done here from `graph.SourceModule`s for test brevity.
fn linkable_of(
  entry: String,
  files: List(#(String, String)),
) -> link.LinkableGraph {
  let g = graph_of(entry, files)
  dict.map_values(g.modules, fn(_specifier, m) {
    link.LinkableModule(
      import_bindings: m.summary.imports,
      export_entries: m.summary.exports,
      specifier_map: m.specifier_map,
    )
  })
}

// ---------------------------------------------------------------------------
// resolve_export — re-export chains
// ---------------------------------------------------------------------------

pub fn resolve_export_through_chain_test() {
  let lg =
    linkable_of("a", [
      #("a", "export { x } from './b';"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, "a", "x")
    == link.ResolvedTo(module: "b", binding: "x")
}

pub fn resolve_export_renamed_chain_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { y } from './a2'; export { y };"),
      #("a2", "export { orig as y } from './b';"),
      #("b", "export const orig = 1;"),
    ])

  // A local export of an imported binding resolves through the dep.
  assert link.resolve_export(lg, "a", "y")
    == link.ResolvedTo(module: "b", binding: "orig")
}

// ---------------------------------------------------------------------------
// resolve_export — `export *`
// ---------------------------------------------------------------------------

pub fn resolve_export_through_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, "a", "x")
    == link.ResolvedTo(module: "b", binding: "x")
  assert link.resolve_export(lg, "a", "own")
    == link.ResolvedTo(module: "a", binding: "own")
}

pub fn resolve_export_star_excludes_default_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export default 1; export const x = 1;"),
    ])

  // `default` is never provided by `export *`.
  assert link.resolve_export(lg, "a", "default") == link.Unresolvable
  assert link.resolve_export(lg, "a", "x")
    == link.ResolvedTo(module: "b", binding: "x")
}

pub fn resolve_export_direct_wins_over_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const x = 99;"),
      #("b", "export const x = 1;"),
    ])

  // The module's own `x` wins over b's `x` — no ambiguity.
  assert link.resolve_export(lg, "a", "x")
    == link.ResolvedTo(module: "a", binding: "x")
}

pub fn resolve_export_ambiguous_test() {
  let lg =
    linkable_of("a", [
      #(
        "a",
        "import { x } from './a'; export * from './b'; export * from './c';",
      ),
      #("b", "export const x = 1;"),
      #("c", "export const x = 2;"),
    ])

  assert link.resolve_export(lg, "a", "x") == link.Ambiguous
}

pub fn resolve_export_missing_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { z } from './m';"),
      #("m", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, "m", "z") == link.Unresolvable
}

pub fn resolve_export_namespace_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * as ns from './b';"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, "a", "ns")
    == link.ResolvedNamespace(module: "b")
}

pub fn resolve_export_deferred_namespace_test() {
  let lg =
    linkable_of("a", [
      #("a", "import defer * as ns from './b'; export { ns };"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, "a", "ns")
    == link.ResolvedDeferredNamespace(module: "b")
}

pub fn resolve_export_circular_terminates_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export * from './a';"),
    ])

  // A circular `export *` chain must terminate (not loop) and yield no export.
  assert link.resolve_export(lg, "a", "anything") == link.Unresolvable
}

// ---------------------------------------------------------------------------
// exported_names — §16.2.1.6.2
// ---------------------------------------------------------------------------

pub fn exported_names_flattens_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export const x = 1; export const y = 2;"),
    ])

  let names = link.exported_names(lg, "a")
  assert list.contains(names, "own")
  assert list.contains(names, "x")
  assert list.contains(names, "y")
  assert list.length(names) == 3
}

pub fn exported_names_excludes_star_default_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export default 1; export const x = 1;"),
    ])

  let names = link.exported_names(lg, "a")
  assert names == ["x"]
}

pub fn exported_names_circular_terminates_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export * from './a';"),
    ])

  // The `export *` cycle must terminate; only the genuine local names survive.
  let names = link.exported_names(lg, "a")
  assert names == ["own"]
}

// ---------------------------------------------------------------------------
// validate — typed LinkErrors + exact SyntaxError strings (parity with the
// runtime link path)
// ---------------------------------------------------------------------------
//
// These assert byte-for-byte equality with the messages the VM surfaces from
// `link_error_message`. The message embeds the RAW (unresolved) specifier as
// written in source — `./m`, NOT the resolved `m` — so each graph imports via
// a `./`-prefixed specifier to prove the raw-vs-resolved choice.

pub fn validate_missing_export_message_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { z } from './m';"),
      #("m", "export const x = 1;"),
    ])

  let expected =
    link.UnresolvedExport(
      requested_module: "./m",
      export_name: "z",
      imported_name: "z",
    )
  assert link.validate(lg) == Error(expected)
  assert link.link_error_message(expected)
    == "The requested module './m' does not provide an export named 'z'"
}

pub fn validate_ambiguous_export_message_test() {
  let lg =
    linkable_of("a", [
      #(
        "a",
        "import { x } from './a'; export * from './b'; export * from './c';",
      ),
      #("b", "export const x = 1;"),
      #("c", "export const x = 2;"),
    ])

  // The self-import `import { x } from './a'` forces resolution of a's own `x`,
  // which is ambiguous across the two `export *` sources. The raw specifier in
  // the message is `./a` (as written), not the resolved `a`.
  let expected = link.AmbiguousExport(requested_module: "./a", export_name: "x")
  assert link.validate(lg) == Error(expected)
  assert link.link_error_message(expected)
    == "The requested module './a' provides an ambiguous export named 'x'"
}

pub fn validate_renaming_reexport_carries_imported_name_test() {
  // `export { orig as renamed } from './m'` where `./m` has no `orig`:
  // the SyntaxError names THIS module's export (`renamed`), and the typed
  // error additionally carries the source-side name (`orig`).
  let lg =
    linkable_of("a", [
      #("a", "export { orig as renamed } from './m';"),
      #("m", "export const x = 1;"),
    ])

  let expected =
    link.UnresolvedExport(
      requested_module: "./m",
      export_name: "renamed",
      imported_name: "orig",
    )
  assert link.validate(lg) == Error(expected)
  assert link.link_error_message(expected)
    == "The requested module './m' does not provide an export named 'renamed'"
}

pub fn validate_ok_for_clean_graph_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { x } from './b'; export { x };"),
      #("b", "export const x = 1;"),
    ])

  assert link.validate(lg) == Ok(Nil)
}
