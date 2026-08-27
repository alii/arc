import arc/esm
import arc/link
import arc/module/graph
import arc/module/load_error
import gleam/dict.{type Dict}
import gleam/list

fn key(specifier: String) -> esm.Resolved {
  esm.resolved_unchecked(specifier)
}

fn graph_of(
  entry: String,
  files: List(#(String, String)),
) -> graph.SourceGraph {
  let sources: Dict(String, String) = dict.from_list(files)
  let assert Ok(entry_source) = dict.get(sources, entry)
  let resolve = fn(request: esm.ModuleRequest, _referrer: esm.Resolved) {
    case esm.raw_text(request.specifier) {
      "./" <> rest -> Ok(key(rest))
      other -> Ok(key(other))
    }
  }
  let load = fn(specifier: esm.Resolved) {
    case dict.get(sources, esm.resolved_text(specifier)) {
      Ok(src) -> Ok(src)
      Error(Nil) -> Error(load_error.LoadNotFound)
    }
  }
  let assert Ok(g) =
    graph.load(key(entry), entry_source, resolve, load, fn(_) { False })
  g
}

fn linkable_of(
  entry: String,
  files: List(#(String, String)),
) -> link.LinkableGraph {
  let g = graph_of(entry, files)
  dict.map_values(g.modules, fn(_specifier, m) {
    let assert Ok(linkable) =
      link.project_module(
        m.parsed.summary.imports,
        m.parsed.summary.exports,
        graph.specifier_map(m),
      )
    linkable
  })
}

pub fn resolve_export_through_chain_test() {
  let lg =
    linkable_of("a", [
      #("a", "export { x } from './b';"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, key("a"), "x")
    == link.ResolvedTo(module: key("b"), binding: "x")
}

pub fn resolve_export_renamed_chain_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { y } from './a2'; export { y };"),
      #("a2", "export { orig as y } from './b';"),
      #("b", "export const orig = 1;"),
    ])

  assert link.resolve_export(lg, key("a"), "y")
    == link.ResolvedTo(module: key("b"), binding: "orig")
}

pub fn resolve_export_through_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, key("a"), "x")
    == link.ResolvedTo(module: key("b"), binding: "x")
  assert link.resolve_export(lg, key("a"), "own")
    == link.ResolvedTo(module: key("a"), binding: "own")
}

pub fn resolve_export_star_excludes_default_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export default 1; export const x = 1;"),
    ])

  assert link.resolve_export(lg, key("a"), "default") == link.Unresolvable
  assert link.resolve_export(lg, key("a"), "x")
    == link.ResolvedTo(module: key("b"), binding: "x")
}

pub fn resolve_export_direct_wins_over_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const x = 99;"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, key("a"), "x")
    == link.ResolvedTo(module: key("a"), binding: "x")
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

  assert link.resolve_export(lg, key("a"), "x") == link.Ambiguous
}

pub fn resolve_export_missing_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { z } from './m';"),
      #("m", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, key("m"), "z") == link.Unresolvable
}

pub fn resolve_export_namespace_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * as ns from './b';"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, key("a"), "ns")
    == link.ResolvedNamespace(module: key("b"))
}

pub fn resolve_export_deferred_namespace_test() {
  let lg =
    linkable_of("a", [
      #("a", "import defer * as ns from './b'; export { ns };"),
      #("b", "export const x = 1;"),
    ])

  assert link.resolve_export(lg, key("a"), "ns")
    == link.ResolvedDeferredNamespace(module: key("b"))
}

pub fn resolve_export_circular_terminates_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export * from './a';"),
    ])

  assert link.resolve_export(lg, key("a"), "anything") == link.Unresolvable
}

pub fn exported_names_flattens_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export const x = 1; export const y = 2;"),
    ])

  let names = link.exported_names(lg, key("a"))
  assert list.contains(names, "own")
  assert list.contains(names, "x")
  assert list.contains(names, "y")
  assert list.length(names) == 3
}

pub fn exported_names_include_destructured_declarations_test() {
  let lg =
    linkable_of("a", [
      #(
        "a",
        "const o = {}; const arr = [];
         export const { a, b: c, ...r } = o;
         export let [x, , y = 1] = arr;",
      ),
    ])

  assert link.exported_names(lg, key("a")) == ["a", "c", "r", "x", "y"]
  assert link.resolve_export(lg, key("a"), "b") == link.Unresolvable
  list.each(["a", "c", "r", "x", "y"], fn(name) {
    assert link.resolve_export(lg, key("a"), name)
      == link.ResolvedTo(module: key("a"), binding: name)
  })
}

pub fn exported_names_excludes_star_default_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export default 1; export const x = 1;"),
    ])

  let names = link.exported_names(lg, key("a"))
  assert names == ["x"]
}

pub fn exported_names_circular_terminates_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export * from './a';"),
    ])

  let names = link.exported_names(lg, key("a"))
  assert names == ["own"]
}

pub fn validate_missing_export_message_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { z } from './m';"),
      #("m", "export const x = 1;"),
    ])

  let expected =
    link.UnresolvedExport(requested_module: esm.raw("./m"), export_name: "z")
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

  let expected =
    link.AmbiguousExport(requested_module: esm.raw("./a"), export_name: "x")
  assert link.validate(lg) == Error(expected)
  assert link.link_error_message(expected)
    == "The requested module './a' provides an ambiguous export named 'x'"
}

pub fn validate_renaming_reexport_names_the_source_side_export_test() {
  let lg =
    linkable_of("a", [
      #("a", "export { orig as renamed } from './m';"),
      #("m", "export const x = 1;"),
    ])

  let expected =
    link.UnresolvedExport(requested_module: esm.raw("./m"), export_name: "orig")
  assert link.validate(lg) == Error(expected)
  assert link.link_error_message(expected)
    == "The requested module './m' does not provide an export named 'orig'"
}

pub fn validate_renaming_reexport_of_present_export_is_ok_test() {
  let lg =
    linkable_of("a", [
      #("a", "export { orig as renamed } from './m';"),
      #("m", "export const orig = 1;"),
    ])

  assert link.validate(lg) == Ok(Nil)
  assert link.resolve_export(lg, key("a"), "renamed")
    == link.ResolvedTo(module: key("m"), binding: "orig")
}

pub fn validate_ok_for_clean_graph_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { x } from './b'; export { x };"),
      #("b", "export const x = 1;"),
    ])

  assert link.validate(lg) == Ok(Nil)
}
