import arc/module/graph
import arc/module/linkable
import arc/module/loader
import arc/module/specifier
import arc/module/summary
import gleam/dict.{type Dict}
import gleam/list

fn key(text: String) -> specifier.Resolved {
  specifier.resolved(text)
}

fn graph_of(
  entry: String,
  files: List(#(String, String)),
) -> graph.SourceGraph {
  let sources: Dict(String, String) = dict.from_list(files)
  let assert Ok(entry_source) = dict.get(sources, entry)
  let resolve = fn(
    request: summary.ModuleRequest,
    _referrer: specifier.Resolved,
  ) {
    case specifier.raw_text(request.request.specifier) {
      "./" <> rest -> Ok(rest)
      other -> Ok(other)
    }
  }
  let load = fn(resolved: specifier.Resolved) {
    case dict.get(sources, specifier.resolved_path(resolved)) {
      Ok(src) -> Ok(loader.SourceText(src))
      Error(Nil) -> Error(loader.LoadNotFound)
    }
  }
  let assert Ok(g) =
    graph.load(
      key(entry),
      loader.SourceText(entry_source),
      resolve,
      load,
      fn(_) { False },
    )
  g
}

fn linkable_of(
  entry: String,
  files: List(#(String, String)),
) -> linkable.LinkableGraph {
  let g = graph_of(entry, files)
  dict.map_values(g.modules, fn(_specifier, loaded) {
    let assert graph.SourceTextModule(m) = loaded
    let assert Ok(linkable_module) =
      linkable.module_of(
        m.parsed.summary.imports,
        m.parsed.summary.exports,
        graph.specifier_map(m),
      )
    linkable_module
  })
}

pub fn resolve_export_through_chain_test() {
  let lg =
    linkable_of("a", [
      #("a", "export { x } from './b';"),
      #("b", "export const x = 1;"),
    ])

  assert linkable.resolve_export(lg, key("a"), "x")
    == linkable.ResolvedTo(module: key("b"), binding: "x")
}

pub fn resolve_export_renamed_chain_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { y } from './a2'; export { y };"),
      #("a2", "export { orig as y } from './b';"),
      #("b", "export const orig = 1;"),
    ])

  assert linkable.resolve_export(lg, key("a"), "y")
    == linkable.ResolvedTo(module: key("b"), binding: "orig")
}

pub fn resolve_export_through_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export const x = 1;"),
    ])

  assert linkable.resolve_export(lg, key("a"), "x")
    == linkable.ResolvedTo(module: key("b"), binding: "x")
  assert linkable.resolve_export(lg, key("a"), "own")
    == linkable.ResolvedTo(module: key("a"), binding: "own")
}

pub fn resolve_export_star_excludes_default_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export default 1; export const x = 1;"),
    ])

  assert linkable.resolve_export(lg, key("a"), "default")
    == linkable.Unresolvable
  assert linkable.resolve_export(lg, key("a"), "x")
    == linkable.ResolvedTo(module: key("b"), binding: "x")
}

pub fn resolve_export_direct_wins_over_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const x = 99;"),
      #("b", "export const x = 1;"),
    ])

  assert linkable.resolve_export(lg, key("a"), "x")
    == linkable.ResolvedTo(module: key("a"), binding: "x")
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

  assert linkable.resolve_export(lg, key("a"), "x") == linkable.Ambiguous
}

pub fn resolve_export_missing_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { z } from './m';"),
      #("m", "export const x = 1;"),
    ])

  assert linkable.resolve_export(lg, key("m"), "z") == linkable.Unresolvable
}

pub fn resolve_export_namespace_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * as ns from './b';"),
      #("b", "export const x = 1;"),
    ])

  assert linkable.resolve_export(lg, key("a"), "ns")
    == linkable.ResolvedNamespace(module: key("b"))
}

pub fn resolve_export_deferred_namespace_test() {
  let lg =
    linkable_of("a", [
      #("a", "import defer * as ns from './b'; export { ns };"),
      #("b", "export const x = 1;"),
    ])

  assert linkable.resolve_export(lg, key("a"), "ns")
    == linkable.ResolvedDeferredNamespace(module: key("b"))
}

pub fn resolve_export_circular_terminates_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export * from './a';"),
    ])

  assert linkable.resolve_export(lg, key("a"), "anything")
    == linkable.Unresolvable
}

pub fn exported_names_flattens_star_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export const x = 1; export const y = 2;"),
    ])

  let names = linkable.exported_names(lg, key("a"))
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

  assert linkable.exported_names(lg, key("a")) == ["a", "c", "r", "x", "y"]
  assert linkable.resolve_export(lg, key("a"), "b") == linkable.Unresolvable
  list.each(["a", "c", "r", "x", "y"], fn(name) {
    assert linkable.resolve_export(lg, key("a"), name)
      == linkable.ResolvedTo(module: key("a"), binding: name)
  })
}

pub fn exported_names_excludes_star_default_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b';"),
      #("b", "export default 1; export const x = 1;"),
    ])

  let names = linkable.exported_names(lg, key("a"))
  assert names == ["x"]
}

pub fn exported_names_circular_terminates_test() {
  let lg =
    linkable_of("a", [
      #("a", "export * from './b'; export const own = 0;"),
      #("b", "export * from './a';"),
    ])

  let names = linkable.exported_names(lg, key("a"))
  assert names == ["own"]
}

pub fn validate_missing_export_message_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { z } from './m';"),
      #("m", "export const x = 1;"),
    ])

  let expected =
    linkable.UnresolvedExport(
      requested_module: specifier.raw("./m"),
      export_name: "z",
    )
  assert linkable.validate(lg) == Error(expected)
  assert linkable.error_message(expected)
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
    linkable.AmbiguousExport(
      requested_module: specifier.raw("./a"),
      export_name: "x",
    )
  assert linkable.validate(lg) == Error(expected)
  assert linkable.error_message(expected)
    == "The requested module './a' provides an ambiguous export named 'x'"
}

pub fn validate_renaming_reexport_names_the_source_side_export_test() {
  let lg =
    linkable_of("a", [
      #("a", "export { orig as renamed } from './m';"),
      #("m", "export const x = 1;"),
    ])

  let expected =
    linkable.UnresolvedExport(
      requested_module: specifier.raw("./m"),
      export_name: "orig",
    )
  assert linkable.validate(lg) == Error(expected)
  assert linkable.error_message(expected)
    == "The requested module './m' does not provide an export named 'orig'"
}

pub fn validate_renaming_reexport_of_present_export_is_ok_test() {
  let lg =
    linkable_of("a", [
      #("a", "export { orig as renamed } from './m';"),
      #("m", "export const orig = 1;"),
    ])

  assert linkable.validate(lg) == Ok(Nil)
  assert linkable.resolve_export(lg, key("a"), "renamed")
    == linkable.ResolvedTo(module: key("m"), binding: "orig")
}

pub fn validate_ok_for_clean_graph_test() {
  let lg =
    linkable_of("a", [
      #("a", "import { x } from './b'; export { x };"),
      #("b", "export const x = 1;"),
    ])

  assert linkable.validate(lg) == Ok(Nil)
}
