// opt-in resolve/load pair that reads modules from the filesystem

import arc/module/loader.{type LoadError, type ModuleSource, type ResolveError}
import arc/module/specifier
import arc/parser/ast.{type ImportAttribute}
import gleam/bit_array
import gleam/option.{None, Some}
import gleam/result
import simplifile

pub fn file_resolve(
  raw_specifier: String,
  parent_specifier: String,
) -> Result(String, ResolveError) {
  let raw = specifier.raw(raw_specifier)
  let parent = specifier.resolved(parent_specifier)
  case specifier.resolve_path(raw, parent) {
    specifier.PathSpecifier(resolved) -> Ok(specifier.resolved_path(resolved))
    specifier.BareSpecifier(_bare) -> Error(loader.UnsupportedBareSpecifier)
  }
}

// the type attribute alone picks the module kind, never the extension
pub fn file_load(
  resolved: String,
  attributes: List(ImportAttribute),
) -> Result(ModuleSource, LoadError) {
  use bytes <- result.try(case simplifile.read_bits(resolved) {
    Ok(bytes) -> Ok(bytes)
    Error(simplifile.Enoent) -> Error(loader.LoadNotFound)
    Error(err) -> Error(loader.ReadFailed(simplifile.describe_error(err)))
  })
  let text = fn(wrap: fn(String) -> ModuleSource) {
    bit_array.to_string(bytes)
    |> result.map(wrap)
    |> result.replace_error(
      loader.ReadFailed(simplifile.describe_error(simplifile.NotUtf8)),
    )
  }
  case loader.module_type(attributes) {
    None -> text(loader.SourceText)
    Some("json") -> text(loader.JsonSource)
    Some("text") -> text(loader.TextSource)
    Some("bytes") -> Ok(loader.BytesSource(bytes))
    Some(other) -> Error(loader.UnsupportedModuleType(other))
  }
}
