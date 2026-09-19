// opt-in resolve/load pair that reads modules from the filesystem

import arc/module/loader.{type LoadError, type ResolveError}
import arc/module/specifier
import simplifile

pub fn file_resolve(
  raw_specifier: String,
  parent_specifier: String,
) -> Result(String, ResolveError) {
  let raw = specifier.raw(raw_specifier)
  let parent = specifier.resolved(parent_specifier)
  case specifier.resolve_path(raw, parent) {
    specifier.PathSpecifier(resolved) -> Ok(specifier.resolved_text(resolved))
    specifier.BareSpecifier(_bare) -> Error(loader.UnsupportedBareSpecifier)
  }
}

pub fn file_load(resolved: String) -> Result(String, LoadError) {
  case simplifile.read(resolved) {
    Ok(source) -> Ok(source)
    Error(simplifile.Enoent) -> Error(loader.LoadNotFound)
    Error(err) -> Error(loader.ReadFailed(simplifile.describe_error(err)))
  }
}
