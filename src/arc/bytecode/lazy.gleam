import arc/compiler/scope.{type CaptureSeed, type FnSource}
import gleam/option.{type Option}

/// what a function that has not been compiled yet needs to compile itself:
/// the unit's text, where it starts in it, and what the code around it hands it
pub type LazyFunction {
  LazyFunction(
    bytes: BitArray,
    is_module: Bool,
    source: FnSource,
    // the name the code around it gave it
    name: Option(String),
    // its captured names: each with the parent's slot and how it is held
    captures: List(#(String, Int, CaptureSeed)),
  )
}
