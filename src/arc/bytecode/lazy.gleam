import arc/bytecode/lexical.{type CodeKind, type LexicalRef}
import arc/compiler/scope.{
  type BindingKind, type CaptureSeed, type FnSource, CaptureBinding, CaptureSeed,
  ConstBinding, FnNameBinding, LetBinding, ParamBinding, VarBinding,
}
import gleam/int
import gleam/list
import gleam/option.{type Option}

/// what a function that has not been compiled yet needs to compile itself:
/// the unit's text, where it starts in it, and what the code around it hands
/// it. Its template's env_descriptors say which parent slot each capture is in
pub type LazyFunction {
  LazyFunction(
    bytes: BitArray,
    is_module: Bool,
    source: FnSource,
    // the name the code around it gave it
    name: Option(String),
    // its captured names, in env order
    capture_names: List(String),
    // how the parent holds each of them, see pack_seeds
    capture_seeds: BitArray,
    // an arrow's this, new.target and the like, see pack_lexical
    lexical: Int,
    // an arrow is the kind of code it is written in
    code_kind: CodeKind,
  )
}

/// a byte for each seed: the declared kind, doubled, plus one when boxed
pub fn pack_seeds(seeds: List(CaptureSeed)) -> BitArray {
  use packed, CaptureSeed(boxed:, declared_kind:) <- list.fold(seeds, <<>>)
  let boxed = case boxed {
    True -> 1
    False -> 0
  }
  <<packed:bits, { kind_number(declared_kind) * 2 + boxed }>>
}

pub fn unpack_seeds(packed: BitArray) -> List(CaptureSeed) {
  case packed {
    <<byte, rest:bits>> -> [
      CaptureSeed(boxed: byte % 2 == 1, declared_kind: numbered_kind(byte / 2)),
      ..unpack_seeds(rest)
    ]
    _ -> []
  }
}

fn kind_number(kind: BindingKind) -> Int {
  case kind {
    VarBinding -> 0
    LetBinding -> 1
    ConstBinding -> 2
    ParamBinding -> 3
    CaptureBinding -> 4
    FnNameBinding -> 5
  }
}

fn numbered_kind(number: Int) -> BindingKind {
  case number {
    0 -> VarBinding
    1 -> LetBinding
    2 -> ConstBinding
    3 -> ParamBinding
    4 -> CaptureBinding
    _ -> FnNameBinding
  }
}

/// which lexical refs are captured, and which of those the parent holds in a
/// box: bit n for the nth of lexical.all_lexical_refs, bit n + 4 when boxed
pub fn pack_lexical(captured: fn(LexicalRef) -> Option(Bool)) -> Int {
  use packed, ref, n <- list.index_fold(lexical.all_lexical_refs, 0)
  case captured(ref) {
    option.None -> packed
    option.Some(False) -> int.bitwise_or(packed, int.bitwise_shift_left(1, n))
    option.Some(True) -> int.bitwise_or(packed, int.bitwise_shift_left(17, n))
  }
}

/// the captured refs in env order, each with whether it is boxed
pub fn unpack_lexical(packed: Int) -> List(#(LexicalRef, Bool)) {
  list.index_map(lexical.all_lexical_refs, fn(ref, n) {
    let bit = fn(at) {
      int.bitwise_and(int.bitwise_shift_right(packed, at), 1) == 1
    }
    #(ref, bit(n), bit(n + 4))
  })
  |> list.filter_map(fn(entry) {
    case entry.1 {
      True -> Ok(#(entry.0, entry.2))
      False -> Error(Nil)
    }
  })
}
