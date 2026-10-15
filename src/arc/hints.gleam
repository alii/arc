import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}

/// Which of one script's functions to compile when the script loads, learned
/// from an earlier run of the same source. Hints only move when a function is
/// compiled, never what it does, and hints for another source are ignored.
///
/// Get them with `engine.learned_hints` after a run, keep them with
/// `to_bit_array`, and hand them back to `engine.eval_script` next time.
pub opaque type Hints {
  /// nothing learned yet; fits any source
  NoHints
  /// where functions start in the source of this size and checksum
  Learned(size: Int, checksum: Int, starts: Set(Int))
}

/// every function waits for its first call
pub fn none() -> Hints {
  NoHints
}

/// the functions of both, when both are for the same source; hints for
/// another source add nothing to `a`
pub fn merge(a: Hints, b: Hints) -> Hints {
  case a, b {
    NoHints, other | other, NoHints -> other
    Learned(size:, checksum:, starts:),
      Learned(size: s, checksum: c, starts: more)
      if size == s && checksum == c
    -> Learned(size:, checksum:, starts: set.union(starts, more))
    Learned(..), Learned(..) -> a
  }
}

const format = <<"arc hints 1":utf8>>

/// bytes to keep between runs
pub fn to_bit_array(hints: Hints) -> BitArray {
  case hints {
    NoHints -> <<format:bits, 0>>
    Learned(size:, checksum:, starts:) -> {
      // sorted, each as its distance from the one before
      let #(packed, _last) = {
        use #(packed, last), start <- list.fold(
          list.sort(set.to_list(starts), int.compare),
          #(<<>>, 0),
        )
        #(put(packed, start - last), start)
      }
      <<format:bits, 1, size:64, checksum:32, packed:bits>>
    }
  }
}

/// Error when the bytes are not hints this version wrote
pub fn from_bit_array(bits: BitArray) -> Result(Hints, Nil) {
  case bits {
    <<"arc hints 1":utf8, 0>> -> Ok(NoHints)
    <<"arc hints 1":utf8, 1, size:64, checksum:32, packed:bits>> -> {
      use starts <- result.map(take_all(packed, 0, []))
      Learned(size:, checksum:, starts: set.from_list(starts))
    }
    _ -> Error(Nil)
  }
}

// a whole number, seven bits to a byte, low bits first, top bit set on all
// but the last byte
fn put(packed: BitArray, number: Int) -> BitArray {
  case number < 128 {
    True -> <<packed:bits, number>>
    False -> put(<<packed:bits, { number % 128 + 128 }>>, number / 128)
  }
}

fn take(packed: BitArray) -> Result(#(Int, BitArray), Nil) {
  case packed {
    <<byte, rest:bits>> if byte < 128 -> Ok(#(byte, rest))
    <<byte, rest:bits>> -> {
      use #(high, rest) <- result.map(take(rest))
      #(byte - 128 + high * 128, rest)
    }
    _ -> Error(Nil)
  }
}

fn take_all(
  packed: BitArray,
  last: Int,
  taken: List(Int),
) -> Result(List(Int), Nil) {
  case packed {
    <<>> -> Ok(taken)
    _ -> {
      use #(distance, rest) <- result.try(take(packed))
      take_all(rest, last + distance, [last + distance, ..taken])
    }
  }
}

@external(erlang, "erlang", "crc32")
fn crc32(source: String) -> Int

@external(erlang, "erlang", "byte_size")
fn byte_size(source: String) -> Int

/// what tells one source from another: its size and a checksum
@internal
pub fn fingerprint(source: String) -> #(Int, Int) {
  #(byte_size(source), crc32(source))
}

/// the hints that say these functions of the source with this fingerprint
/// are worth compiling at load
@internal
pub fn learned(fingerprint: #(Int, Int), starts: Set(Int)) -> Hints {
  Learned(size: fingerprint.0, checksum: fingerprint.1, starts:)
}

/// where the functions to compile at load start, for the source with this
/// fingerprint: none when the hints are for another source
@internal
pub fn starts_for(hints: Hints, fingerprint: #(Int, Int)) -> Set(Int) {
  case hints {
    Learned(size:, checksum:, starts:)
      if size == fingerprint.0 && checksum == fingerprint.1
    -> starts
    NoHints | Learned(..) -> set.new()
  }
}
