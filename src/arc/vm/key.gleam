//// Canonical JavaScript property keys.
////
//// THE single canonicalizer for string → PropertyKey, shared by the compiler
//// (resolve.gleam bakes canonical keys into GetField/PutField/... opcodes)
//// and the runtime (dynamic `obj[expr]` access, JSON, Object.keys, ...).
//// Having exactly one implementation is load-bearing: if compile-time and
//// runtime canonicalized the same string differently, the same property
//// would land in two different dict slots.
////
//// This module is a leaf (only gleam stdlib imports) so both `arc/vm/opcode`
//// and `arc/vm/value` can depend on it without a cycle.

import gleam/bit_array
import gleam/int
import gleam/string

/// Largest valid array index (§6.1.7): an array index is an integer in
/// [0, 2^32-1), i.e. at most 2^32 - 2. Anything larger is an ordinary
/// string-named property even when it looks numeric.
pub const max_array_index = 4_294_967_294

/// Canonical property key. Per spec, property keys are String | Symbol, but
/// we distinguish array-index strings (canonical numeric strings in [0, 2^32-1))
/// at the type level so `arr[5]` never round-trips through string conversion.
/// Symbols are stored separately in `symbol_properties` so they're not here.
pub type PropertyKey {
  /// Canonical array index — a non-negative integer whose ToString form equals
  /// the original key. `"5"` → `Index(5)`, but `"05"` stays `Named("05")`.
  Index(n: Int)
  /// Any other string key.
  Named(name: String)
}

/// Canonicalize a string key. Implements CanonicalNumericIndexString (§7.1.21)
/// combined with the array-index range check: if `s` parses to a non-negative
/// int and `int.to_string(n) == s`, it's `Index(n)`; otherwise `Named(s)`.
///
/// HOT-PATH NOTE: this does an int.parse + int.to_string + string-compare.
/// The interpreter must NOT call it for compile-time-constant property names —
/// resolve.gleam canonicalizes those once into the opcode's `PropertyKey`
/// payload, so the dispatch path is an O(1) tag match.
pub fn canonical_key(s: String) -> PropertyKey {
  // Cheap leading-byte guard: a canonical array index must start with a digit
  // ("0".."9" — sign-prefixed strings can never satisfy `n >= 0` round-trip).
  // gleam's int.parse is binary_to_integer wrapped in try/catch on BEAM, so
  // without this guard every non-numeric key (the common case) raises and
  // catches a badarg exception per access. bit_array.from_string is identity
  // on Erlang, so the guard is a single byte comparison.
  case bit_array.from_string(s) {
    <<c, _:bytes>> if c >= 48 && c <= 57 ->
      case int.parse(s) {
        // Array-index range check (§6.1.7). BEAM ints are arbitrary
        // precision, so without the cap "1000000000000000000000" would
        // round-trip and wrongly become an Index — but per spec it is a
        // plain string key (its ToNumber → ToString form is "1e+21", so it
        // isn't even a canonical numeric index string).
        Ok(n) if n >= 0 && n <= max_array_index ->
          case int.to_string(n) == s {
            True -> Index(n)
            False -> Named(s)
          }
        _ -> Named(s)
      }
    _ -> Named(s)
  }
}

/// Render a PropertyKey back to its spec string form (for error messages,
/// for-in enumeration, etc.).
pub fn key_to_string(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    // Private-element keys render as their source text ("#x"), without the
    // internal NUL marker or the per-evaluation uid suffix (see private_key
    // and value.mint_private_key).
    Named("\u{0}" <> s) ->
      case string.split_once(s, "\u{0}") {
        Ok(#(name, _uid)) -> name
        Error(Nil) -> s
      }
    Named(s) -> s
  }
}

/// Build the storage key for a class private element ("#x"). Arc stores
/// private fields/methods in the ordinary property table, but the spec keeps
/// private elements in a separate [[PrivateElements]] list invisible to ALL
/// ordinary property reflection (hasOwnProperty, ownKeys, for-in, `in`,
/// spread, JSON, freeze, ...) — AND a plain string property named "#x"
/// (created via o["#x"], computed keys, defineProperty, JSON.parse, ...) is a
/// perfectly ordinary, fully reflectable property. So privates are keyed with
/// a NUL-byte marker prefix ("\u{0}#x") that string-to-key conversion of
/// source-level identifiers/literals never produces, keeping the two
/// namespaces apart.
pub fn private_key(name: String) -> PropertyKey {
  Named("\u{0}" <> name)
}
