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
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

/// Largest valid array index (§6.1.7): an array index is an integer in
/// [0, 2^32-1), i.e. at most 2^32 - 2. Anything larger is an ordinary
/// string-named property even when it looks numeric.
///
/// Deliberately one less than `max_array_length`: an array of the maximum
/// length 2^32 - 1 has its last element at index 2^32 - 2.
pub const max_array_index = 4_294_967_294

/// Largest valid array length (§10.4.2.4 ArraySetLength): an Array's `length`
/// is a uint32, so it is at most 2^32 - 1 and a larger length throws
/// RangeError. Exactly one more than `max_array_index` — see above.
pub const max_array_length = 4_294_967_295

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
  /// A class private element ("#x"). Structurally distinct from `Named("#x")`,
  /// so a private element and an ordinary property of the same source text
  /// coexist in the same property dict without colliding, and no string a
  /// program can produce is ever mistaken for a private key. Only the
  /// `private_key*` constructors below can build one.
  Private(text: String)
}

/// Canonicalize a string key. Implements CanonicalNumericIndexString (§7.1.21)
/// combined with the array-index range check: if `s` parses to a non-negative
/// int and `int.to_string(n) == s`, it's `Index(n)`; otherwise `Named(s)`.
///
/// Returns only `Index | Named` — no user text can ever canonicalize into a
/// `Private` key, which is what keeps the private-element namespace unforgeable.
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

/// Canonical PropertyKey for an integer index — the Int-side sibling of
/// `canonical_key`, and the ONLY sanctioned way to build a key from an
/// integer index. Array indices (§6.1.7) are integers in [0, 2^32-2] and are
/// stored as `Index(n)`; anything outside that range is stored under its
/// ToString form as `Named`, exactly as `canonical_key` would classify it.
///
/// Array.prototype methods are generic over array-likes whose length can
/// reach 2^53-1, so per-element keys derived from such lengths MUST go
/// through this — a raw `Index(n)` for n > 2^32-2 can never match how the
/// property was actually stored.
pub fn index_key(n: Int) -> PropertyKey {
  case n >= 0 && n <= max_array_index {
    True -> Index(n)
    False -> Named(int.to_string(n))
  }
}

/// Canonical array index of a *number* key — the Float-side sibling of
/// `canonical_key`, and the ONLY sanctioned way to ask "does this JS number
/// name an array index?".
///
/// Returns `Some(i)` iff `f` is a canonical array index (§6.1.7): an integral
/// value in [0, 2^32-2] whose ToString form is `int.to_string(i)`. Everything
/// else — fractions, negatives (including -0.0's sibling -1.0), values past
/// the index cap, non-integral round-trip failures — is `None`, meaning the
/// key must be stringified (`Named(js_format_number(f))`) so that the numeric
/// and string forms of the same key land in the same dict slot.
///
/// -0.0 is normalized to +0.0 first (`0.0 == -0.0` in JS, but BEAM's `=:=`
/// distinguishes them), so `a[-0]` is `a[0]`.
///
/// HOT PATH: the interpreter's GetElem/PutElem fast paths call this per
/// element access. Keep it allocation-light and free of stringification.
pub fn array_index_of_float(f: Float) -> Option(Int) {
  let n = f +. 0.0
  let i = float.truncate(n)
  case int.to_float(i) == n && i >= 0 && i <= max_array_index {
    True -> Some(i)
    False -> None
  }
}

/// Render a PropertyKey the way a human should see it (error messages,
/// `inspect` output, function names). This is a *renderer*, not the inverse
/// of `canonical_key`: it deliberately mangles private-element keys down to
/// their source text ("#x"), dropping the per-evaluation uid. When you need
/// the exact property-name text — a proxy trap argument, `Object.keys`,
/// for-in — use `key_to_text`.
pub fn key_display_string(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(name) -> name
    Private(text) -> private_display_name(text)
  }
}

/// The exact property-name text of a PropertyKey — the true inverse of
/// `canonical_key` (`canonical_key(key_to_text(k)) == k` for every key that
/// `canonical_key` can produce). Use this wherever the string is *data*:
/// proxy trap arguments, `Object.keys` / [[OwnPropertyKeys]] results, for-in
/// bindings, module-namespace export lookups. For human-facing text use
/// `key_display_string`.
///
/// Private keys never reach ordinary reflection (callers filter them with
/// `is_private_key`), so their storage text is what comes back here.
pub fn key_to_text(key: PropertyKey) -> String {
  case key {
    Index(n) -> int.to_string(n)
    Named(s) -> s
    Private(text) -> text
  }
}

// --- The private-element namespace ------------------------------------------
//
// Arc stores private fields/methods in the ordinary property table, but the
// spec keeps private elements in a separate [[PrivateElements]] list invisible
// to ALL ordinary property reflection (hasOwnProperty, ownKeys, for-in, `in`,
// spread, JSON, freeze, ...) — AND a plain string property named "#x" (created
// via o["#x"], computed keys, defineProperty, JSON.parse, ...) is a perfectly
// ordinary, fully reflectable property. So privates get their own PropertyKey
// variant, `Private(_)`: dict keys are structural, so `Private("#x")` and
// `Named("#x")` are different slots, and — because `canonical_key` can only
// ever return `Index | Named` — no string a program can produce ever lands in
// the private namespace.
//
// The three pieces of that convention live here and nowhere else: the
// constructors (`private_key` / `private_key_from_text` / `private_key_text`),
// the predicate (`is_private_key`) and the renderer (`private_display_name`).

/// Separator between a minted PrivateName's source text and its uid inside the
/// storage text. Purely internal — nothing outside this module needs it.
const uid_separator = "\u{0}"

/// Build the storage key for a class private element ("#x").
pub fn private_key(name: String) -> PropertyKey {
  Private(name)
}

/// Wrap a minted PrivateName's storage text (see `private_key_text`) back into
/// a key — the only way a runtime-carried private-name string re-enters the
/// private namespace.
pub fn private_key_from_text(text: String) -> PropertyKey {
  Private(text)
}

/// Storage-key *text* for a freshly minted PrivateName (§15.7.14
/// ClassDefinitionEvaluation step 5/6): source text <> separator <> uid. The
/// uid makes each class evaluation's names distinct (spec PrivateName
/// identity); the key text is carried at runtime as a JsString and wrapped
/// with `private_key_from_text` at the access sites. See
/// `value.mint_private_key`.
pub fn private_key_text(name: String, uid: Int) -> String {
  name <> uid_separator <> int.to_string(uid)
}

/// Whether a PropertyKey lives in the private-element namespace. Reflection
/// sites call this to skip private keys.
pub fn is_private_key(key: PropertyKey) -> Bool {
  case key {
    Private(_) -> True
    Index(_) | Named(_) -> False
  }
}

/// Source-text name ("#x") of a private storage-key text — any uid suffix
/// stripped. A non-minted text (a bare "#x") is returned unchanged.
pub fn private_display_name(key_text: String) -> String {
  case string.split_once(key_text, uid_separator) {
    Ok(#(name, _uid)) -> name
    Error(Nil) -> key_text
  }
}
