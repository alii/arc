import arc/internal/digits
import arc/internal/utf16
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ============================================================================
// URI handling — the string transforms behind the global builtins
// encodeURI / encodeURIComponent (§19.2.6.4/.5), decodeURI /
// decodeURIComponent (§19.2.6.2/.3) and the AnnexB escape / unescape pair
// (B.2.1.1 / B.2.1.2). The VM-facing natives in `arc/vm/exec/call` are thin
// wrappers that coerce the argument to a string and, for the decoders, turn
// `Error(Malformed(..))` into a thrown URIError.
// ============================================================================

/// Why `uri_decode` rejected its input (§19.2.6.2 Decode → URIError).
pub type UriDecodeError {
  /// The escape sequence starting at byte `offset` of the input is not a
  /// valid encoding: a truncated `%`, non-hex digits, a missing or
  /// out-of-range continuation escape, or octets that are not the UTF-8
  /// encoding of a Unicode scalar value (overlong forms, surrogates,
  /// codepoints above U+10FFFF).
  Malformed(offset: Int)
}

/// §19.2.6.4/.5 Encode. When `preserve_uri_chars` is `True` this is
/// encodeURI (the uriReserved-plus-'#' set `;/?:@&=+$,#` passes through
/// unencoded); when `False` it is encodeURIComponent.
@external(erlang, "arc_uri_ffi", "encode")
pub fn uri_encode(str: String, preserve_uri_chars: Bool) -> String

/// §19.2.6.2/.3 Decode. When `preserve_reserved` is `True` this is decodeURI
/// (escapes of the reserved set `;/?:@&=+$,#` are left as their literal
/// `%XY` text); when `False` it is decodeURIComponent. Any malformed or
/// non-UTF-8 escape sequence is an `Error` — the returned `String` is always
/// well-formed UTF-8, and the caller must throw a URIError on `Error`.
@external(erlang, "arc_uri_ffi", "decode")
pub fn uri_decode(
  str: String,
  preserve_reserved: Bool,
) -> Result(String, UriDecodeError)

/// Characters that escape() preserves as-is (unreserved set).
/// Per B.2.1.1: A-Z, a-z, 0-9, @, *, _, +, -, ., /
fn is_escape_safe(cp: Int) -> Bool {
  // A-Z
  { cp >= 65 && cp <= 90 }
  // a-z
  || { cp >= 97 && cp <= 122 }
  // 0-9
  || { cp >= 48 && cp <= 57 }
  // @
  || cp == 64
  // *
  || cp == 42
  // _
  || cp == 95
  // +
  || cp == 43
  // -
  || cp == 45
  // .
  || cp == 46
  // /
  || cp == 47
}

/// Format an integer as uppercase hex with at least `width` digits.
fn to_hex_upper(n: Int, width: Int) -> String {
  // to_base_string only fails for base < 2 or > 36 — 16 is in range.
  let assert Ok(hex) = int.to_base_string(n, 16)
  hex |> string.uppercase |> string.pad_start(to: width, with: "0")
}

/// ES AnnexB B.2.1.1 escape ( string )
///
/// `escape` is specified over UTF-16 code units, so a supplementary-plane
/// code point is emitted as its two surrogate code units — `%uHHHH%uLLLL` —
/// never as a single five-digit `%uHHHHH` (which nothing, including our own
/// `unescape`, could parse back).
pub fn js_escape(input: String) -> String {
  string.to_utf_codepoints(input)
  |> list.map(fn(cp) {
    let code = string.utf_codepoint_to_int(cp)
    case is_escape_safe(code) {
      True -> string.from_utf_codepoints([cp])
      False -> escape_code_point(code)
    }
  })
  |> string.join("")
}

fn escape_code_point(code: Int) -> String {
  case code {
    _ if code < 0x100 -> "%" <> to_hex_upper(code, 2)
    _ if code < 0x10000 -> "%u" <> to_hex_upper(code, 4)
    _ -> {
      let #(high, low) = utf16.split(code)
      "%u" <> to_hex_upper(high, 4) <> "%u" <> to_hex_upper(low, 4)
    }
  }
}

/// ES AnnexB B.2.1.2 unescape ( string )
///
/// Walks the input as code points. At each `%` it first tries the
/// six-character `%uXXXX` form and then the three-character `%XX` form; an
/// escape is taken only when every one of its characters is a hex DIGIT.
/// Otherwise the `%` passes through literally and scanning resumes at the
/// next character.
pub fn js_unescape(input: String) -> String {
  string.to_utf_codepoints(input)
  |> list.map(string.utf_codepoint_to_int)
  |> js_unescape_loop([])
  |> string.from_utf_codepoints()
}

fn js_unescape_loop(
  codes: List(Int),
  acc: List(UtfCodepoint),
) -> List(UtfCodepoint) {
  case codes {
    [] -> list.reverse(acc)
    // '%'
    [0x25, ..after_percent] -> {
      let escape =
        option.lazy_or(take_unicode_escape(after_percent), fn() {
          take_hex_escape(after_percent)
        })
      case escape {
        Some(#(code, rest)) ->
          js_unescape_loop(rest, [scalar_to_codepoint(code), ..acc])
        None ->
          js_unescape_loop(after_percent, [scalar_to_codepoint(0x25), ..acc])
      }
    }
    [code, ..rest] -> js_unescape_loop(rest, [scalar_to_codepoint(code), ..acc])
  }
}

/// Try to take `uXXXX` (the input just after a `%`), yielding the decoded
/// code point and the unconsumed input. A high surrogate immediately
/// followed by a `%uXXXX` low surrogate consumes both escapes and yields
/// the combined supplementary-plane code point.
fn take_unicode_escape(after_percent: List(Int)) -> Option(#(Int, List(Int))) {
  case after_percent {
    // 'u'
    [0x75, a, b, c, d, ..rest] -> {
      use unit <- option.map(hex4(a, b, c, d))
      case utf16.is_high(unit) {
        True ->
          case take_low_surrogate_escape(rest) {
            Some(#(low, after_pair)) -> #(utf16.combine(unit, low), after_pair)
            None -> #(replacement_character, rest)
          }
        False ->
          case utf16.is_low(unit) {
            True -> #(replacement_character, rest)
            False -> #(unit, rest)
          }
      }
    }
    _ -> None
  }
}

/// Try to take a whole `%uXXXX` escape whose value is a low surrogate.
fn take_low_surrogate_escape(input: List(Int)) -> Option(#(Int, List(Int))) {
  case input {
    // "%u"
    [0x25, 0x75, a, b, c, d, ..rest] -> {
      use unit <- option.then(hex4(a, b, c, d))
      case utf16.is_low(unit) {
        True -> Some(#(unit, rest))
        False -> None
      }
    }
    _ -> None
  }
}

/// Try to take `XX` (the input just after a `%`).
fn take_hex_escape(after_percent: List(Int)) -> Option(#(Int, List(Int))) {
  case after_percent {
    [a, b, ..rest] -> {
      use code <- option.map(hex2(a, b))
      #(code, rest)
    }
    _ -> None
  }
}

/// U+FFFD REPLACEMENT CHARACTER. `unescape` substitutes it for a lone
/// UTF-16 surrogate: a JS string can hold one, but a Gleam `String` is
/// well-formed UTF-8 and cannot (a deliberate deviation from §B.2.1.2).
const replacement_character = 0xfffd

/// A decoded code point as a `UtfCodepoint`. Callers only pass Unicode
/// scalar values (lone surrogates were already replaced), so the assertion
/// cannot fail.
fn scalar_to_codepoint(code: Int) -> UtfCodepoint {
  let assert Ok(cp) = string.utf_codepoint(code)
  cp
}

/// Combine two hex digits; `None` if either is not a hex digit.
///
/// §B.2.1.2 admits only hex DIGITS inside an escape sequence, which is why
/// this goes through `digits.hex_value_code` rather than `int.base_parse`
/// (that one also accepts a leading sign, so `unescape("%+4")` would decode
/// to U+0004 instead of passing through unchanged).
fn hex2(a: Int, b: Int) -> Option(Int) {
  use high <- option.then(digits.hex_value_code(a))
  use low <- option.map(digits.hex_value_code(b))
  high * 16 + low
}

/// Combine four hex digits; `None` if any is not a hex digit.
fn hex4(a: Int, b: Int, c: Int, d: Int) -> Option(Int) {
  use high <- option.then(hex2(a, b))
  use low <- option.map(hex2(c, d))
  high * 256 + low
}
