import gleam/int
import gleam/list
import gleam/result
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
  let hex =
    int.to_base_string(n, 16) |> result.unwrap("0") |> string.uppercase()
  let pad = width - string.length(hex)
  case pad > 0 {
    True -> string.repeat("0", pad) <> hex
    False -> hex
  }
}

/// ES AnnexB B.2.1.1 escape ( string )
pub fn js_escape(input: String) -> String {
  string.to_utf_codepoints(input)
  |> list.map(fn(cp) {
    let code = string.utf_codepoint_to_int(cp)
    case is_escape_safe(code) {
      True -> string.from_utf_codepoints([cp])
      False ->
        case code < 256 {
          True -> "%" <> to_hex_upper(code, 2)
          False -> "%u" <> to_hex_upper(code, 4)
        }
    }
  })
  |> string.join("")
}

/// ES AnnexB B.2.1.2 unescape ( string )
pub fn js_unescape(input: String) -> String {
  js_unescape_loop(string.to_graphemes(input), "")
}

fn js_unescape_loop(chars: List(String), acc: String) -> String {
  case chars {
    [] -> acc
    ["%", "u", a, b, c, d, ..rest] -> {
      let hex = a <> b <> c <> d
      case int.base_parse(hex, 16) {
        Ok(code) ->
          case string.utf_codepoint(code) {
            Ok(cp) ->
              js_unescape_loop(rest, acc <> string.from_utf_codepoints([cp]))
            Error(Nil) -> js_unescape_loop(rest, acc <> "%u" <> hex)
          }
        Error(Nil) -> js_unescape_loop([a, b, c, d, ..rest], acc <> "%u")
      }
    }
    ["%", a, b, ..rest] -> {
      let hex = a <> b
      case int.base_parse(hex, 16) {
        Ok(code) ->
          case string.utf_codepoint(code) {
            Ok(cp) ->
              js_unescape_loop(rest, acc <> string.from_utf_codepoints([cp]))
            Error(Nil) -> js_unescape_loop(rest, acc <> "%" <> hex)
          }
        Error(Nil) -> js_unescape_loop([a, b, ..rest], acc <> "%")
      }
    }
    [c, ..rest] -> js_unescape_loop(rest, acc <> c)
  }
}
