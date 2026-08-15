//// The §19.2 global function properties: eval / parseInt / parseFloat /
//// isNaN / isFinite / encodeURI / encodeURIComponent / decodeURI /
//// decodeURIComponent / escape / unescape.
////
//// Faithful port of arc/vm/builtins/number.gleam (parseInt/parseFloat/isNaN/
//// isFinite) + arc/vm/builtins/uri.gleam (URI codecs + escape/unescape) over
//// 2core's threaded Agent. Return-tuple order is `#(JsVal,
//// Agent)` (R1).
////
//// `init` returns the function handles that are ALSO installed as
//// `Number.parseInt`/`Number.parseFloat` (ES2024 §21.1.2.13/§21.1.2.12
//// require identity: `Number.parseInt === parseInt`).

import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type GlobalNative, type Handle, type JsNum, type JsVal,
  GlobalDecodeUri, GlobalDecodeUriComponent, GlobalEncodeUri,
  GlobalEncodeUriComponent, GlobalEscape, GlobalIsFinite, GlobalIsNaN, GlobalN,
  GlobalParseFloat, GlobalParseInt, GlobalUnescape, JFloat, JInt, JNan, JNegInf,
  JPosInf, mk_bool, mk_number, mk_object, mk_string,
} as rt_types
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// The global-function handles `init_realm` installs on the global object.
/// `parse_int`/`parse_float` are also installed on `Number` (identity).
pub type GlobalFns {
  GlobalFns(
    eval: Handle,
    parse_int: Handle,
    parse_float: Handle,
    is_nan: Handle,
    is_finite: Handle,
    decode_uri: Handle,
    encode_uri: Handle,
    decode_uri_component: Handle,
    encode_uri_component: Handle,
    escape: Handle,
    unescape: Handle,
  )
}

/// Allocate the global function objects. Rooted (they live for the realm's
/// lifetime); `init_realm` installs them as global-object properties.
/// `parse_int`/`parse_float`/`is_nan`/`is_finite` are passed in from
/// `b_number.init` — §21.1.2.12/.13 require `Number.parseInt === parseInt`,
/// so the SAME handles are installed on both the global object and Number.
pub fn init(
  st: Agent,
  function_proto: Handle,
  parse_int parse_int: Handle,
  parse_float parse_float: Handle,
  is_nan is_nan: Handle,
  is_finite is_finite: Handle,
) -> #(GlobalFns, Agent) {
  let alloc = fn(st, tag, name, len) {
    common.alloc_rooted_native_fn(st, function_proto, GlobalN(tag), name, len)
  }
  // `eval` is dispatched via JsOps.eval_hook (M19); the token here reuses
  // GlobalParseInt's slot ONLY to allocate a callable object — the top-level
  // dispatch table routes eval separately. A dedicated GlobalEval variant is
  // added when M19 lands.
  let #(eval, st) =
    common.alloc_rooted_native_fn(
      st,
      function_proto,
      rt_types.NativeUnseeded,
      "eval",
      1,
    )
  let #(encode_uri, st) = alloc(st, GlobalEncodeUri, "encodeURI", 1)
  let #(encode_uri_component, st) =
    alloc(st, GlobalEncodeUriComponent, "encodeURIComponent", 1)
  let #(decode_uri, st) = alloc(st, GlobalDecodeUri, "decodeURI", 1)
  let #(decode_uri_component, st) =
    alloc(st, GlobalDecodeUriComponent, "decodeURIComponent", 1)
  let #(escape, st) = alloc(st, GlobalEscape, "escape", 1)
  let #(unescape, st) = alloc(st, GlobalUnescape, "unescape", 1)
  #(
    GlobalFns(
      eval:,
      parse_int:,
      parse_float:,
      is_nan:,
      is_finite:,
      decode_uri:,
      encode_uri:,
      decode_uri_component:,
      encode_uri_component:,
      escape:,
      unescape:,
    ),
    st,
  )
}

/// Per-module dispatch for the §19.2 global functions.
pub fn dispatch(
  st: Agent,
  native: GlobalNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case native {
    GlobalParseInt -> {
      let #(val, radix) = helpers.two_args_or_undefined(args)
      let #(n, st) = parse_int_value(st, val, radix)
      #(mk_number(n), st)
    }
    GlobalParseFloat -> {
      let val = helpers.first_arg_or_undefined(args)
      let #(n, st) = parse_float_value(st, val)
      #(mk_number(n), st)
    }
    GlobalIsNaN -> global_is_nan(args, st)
    GlobalIsFinite -> global_is_finite(args, st)
    GlobalEncodeUri -> uri_encode_dispatch(args, st, True)
    GlobalEncodeUriComponent -> uri_encode_dispatch(args, st, False)
    GlobalDecodeUri -> uri_decode_dispatch(args, st, True)
    GlobalDecodeUriComponent -> uri_decode_dispatch(args, st, False)
    GlobalEscape -> {
      let #(s, st) =
        rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
      #(mk_string(js_escape(s)), st)
    }
    GlobalUnescape -> {
      let #(s, st) =
        rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
      #(mk_string(js_unescape(s)), st)
    }
  }
}

// ============================================================================
// parseInt / parseFloat
// ============================================================================

/// parseInt(string, radix) — ES2024 §19.2.5. Plain function on JsVals so
/// `%parseInt%` (`dispatch`) and Console's `%i` specifier share one impl.
pub fn parse_int_value(
  st: Agent,
  val: JsVal,
  radix_val: JsVal,
) -> #(JsNum, Agent) {
  // Step 1: Let inputString be ? ToString(string).
  let #(s, st) = rt_val.t_to_string(st, val)
  // Step 2: TrimString(inputString, START) — StrWhiteSpace, NOT Unicode WS.
  let str = trim_leading_js_whitespace(s)
  // Step 6: R = ToInt32(radix) — NaN/±∞ → 0, wraps modulo 2^32.
  let #(radix_int, st) = rt_val.t_to_int32(st, radix_val)
  // Steps 3-5: Strip the sign BEFORE the prefix check so "-0x10" reaches "0x".
  let #(str, negative) = case string.first(str) {
    Ok("-") -> #(string.drop_start(str, 1), True)
    Ok("+") -> #(string.drop_start(str, 1), False)
    _ -> #(str, False)
  }
  // Steps 7-9: R = 0 → default 10 with prefix detection; explicit 16 keeps
  // prefix detection; any other explicit radix leaves "0x" alone.
  let #(radix, strip_prefix) = case radix_int {
    0 -> #(10, True)
    16 -> #(16, True)
    n -> #(n, False)
  }
  // Step 10: A "0x"/"0X" prefix (only when stripPrefix) forces radix 16.
  let has_hex_prefix =
    string.starts_with(str, "0x") || string.starts_with(str, "0X")
  let #(str, radix) = case strip_prefix && has_hex_prefix {
    True -> #(string.drop_start(str, 2), 16)
    False -> #(str, radix)
  }
  // Step 8a: If R < 2 or R > 36, return NaN.
  case radix >= 2 && radix <= 36 {
    False -> #(JNan, st)
    // Steps 11-16: Parse the longest digit prefix and apply the sign.
    True -> #(parse_int_digits(str, radix, negative), st)
  }
}

/// parseFloat(string) — ES2024 §19.2.4. Plain function — see `parse_int_value`.
pub fn parse_float_value(st: Agent, val: JsVal) -> #(JsNum, Agent) {
  // Steps 1-2: ToString + TrimString(START).
  let #(s, st) = rt_val.t_to_string(st, val)
  let str = trim_leading_js_whitespace(s)
  // Steps 3-6: longest StrDecimalLiteral prefix → StringNumericValue.
  #(parse_decimal_string(str), st)
}

/// parseInt steps 11-16: parse the longest radix-R digit prefix. Step 15:
/// mathInt = 0 with a leading '-' → -0.
fn parse_int_digits(str: String, radix: Int, negative: Bool) -> JsNum {
  case scan_radix_digits(to_codepoint_chars(str), radix, 0, 0) {
    // Step 13: empty Z → NaN.
    #(0, _) -> JNan
    #(_, math_int) ->
      case negative, math_int {
        // Step 15: -0.
        True, 0 -> JFloat(-0.0)
        True, n -> rt_val.num_from_int(0 - n)
        False, n -> rt_val.num_from_int(n)
      }
  }
}

/// Consume a leading run of radix-R digit characters, returning
/// #(count, accumulated_value).
fn scan_radix_digits(
  chars: List(String),
  radix: Int,
  count: Int,
  acc: Int,
) -> #(Int, Int) {
  case chars {
    [ch, ..rest] ->
      case digit_value(ch, radix) {
        Some(d) -> scan_radix_digits(rest, radix, count + 1, acc * radix + d)
        None -> #(count, acc)
      }
    [] -> #(count, acc)
  }
}

/// Value of a single radix-R digit character (0-9, A-Z, a-z), or None.
fn digit_value(ch: String, radix: Int) -> Option(Int) {
  case string.to_utf_codepoints(ch) {
    [cp] -> {
      let c = string.utf_codepoint_to_int(cp)
      let v = case c {
        _ if c >= 48 && c <= 57 -> Some(c - 48)
        _ if c >= 65 && c <= 90 -> Some(c - 55)
        _ if c >= 97 && c <= 122 -> Some(c - 87)
        _ -> None
      }
      case v {
        Some(d) if d < radix -> Some(d)
        _ -> None
      }
    }
    _ -> None
  }
}

/// parseFloat steps 3-6: longest StrDecimalLiteral prefix; NaN when none.
fn parse_decimal_string(str: String) -> JsNum {
  let chars = to_codepoint_chars(str)
  case scan_decimal_literal(chars) {
    0 -> JNan
    len -> rt_val.string_to_number(string.concat(list.take(chars, len)))
  }
}

/// Split a string into single-code-point strings. NOT graphemes: a combining
/// mark must terminate the literal AFTER the digit ("1\u{0301}" parses as 1).
fn to_codepoint_chars(s: String) -> List(String) {
  use cp <- list.map(string.to_utf_codepoints(s))
  string.from_utf_codepoints([cp])
}

/// Longest-prefix StrDecimalLiteral scanner (§19.2.4 steps 3-4).
fn scan_decimal_literal(chars: List(String)) -> Int {
  let #(sign_len, rest) = case chars {
    ["+", ..r] | ["-", ..r] -> #(1, r)
    _ -> #(0, chars)
  }
  case rest {
    ["I", "n", "f", "i", "n", "i", "t", "y", ..] -> sign_len + 8
    _ ->
      case scan_unsigned_decimal(rest) {
        0 -> 0
        n -> sign_len + n
      }
  }
}

fn scan_unsigned_decimal(gs: List(String)) -> Int {
  let #(icount, after_int) = scan_digit_run(gs, 0)
  let #(mantissa_len, after_mantissa) = case after_int {
    [".", ..after_dot] -> {
      let #(fcount, after_frac) = scan_digit_run(after_dot, 0)
      case icount + fcount > 0 {
        True -> #(icount + 1 + fcount, after_frac)
        False -> #(0, after_frac)
      }
    }
    _ -> #(icount, after_int)
  }
  case mantissa_len {
    0 -> 0
    _ -> mantissa_len + scan_exponent_length(after_mantissa)
  }
}

fn scan_digit_run(gs: List(String), count: Int) -> #(Int, List(String)) {
  case gs {
    [ch, ..rest] ->
      case digit_value(ch, 10) {
        Some(_) -> scan_digit_run(rest, count + 1)
        None -> #(count, gs)
      }
    [] -> #(count, gs)
  }
}

fn scan_exponent_length(gs: List(String)) -> Int {
  case gs {
    ["e", ..rest] | ["E", ..rest] -> {
      let #(sign_len, digits) = case rest {
        ["+", ..r] | ["-", ..r] -> #(1, r)
        _ -> #(0, rest)
      }
      case scan_digit_run(digits, 0) {
        #(0, _) -> 0
        #(dcount, _) -> 1 + sign_len + dcount
      }
    }
    _ -> 0
  }
}

// ============================================================================
// isNaN / isFinite
// ============================================================================

/// isNaN(number) — ES2024 §19.2.3. Unlike Number.isNaN, coerces via ToNumber
/// first: isNaN("hello") is true (ToNumber("hello") = NaN).
fn global_is_nan(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(num, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  let result = case num {
    JNan -> True
    _ -> False
  }
  #(mk_bool(result), st)
}

/// isFinite(number) — ES2024 §19.2.2. Unlike Number.isFinite, coerces via
/// ToNumber first: isFinite("42") is true.
fn global_is_finite(args: List(JsVal), st: Agent) -> #(JsVal, Agent) {
  let #(num, st) = rt_val.t_to_number(st, helpers.first_arg_or_undefined(args))
  let result = case num {
    JInt(_) | JFloat(_) -> True
    JNan | JPosInf | JNegInf -> False
  }
  #(mk_bool(result), st)
}

// ============================================================================
// encodeURI / decodeURI / escape / unescape
// ============================================================================

fn uri_encode_dispatch(
  args: List(JsVal),
  st: Agent,
  preserve_uri_chars: Bool,
) -> #(JsVal, Agent) {
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  #(mk_string(uri_encode(s, preserve_uri_chars)), st)
}

fn uri_decode_dispatch(
  args: List(JsVal),
  st: Agent,
  preserve_reserved: Bool,
) -> #(JsVal, Agent) {
  let #(s, st) = rt_val.t_to_string(st, helpers.first_arg_or_undefined(args))
  case uri_decode(s, preserve_reserved) {
    Ok(decoded) -> #(mk_string(decoded), st)
    Error(offset) ->
      throw_uri_error(st, "URI malformed at position " <> int.to_string(offset))
  }
}

/// §19.2.6.2 Decode step 4.d.vii — arc `throw_uri_error`. Inline (not routed
/// through `ErrorKind`) so `realm_ops.error_kind_intrinsics` stays untouched.
fn throw_uri_error(st: Agent, msg: String) -> a {
  let proto = st.realm.uri_error.prototype
  let #(msg_prop, st) = common.builtin_property(st, mk_string(msg))
  let #(h, st) = common.alloc_error_slot(st, proto, [#("message", msg_prop)])
  rt_store.t_throw(st, mk_object(h))
}

/// §19.2.6.4/.5 Encode. `preserve_uri_chars` True → encodeURI (the
/// uriReserved-plus-'#' set `;/?:@&=+$,#` passes through); False →
/// encodeURIComponent.
pub fn uri_encode(str: String, preserve_uri_chars: Bool) -> String {
  string.to_utf_codepoints(str)
  |> list.map(fn(cp) {
    let c = string.utf_codepoint_to_int(cp)
    case is_uri_unescaped(c, preserve_uri_chars) {
      True -> string.from_utf_codepoints([cp])
      False -> percent_encode_utf8(c)
    }
  })
  |> string.concat
}

fn is_uri_unescaped(c: Int, preserve_uri_chars: Bool) -> Bool {
  { c >= 65 && c <= 90 }
  || { c >= 97 && c <= 122 }
  || { c >= 48 && c <= 57 }
  || c == 45
  || c == 95
  || c == 46
  || c == 33
  || c == 126
  || c == 42
  || c == 39
  || c == 40
  || c == 41
  || {
    preserve_uri_chars
    && {
      c == 59
      || c == 47
      || c == 63
      || c == 58
      || c == 64
      || c == 38
      || c == 61
      || c == 43
      || c == 36
      || c == 44
      || c == 35
    }
  }
}

fn percent_encode_utf8(cp: Int) -> String {
  let assert Ok(ucp) = string.utf_codepoint(cp)
  <<string.from_utf_codepoints([ucp]):utf8>>
  |> percent_encode_bytes("")
}

fn percent_encode_bytes(bytes: BitArray, acc: String) -> String {
  case bytes {
    <<>> -> acc
    <<b, rest:bytes>> ->
      percent_encode_bytes(rest, acc <> "%" <> to_hex_upper(b, 2))
    _ -> acc
  }
}

/// §19.2.6.2/.3 Decode. `preserve_reserved` True → decodeURI (escapes of the
/// reserved set are left literal); False → decodeURIComponent. Any malformed
/// escape → `Error(offset)` and the caller throws URIError.
pub fn uri_decode(str: String, preserve_reserved: Bool) -> Result(String, Int) {
  uri_decode_loop(<<str:utf8>>, preserve_reserved, 0, "")
}

fn uri_decode_loop(
  bytes: BitArray,
  preserve_reserved: Bool,
  offset: Int,
  acc: String,
) -> Result(String, Int) {
  case bytes {
    <<>> -> Ok(acc)
    <<0x25, _:bytes>> ->
      case decode_utf8_escape(bytes, offset) {
        Error(e) -> Error(e)
        Ok(#(cp, consumed, rest)) -> {
          let sub = case
            preserve_reserved && cp < 128 && is_uri_reserved_byte(cp)
          {
            True -> "%" <> to_hex_upper(cp, 2)
            False -> {
              let assert Ok(ucp) = string.utf_codepoint(cp)
              string.from_utf_codepoints([ucp])
            }
          }
          uri_decode_loop(
            rest,
            preserve_reserved,
            offset + consumed,
            acc <> sub,
          )
        }
      }
    <<cp:utf8_codepoint, rest:bytes>> -> {
      let ch = string.from_utf_codepoints([cp])
      uri_decode_loop(
        rest,
        preserve_reserved,
        offset + string.byte_size(ch),
        acc <> ch,
      )
    }
    _ -> Error(offset)
  }
}

fn decode_utf8_escape(
  bytes: BitArray,
  offset: Int,
) -> Result(#(Int, Int, BitArray), Int) {
  case take_percent_byte(bytes) {
    None -> Error(offset)
    Some(#(b0, rest)) ->
      case b0 {
        _ if b0 < 0x80 -> Ok(#(b0, 3, rest))
        _ if b0 >= 0xc2 && b0 <= 0xdf ->
          case take_percent_cont(rest) {
            Some(#(b1, rest)) ->
              Ok(#(
                int.bitwise_and(b0, 0x1f) * 64 + int.bitwise_and(b1, 0x3f),
                6,
                rest,
              ))
            None -> Error(offset)
          }
        _ if b0 >= 0xe0 && b0 <= 0xef ->
          case take_percent_cont(rest) {
            None -> Error(offset)
            Some(#(b1, rest)) ->
              case take_percent_cont(rest) {
                None -> Error(offset)
                Some(#(b2, rest)) -> {
                  let cp =
                    int.bitwise_and(b0, 0x0f)
                    * 4096
                    + int.bitwise_and(b1, 0x3f)
                    * 64
                    + int.bitwise_and(b2, 0x3f)
                  case cp >= 0x800 && { cp < 0xd800 || cp > 0xdfff } {
                    True -> Ok(#(cp, 9, rest))
                    False -> Error(offset)
                  }
                }
              }
          }
        _ if b0 >= 0xf0 && b0 <= 0xf4 ->
          case take_percent_cont(rest) {
            None -> Error(offset)
            Some(#(b1, rest)) ->
              case take_percent_cont(rest) {
                None -> Error(offset)
                Some(#(b2, rest)) ->
                  case take_percent_cont(rest) {
                    None -> Error(offset)
                    Some(#(b3, rest)) -> {
                      let cp =
                        int.bitwise_and(b0, 0x07)
                        * 262_144
                        + int.bitwise_and(b1, 0x3f)
                        * 4096
                        + int.bitwise_and(b2, 0x3f)
                        * 64
                        + int.bitwise_and(b3, 0x3f)
                      case cp >= 0x10000 && cp <= 0x10ffff {
                        True -> Ok(#(cp, 12, rest))
                        False -> Error(offset)
                      }
                    }
                  }
              }
          }
        _ -> Error(offset)
      }
  }
}

fn take_percent_byte(bytes: BitArray) -> Option(#(Int, BitArray)) {
  case bytes {
    <<0x25, a, b, rest:bytes>> ->
      case hex2(a, b) {
        Some(v) -> Some(#(v, rest))
        None -> None
      }
    _ -> None
  }
}

fn take_percent_cont(bytes: BitArray) -> Option(#(Int, BitArray)) {
  case take_percent_byte(bytes) {
    Some(#(b, rest)) if b >= 0x80 && b <= 0xbf -> Some(#(b, rest))
    _ -> None
  }
}

fn is_uri_reserved_byte(c: Int) -> Bool {
  c == 59
  || c == 47
  || c == 63
  || c == 58
  || c == 64
  || c == 38
  || c == 61
  || c == 43
  || c == 36
  || c == 44
  || c == 35
}

/// Annex B B.2.1.1 escape ( string ).
pub fn js_escape(input: String) -> String {
  string.to_utf_codepoints(input)
  |> list.map(fn(cp) {
    let code = string.utf_codepoint_to_int(cp)
    case is_escape_safe(code) {
      True -> string.from_utf_codepoints([cp])
      False -> escape_code_point(code)
    }
  })
  |> string.concat
}

fn is_escape_safe(cp: Int) -> Bool {
  { cp >= 65 && cp <= 90 }
  || { cp >= 97 && cp <= 122 }
  || { cp >= 48 && cp <= 57 }
  || cp == 64
  || cp == 42
  || cp == 95
  || cp == 43
  || cp == 45
  || cp == 46
  || cp == 47
}

fn escape_code_point(code: Int) -> String {
  case code {
    _ if code < 0x100 -> "%" <> to_hex_upper(code, 2)
    _ if code < 0x10000 -> "%u" <> to_hex_upper(code, 4)
    _ -> {
      let u = code - 0x10000
      let high = 0xd800 + int.bitwise_shift_right(u, 10)
      let low = 0xdc00 + int.bitwise_and(u, 0x3ff)
      "%u" <> to_hex_upper(high, 4) <> "%u" <> to_hex_upper(low, 4)
    }
  }
}

/// Annex B B.2.1.2 unescape ( string ).
pub fn js_unescape(input: String) -> String {
  string.to_utf_codepoints(input)
  |> list.map(string.utf_codepoint_to_int)
  |> js_unescape_loop([])
  |> string.from_utf_codepoints
}

fn js_unescape_loop(
  codes: List(Int),
  acc: List(UtfCodepoint),
) -> List(UtfCodepoint) {
  case codes {
    [] -> list.reverse(acc)
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

fn take_unicode_escape(after_percent: List(Int)) -> Option(#(Int, List(Int))) {
  case after_percent {
    [0x75, a, b, c, d, ..rest] -> {
      use unit <- option.map(hex4(a, b, c, d))
      case unit >= 0xd800 && unit <= 0xdbff {
        True ->
          case take_low_surrogate_escape(rest) {
            Some(#(low, after_pair)) -> #(
              0x10000 + { unit - 0xd800 } * 1024 + { low - 0xdc00 },
              after_pair,
            )
            None -> #(0xfffd, rest)
          }
        False ->
          case unit >= 0xdc00 && unit <= 0xdfff {
            True -> #(0xfffd, rest)
            False -> #(unit, rest)
          }
      }
    }
    _ -> None
  }
}

fn take_low_surrogate_escape(input: List(Int)) -> Option(#(Int, List(Int))) {
  case input {
    [0x25, 0x75, a, b, c, d, ..rest] -> {
      use unit <- option.then(hex4(a, b, c, d))
      case unit >= 0xdc00 && unit <= 0xdfff {
        True -> Some(#(unit, rest))
        False -> None
      }
    }
    _ -> None
  }
}

fn take_hex_escape(after_percent: List(Int)) -> Option(#(Int, List(Int))) {
  case after_percent {
    [a, b, ..rest] -> {
      use code <- option.map(hex2(a, b))
      #(code, rest)
    }
    _ -> None
  }
}

fn scalar_to_codepoint(code: Int) -> UtfCodepoint {
  let assert Ok(cp) = string.utf_codepoint(code)
  cp
}

// ── shared hex/whitespace helpers ────────────────────────────────────────────

fn to_hex_upper(n: Int, width: Int) -> String {
  let assert Ok(hex) = int.to_base_string(n, 16)
  hex |> string.uppercase |> string.pad_start(to: width, with: "0")
}

fn hex_byte_value(b: Int) -> Option(Int) {
  case b {
    _ if b >= 48 && b <= 57 -> Some(b - 48)
    _ if b >= 65 && b <= 70 -> Some(b - 55)
    _ if b >= 97 && b <= 102 -> Some(b - 87)
    _ -> None
  }
}

fn hex2(a: Int, b: Int) -> Option(Int) {
  use high <- option.then(hex_byte_value(a))
  use low <- option.map(hex_byte_value(b))
  high * 16 + low
}

fn hex4(a: Int, b: Int, c: Int, d: Int) -> Option(Int) {
  use high <- option.then(hex2(a, b))
  use low <- option.map(hex2(c, d))
  high * 256 + low
}

/// TrimString(START) with the ES StrWhiteSpace set — NOT Erlang's Unicode
/// White_Space set (which misses ZWNBSP).
fn trim_leading_js_whitespace(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(ch, rest)) ->
      case is_js_whitespace(ch) {
        True -> trim_leading_js_whitespace(rest)
        False -> s
      }
    Error(Nil) -> s
  }
}

fn is_js_whitespace(ch: String) -> Bool {
  case ch {
    " "
    | "\t"
    | "\n"
    | "\r"
    | "\u{000B}"
    | "\u{000C}"
    | "\u{00A0}"
    | "\u{FEFF}"
    | "\u{2028}"
    | "\u{2029}"
    | "\u{1680}"
    | "\u{2000}"
    | "\u{2001}"
    | "\u{2002}"
    | "\u{2003}"
    | "\u{2004}"
    | "\u{2005}"
    | "\u{2006}"
    | "\u{2007}"
    | "\u{2008}"
    | "\u{2009}"
    | "\u{200A}"
    | "\u{202F}"
    | "\u{205F}"
    | "\u{3000}" -> True
    _ -> False
  }
}
