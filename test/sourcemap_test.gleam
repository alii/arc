//// Unit tests for the source-map foundations: the base64 VLQ codec
//// (known int->string pairs cross-checked against @jridgewell/sourcemap-codec),
//// AST span slice round-trips, and a small magic-string edit whose generated
//// map is decoded back to the original mapping.

import arc/parser
import arc/parser/ast
import arc/sourcemap/codec
import arc/sourcemap/magic_string
import gleam/bit_array
import gleam/list
import gleam/option.{Some}
import gleam/string

/// Validate that a string is well-formed JSON using OTP's `json` module.
@external(erlang, "sourcemap_test_ffi", "is_valid_json")
fn is_valid_json(json: String) -> Bool

// --- VLQ known-pairs --------------------------------------------------------

/// Each pair is (signed integer, expected base64 VLQ string). Values were
/// cross-checked against @jridgewell/sourcemap-codec's reference encoder.
const vlq_known_pairs = [
  #(0, "A"),
  #(1, "C"),
  #(-1, "D"),
  #(2, "E"),
  #(-2, "F"),
  #(15, "e"),
  #(-15, "f"),
  #(16, "gB"),
  #(-16, "hB"),
  #(17, "iB"),
  #(32, "gC"),
  #(-32, "hC"),
  #(123, "2H"),
  #(-123, "3H"),
  #(511, "+f"),
  #(-511, "/f"),
  #(512, "ggB"),
  #(1000, "w+B"),
  #(-1000, "x+B"),
  #(123_456_789, "qxmvrH"),
  #(2_147_483_647, "+/////D"),
]

pub fn vlq_known_pairs_test() {
  list.each(vlq_known_pairs, fn(pair) {
    let #(value, expected) = pair
    assert codec.encode_vlq(value) == expected
  })

  // Round-trip every known value through a local decoder so the encoder and a
  // spec-faithful decoder agree.
  list.each(vlq_known_pairs, fn(pair) {
    let #(value, expected) = pair
    assert decode_vlq(expected) == [value]
  })
}

// --- AST span slice round-trip ----------------------------------------------

pub fn span_slice_roundtrip_test() {
  // Export nodes carry byte spans; slicing the source by a node's span must
  // reproduce its exact original text.
  let source = "export default 42;\nexport * from \"mod\";\n"
  let assert Ok(ast.Module(items)) = parser.parse(source, parser.Module)

  let slices =
    list.filter_map(items, fn(item) {
      case item {
        ast.ExportDefaultDeclaration(_, span) -> Ok(slice_span(source, span))
        ast.ExportAllDeclaration(_, _, span) -> Ok(slice_span(source, span))
        _ -> Error(Nil)
      }
    })

  assert slices == ["export default 42;", "export * from \"mod\""]
}

fn slice_span(source: String, span: ast.Span) -> String {
  let bytes = bit_array.from_string(source)
  let assert Ok(slice) =
    bit_array.slice(bytes, span.start, span.end - span.start)
  let assert Ok(text) = bit_array.to_string(slice)
  text
}

// --- magic-string edit + map decode -----------------------------------------

pub fn magic_string_map_decode_test() {
  // Overwrite `const` with `var`: a 5-byte original slice replaced by a 3-byte
  // insert. The surviving tail (" x = 1;") starts at original byte 5.
  let source = "const x = 1;"
  let ms = magic_string.overwrite(magic_string.new(source), 0, 5, "var")

  // The edit log materializes to the expected output text.
  assert magic_string.to_string(ms) == "var x = 1;"

  let map =
    magic_string.generate_map(
      ms,
      "input.js",
      magic_string.default_map_options(),
    )

  // The map names the source, embeds its content, and uses version 3.
  assert map.version == 3
  assert map.sources == ["input.js"]
  assert map.sources_content == [Some(source)]

  // to_json must serialize to syntactically valid JSON.
  let json = codec.to_json(map)
  assert is_valid_json(json)

  // The single output line has one segment for the surviving tail. Decoding it
  // recovers [gen_col=3, source_idx=0, orig_line=0, orig_col=5]: the tail sits
  // at generated column 3 (after "var") and originally began at column 5
  // (after "const").
  let decoded = decode_mappings(map.mappings)
  assert decoded == [[[3, 0, 0, 5]]]
}

// --- VLQ / mappings decoder (test-only, spec-faithful) -----------------------

const base64_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

/// Decode a base64 VLQ string into the list of signed integers it encodes.
fn decode_vlq(encoded: String) -> List(Int) {
  decode_vlq_loop(string.to_graphemes(encoded), 0, 0, [])
}

fn decode_vlq_loop(
  chars: List(String),
  acc: Int,
  shift: Int,
  out: List(Int),
) -> List(Int) {
  case chars {
    [] -> list.reverse(out)
    [ch, ..rest] -> {
      let digit = base64_value(ch)
      let continued = int_band(digit, 32) != 0
      let value = acc + int_shl(int_band(digit, 31), shift)
      case continued {
        True -> decode_vlq_loop(rest, value, shift + 5, out)
        False -> decode_vlq_loop(rest, 0, 0, [unsign_vlq(value), ..out])
      }
    }
  }
}

/// Undo the sign-in-low-bit folding: low bit set => negative magnitude.
fn unsign_vlq(value: Int) -> Int {
  let magnitude = int_shr(value, 1)
  case int_band(value, 1) {
    0 -> magnitude
    _ -> -magnitude
  }
}

fn base64_value(ch: String) -> Int {
  case string.split_once(base64_alphabet, ch) {
    Ok(#(before, _)) -> string.length(before)
    Error(Nil) -> 0
  }
}

/// Decode a full `mappings` string into output lines -> segments -> fields.
fn decode_mappings(mappings: String) -> List(List(List(Int))) {
  case mappings {
    "" -> []
    _ ->
      mappings
      |> string.split(";")
      |> list.map(decode_line)
  }
}

fn decode_line(line: String) -> List(List(Int)) {
  case line {
    "" -> []
    _ ->
      line
      |> string.split(",")
      |> list.map(decode_vlq)
  }
}

// --- bitwise helpers (test-local) -------------------------------------------

@external(erlang, "erlang", "band")
fn int_band(a: Int, b: Int) -> Int

@external(erlang, "erlang", "bsl")
fn int_shl(a: Int, b: Int) -> Int

@external(erlang, "erlang", "bsr")
fn int_shr(a: Int, b: Int) -> Int
