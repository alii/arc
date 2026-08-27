import arc/internal/digits
import arc/rt/buffer
import arc/rt/builtins/common
import arc/rt/builtins/helpers
import arc/rt/builtins/realm_ops
import arc/rt/js_string
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/typed_array_ffi.{splice_clamped}
import arc/rt/types.{
  type Agent, type Handle, type JsVal, ArrayBufferObj, Bytes, JInt, KHandle,
  KStr, KUndef, Named, NumKind, SObject, StringKey, TypedArrayObj, Uint8Kind,
  classify, mk_number, mk_object, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

const max_byte_length = 2_147_483_647

// 2^53 - 1
const max_safe_integer = 9_007_199_254_740_991

type B64Alphabet {
  Base64
  Base64Url
}

type LastChunkHandling {
  Loose
  Strict
  StopBeforePartial
}

type Codec {
  Base64Codec
  HexCodec
}

fn parse_b64_alphabet(s: String) -> Option(B64Alphabet) {
  case s {
    "base64" -> Some(Base64)
    "base64url" -> Some(Base64Url)
    _ -> None
  }
}

fn parse_last_chunk_handling(s: String) -> Option(LastChunkHandling) {
  case s {
    "loose" -> Some(Loose)
    "strict" -> Some(Strict)
    "stop-before-partial" -> Some(StopBeforePartial)
    _ -> None
  }
}

fn codec_name(codec: Codec) -> String {
  case codec {
    Base64Codec -> "base64"
    HexCodec -> "hex"
  }
}

type U8Slot {
  U8Slot(buffer: Handle, byte_offset: Int, length: Option(Int))
}

fn u8_slot(st: Agent, v: JsVal) -> Option(U8Slot) {
  case classify(v) {
    KHandle(ref) ->
      case rt_store.t_cell_get(st, ref) {
        SObject(
          kind: TypedArrayObj(
            buffer:,
            elem_kind: NumKind(Uint8Kind),
            byte_offset:,
            length:,
          ),
          ..,
        ) -> Some(U8Slot(buffer:, byte_offset:, length:))
        _ -> None
      }
    _ -> None
  }
}

fn validate_u8(st: Agent, this: JsVal) -> Nil {
  case u8_slot(st, this) {
    Some(_) -> Nil
    None ->
      rt_val.t_throw_type_error(st, "Method must be called on a Uint8Array")
  }
}

fn u8_require_mutable(st: Agent, this: JsVal) -> Nil {
  let immutable = case u8_slot(st, this) {
    Some(U8Slot(buffer:, ..)) -> buffer.buffer_is_immutable(st, buffer)
    None -> False
  }
  case immutable {
    True ->
      rt_val.t_throw_type_error(
        st,
        "Cannot modify a Uint8Array backed by an immutable ArrayBuffer",
      )
    False -> Nil
  }
}

type U8LiveView {
  U8LiveView(buffer: Handle, data: BitArray, byte_offset: Int, length: Int)
}

// resolve length from the same read as data, never re-read
fn u8_live_view(st: Agent, this: JsVal) -> U8LiveView {
  case u8_slot(st, this) {
    Some(U8Slot(buffer:, byte_offset:, length:)) ->
      case buffer.buffer_bytes(st, buffer) {
        None ->
          rt_val.t_throw_type_error(
            st,
            buffer.view_witness_error_message(buffer.BufferDetached),
          )
        Some(data) -> {
          let byte_size = bit_array.byte_size(data)
          let oob = case length {
            Some(n) -> byte_offset + n > byte_size
            None -> byte_offset > byte_size
          }
          case oob {
            True ->
              rt_val.t_throw_type_error(
                st,
                buffer.view_witness_error_message(buffer.OutOfBoundsView),
              )
            False -> {
              let resolved =
                buffer.resolve_view(
                  byte_size,
                  NumKind(Uint8Kind),
                  byte_offset,
                  length,
                )
              U8LiveView(
                buffer:,
                data:,
                byte_offset:,
                length: buffer.view_len(resolved),
              )
            }
          }
        }
      }
    None ->
      rt_val.t_throw_type_error(st, "Method must be called on a Uint8Array")
  }
}

fn get_opts_object(st: Agent, v: JsVal) -> Option(Handle) {
  case classify(v) {
    KUndef -> None
    KHandle(ref) -> Some(ref)
    _ -> rt_val.t_throw_type_error(st, "options must be an object or undefined")
  }
}

fn get_option_value(
  st: Agent,
  opts: Option(Handle),
  key: String,
) -> #(JsVal, Agent) {
  case opts {
    None -> #(mk_undefined(), st)
    Some(ref) -> rt_obj.t_get_prop(st, mk_object(ref), StringKey(Named(key)))
  }
}

fn get_enum_option(
  st: Agent,
  opts: Option(Handle),
  key: String,
  parse: fn(String) -> Option(a),
  default: a,
) -> #(a, Agent) {
  let #(got, st) = get_option_value(st, opts, key)
  case classify(got) {
    KUndef -> #(default, st)
    KStr(s) ->
      case parse(s) {
        Some(v) -> #(v, st)
        None ->
          rt_val.t_throw_type_error(
            st,
            "\"" <> s <> "\" is not a valid value for option " <> key,
          )
      }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "option " <> key <> " must be a string, got " <> type_of(st, got),
      )
  }
}

fn type_of(st: Agent, v: JsVal) -> String {
  let #(ty, _) = rt_val.t_type_of(st, v)
  ty
}

fn read_b64_options(
  st: Agent,
  opt_arg: JsVal,
) -> #(B64Alphabet, LastChunkHandling, Agent) {
  let opts = get_opts_object(st, opt_arg)
  let #(alphabet, st) =
    get_enum_option(st, opts, "alphabet", parse_b64_alphabet, Base64)
  let #(handling, st) =
    get_enum_option(
      st,
      opts,
      "lastChunkHandling",
      parse_last_chunk_handling,
      Loose,
    )
  #(alphabet, handling, st)
}

fn require_string(st: Agent, v: JsVal) -> String {
  case classify(v) {
    KStr(s) -> s
    _ ->
      rt_val.t_throw_type_error(
        st,
        "expected input to be a string, got " <> type_of(st, v),
      )
  }
}

pub fn u8_to_base64(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let Nil = validate_u8(st, this)
  let opts = get_opts_object(st, helpers.first_arg_or_undefined(args))
  let #(alphabet, st) =
    get_enum_option(st, opts, "alphabet", parse_b64_alphabet, Base64)
  let #(omit_val, st) = get_option_value(st, opts, "omitPadding")
  let padding = !rt_val.to_boolean(omit_val)
  let view = u8_live_view(st, this)
  let assert Ok(bytes) =
    bit_array.slice(view.data, view.byte_offset, view.length)
  let out = case alphabet {
    Base64Url -> bit_array.base64_url_encode(bytes, padding)
    Base64 -> bit_array.base64_encode(bytes, padding)
  }
  #(mk_string(out), st)
}

pub fn u8_to_hex(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  let Nil = validate_u8(st, this)
  let view = u8_live_view(st, this)
  let assert Ok(bytes) =
    bit_array.slice(view.data, view.byte_offset, view.length)
  #(mk_string(string.lowercase(bit_array.base16_encode(bytes))), st)
}

pub fn u8_set_from_base64(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let Nil = validate_u8(st, this)
  let Nil = u8_require_mutable(st, this)
  let s = require_string(st, helpers.first_arg_or_undefined(args))
  let #(alphabet, handling, st) = read_b64_options(st, helpers.arg_at(args, 1))
  let view = u8_live_view(st, this)
  let res = from_base64(s, alphabet, handling, view.length)
  decode_into_view(st, view, res, Base64Codec)
}

pub fn u8_set_from_hex(
  st: Agent,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let Nil = validate_u8(st, this)
  let Nil = u8_require_mutable(st, this)
  let s = require_string(st, helpers.first_arg_or_undefined(args))
  let view = u8_live_view(st, this)
  let res = from_hex(s, view.length)
  decode_into_view(st, view, res, HexCodec)
}

pub fn u8_from_base64(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let s = require_string(st, helpers.first_arg_or_undefined(args))
  let #(alphabet, handling, st) = read_b64_options(st, helpers.arg_at(args, 1))
  let res = from_base64(s, alphabet, handling, max_safe_integer)
  decode_to_new_u8(st, res, Base64Codec)
}

pub fn u8_from_hex(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let s = require_string(st, helpers.first_arg_or_undefined(args))
  let res = from_hex(s, max_safe_integer)
  decode_to_new_u8(st, res, HexCodec)
}

fn read_written_result(st: Agent, read: Int, written: Int) -> #(JsVal, Agent) {
  let #(ref, st) =
    common.alloc_pojo(st, st.realm.object.prototype, [
      #("read", mk_number(JInt(read))),
      #("written", mk_number(JInt(written))),
    ])
  #(mk_object(ref), st)
}

fn u8_write_bytes(
  st: Agent,
  buffer: Handle,
  data: BitArray,
  off: Int,
  bytes: BitArray,
) -> Agent {
  let #(new_data, written) = splice_clamped(data, off, bytes)
  case written {
    0 -> st
    _ -> buffer.store_region(st, buffer, new_data, off, written)
  }
}

fn decode_error(st: Agent, codec: Codec) -> a {
  rt_val.t_throw_syntax_error(
    st,
    "unable to decode " <> codec_name(codec) <> " string",
  )
}

// partial bytes are written before the syntaxerror throws
fn decode_into_view(
  st: Agent,
  view: U8LiveView,
  res: DecodeResult,
  codec: Codec,
) -> #(JsVal, Agent) {
  let U8LiveView(buffer:, data:, byte_offset: off, ..) = view
  case res {
    DecodeFailed(partial:) ->
      decode_error(u8_write_bytes(st, buffer, data, off, partial), codec)
    Decoded(read:, bytes:) -> {
      let st = u8_write_bytes(st, buffer, data, off, bytes)
      read_written_result(st, read, bit_array.byte_size(bytes))
    }
  }
}

fn decode_to_new_u8(
  st: Agent,
  res: DecodeResult,
  codec: Codec,
) -> #(JsVal, Agent) {
  case res {
    DecodeFailed(partial: _) -> decode_error(st, codec)
    Decoded(read: _, bytes:) -> u8_alloc_from_bytes(st, bytes)
  }
}

fn u8_alloc_from_bytes(st: Agent, bytes: BitArray) -> #(JsVal, Agent) {
  let len = bit_array.byte_size(bytes)
  use <- bool.lazy_guard(len > max_byte_length, fn() {
    rt_val.t_throw_range_error(st, "Invalid typed array length")
  })
  let kind = NumKind(Uint8Kind)
  let #(buf, st) =
    realm_ops.alloc_wrapper(
      st,
      ArrayBufferObj(storage: Bytes(bytes:, max_byte_length: None)),
      st.realm.array_buffer.prototype,
    )
  let #(ta_ref, st) =
    realm_ops.alloc_wrapper(
      st,
      TypedArrayObj(
        buffer: buf,
        elem_kind: kind,
        byte_offset: 0,
        length: Some(len),
      ),
      u8_prototype(st),
    )
  #(mk_object(ta_ref), st)
}

fn u8_prototype(st: Agent) -> Handle {
  let assert Ok(bt) =
    dict.get(st.realm.typed_arrays.by_kind, NumKind(Uint8Kind))
    as "uint8_codec: Uint8Array missing from realm.typed_arrays"
  bt.prototype
}

type DecodeResult {
  Decoded(read: Int, bytes: BitArray)
  DecodeFailed(partial: BitArray)
}

fn decode_bytes(acc: List(BitArray)) -> BitArray {
  bit_array.concat(list.reverse(acc))
}

fn from_base64(
  s: String,
  alphabet: B64Alphabet,
  handling: LastChunkHandling,
  max_len: Int,
) -> DecodeResult {
  use <- bool.guard(max_len == 0, Decoded(0, <<>>))
  b64_loop(
    bit_array.from_string(s),
    0,
    0,
    [],
    0,
    0,
    0,
    alphabet,
    handling,
    max_len,
  )
}

// tab lf ff cr space
fn b64_skip_ws(bin: BitArray, index: Int) -> #(BitArray, Int) {
  case bin {
    <<c, rest:bits>> if c == 9 || c == 10 || c == 12 || c == 13 || c == 32 ->
      b64_skip_ws(rest, index + 1)
    _ -> #(bin, index)
  }
}

fn b64_loop(
  bin: BitArray,
  index: Int,
  read: Int,
  acc: List(BitArray),
  written: Int,
  chunk: Int,
  chunk_len: Int,
  alphabet: B64Alphabet,
  handling: LastChunkHandling,
  max_len: Int,
) -> DecodeResult {
  let #(bin, index) = b64_skip_ws(bin, index)
  case bin {
    <<>> ->
      case chunk_len > 0 {
        True ->
          case handling {
            StopBeforePartial -> Decoded(read, decode_bytes(acc))
            Loose ->
              case chunk_len == 1 {
                True -> DecodeFailed(decode_bytes(acc))
                False ->
                  case b64_decode_partial(chunk, chunk_len, False) {
                    Some(tail) -> Decoded(index, decode_bytes([tail, ..acc]))
                    None -> DecodeFailed(decode_bytes(acc))
                  }
              }
            Strict -> DecodeFailed(decode_bytes(acc))
          }
        False -> Decoded(index, decode_bytes(acc))
      }
    // '='
    <<61, rest:bits>> ->
      b64_padding(rest, index + 1, read, acc, chunk, chunk_len, handling)
    <<c, rest:bits>> ->
      case b64_value(c, alphabet) {
        None -> DecodeFailed(decode_bytes(acc))
        Some(v) -> {
          let remaining = max_len - written
          let stop =
            { remaining == 1 && chunk_len == 2 }
            || { remaining == 2 && chunk_len == 3 }
          case stop {
            True -> Decoded(read, decode_bytes(acc))
            False -> {
              let chunk = chunk * 64 + v
              case chunk_len + 1 == 4 {
                True -> {
                  let acc = [<<chunk:size(24)>>, ..acc]
                  let written = written + 3
                  case written == max_len {
                    True -> Decoded(index + 1, decode_bytes(acc))
                    False ->
                      b64_loop(
                        rest,
                        index + 1,
                        index + 1,
                        acc,
                        written,
                        0,
                        0,
                        alphabet,
                        handling,
                        max_len,
                      )
                  }
                }
                False ->
                  b64_loop(
                    rest,
                    index + 1,
                    read,
                    acc,
                    written,
                    chunk,
                    chunk_len + 1,
                    alphabet,
                    handling,
                    max_len,
                  )
              }
            }
          }
        }
      }
    _ -> DecodeFailed(decode_bytes(acc))
  }
}

fn b64_padding(
  bin: BitArray,
  index: Int,
  read: Int,
  acc: List(BitArray),
  chunk: Int,
  chunk_len: Int,
  handling: LastChunkHandling,
) -> DecodeResult {
  use <- bool.guard(chunk_len < 2, DecodeFailed(decode_bytes(acc)))
  let #(bin, index) = b64_skip_ws(bin, index)
  case chunk_len == 2 {
    True ->
      case bin {
        <<>> ->
          case handling {
            StopBeforePartial -> Decoded(read, decode_bytes(acc))
            Loose | Strict -> DecodeFailed(decode_bytes(acc))
          }
        // second '='
        <<61, rest:bits>> -> {
          let #(rest, index) = b64_skip_ws(rest, index + 1)
          b64_finish_padding(rest, index, acc, chunk, chunk_len, handling)
        }
        _ -> DecodeFailed(decode_bytes(acc))
      }
    False -> b64_finish_padding(bin, index, acc, chunk, chunk_len, handling)
  }
}

fn b64_finish_padding(
  bin: BitArray,
  index: Int,
  acc: List(BitArray),
  chunk: Int,
  chunk_len: Int,
  handling: LastChunkHandling,
) -> DecodeResult {
  case bin {
    <<>> ->
      case b64_decode_partial(chunk, chunk_len, handling == Strict) {
        Some(tail) -> Decoded(index, decode_bytes([tail, ..acc]))
        None -> DecodeFailed(decode_bytes(acc))
      }
    _ -> DecodeFailed(decode_bytes(acc))
  }
}

fn b64_decode_partial(
  chunk: Int,
  chunk_len: Int,
  throw_on_extra_bits: Bool,
) -> Option(BitArray) {
  case chunk_len {
    2 -> {
      // 12 bits: 1 byte + 4 extra
      let extra = int.bitwise_and(chunk, 0xF)
      case throw_on_extra_bits && extra != 0 {
        True -> None
        False -> Some(<<int.bitwise_shift_right(chunk, 4)>>)
      }
    }
    _ -> {
      // 18 bits: 2 bytes + 2 extra
      let extra = int.bitwise_and(chunk, 0x3)
      case throw_on_extra_bits && extra != 0 {
        True -> None
        False -> Some(<<int.bitwise_shift_right(chunk, 2):size(16)>>)
      }
    }
  }
}

fn b64_value(c: Int, alphabet: B64Alphabet) -> Option(Int) {
  let url = alphabet == Base64Url
  case c {
    _ if c >= 65 && c <= 90 -> Some(c - 65)
    _ if c >= 97 && c <= 122 -> Some(c - 71)
    _ if c >= 48 && c <= 57 -> Some(c + 4)
    43 if !url -> Some(62)
    47 if !url -> Some(63)
    45 if url -> Some(62)
    95 if url -> Some(63)
    _ -> None
  }
}

fn from_hex(s: String, max_len: Int) -> DecodeResult {
  // odd check is on utf-16 length, not bytes
  case js_string.length(s) % 2 != 0 {
    True -> DecodeFailed(<<>>)
    False -> hex_loop(bit_array.from_string(s), 0, [], 0, max_len)
  }
}

fn hex_loop(
  bin: BitArray,
  read: Int,
  acc: List(BitArray),
  written: Int,
  max_len: Int,
) -> DecodeResult {
  case bin {
    <<>> -> Decoded(read, decode_bytes(acc))
    _ if written >= max_len -> Decoded(read, decode_bytes(acc))
    <<h1, h2, rest:bits>> ->
      case digits.hex_value_code(h1), digits.hex_value_code(h2) {
        Some(a), Some(b) -> {
          let byte = a * 16 + b
          hex_loop(rest, read + 2, [<<byte>>, ..acc], written + 1, max_len)
        }
        _, _ -> DecodeFailed(decode_bytes(acc))
      }
    _ -> DecodeFailed(decode_bytes(acc))
  }
}
