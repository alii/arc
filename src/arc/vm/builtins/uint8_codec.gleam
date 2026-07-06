/// Uint8Array base64/hex — proposal-arraybuffer-base64 (ES2026)
///
/// The six Uint8Array-only builtins (toBase64 / toHex / setFromBase64 /
/// setFromHex / fromBase64 / fromHex) and the pure decoders that back them.
/// Split out of typed_array so the ~740-line codec surface — its own type
/// vocabulary, its own option-object grammar, its own decode loop — sits in
/// one file a reader opens for "how does base64 work here", not buried at
/// the tail of the general TypedArray prototype module.
import arc/internal/digits
import arc/vm/builtins/common
import arc/vm/builtins/helpers
import arc/vm/heap
import arc/vm/internal/typed_array_ffi.{splice_clamped}
import arc/vm/js_string
import arc/vm/key.{Named}
import arc/vm/ops/buffer as ops_buffer
import arc/vm/ops/object
import arc/vm/ops/operators
import arc/vm/ops/typed_array_elements
import arc/vm/state.{type Heap, type State, State}
import arc/vm/value.{
  type JsValue, type Ref, JsObject, JsString, JsUndefined, ObjectSlot,
}
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Hard cap on a typed array's backing byte length (matches the engine's
/// practical allocation limit; over this → RangeError like real engines).
const max_byte_length = 2_147_483_647

/// 2^53 - 1 — MAX_SAFE_INTEGER, the ToIndex/ToLength upper bound.
const max_safe_integer = 9_007_199_254_740_991

/// The "alphabet" option — proposal-arraybuffer-base64.
type B64Alphabet {
  Base64
  Base64Url
}

/// The "lastChunkHandling" option — proposal-arraybuffer-base64.
type LastChunkHandling {
  Loose
  Strict
  StopBeforePartial
}

/// Which decoder produced a DecodeResult — names the SyntaxError.
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

// ----------------------------------------------------------------------------
// Uint8Array receiver helpers
// ----------------------------------------------------------------------------

/// The receiver's [[ViewedArrayBuffer]]/[[ByteOffset]]/[[ArrayLength]] slots
/// when it is specifically a Uint8Array — the only element kind these six
/// methods accept. Encodes the brand check in the type: a `U8Slot` never
/// describes a non-Uint8 view, so `elem_kind` is not carried (it is always 1
/// byte/element wherever this record flows).
type U8Slot {
  U8Slot(buffer: Ref, byte_offset: Int, length: Option(Int))
}

fn u8_slot(h: Heap(host), v: JsValue) -> Option(U8Slot) {
  case v {
    JsObject(ref) ->
      case heap.read(h, ref) {
        Some(ObjectSlot(
          kind: value.TypedArrayObject(
            buffer:,
            elem_kind: value.NumKind(value.Uint8Kind),
            byte_offset:,
            length:,
          ),
          ..,
        )) -> Some(U8Slot(buffer:, byte_offset:, length:))
        _ -> None
      }
    _ -> None
  }
}

/// ValidateUint8Array — RequireInternalSlot([[TypedArrayName]]) plus the
/// Uint8Array brand check. Does NOT check buffer liveness (that happens
/// later, after option coercion, per GetUint8ArrayBytes ordering).
fn validate_u8(
  this: JsValue,
  state: State(host),
) -> Result(State(host), #(JsValue, State(host))) {
  case u8_slot(state.heap, this) {
    Some(_) -> Ok(state)
    None -> state.type_error_op(state, "Method must be called on a Uint8Array")
  }
}

/// Immutable ArrayBuffer proposal: the write direction of ValidateUint8Array
/// — setFromBase64/setFromHex reject an immutable-backed target BEFORE any
/// option getter runs (observable; toBase64/toHex stay read-only).
fn u8_require_mutable(
  state: State(host),
  this: JsValue,
) -> Result(Nil, #(JsValue, State(host))) {
  // Same predicate as `require_mutable` / the [[Set]] element path; only the
  // prose differs (these methods are Uint8Array-only).
  let immutable = case u8_slot(state.heap, this) {
    Some(U8Slot(buffer:, ..)) ->
      typed_array_elements.buffer_is_immutable(state.heap, buffer)
    None -> False
  }
  case immutable {
    True ->
      state.type_error_op(
        state,
        "Cannot modify a Uint8Array backed by an immutable ArrayBuffer",
      )
    False -> Ok(Nil)
  }
}

/// The LIVE Uint8Array view a base64/hex method operates on: the buffer it
/// writes back into, the bytes it just proved are there, and the byte range it
/// covers. A record rather than a `#(Ref, BitArray, Int, Int)` — the last two
/// fields were adjacent bare `Int`s, so `off` and `len` could be swapped at a
/// call site and still type-check.
type U8LiveView {
  U8LiveView(buffer: Ref, data: BitArray, byte_offset: Int, length: Int)
}

/// MakeTypedArrayWithBufferWitnessRecord + IsTypedArrayOutOfBounds: resolve
/// the LIVE view right now (option getters may have detached/shrunk the
/// buffer). The bounds proof is the same one every %TypedArray% method uses.
///
/// TypedArrayLength is resolved against the very bytes read to prove bounds,
/// NEVER by re-reading the buffer: for a length-tracking view over a growable
/// SharedArrayBuffer, a second read can see a longer buffer than the snapshot
/// in `data`, and `length` would then run past the bytes it is supposed to
/// describe. Resolving both from one read is what makes
/// `byte_offset + length <= byte_size(data)` an invariant of this record.
fn u8_live_view(
  state: State(host),
  this: JsValue,
) -> Result(U8LiveView, #(JsValue, State(host))) {
  case u8_slot(state.heap, this) {
    Some(U8Slot(buffer:, byte_offset:, length:)) ->
      case object.typed_array_buffer_data(state.heap, buffer) {
        None ->
          Error(state.type_error_value(
            state,
            object.view_witness_error_message(object.BufferDetached),
          ))
        Some(data) -> {
          let byte_size = bit_array.byte_size(data)
          // Uint8 elem_size == 1, so the OOB check is in bytes directly.
          let oob = case length {
            Some(n) -> byte_offset + n > byte_size
            None -> byte_offset > byte_size
          }
          case oob {
            True ->
              Error(state.type_error_value(
                state,
                object.view_witness_error_message(object.OutOfBoundsView),
              ))
            False -> {
              let resolved =
                typed_array_elements.resolve_view(
                  byte_size,
                  value.NumKind(value.Uint8Kind),
                  byte_offset,
                  length,
                )
              Ok(U8LiveView(
                buffer:,
                data:,
                byte_offset:,
                length: typed_array_elements.view_len(resolved),
              ))
            }
          }
        }
      }
    None -> state.type_error_op(state, "Method must be called on a Uint8Array")
  }
}

// ----------------------------------------------------------------------------
// Option-object grammar
// ----------------------------------------------------------------------------

/// GetOptionsObject: undefined → absent, object → Some(ref), else TypeError.
fn get_opts_object(
  state: State(host),
  v: JsValue,
) -> Result(#(Option(Ref), State(host)), #(JsValue, State(host))) {
  case v {
    JsUndefined -> Ok(#(None, state))
    JsObject(ref) -> Ok(#(Some(ref), state))
    _ -> state.type_error_op(state, "options must be an object or undefined")
  }
}

/// Get(opts, key) — observable property read on the options object.
fn get_option_value(
  state: State(host),
  opts: Option(Ref),
  key: String,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case opts {
    None -> Ok(#(JsUndefined, state))
    Some(ref) -> object.get_value(state, ref, Named(key), JsObject(ref))
  }
}

/// String-enum option per the proposal: undefined → default; a non-String
/// value or a String `parse` rejects → TypeError (NO ToString coercion). The
/// accepted spellings live in `parse`, so the option's value can only ever
/// leave here as one of the enum's variants — a downstream `== "base64url"`
/// against a typo'd string is not expressible.
fn get_enum_option(
  state: State(host),
  opts: Option(Ref),
  key: String,
  parse: fn(String) -> Option(a),
  default: a,
) -> Result(#(a, State(host)), #(JsValue, State(host))) {
  use #(got, state) <- result.try(get_option_value(state, opts, key))
  case got {
    JsUndefined -> Ok(#(default, state))
    JsString(s) ->
      case parse(s) {
        Some(v) -> Ok(#(v, state))
        None ->
          state.type_error_op(
            state,
            "\"" <> s <> "\" is not a valid value for option " <> key,
          )
      }
    other ->
      state.type_error_op(
        state,
        "option "
          <> key
          <> " must be a string, got "
          <> operators.typeof(state.heap, other),
      )
  }
}

/// Shared option reads of setFromBase64/fromBase64, in spec order:
/// GetOptionsObject, then "alphabet", then "lastChunkHandling".
fn read_b64_options(
  state: State(host),
  opt_arg: JsValue,
) -> Result(
  #(B64Alphabet, LastChunkHandling, State(host)),
  #(JsValue, State(host)),
) {
  use #(opts, state) <- result.try(get_opts_object(state, opt_arg))
  use #(alphabet, state) <- result.try(get_enum_option(
    state,
    opts,
    "alphabet",
    parse_b64_alphabet,
    Base64,
  ))
  use #(handling, state) <- result.map(get_enum_option(
    state,
    opts,
    "lastChunkHandling",
    parse_last_chunk_handling,
    Loose,
  ))
  #(alphabet, handling, state)
}

/// Step 3 of setFromBase64/setFromHex/fromBase64/fromHex: the input must
/// already be a String — no coercion.
fn require_string(
  state: State(host),
  v: JsValue,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  case v {
    JsString(s) -> Ok(#(s, state))
    other ->
      state.type_error_op(
        state,
        "expected input to be a string, got "
          <> operators.typeof(state.heap, other),
      )
  }
}

// ----------------------------------------------------------------------------
// The six dispatch handlers
// ----------------------------------------------------------------------------

/// Uint8Array.prototype.toBase64 ( [ options ] )
pub fn u8_to_base64(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  helpers.lift_result({
    use state <- result.try(validate_u8(this, state))
    use #(opts, state) <- result.try(get_opts_object(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    use #(alphabet, state) <- result.try(get_enum_option(
      state,
      opts,
      "alphabet",
      parse_b64_alphabet,
      Base64,
    ))
    use #(omit_val, state) <- result.try(get_option_value(
      state,
      opts,
      "omitPadding",
    ))
    let padding = !value.is_truthy(omit_val)
    use view <- result.map(u8_live_view(state, this))
    // u8_live_view proved byte_offset + length <= byte_size(data).
    let assert Ok(bytes) =
      bit_array.slice(view.data, view.byte_offset, view.length)
    let out = case alphabet {
      Base64Url -> bit_array.base64_url_encode(bytes, padding)
      Base64 -> bit_array.base64_encode(bytes, padding)
    }
    #(JsString(out), state)
  })
}

/// Uint8Array.prototype.toHex ( )
pub fn u8_to_hex(
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  helpers.lift_result({
    use state <- result.try(validate_u8(this, state))
    use view <- result.map(u8_live_view(state, this))
    // u8_live_view proved byte_offset + length <= byte_size(data).
    let assert Ok(bytes) =
      bit_array.slice(view.data, view.byte_offset, view.length)
    #(JsString(string.lowercase(bit_array.base16_encode(bytes))), state)
  })
}

/// Uint8Array.prototype.setFromBase64 ( string [ , options ] )
pub fn u8_set_from_base64(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  helpers.lift_result({
    use state <- result.try(validate_u8(this, state))
    use Nil <- result.try(u8_require_mutable(state, this))
    use #(s, state) <- result.try(require_string(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    use #(alphabet, handling, state) <- result.try(read_b64_options(
      state,
      helpers.arg_at(args, 1),
    ))
    use view <- result.try(u8_live_view(state, this))
    let res = from_base64(s, alphabet, handling, view.length)
    decode_into_view(state, view, res, Base64Codec)
  })
}

/// Uint8Array.prototype.setFromHex ( string )
pub fn u8_set_from_hex(
  this: JsValue,
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  helpers.lift_result({
    use state <- result.try(validate_u8(this, state))
    use Nil <- result.try(u8_require_mutable(state, this))
    use #(s, state) <- result.try(require_string(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    use view <- result.try(u8_live_view(state, this))
    let res = from_hex(s, view.length)
    decode_into_view(state, view, res, HexCodec)
  })
}

/// Uint8Array.fromBase64 ( string [ , options ] )
pub fn u8_from_base64(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  helpers.lift_result({
    use #(s, state) <- result.try(require_string(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    use #(alphabet, handling, state) <- result.try(read_b64_options(
      state,
      helpers.arg_at(args, 1),
    ))
    let res = from_base64(s, alphabet, handling, max_safe_integer)
    decode_to_new_u8(state, res, Base64Codec)
  })
}

/// Uint8Array.fromHex ( string )
pub fn u8_from_hex(
  args: List(JsValue),
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  helpers.lift_result({
    use #(s, state) <- result.try(require_string(
      state,
      helpers.first_arg_or_undefined(args),
    ))
    let res = from_hex(s, max_safe_integer)
    decode_to_new_u8(state, res, HexCodec)
  })
}

// ----------------------------------------------------------------------------
// Shared write/alloc tails
// ----------------------------------------------------------------------------

/// Allocate the `{ read, written }` result object of setFromBase64/setFromHex.
fn read_written_result(
  state: State(host),
  read: Int,
  written: Int,
) -> #(JsValue, State(host)) {
  let #(heap, ref) =
    common.alloc_pojo(state.heap, state.builtins.object.prototype, [
      #("read", value.data_property(value.from_int(read))),
      #("written", value.data_property(value.from_int(written))),
    ])
  #(JsObject(ref), State(..state, heap:))
}

/// SetUint8ArrayBytes — splice the decoded bytes into the buffer at the
/// view's byte offset. Decoders never run user code, so `data` is current.
fn u8_write_bytes(
  state: State(host),
  buffer: Ref,
  data: BitArray,
  off: Int,
  bytes: BitArray,
) -> State(host) {
  let #(new_data, written) = splice_clamped(data, off, bytes)
  case written {
    0 -> state
    _ ->
      State(
        ..state,
        heap: ops_buffer.store_region(
          state.heap,
          buffer,
          new_data,
          off,
          written,
        ),
      )
  }
}

fn decode_error(state: State(host), codec: Codec) -> #(JsValue, State(host)) {
  state.syntax_error_value(
    state,
    "unable to decode " <> codec_name(codec) <> " string",
  )
}

/// Shared setFromBase64/setFromHex tail. Per spec, bytes decoded before an
/// error ARE written, THEN the SyntaxError is thrown ("writes up to error").
fn decode_into_view(
  state: State(host),
  view: U8LiveView,
  res: DecodeResult,
  codec: Codec,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let U8LiveView(buffer:, data:, byte_offset: off, ..) = view
  case res {
    DecodeFailed(partial:) ->
      Error(decode_error(
        u8_write_bytes(state, buffer, data, off, partial),
        codec,
      ))
    Decoded(read:, bytes:) -> {
      let state = u8_write_bytes(state, buffer, data, off, bytes)
      Ok(read_written_result(state, read, bit_array.byte_size(bytes)))
    }
  }
}

/// Shared fromBase64/fromHex tail: allocate a fresh Uint8Array on success.
/// (No view to write into here, so a failed decode's partial bytes are
/// dropped — only the SyntaxError surfaces.)
fn decode_to_new_u8(
  state: State(host),
  res: DecodeResult,
  codec: Codec,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case res {
    DecodeFailed(partial: _) -> Error(decode_error(state, codec))
    Decoded(read: _, bytes:) -> u8_alloc_from_bytes(state, bytes)
  }
}

/// Allocate a fresh Uint8Array holding exactly `bytes` — a fresh
/// non-resizable ArrayBuffer with `bytes` as its data plus a fixed
/// full-length Uint8 view over it. (No zeroed intermediate: the buffer is
/// created directly around the decoded bytes.)
fn u8_alloc_from_bytes(
  state: State(host),
  bytes: BitArray,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  let len = bit_array.byte_size(bytes)
  use <- bool.lazy_guard(len > max_byte_length, fn() {
    state.range_error_op(state, "Invalid typed array length")
  })
  let kind = value.NumKind(value.Uint8Kind)
  let #(h, buf) =
    common.alloc_wrapper(
      state.heap,
      value.ArrayBufferObject(storage: value.Bytes(
        bytes: bytes,
        max_byte_length: None,
      )),
      state.builtins.array_buffer.prototype,
    )
  let #(h, ta_ref) =
    common.alloc_wrapper(
      h,
      value.TypedArrayObject(
        buffer: buf,
        elem_kind: kind,
        byte_offset: 0,
        length: Some(len),
      ),
      common.typed_array_builtin(state.builtins, kind).prototype,
    )
  Ok(#(JsObject(ta_ref), State(..state, heap: h)))
}

// ----------------------------------------------------------------------------
// FromBase64 / FromHex — the decoders (no user code runs inside).
// `read` is in string code units; positions it reports always fall inside an
// all-ASCII prefix, so iterating the UTF-8 bytes keeps the counts exact (any
// non-ASCII byte is an immediate decode error).
// ----------------------------------------------------------------------------

/// Result of FromBase64/FromHex. `Decoded` is a clean decode: `read` input
/// code units consumed and the `bytes` they produced. `DecodeFailed` is the
/// spec record that carried a SyntaxError; `partial` holds the bytes decoded
/// BEFORE the error (setFromBase64/setFromHex still write those, then throw).
type DecodeResult {
  Decoded(read: Int, bytes: BitArray)
  DecodeFailed(partial: BitArray)
}

/// Join decoded chunks (accumulated newest-first) into the final byte string.
/// A single concat at the end keeps decoding O(n); appending to a growing
/// BitArray per chunk copies the whole accumulator each time (O(n^2)).
fn decode_bytes(acc: List(BitArray)) -> BitArray {
  bit_array.concat(list.reverse(acc))
}

/// FromBase64 ( string, alphabet, lastChunkHandling, maxLength )
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

/// SkipAsciiWhitespace — TAB LF FF CR SPACE.
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
    // Unreachable: the input is a UTF-8 binary, always whole bytes.
    _ -> DecodeFailed(decode_bytes(acc))
  }
}

/// The '=' branch of FromBase64: validate padding, decode the partial chunk.
/// On entry `index` is the position just after the first '='.
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

/// After padding: anything left in the string is an error; otherwise decode
/// the 2- or 3-char chunk ("strict" rejects non-zero extra bits).
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

/// DecodeBase64Chunk for 2- or 3-char chunks. None = non-zero extra bits
/// rejected under `throw_on_extra_bits`.
fn b64_decode_partial(
  chunk: Int,
  chunk_len: Int,
  throw_on_extra_bits: Bool,
) -> Option(BitArray) {
  case chunk_len {
    2 -> {
      // 12 bits: 1 byte + 4 extra bits.
      let extra = int.bitwise_and(chunk, 0xF)
      case throw_on_extra_bits && extra != 0 {
        True -> None
        False -> Some(<<int.bitwise_shift_right(chunk, 4)>>)
      }
    }
    _ -> {
      // 18 bits: 2 bytes + 2 extra bits.
      let extra = int.bitwise_and(chunk, 0x3)
      case throw_on_extra_bits && extra != 0 {
        True -> None
        False -> Some(<<int.bitwise_shift_right(chunk, 2):size(16)>>)
      }
    }
  }
}

/// Map a base64 character (as its code unit) to its 6-bit value. In the
/// base64url alphabet '-'/'_' replace '+'/'/'.
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

/// FromHex ( string, maxLength )
fn from_hex(s: String, max_len: Int) -> DecodeResult {
  // The odd-length check is on the string's UTF-16 length, not its UTF-8
  // byte count (they can differ when the bad char is non-ASCII).
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
