import arc/engine.{Returned}
import arc/vm/value.{JsBool, JsString}

// ----------------------------------------------------------------------------
// Global URI functions: decodeURI / decodeURIComponent / encodeURI /
// encodeURIComponent and the AnnexB escape / unescape pair.
// ----------------------------------------------------------------------------

/// Helper: evaluate `source` in a fresh engine and return the outcome.
fn eval_outcome(source: String) -> engine.Outcome {
  let eng: engine.Engine(Nil) = engine.new()
  let assert Ok(#(outcome, _)) = engine.eval(eng, source)
  outcome
}

pub fn decode_uri_component_decodes_reserved_and_multibyte_test() {
  assert eval_outcome("decodeURIComponent('%E4%B8%AD%26%2F')")
    == Returned(JsString("中&/"))
}

/// §19.2.6.2 step 4: decodeURI must leave escapes of the reserved set
/// `;/?:@&=+$,#` intact (with their original hex case), while still decoding
/// non-reserved escapes.
pub fn decode_uri_preserves_reserved_escapes_test() {
  assert eval_outcome("decodeURI('a%26b%2fc%41')")
    == Returned(JsString("a%26b%2fcA"))
}

pub fn decode_uri_truncated_escape_throws_uri_error_test() {
  assert eval_outcome(
      "(function () {
         try { decodeURIComponent('%'); } catch (e) {
           return e instanceof URIError && e.name === 'URIError';
         }
         return false;
       })()",
    )
    == Returned(JsBool(True))
}

/// Malformed escape runs must throw a URIError, never leak invalid UTF-8
/// into the result: overlong form, UTF-8-encoded surrogate, truncated
/// multi-byte sequence, lone continuation byte, non-hex digits.
pub fn decode_uri_invalid_utf8_throws_uri_error_test() {
  assert eval_outcome(
      "['%C0%80', '%ED%A0%80', '%E4%B8', '%80', '%zz'].every(function (s) {
         try { decodeURI(s); } catch (e) { return e instanceof URIError; }
         return false;
       })",
    )
    == Returned(JsBool(True))
}

pub fn encode_uri_component_encodes_reserved_test() {
  assert eval_outcome("encodeURIComponent('a b&c/d')")
    == Returned(JsString("a%20b%26c%2Fd"))
}

pub fn encode_uri_preserves_reserved_test() {
  assert eval_outcome("encodeURI('a b&c/d')") == Returned(JsString("a%20b&c/d"))
}

pub fn escape_unescape_roundtrip_test() {
  assert eval_outcome("unescape(escape('a b\\u00e9\\u4e2d'))")
    == Returned(JsString("a bé中"))
}
