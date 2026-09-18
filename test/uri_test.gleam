import arc/engine.{Returned}
import arc/rt/types.{type JsValKind, KBool, KStr}
import rt_helpers

fn eval_outcome(source: String) -> JsValKind {
  let eng: engine.Engine(Nil) = engine.new()
  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, source)
  rt_helpers.classify(value)
}

pub fn decode_uri_component_decodes_reserved_and_multibyte_test() {
  assert eval_outcome("decodeURIComponent('%E4%B8%AD%26%2F')") == KStr("中&/")
}

pub fn decode_uri_preserves_reserved_escapes_test() {
  assert eval_outcome("decodeURI('a%26b%2fc%41')") == KStr("a%26b%2fcA")
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
    == KBool(True)
}

pub fn decode_uri_invalid_utf8_throws_uri_error_test() {
  assert eval_outcome(
      "['%C0%80', '%ED%A0%80', '%E4%B8', '%80', '%zz'].every(function (s) {
         try { decodeURI(s); } catch (e) { return e instanceof URIError; }
         return false;
       })",
    )
    == KBool(True)
}

pub fn encode_uri_component_encodes_reserved_test() {
  assert eval_outcome("encodeURIComponent('a b&c/d')") == KStr("a%20b%26c%2Fd")
}

pub fn encode_uri_preserves_reserved_test() {
  assert eval_outcome("encodeURI('a b&c/d')") == KStr("a%20b&c/d")
}

pub fn escape_unescape_roundtrip_test() {
  assert eval_outcome("unescape(escape('a b\\u00e9\\u4e2d'))") == KStr("a bé中")
}

pub fn escape_astral_emits_surrogate_pair_test() {
  assert eval_outcome("escape('\u{1F600}')") == KStr("%uD83D%uDE00")
}

pub fn unescape_astral_roundtrip_test() {
  assert eval_outcome("unescape(escape('\u{1F600}')) === '\u{1F600}'")
    == KBool(True)
}

pub fn unescape_rejects_signed_hex_test() {
  assert eval_outcome("unescape('%+41') + unescape('%u+0061')")
    == KStr("%+41%u+0061")
}
