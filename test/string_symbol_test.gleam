import arc/engine.{type JsValueKind, JsBool, JsString, Returned}

fn eval(source: String) -> JsValueKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  engine.classify(value)
}

fn thrown_name(expr: String) -> JsValueKind {
  eval(
    "(function () { try { "
    <> expr
    <> "; return 'no throw'; } catch (e) { return e.name; } })()",
  )
}

pub fn match_null_receiver_throws_before_delegation_test() {
  assert thrown_name(
      "var o = {}; o[Symbol.match] = function () { return 1; };"
      <> "String.prototype.match.call(null, o)",
    )
    == JsString("TypeError")
}

pub fn search_null_receiver_throws_before_delegation_test() {
  assert thrown_name(
      "var o = {}; o[Symbol.search] = function () { return 1; };"
      <> "String.prototype.search.call(null, o)",
    )
    == JsString("TypeError")
}

pub fn split_null_receiver_throws_before_delegation_test() {
  assert thrown_name(
      "var o = {}; o[Symbol.split] = function () { return 1; };"
      <> "String.prototype.split.call(null, o)",
    )
    == JsString("TypeError")
}

pub fn match_delegation_receives_original_receiver_test() {
  assert eval(
      "var o = {}; o[Symbol.match] = function (t) { return typeof t; };"
      <> "String.prototype.match.call({}, o)",
    )
    == JsString("object")
}

pub fn search_delegation_receives_original_receiver_test() {
  assert eval(
      "var o = {}; o[Symbol.search] = function (t) { return typeof t; };"
      <> "String.prototype.search.call({}, o)",
    )
    == JsString("object")
}

pub fn match_delegation_receives_string_receiver_test() {
  assert eval(
      "var o = {}; o[Symbol.match] = function (t) { return t; };"
      <> "'abc'.match(o)",
    )
    == JsString("abc")
}

pub fn match_does_not_stringify_receiver_before_delegation_test() {
  assert eval(
      "var bad = { toString: function () { throw new Error('boom'); } };"
      <> "var o = {}; o[Symbol.match] = function () { return 'delegated'; };"
      <> "String.prototype.match.call(bad, o)",
    )
    == JsString("delegated")
}

pub fn search_does_not_stringify_receiver_before_delegation_test() {
  assert eval(
      "var bad = { toString: function () { throw new Error('boom'); } };"
      <> "var o = {}; o[Symbol.search] = function () { return 'delegated'; };"
      <> "String.prototype.search.call(bad, o)",
    )
    == JsString("delegated")
}

pub fn match_fallback_still_matches_test() {
  assert eval("'abc'.match('b')[0]") == JsString("b")
}

pub fn search_fallback_still_searches_test() {
  assert eval("'abc'.search('b') === 1") == JsBool(True)
}
