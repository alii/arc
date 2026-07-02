/// §22.1.3.13 / §22.1.3.20 / §22.1.3.23 — String.prototype match/search/split
/// must run RequireObjectCoercible(this) FIRST, then delegate to the
/// argument's @@match/@@search/@@split with the ORIGINAL receiver. ToString of
/// the receiver is only allowed on the non-delegated (RegExp fallback) path.
import arc/engine.{Returned}
import arc/vm/value.{JsBool, JsString}

/// Eval source on a fresh engine, assert normal completion, return the value.
fn eval(source: String) -> value.JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  value
}

/// Eval an expression expected to throw; return the thrown error's `name`
/// (e.g. "TypeError"), or "no throw" if it completed normally.
fn thrown_name(expr: String) -> value.JsValue {
  eval(
    "(function () { try { "
    <> expr
    <> "; return 'no throw'; } catch (e) { return e.name; } })()",
  )
}

// ---------------------------------------------------------------------------
// Step 1: RequireObjectCoercible(this) runs BEFORE @@method delegation, so a
// null/undefined receiver throws TypeError even when the argument has the
// symbol method.
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Step 2: the delegated @@method receives the ORIGINAL receiver — an object
// receiver must arrive as an object, not as a pre-coerced string.
// ---------------------------------------------------------------------------

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
  // A primitive string receiver still arrives unchanged.
  assert eval(
      "var o = {}; o[Symbol.match] = function (t) { return t; };"
      <> "'abc'.match(o)",
    )
    == JsString("abc")
}

// ---------------------------------------------------------------------------
// Step 3: ToString(this) is deferred until AFTER delegation was declined, so
// a receiver with a throwing toString must not throw when the argument
// delegates.
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// The non-delegated RegExp fallback path still coerces the receiver.
// ---------------------------------------------------------------------------

pub fn match_fallback_still_matches_test() {
  assert eval("'abc'.match('b')[0]") == JsString("b")
}

pub fn search_fallback_still_searches_test() {
  assert eval("'abc'.search('b') === 1") == JsBool(True)
}
