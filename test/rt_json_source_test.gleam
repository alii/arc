import arc/bytecode/key.{canonical_key}
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type JsVal, DataProperty, FnFlags, JFloat, JInt, KBool, KHandle,
  KNum, KStr, StringKey, classify, mk_number, mk_string,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import rt_helpers

fn key(name: String) {
  StringKey(canonical_key(name))
}

fn json(st: Agent, method: String, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(ns, st) = rt_helpers.global(st, "JSON")
  rt_call.t_call_method(st, ns, key(method), args)
}

fn reviver(
  st: Agent,
  body: fn(Agent, JsVal, List(JsVal)) -> #(JsVal, Agent),
) -> #(JsVal, Agent) {
  let flags =
    FnFlags(
      is_constructor: False,
      is_class_constructor: False,
      is_derived_constructor: False,
      is_arrow: False,
      is_method: False,
      is_generator: False,
      is_async: False,
      is_strict: True,
    )
  let code =
    rt_helpers.as_code(fn(st, frame, args) {
      body(st, rt_helpers.frame_at(1, frame), args)
    })
  let #(h, st) = rt_call.t_fn_new(st, code, flags, "reviver", 3, None, None)
  #(types.mk_object(h), st)
}

fn record_call(st: Agent, args: List(JsVal)) -> Agent {
  let assert [k, _v, context] = args
  let assert KStr(name) = classify(k)
  let assert KHandle(ctx) = classify(context)
  let #(proto, st) = rt_obj.t_get_prototype_of(st, ctx)
  assert proto == Some(st.realm.object.prototype)
  let #(keys, st) = rt_obj.t_own_keys(st, ctx)
  let #(source, st) = case keys {
    [] -> #(None, st)
    [k] -> {
      assert k == key("source")
      let #(desc, st) = rt_obj.t_get_own_property(st, ctx, k)
      let assert Some(DataProperty(
        value:,
        writable: True,
        enumerable: True,
        configurable: True,
        ..,
      )) = desc
      let assert KStr(text) = classify(value)
      #(Some(text), st)
    }
    _ -> panic as "context has more than one own property"
  }
  rt_helpers.record(#(name, source))
  st
}

fn logging_reviver(st: Agent) -> #(JsVal, Agent) {
  use st, _this, args <- reviver(st)
  let assert [_, v, _] = args
  #(v, record_call(st, args))
}

fn parse_logged(st: Agent, text: String) -> #(JsVal, Agent) {
  let #(f, st) = logging_reviver(st)
  json(st, "parse", [mk_string(text), f])
}

fn calls() -> List(#(String, Option(String))) {
  rt_helpers.recorded()
}

fn num(v: JsVal) -> Float {
  case classify(v) {
    KNum(JInt(n)) -> int.to_float(n)
    KNum(JFloat(f)) -> f
    _ -> panic as "not a finite number"
  }
}

fn is_one(v: JsVal) -> Bool {
  case classify(v) {
    KNum(JInt(1)) -> True
    KNum(JFloat(f)) -> f == 1.0
    _ -> False
  }
}

// test262: reviver-context-source-primitive-literal.js
pub fn primitive_literal_source_test() {
  let st = rt_helpers.agent()
  let st =
    list.fold(
      [
        "1", "1.1", "-1", "-1.1", "1.1e1", "1.1e+1", "1.1e-1", "1.1E1", "1.1E+1",
        "1.1E-1", "null", "true", "false", "\"foo\"",
      ],
      st,
      fn(st, text) {
        let #(_, st) = parse_logged(st, text)
        assert calls() == [#("", Some(text))]
        st
      },
    )
  let #(v, _) = parse_logged(st, " 1.1e1 ")
  assert num(v) == 11.0
  assert calls() == [#("", Some("1.1e1"))]
}

// test262: reviver-context-source-array-literal.js
pub fn array_literal_source_test() {
  let st = rt_helpers.agent()
  let #(_, st) = parse_logged(st, "[1.0]")
  assert calls() == [#("0", Some("1.0")), #("", None)]
  let #(_, st) = parse_logged(st, "[1.1]")
  assert calls() == [#("0", Some("1.1")), #("", None)]
  let #(_, st) = parse_logged(st, "[]")
  assert calls() == [#("", None)]
  let #(_, st) =
    parse_logged(st, "[1, \"2\", true, null, {\"x\": 1, \"y\": 1}]")
  assert calls()
    == [
      #("0", Some("1")),
      #("1", Some("\"2\"")),
      #("2", Some("true")),
      #("3", Some("null")),
      #("x", Some("1")),
      #("y", Some("1")),
      #("4", None),
      #("", None),
    ]
  let #(_, _) = parse_logged(st, "[1.0, \"2\", true, null, {\"a\": -0}]")
  assert calls()
    == [
      #("0", Some("1.0")),
      #("1", Some("\"2\"")),
      #("2", Some("true")),
      #("3", Some("null")),
      #("a", Some("-0")),
      #("4", None),
      #("", None),
    ]
}

// test262: reviver-context-source-object-literal.js
pub fn object_literal_source_test() {
  let st = rt_helpers.agent()
  let #(_, st) = parse_logged(st, "{}")
  assert calls() == [#("", None)]
  let #(single, st) = parse_logged(st, "{\"42\":37}")
  assert calls() == [#("42", Some("37")), #("", None)]
  let #(v, st) = rt_obj.t_get_prop(st, single, key("42"))
  assert num(v) == 37.0
  let #(_, st) = parse_logged(st, "{\"x\": 1, \"y\": 2}")
  assert calls() == [#("x", Some("1")), #("y", Some("2")), #("", None)]
  let #(_, st) = parse_logged(st, "{\"x\": [1,2], \"y\": [2,3]}")
  assert calls()
    == [
      #("0", Some("1")),
      #("1", Some("2")),
      #("x", None),
      #("0", Some("2")),
      #("1", Some("3")),
      #("y", None),
      #("", None),
    ]
  let #(_, _) = parse_logged(st, "{\"x\": {\"x\": 1, \"y\": 2}}")
  assert calls()
    == [#("x", Some("1")), #("y", Some("2")), #("x", None), #("", None)]
}

pub fn string_source_is_verbatim_test() {
  let st = rt_helpers.agent()
  let #(v, st) = parse_logged(st, "{\"s\": \"a\\u0041\\n\"}")
  assert calls() == [#("s", Some("\"a\\u0041\\n\"")), #("", None)]
  let #(s, _) = rt_obj.t_get_prop(st, v, key("s"))
  assert classify(s) == KStr("aA\n")
}

pub fn duplicate_and_numeric_keys_test() {
  let st = rt_helpers.agent()
  let #(o, st) = parse_logged(st, "{\"a\":1,\"b\":2,\"a\":3,\"7\":4,\"05\":5}")
  assert calls()
    == [
      #("7", Some("4")),
      #("a", Some("3")),
      #("b", Some("2")),
      #("05", Some("5")),
      #("", None),
    ]
  let #(s, _) = json(st, "stringify", [o])
  assert classify(s) == KStr("{\"7\":4,\"a\":3,\"b\":2,\"05\":5}")
}

fn forward_modifier(
  st: Agent,
  first: String,
  later: String,
  replacement: JsVal,
) -> #(JsVal, Agent) {
  use st, this, args <- reviver(st)
  let assert [k, ..] = args
  let assert KStr(name) = classify(k)
  let st = record_call(st, args)
  let st = case name == first {
    True -> rt_obj.t_set_prop(st, this, key(later), replacement).1
    False -> st
  }
  rt_obj.t_get_prop(st, this, key(name))
}

// test262: reviver-forward-modifies-object.js
pub fn array_forward_modification_drops_source_test() {
  let st = rt_helpers.agent()
  let #(f, st) = forward_modifier(st, "0", "1", mk_number(JInt(42)))
  let #(o, st) = json(st, "parse", [mk_string("[1, 2]"), f])
  assert calls() == [#("0", Some("1")), #("1", None), #("", None)]
  let #(second, st) = rt_obj.t_get_prop(st, o, key("1"))
  assert num(second) == 42.0
  let #(repl, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = rt_obj.t_set_prop(st, repl, key("foo"), mk_string("bar"))
  let #(f, st) = forward_modifier(st, "0", "1", repl)
  let #(_, _) = json(st, "parse", [mk_string("[1, 2]"), f])
  assert calls()
    == [#("0", Some("1")), #("foo", None), #("1", None), #("", None)]
}

pub fn object_forward_modification_drops_source_test() {
  let st = rt_helpers.agent()
  let #(f, st) = forward_modifier(st, "p", "q", mk_string("foo"))
  let #(o, st) = json(st, "parse", [mk_string("{\"p\":1, \"q\":2}"), f])
  assert calls() == [#("p", Some("1")), #("q", None), #("", None)]
  let #(q, st) = rt_obj.t_get_prop(st, o, key("q"))
  assert classify(q) == KStr("foo")
  let #(repl, st) = rt_obj.t_new_array(st, [mk_string("foo")])
  let #(f, st) = forward_modifier(st, "p", "q", repl)
  let #(_, _) = json(st, "parse", [mk_string("{\"p\":1, \"q\":2}"), f])
  assert calls() == [#("p", Some("1")), #("0", None), #("q", None), #("", None)]
}

pub fn chained_forward_modifications_test() {
  let st = rt_helpers.agent()
  let #(f, st) =
    reviver(st, fn(st, this, args) {
      let assert [k, v, _] = args
      let assert KStr(name) = classify(k)
      let st = record_call(st, args)
      let st = case name {
        "a" -> rt_obj.t_set_prop(st, this, key("b"), mk_number(JInt(2))).1
        "b" -> {
          assert num(v) == 2.0
          rt_obj.t_set_prop(st, this, key("c"), mk_number(JInt(3))).1
        }
        "c" -> {
          assert num(v) == 3.0
          st
        }
        _ -> st
      }
      #(v, st)
    })
  let #(_, _) =
    json(st, "parse", [mk_string("{\"a\": 0, \"b\": 1, \"c\": [1, 2]}"), f])
  assert calls() == [#("a", Some("0")), #("b", None), #("c", None), #("", None)]
}

// test262: reviver-call-args-after-forward-modification.js
pub fn appended_element_has_no_source_test() {
  let st = rt_helpers.agent()
  let #(f, st) =
    reviver(st, fn(st, this, args) {
      let assert [k, v, _] = args
      let assert KStr(name) = classify(k)
      let st = record_call(st, args)
      let st = case is_one(v) {
        True -> {
          let #(inner, st) = rt_obj.t_get_prop(st, this, key("1"))
          rt_helpers.call_method(st, inner, "push", [mk_string("barf")]).1
        }
        False -> st
      }
      rt_obj.t_get_prop(st, this, key(name))
    })
  let #(o, st) = json(st, "parse", [mk_string("[1,[]]"), f])
  assert calls() == [#("0", Some("1")), #("0", None), #("1", None), #("", None)]
  let #(s, _) = json(st, "stringify", [o])
  assert classify(s) == KStr("[1,[\"barf\"]]")
}

pub fn added_property_has_no_source_test() {
  let st = rt_helpers.agent()
  let #(f, st) =
    reviver(st, fn(st, this, args) {
      let assert [k, v, _] = args
      let assert KStr(name) = classify(k)
      let st = record_call(st, args)
      let st = case is_one(v) {
        True -> {
          let #(q, st) = rt_obj.t_get_prop(st, this, key("q"))
          rt_obj.t_set_prop(st, q, key("added"), mk_string("barf")).1
        }
        False -> st
      }
      rt_obj.t_get_prop(st, this, key(name))
    })
  let #(o, st) = json(st, "parse", [mk_string("{\"p\":1,\"q\":{}}"), f])
  assert calls()
    == [#("p", Some("1")), #("added", None), #("q", None), #("", None)]
  let #(s, _) = json(st, "stringify", [o])
  assert classify(s) == KStr("{\"p\":1,\"q\":{\"added\":\"barf\"}}")
}

pub fn raw_json_stringifies_verbatim_test() {
  let st = rt_helpers.agent()
  let #(raw, st) = json(st, "rawJSON", [mk_string("1e3")])
  let #(holder, st) = rt_obj.t_new_object_literal(st)
  let #(_, st) = rt_obj.t_set_prop(st, holder, key("n"), raw)
  let #(out, st) = json(st, "stringify", [holder])
  assert classify(out) == KStr("{\"n\":1e3}")
  let #(yes, st) = json(st, "isRawJSON", [raw])
  assert classify(yes) == KBool(True)
  let #(no, st) = json(st, "isRawJSON", [mk_string("1e3")])
  assert classify(no) == KBool(False)
  let #(no, _) = json(st, "isRawJSON", [])
  assert classify(no) == KBool(False)
}
