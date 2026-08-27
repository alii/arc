import arc/compiler
import arc/engine.{type JsValueKind, Finite, JsNumber, JsString}
import arc/host_hooks
import arc/interp/entry
import arc/interp/safepoint
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion}
import arc/rt/inspect as rt_inspect
import gleam/string

fn run(source: String) -> Result(JsValueKind, String) {
  case parser.parse_script(source) {
    Error(err) -> Error("parse: " <> parser.parse_error_to_string(err))
    Ok(#(body, sb)) ->
      case compiler.compile(body, sb) {
        Error(ce) -> Error("compile: " <> string.inspect(ce))
        Ok(template) -> {
          let st =
            rt_builtins.new_agent(host_hooks.default_host_hooks())
            |> entry.link
          case entry.run_script(st, template) {
            #(NormalCompletion(v), st) -> {
              let _st = safepoint.end_turn(st, [v])
              Ok(engine.classify(v))
            }
            #(ThrowCompletion(v), st) ->
              Error("threw: " <> rt_inspect.inspect(st, v))
          }
        }
      }
  }
}

fn expect(source: String, want: JsValueKind) -> Nil {
  case run(source) {
    Ok(got) ->
      case got == want {
        True -> Nil
        False ->
          panic as {
            "scope_fusion: "
            <> source
            <> "\n  want "
            <> string.inspect(want)
            <> "\n  got  "
            <> string.inspect(got)
          }
      }
    Error(why) ->
      panic as { "scope_fusion: " <> source <> "\n  failed: " <> why }
  }
}

fn n(f: Float) -> JsValueKind {
  JsNumber(Finite(f))
}

fn parses(source: String) -> Bool {
  case parser.parse(source, parser.Script) {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn rejects(source: String) -> Bool {
  !parses(source)
}

pub fn fusion_c7_var_hoist_then_outer_let_test() {
  assert rejects("{ { var x; } let x; }")
  assert rejects("{ { { var x; } } let x; }")
  assert rejects("function f(){ { { var x; } let x; } }")
  assert rejects("{ let x; { var x; } }")
  assert parses("(function(){ { var x; } })(); let x;")
  assert rejects("{ try {} catch (e) { var x; } let x; }")
}

pub fn fusion_arrow_backtrack_discards_state_test() {
  assert parses("(x); let x = 1;")
  assert parses("(x, y); let x = 1; let y = 2;")
  assert parses("let a = [0]; ([a[0]] = [1]); let z = 2;")
  assert parses("((p => p), 1); let p = 2;")
  expect("let r = (function(){ (x); var x = 5; return x; })(); r", n(5.0))
  expect("let f = (x) => x * 2; let x = 10; f(3) + x", n(16.0))
}

pub fn fusion_block_elision_reparents_children_test() {
  expect(
    "function outer(){ let v = 41; { return (function(){ return v + 1; })(); } } outer()",
    n(42.0),
  )
  expect("function o(){ let v = 7; { { return (() => v)(); } } } o()", n(7.0))
  expect(
    "function o(){ let r = 0;"
      <> " { let a = 1; r += (() => a)(); }"
      <> " { r += (() => 10)(); }"
      <> " { let b = 100; r += (() => b)(); }"
      <> " return r; } o()",
    n(111.0),
  )
  expect("let v = 1; let r; { r = () => v; let v = 2; } r()", n(2.0))
}

pub fn fusion_switch_hoist_order_across_cases_test() {
  expect(
    "function t(){ switch (1) {"
      <> " case 1: return g();"
      <> " case 2: function g(){ return 99; }"
      <> " } } t()",
    n(99.0),
  )
  expect(
    "function t(){ let a = 10, b = 3, e, r;"
      <> " switch (0) {"
      <> "  case 0: e = function(){ return b; };"
      <> "  default: function d(){ return a; } r = d() * e();"
      <> " } return r; } t()",
    n(30.0),
  )
  expect(
    "function t(){ switch (0) {"
      <> " case 0: let k = 5;"
      <> " case 1: return k * 2;"
      <> " } } t()",
    n(10.0),
  )
}

pub fn fusion_class_seven_step_children_order_test() {
  expect(
    "let key = 'name';"
      <> " class Base { tag(){ return 'B'; } }"
      <> " class C extends Base {"
      <> "   f = 1;"
      <> "   constructor(){ super(); this.g = 2; }"
      <> "   [key](){ return super.tag() + ':' + (this.f + this.g); }"
      <> "   static sm(){ return 10; }"
      <> "   static sf = C.sm() + 4;"
      <> " }"
      <> " new C().name() + '/' + C.sf",
    JsString("B:3/14"),
  )
  expect("class K { static v; static { K.v = 6 * 7; } } K.v", n(42.0))
  expect(
    "let pick = 0;"
      <> " class A { id(){ return 'A'; } }"
      <> " class B { id(){ return 'B'; } }"
      <> " class C extends ((() => pick ? B : A)()) {"
      <> "   who(){ return super.id(); }"
      <> " }"
      <> " new C().who()",
    JsString("A"),
  )
}

pub fn fusion_template_substitution_refs_captured_test() {
  expect(
    "function o(){ let x = 'q'; return (function(){ return `[${x}]`; })(); } o()",
    JsString("[q]"),
  )
  expect(
    "function o(){ let x = 1, y = 2;"
      <> " let f = () => `${x}-${y}`;"
      <> " x = 9; return f(); } o()",
    JsString("9-2"),
  )
  expect(
    "function o(){ let n = 3;"
      <> " function tag(s, a){ return s[0] + (a * 2); }"
      <> " return (() => tag`v=${n}`)(); } o()",
    JsString("v=6"),
  )
}

pub fn fusion_nfe_self_name_binding_test() {
  expect("let g = function f(){ return typeof f; }; g()", JsString("function"))
  expect("let g = function f(){ return 1; }; typeof f", JsString("undefined"))
  expect("(function f(n){ return n <= 1 ? 1 : n * f(n - 1); })(5)", n(120.0))
  expect("(function f(){ f = 0; return typeof f; })()", JsString("function"))
  expect(
    "let f = 7; let r = (function f(){ return typeof f; })(); r + ':' + f",
    JsString("function:7"),
  )
}

// §B.3.2 sloppy function-in-block var promotion

pub fn fusion_annexb_sloppy_fn_in_block_test() {
  expect(
    "function t(){ { function f(){ return 3; } } return f(); } t()",
    n(3.0),
  )
  expect(
    "function t(){ let r = typeof f; { function f(){} } return r; } t()",
    JsString("undefined"),
  )
  expect(
    "function t(){ 'use strict'; { function f(){ return 1; } } return typeof f; } t()",
    JsString("undefined"),
  )
  expect(
    "function t(){ let f = 1; { function f(){ return 2; } } return f; } t()",
    n(1.0),
  )
  assert parses("function t(){ if (true) function f(){} return f; }")
  expect(
    "function t(){ if (true) function f(){ return 8; } return f(); } t()",
    n(8.0),
  )
}

pub fn fusion_for_var_no_block_scope_test() {
  expect(
    "function f(){ for (var i = 0; i < 1; i++){} { let x = 11; return x; } } f()",
    n(11.0),
  )
  expect(
    "function f(){ let a = []; for (var i = 0; i < 3; i++) a.push(() => i);"
      <> " return a[0]() + a[1]() + a[2](); } f()",
    n(9.0),
  )
  expect(
    "function f(){ let a = []; for (let i = 0; i < 3; i++) a.push(() => i);"
      <> " return a[0]() + a[1]() + a[2](); } f()",
    n(3.0),
  )
}

// §10.2.11 step 28: separate var env for non-simple params

pub fn fusion_paramsbody_separate_var_env_test() {
  expect(
    "var x = 'outside'; var pp, pb;"
      <> " (function(_ = pp = function(){ return x; }){"
      <> "   var x = 'inside'; pb = function(){ return x; };"
      <> " }());"
      <> " pp() + ' ' + pb()",
    JsString("outside inside"),
  )
  expect("function f(x = 1){ var x; return x; } f()", n(1.0))
  expect("function f(x = 1){ var x = 2; return x; } f()", n(2.0))
  expect(
    "function f(g = 1){ var r = typeof g; function g(){}; return r; } f()",
    JsString("function"),
  )
  expect(
    "var q = 'out'; function f(a = function(){ return q; }){ var q;"
      <> " return a() + ':' + q; } f()",
    JsString("out:undefined"),
  )
  expect("function f(a = 1, ...r){ var r; return r.length; } f(1,2,3)", n(2.0))
  expect(
    "function f(a = 1){ var arguments; return arguments.length; } f(7,8)",
    n(2.0),
  )
  expect(
    "var y = 'out'; var p;"
      <> " ((q = (p = () => y)) => { var y = 'in'; return p(); })()",
    JsString("out"),
  )
  expect(
    "function f(a = 1){ return g(); function g(){ return a; } } f()",
    n(1.0),
  )
  assert rejects("function f(a=1){ let a; }")
  assert rejects("function f(a=1){ const a = 0; }")
  assert rejects("function f(a=1){ class a {} }")
  assert rejects("function f([a]){ let a; }")
  assert rejects("(a=1) => { let a; };")
  assert parses("function f(a=1){ var a; }")
  assert parses("function f(a=1){ { let a; } }")
  assert parses("function f(a=1){ function a(){} }")
  assert parses("function f(a=1){ let arguments; }")
  assert parses("(function f(a=1){ let f; })")
}

pub fn fusion_catch_param_body_env_split_test() {
  expect(
    "var pp, pb; let x = 'outside';"
      <> " try { throw []; } catch ([_ = pp = function(){ return x; }]) {"
      <> "   pb = function(){ return x; }; let x = 'inside';"
      <> " }"
      <> " pp() + ' ' + pb()",
    JsString("outside inside"),
  )
  expect(
    "var o = [];"
      <> " try { throw 1; } catch (e) { let x = e;"
      <> "   { let v = 'a'; o.push(function(){ return v; }); }"
      <> "   { let v = 'b'; o.push(function(){ return v; }); } }"
      <> " o[0]() + o[1]()",
    JsString("ab"),
  )
  expect(
    "var o = []; try { throw 1; } catch { o.push(1); }"
      <> " { let z = 'sib'; o.push(function(){ return z; }); }"
      <> " '' + o[0] + o[1]()",
    JsString("1sib"),
  )
  assert rejects("try {} catch (e) { let e; }")
  assert rejects("try {} catch (e) { class e {} }")
  assert rejects("try {} catch (e) { function e(){} }")
  assert parses("try {} catch (e) { var e; }")
  assert parses("try {} catch (e) { { let e; } }")
}
