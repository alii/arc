//// Dynamic code through the new interpreter: the Function-family
//// constructors and indirect eval reaching `JsOps.eval_hook`, direct eval
//// seeing and extending its calling frame, `with` object environments, and
//// the configurable/non-configurable split of global var declarations.

import arc/compiler
import arc/interp/entry
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/inspect as rt_inspect
import arc/rt/realm as rt_realm
import arc/rt/types.{type Agent, type JsVal, JInt, KBool, KNum, KStr, classify}
import rt_helpers

fn agent() -> Agent {
  let st = rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
  let #(_, st) = rt_realm.install_262(st, st.realm)
  st
}

fn run(source: String) -> #(rt_call.Completion, Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compiler.compile(body, sb)
    as { "compile failed: " <> source }
  entry.run_script(agent(), template)
}

fn eval(source: String) -> #(JsVal, Agent) {
  case run(source) {
    #(NormalCompletion(v), st) -> #(v, st)
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

fn eval_int(source: String) -> Int {
  let #(v, st) = eval(source)
  case classify(v) {
    KNum(JInt(n)) -> n
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

fn eval_string(source: String) -> String {
  let #(v, st) = eval(source)
  case classify(v) {
    KStr(s) -> s
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

fn eval_bool(source: String) -> Bool {
  let #(v, st) = eval(source)
  case classify(v) {
    KBool(b) -> b
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

fn thrown_name(source: String) -> String {
  case run(source) {
    #(ThrowCompletion(e), st) -> {
      let #(name, _) = rt_helpers.get(st, e, "name")
      case classify(name) {
        KStr(s) -> s
        _ -> rt_inspect.inspect(st, e)
      }
    }
    #(NormalCompletion(v), st) ->
      panic as { source <> " returned " <> rt_inspect.inspect(st, v) }
  }
}

// -- Function() family ---------------------------------------------------------

pub fn function_constructor_test() {
  assert eval_int("Function('a', 'b', 'return a + b')(2, 3)") == 5
  assert eval_int("new Function('return 7')()") == 7
  assert eval_int("Function()() === undefined ? 1 : 0") == 1
}

pub fn function_constructor_name_test() {
  assert eval_string("Function('return 1').name") == "anonymous"
  // §20.2.1.1.1: no self-name binding, unlike a named function expression.
  assert eval_string("Function('return typeof anonymous')()") == "undefined"
  assert eval_int("Function('a', 'b', '').length") == 2
}

pub fn function_constructor_syntax_error_test() {
  assert thrown_name("Function('{')") == "SyntaxError"
  // A trailing line comment in the last parameter must not eat the ")".
  assert eval_int("Function('a // note', 'return a')(4)") == 4
}

pub fn function_constructor_global_scope_test() {
  // The body closes over the global environment, not the caller's.
  assert eval_string(
      "var x = 'global'; (function () { var x = 'local'; return Function('return x')(); })()",
    )
    == "global"
}

pub fn generator_function_constructor_test() {
  assert eval_int(
      "var GF = Object.getPrototypeOf(function* () {}).constructor; GF('yield 9')().next().value",
    )
    == 9
}

pub fn async_function_constructor_test() {
  assert eval_bool(
    "var AF = Object.getPrototypeOf(async function () {}).constructor; AF('return 1')() instanceof Promise",
  )
}

// -- indirect eval ---------------------------------------------------------------

pub fn indirect_eval_test() {
  assert eval_int("(0, eval)('1 + 2')") == 3
  assert eval_int("var e = eval; e('40 + 2')") == 42
  // Non-string argument comes back unchanged.
  assert eval_int("(0, eval)(5)") == 5
  assert eval_int("[\"6\"].map(eval)[0]") == 6
}

pub fn indirect_eval_is_global_test() {
  assert eval_string(
      "var w = 'g'; (function () { var w = 'l'; return (0, eval)('w'); })()",
    )
    == "g"
  // Eval-introduced globals are configurable; script vars are not.
  assert eval_bool("(0, eval)('var ev = 1'); delete ev")
  assert !eval_bool("var sv = 1; delete sv")
}

pub fn indirect_eval_syntax_error_test() {
  assert thrown_name("(0, eval)('}')") == "SyntaxError"
  assert eval_bool(
    "try { (0, eval)('}'); false } catch (e) { e instanceof SyntaxError }",
  )
}

pub fn other_realm_eval_is_indirect_test() {
  // §13.3.6.1 step 6.a: only the CURRENT realm's %eval% makes `eval(...)`
  // a direct eval. Another realm's eval called through the name `eval` is
  // an ordinary call: indirect, global scope, and in that eval's realm.
  assert thrown_name(
      "(function () { var eval = $262.createRealm().global.eval; var y = 1; return eval('y') })()",
    )
    == "ReferenceError"
  assert eval_string(
      "var x = 'outer'; var r = $262.createRealm(); "
      <> "(function () { var eval = r.global.eval; eval('var x = \"inner\"') })(); "
      <> "x + '/' + r.global.x",
    )
    == "outer/inner"
  assert eval_bool(
    "var r = $262.createRealm(); r.global.eval('[]') instanceof r.global.Array",
  )
  // The current realm's own eval, however it was fetched, is still direct.
  assert eval_int(
      "(function () { var eval = globalThis.eval; var y = 41; return eval('y + 1') })()",
    )
    == 42
}

// -- direct eval -----------------------------------------------------------------

pub fn direct_eval_reads_locals_test() {
  assert eval_int("(function () { var x = 3; return eval('x + 1'); })()") == 4
  assert eval_int("(function (p) { return eval('p * 2'); })(21)") == 42
}

pub fn direct_eval_writes_locals_test() {
  assert eval_int("(function () { var x = 1; eval('x = 10'); return x; })()")
    == 10
}

pub fn direct_eval_var_injection_test() {
  // Sloppy: the eval's var lands in the calling function's scope.
  assert eval_int("(function () { eval('var y = 5'); return y; })()") == 5
  assert eval_int(
      "(function () { eval('var y = 5'); eval('y = y + 1'); return y; })()",
    )
    == 6
  assert eval_string(
      "(function () { eval('var y = 5'); return typeof y; })(); typeof y",
    )
    == "undefined"
}

pub fn direct_eval_strict_isolation_test() {
  assert eval_string(
      "(function () { 'use strict'; eval('var z = 1'); return typeof z; })()",
    )
    == "undefined"
  assert eval_string(
      "(function () { eval('\"use strict\"; var z = 1'); return typeof z; })()",
    )
    == "undefined"
}

pub fn direct_eval_this_test() {
  assert eval_bool(
    "var o = {}; (function () { return eval('this'); }).call(o) === o",
  )
  assert eval_int("var t = 8; eval('this.t')") == 8
}

pub fn direct_eval_at_top_level_test() {
  assert eval_int("var g = 2; eval('var h = g + 1'); h") == 3
  assert eval_int("eval('1; 2; 3')") == 3
}

pub fn direct_eval_throw_unwinds_test() {
  assert eval_string(
      "(function () { try { eval('throw new RangeError(1)') } catch (e) { return e.name } })()",
    )
    == "RangeError"
  assert thrown_name("(function () { eval('null.x') })()") == "TypeError"
}

pub fn direct_eval_escaping_throw_restores_depth_test() {
  // A throw raised in a bytecode function called from eval code, escaping
  // the eval, must leave the frame and depth bookkeeping where it was.
  let depth =
    "(function () { try { null.x } catch (e) { return e.stack.split('\\n').length } })()"
  let leak =
    "try { (function () { eval('(function a () { (function b () { null.x })() })()') })() } catch (e) {} "
  assert eval_int(leak <> leak <> leak <> depth) == eval_int(depth)
  let #(_, st) = eval(leak <> leak)
  assert st.call_depth == 0
  assert st.frames == []
  // Repeated leaks used to exhaust the call stack for unrelated code.
  assert eval_int(
      "for (var i = 0; i < 300; i++) { "
      <> leak
      <> "} (function rec (n) { return n === 0 ? 0 : 1 + rec(n - 1) })(9500)",
    )
    == 9500
}

pub fn shadowed_eval_is_a_plain_call_test() {
  assert eval_int(
      "(function (eval) { return eval('nope'); })(function (s) { return s.length; })",
    )
    == 4
}

// -- with --------------------------------------------------------------------------

pub fn with_read_write_test() {
  assert eval_int("var o = { a: 1 }; with (o) { a = a + 1 } o.a") == 2
  // An unbound name falls through to the enclosing scope.
  assert eval_int("var o = { a: 1 }, b = 10; with (o) { b = a + b } b") == 11
}

pub fn with_call_receiver_test() {
  assert eval_bool(
    "var o = { f: function () { return this } }; var r; with (o) { r = f() } r === o",
  )
}

pub fn with_unscopables_test() {
  assert eval_int(
      "var a = 7; var o = { a: 1 }; o[Symbol.unscopables] = { a: true }; var r; with (o) { r = a } r",
    )
    == 7
}

pub fn with_primitive_head_test() {
  assert eval_int("var r; with ('abc') { r = length } r") == 3
  assert thrown_name("with (null) {}") == "TypeError"
}

pub fn with_typeof_and_delete_test() {
  assert eval_string("var o = { a: 1 }; var r; with (o) { r = typeof a } r")
    == "number"
  assert eval_bool("var o = { a: 1 }; with (o) { delete a } !('a' in o)")
}
