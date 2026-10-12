import arc/bytecode/opcode
import arc/compiler
import arc/host_hooks
import arc/internal/tuple_array
import arc/interp/entry
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/call.{NormalCompletion, ThrowCompletion}
import arc/rt/types.{type JsValKind, KStr}
import gleam/option.{Some}
import rt_helpers

fn compile(source: String) -> FuncTemplate {
  let assert Ok(#(body, scopes)) = parser.parse_script(source)
  let assert Ok(template) = compiler.compile_script_lazy(body, scopes)
  template
}

// the script's value, with every plain function compiled by its first call
fn run(source: String) -> Result(JsValKind, JsValKind) {
  let agent = rt_builtins.new_agent(host_hooks.default()) |> entry.link
  let #(completion, agent) = entry.run_script(agent, compile(source))
  let value = case completion {
    NormalCompletion(value) -> value
    ThrowCompletion(value) -> value
  }
  let _ = rt_helpers.end_turn(agent, [value])
  case completion {
    NormalCompletion(value) -> Ok(rt_helpers.classify(value))
    ThrowCompletion(value) -> Error(rt_helpers.classify(value))
  }
}

pub fn a_plain_function_is_left_as_a_stub_test() {
  let script = compile("function add(a, b) { return a + b; } add(1, 2)")
  let assert [add] = tuple_array.to_list(script.functions)
  let assert [opcode.CompileSelf(_), opcode.Return] =
    tuple_array.to_list(add.bytecode)
  assert add.name == Some("add")
  assert add.arity == 2
  assert add.length == 2
}

pub fn calls_recursion_and_captures_test() {
  assert run(
      "(function () {
         var kept = 'k', count = 0;
         function fib(n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }
         function bump(by) { count += by; return kept + count; }
         var self = function me(n) { return n === 0 ? 'done' : me(n - 1); };
         return [fib(10), bump(2), bump(3), self(3), bump.name, fib.length].join(',');
       })()",
    )
    == Ok(KStr("55,k2,k5,done,bump,1"))
}

pub fn every_closure_of_one_function_works_test() {
  assert run(
      "(function () {
         var made = [];
         for (var i = 0; i < 4; i++) made.push((function (n) { return function () { return n * 2; }; })(i));
         return made.map(function (f) { return f() + f(); }).join(',');
       })()",
    )
    == Ok(KStr("0,4,8,12"))
}

pub fn constructors_this_and_arguments_test() {
  assert run(
      "(function () {
         function Point(x, y) { this.x = x; this.y = y; }
         Point.prototype.sum = function () { return this.x + this.y; };
         function sloppy() { return typeof this; }
         function strict() { 'use strict'; return typeof this; }
         function count() { return arguments.length + ':' + arguments[1]; }
         function rest(a, b = 5, ...more) { return a + b + more.length; }
         var p = new Point(3, 4);
         return [p.sum(), p instanceof Point, sloppy(), strict(), count(1, 2, 3), rest(1), rest(1, 2, 3, 4)].join(',');
       })()",
    )
    == Ok(KStr("7,true,object,undefined,3:2,6,5"))
}

pub fn generators_and_nested_lazy_functions_test() {
  assert run(
      "(function () {
         function* upTo(n) { for (var i = 0; i < n; i++) yield (function (v) { return v * v; })(i); }
         function outer(a) { function inner(b) { function innermost(c) { return a + b + c; } return innermost; } return inner; }
         return [Array.from(upTo(4)).join(''), outer(1)(2)(3)].join(',');
       })()",
    )
    == Ok(KStr("0149,6"))
}

pub fn a_throw_from_a_lazy_function_is_caught_test() {
  assert run(
      "(function () {
         function fail(what) { throw new RangeError(what); }
         try { fail('late'); } catch (e) { return e.name + ':' + e.message; }
       })()",
    )
    == Ok(KStr("RangeError:late"))
}

fn is_stub(template: FuncTemplate) -> Bool {
  case tuple_array.to_list(template.bytecode) {
    [opcode.CompileSelf(_), ..] -> True
    _ -> False
  }
}

// a function called where it is written would only be parsed twice
pub fn a_function_called_where_it_is_written_is_compiled_at_once_test() {
  let script =
    compile(
      "(function () { function inner() { return 1; } return inner(); })();
       !function () { return 2; }.call(this);
       var wrapped = (function () { return 3; });
       var later = function () { return 4; };",
    )
  let assert [called, called_with_this, wrapped, later] =
    tuple_array.to_list(script.functions)
  assert !is_stub(called)
  assert !is_stub(called_with_this)
  assert !is_stub(wrapped)
  assert is_stub(later)
  let assert [inner] = tuple_array.to_list(called.functions)
  assert is_stub(inner)
}
