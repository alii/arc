import arc/bytecode/lazy.{type LazyFunction}
import arc/bytecode/opcode
import arc/compiler
import arc/host_hooks
import arc/internal/tuple_array
import arc/interp/entry
import arc/parser
import arc/parser/ast
import arc/rt/builtins as rt_builtins
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/call.{NormalCompletion, ThrowCompletion}
import arc/rt/types.{type JsValKind, KStr}
import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/set
import gleam/string
import rt_helpers

fn compile(source: String) -> FuncTemplate {
  compile_except(source, [])
}

// lazily, but with the functions that start with these words compiled now
fn compile_except(source: String, compile_now: List(String)) -> FuncTemplate {
  let starts =
    list.map(compile_now, fn(words) {
      let assert Ok(#(before, _)) = string.split_once(source, words)
        as "the source has no such words"
      string.byte_size(before)
    })
  let assert Ok(#(body, scopes)) =
    parser.parse_script_lazily(source, compile_now: set.from_list(starts))
  let assert Ok(template) = compiler.compile_script(body, scopes)
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

// the functions a template makes lazy closures of, as their records have them
fn lazy_functions(template: FuncTemplate) -> List(LazyFunction) {
  use op <- list.filter_map(tuple_array.to_list(template.bytecode))
  case op {
    opcode.MakeLazyClosure(record:, ..) -> lazy.unpack(record)
    _ -> Error(Nil)
  }
}

fn is_stub(template: FuncTemplate) -> Bool {
  case tuple_array.to_list(template.bytecode) {
    [opcode.CompileSelf, ..] -> True
    _ -> False
  }
}

pub fn a_plain_function_is_left_for_its_first_call_test() {
  let script =
    compile(
      "function add(a, b) { /* long enough to be worth leaving for later */ return a + b; }
       function also(a, b, c) { /* long enough to be worth leaving for later */ return a; }
       function tiny(a) { return a; }
       add(1, 2)",
    )
  let assert [add, also] = lazy_functions(script)
  assert add.name == Some("add")
  assert add.arity == 2
  assert also.name == Some("also")
  assert also.length == 3
  // the two share one stub; what a short function compiles to is smaller
  // than a record, so tiny is compiled at once
  let assert [stub, tiny] = tuple_array.to_list(script.functions)
  assert is_stub(stub)
  assert !is_stub(tiny)
}

pub fn a_function_named_to_compile_now_is_not_left_for_later_test() {
  let source =
    "function add(a, b) { /* long enough to be worth leaving for later */ return a + b; }
     function also(a, b, c) { /* long enough to be worth leaving for later */ return a; }
     var arrow = (a) => { /* long enough to be worth leaving for later */ return a; };
     add(1, 2)"
  let script = compile_except(source, ["function also", "(a) =>"])
  let assert [add] = lazy_functions(script)
  assert add.name == Some("add")
  let assert [stub, also, arrow] = tuple_array.to_list(script.functions)
  assert is_stub(stub)
  assert !is_stub(also)
  assert !is_stub(arrow)
  assert also.name == Some("also")
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

// a function called where it is written would only be parsed twice
pub fn a_function_called_where_it_is_written_is_compiled_at_once_test() {
  let script =
    compile(
      "(function () { function inner() { /* long enough to be left for its first call */ return 1; } return inner(); })();
       !function () { /* long enough to be left for its first call, if it were */ return 2; }.call(this);
       var wrapped = (function () { /* long enough to be left for its first call, if it were */ return 3; });
       var later = function () { /* long enough to be left for its first call */ return 4; };
       var arrow = () => { /* long enough to be left for its first call too */ return 5; };
       var called_arrow = (() => { /* long enough to be left for its first call, if it were */ return 6; })();",
    )
  assert list.map(lazy_functions(script), fn(function) { function.name })
    == [Some("later"), Some("arrow")]
  // three compiled at once, a stub for the function, three more, one for the
  // arrow
  let assert [called, called_with_this, wrapped, later, arrow, called_arrow] =
    tuple_array.to_list(script.functions)
  assert !is_stub(called)
  assert !is_stub(called_with_this)
  assert !is_stub(wrapped)
  assert is_stub(later)
  assert is_stub(arrow)
  assert !is_stub(called_arrow)
  let assert [inner] = lazy_functions(called)
  assert inner.name == Some("inner")
}

pub fn arrows_take_this_and_arguments_from_around_them_test() {
  assert run(
      "(function () {
         function Counter(start) {
           this.n = start;
           this.bump = (by = 1) => { this.n += by; return this.n; };
           this.args = () => arguments.length;
           this.target = () => new.target === Counter;
         }
         var c = new Counter(5, 'x');
         var twice = [1, 2, 3].map(v => v * 2).join('');
         var nested = (a) => (b) => (c2) => a + b + c2;
         var later = async (v) => (await v) + 1;
         var seen = [];
         later(Promise.resolve(41)).then(v => seen.push(v));
         return [c.bump(), c.bump(3), c.args(), c.target(), twice, nested(1)(2)(3)].join(',');
       })()",
    )
    == Ok(KStr("6,9,2,true,246,6"))
}

pub fn an_arrow_in_a_method_reaches_super_test() {
  assert run(
      "(function () {
         var base = { greet() { return 'base'; } };
         var derived = { __proto__: base, greet() { var up = () => super.greet(); return up() + '+derived'; } };
         return derived.greet();
       })()",
    )
    == Ok(KStr("base+derived"))
}

pub fn a_waiting_function_s_insides_are_forgotten_at_once_test() {
  let source =
    "var outer = 1;
     function waits(a) {
       /* long enough to be worth leaving for later */
       var inner = a;
       { let block = inner; if (block) { let deeper = block; outer = deeper; } }
       return function nested() { return inner + outer; };
     }
     function tiny(a) { return a; }"
  let assert Ok(#(body, scopes)) =
    parser.parse_script_lazily(source, compile_now: set.new())
  // its body is gone from the tree
  let assert [_, ast.StmtWithLine(statement: waits, ..), ..] = body
  let assert ast.FunctionDeclaration(body: [], ..) = waits
  // and so is every scope inside it: the script, the two functions, no more
  assert dict.size(scopes.scopes) == 3
  // what is kept is what it does to the names around it; tiny's read of its
  // own a is the other read, newest first
  assert list.map(scopes.raw_refs, fn(ref) { ref.name }) == ["a", "outer"]
  // its write of outer, filed on the function itself, after the script's own
  assert list.map(scopes.assign_refs, fn(ref) { #(ref.name, ref.scope) })
    == [#("outer", 1), #("outer", 0)]
}

pub fn what_a_waiting_function_does_outside_itself_still_counts_test() {
  assert run(
      "(function () {
         var count = 0, kept = 1, guarded = 0;
         function bump() { /* long enough to be worth leaving for later */ count++; return count; }
         function read() { /* long enough to be worth leaving for later */ return kept; }
         function risky() {
           /* long enough to be worth leaving for later */
           try { guarded = 1; throw 0; } catch (e) { guarded = 2; }
           return guarded;
         }
         var arrow = () => { /* long enough to be worth leaving for later */ return this.tag + arguments.length; };
         var deep = function () {
           /* long enough to be worth leaving for later */
           return [1].map(() => { /* long enough to be worth leaving for later */ return kept + count; })[0];
         };
         bump(); bump();
         return [count, read(), risky(), guarded, arrow.call({ tag: 'no' }), deep()].join(',');
       }).call({ tag: 'yes' }, 1, 2)",
    )
    == Ok(KStr("2,1,2,2,yes2,3"))
}
