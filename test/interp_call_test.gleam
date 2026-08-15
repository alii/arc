//// Calls through the new interpreter: flat bytecode activation, native
//// re-entry both ways, bound / call / apply unwrapping, spread and optional
//// calls, tagged templates, `arguments`, rest and default parameters,
//// closures, `this` binding, `new.target`, accessors, classes (fields,
//// private names, super), the call-depth RangeError and `Error.stack` frames.

import arc/compiler
import arc/interp/entry
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/inspect as rt_inspect
import arc/rt/types.{
  type Agent, type JsVal, JInt, KBool, KNum, KStr, classify, mk_number,
  mk_undefined,
}
import gleam/string
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
}

fn run_on(st: Agent, source: String) -> #(rt_call.Completion, Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compiler.compile(body, sb)
    as { "compile failed: " <> source }
  entry.run_script(st, compiler.shared_template(template))
}

fn run(source: String) -> #(rt_call.Completion, Agent) {
  run_on(agent(), source)
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

/// The thrown value of a script that must throw, rendered.
fn thrown(source: String) -> String {
  case run(source) {
    #(ThrowCompletion(e), st) -> rt_inspect.inspect(st, e)
    #(NormalCompletion(v), st) ->
      panic as { source <> " returned " <> rt_inspect.inspect(st, v) }
  }
}

// -- flat calls ------------------------------------------------------------------

pub fn plain_calls_and_recursion_test() {
  assert eval_int("function sq(x) { return x * x } sq(7) + sq(2)") == 53
  assert eval_int(
      "function fib(n) { return n < 2 ? n : fib(n - 1) + fib(n - 2) } fib(20)",
    )
    == 6765
  assert eval_int(
      "function even(n) { return n === 0 ? 1 : odd(n - 1) }\n"
      <> "function odd(n) { return n === 0 ? 0 : even(n - 1) }\n"
      <> "even(200) + odd(7)",
    )
    == 2
  assert eval_string("function f() {} typeof f()") == "undefined"
  assert eval_int(
      "(function (a, b, c) { return a + b + (c === undefined ? 100 : 0) })(1, 2)",
    )
    == 103
}

pub fn closures_capture_environment_test() {
  assert eval_int(
      "function counter() { let n = 0; return function () { n += 1; return n } }\n"
      <> "let c = counter(); c(); c(); c()",
    )
    == 3
  assert eval_int(
      "function adder(a) { return b => c => a + b + c } adder(1)(20)(300)",
    )
    == 321
  assert eval_string(
      "let fs = []; for (let i = 0; i < 3; i++) fs.push(() => i); fs.map(f => f()).join('')",
    )
    == "012"
}

pub fn default_and_rest_parameters_test() {
  assert eval_int("function f(a, b = a * 10) { return a + b } f(2) + f(1, 1)")
    == 24
  assert eval_string(
      "function g(head, ...tail) { return head + ':' + tail.length + ':' + tail.join('') }\n"
      <> "g('h', 1, 2, 3)",
    )
    == "h:3:123"
  assert eval_int("((...xs) => xs.reduce((a, b) => a + b, 0))(1, 2, 3, 4)")
    == 10
  assert eval_int("function h(a, b = () => a) { return b() } h(9)") == 9
}

pub fn arguments_object_test() {
  assert eval_int(
      "function f() { return arguments.length * 100 + arguments[1] } f(5, 6, 7)",
    )
    == 306
  // Sloppy simple parameter list: `callee` is the function itself.
  assert eval_bool("function g() { return arguments.callee === g } g()")
  // Strict: `callee` is the %ThrowTypeError% accessor.
  assert string.contains(
    thrown("function s() { 'use strict'; return arguments.callee } s()"),
    "TypeError",
  )
  // Non-simple parameters also get the unmapped form.
  assert string.contains(
    thrown("function d(a = 1) { return arguments.callee } d()"),
    "TypeError",
  )
  assert eval_string(
      "function j() { return Array.prototype.join.call(arguments, '-') } j(1, 2, 3)",
    )
    == "1-2-3"
  assert eval_string(
      "function k() { return [...arguments].join('') } k('a', 'b')",
    )
    == "ab"
}

/// Coroutine bodies see their own argument list: a generator builds
/// `arguments` / rest before `InitialYield`, an async function on its first
/// (synchronous) turn from the parked frame.
pub fn coroutine_arguments_and_rest_test() {
  assert eval_int(
      "var out = 0;\n"
      <> "(async function (a, ...r) { out = arguments.length * 10 + r.length })(1, 2, 3)\n"
      <> "out",
    )
    == 32
  assert eval_int(
      "var out = 0;\n"
      <> "(async (...r) => { out = r[0] + r[1] })(4, 5)\n"
      <> "out",
    )
    == 9
  assert eval_int(
      "function* g(a, ...r) { yield arguments.length * 10 + r.length }\n"
      <> "g(1, 2, 3).next().value",
    )
    == 32
  assert eval_int(
      "var out = 0;\n"
      <> "(async function* (a, ...r) { out = arguments.length * 10 + r.length })(1, 2).next()\n"
      <> "out",
    )
    == 21
}

pub fn this_binding_test() {
  // Sloppy: undefined this → globalThis; primitives box.
  assert eval_bool("function f() { return this === globalThis } f()")
  assert eval_string("function t() { return typeof this } t.call(5)")
    == "object"
  // Strict: passed through untouched.
  assert eval_string("function s() { 'use strict'; return typeof this } s()")
    == "undefined"
  assert eval_int("function s2() { 'use strict'; return this } s2.call(7)") == 7
  // Method call binds the receiver; arrows keep the lexical this.
  assert eval_int(
      "let o = {v: 4, m() { return this.v }, a() { return (() => this.v)() } }; o.m() + o.a()",
    )
    == 8
  assert eval_string("let o = {m() { return typeof this }}; let m = o.m; m()")
    == "object"
}

pub fn call_apply_bind_and_reflect_test() {
  assert eval_int(
      "function add(a, b) { return this.k + a + b } add.call({k: 1}, 2, 3)",
    )
    == 6
  assert eval_int(
      "function add(a, b) { return this.k + a + b } add.apply({k: 10}, [2, 3])",
    )
    == 15
  assert eval_int("function n() { return arguments.length } n.apply(null)") == 0
  assert eval_int(
      "function add(a, b) { return this.k + a + b } let b = add.bind({k: 100}, 2); b(3)",
    )
    == 105
  assert eval_int(
      "function add(a, b, c) { return a + b + c } add.bind(null, 1).bind(null, 2)(3)",
    )
    == 6
  assert eval_int(
      "Reflect.apply(function (a) { return this.k * a }, {k: 6}, [7])",
    )
    == 42
  assert eval_int("Reflect.apply(Math.max, undefined, [3, 9, 4])") == 9
  // call/apply on a native target.
  assert eval_int("Math.max.apply(null, [1, 8, 2])") == 8
  assert eval_string("Array.prototype.join.call([1, 2], '+')") == "1+2"
  assert string.contains(thrown("Reflect.apply(1, null, [])"), "TypeError")
  assert string.contains(
    thrown("Function.prototype.apply.call({}, null, [])"),
    "TypeError",
  )
  assert string.contains(thrown("(function () {}).apply(null, 3)"), "TypeError")
}

pub fn bound_constructors_test() {
  assert eval_int(
      "function P(a, b) { this.s = a + b } let B = P.bind(null, 1); new B(2).s",
    )
    == 3
  assert eval_bool(
    "function P() { this.nt = new.target } let B = P.bind(null); new B().nt === P",
  )
  assert eval_bool("class C {} let B = C.bind(null); new B() instanceof C")
}

pub fn spread_and_optional_calls_test() {
  assert eval_int(
      "function sum(a, b, c) { return a + b + c } sum(...[1, 2], 3)",
    )
    == 6
  assert eval_int(
      "let o = {k: 2, m(a, b) { return this.k * (a + b) }}; o.m(...[3, 4])",
    )
    == 14
  assert eval_int("function P(a, b) { this.v = a * b } new P(...[6, 7]).v")
    == 42
  assert eval_int("Math.max(...[1, 5], ...[3])") == 5
  assert eval_string("let o = null; String(o?.m())") == "undefined"
  assert eval_string("let o = {}; String(o.m?.())") == "undefined"
  assert eval_int("let o = {m() { return 3 }}; o.m?.() + o?.m()") == 6
  assert eval_string("let f; String(f?.(1))") == "undefined"
}

pub fn tagged_templates_test() {
  assert eval_string(
      "function tag(strs, ...vals) { return strs.raw.join('|') + '#' + vals.join(',') }\n"
      <> "tag`a${1}b${2}c`",
    )
    == "a|b|c#1,2"
  // §13.2.8.4: one template object per site, reused across evaluations.
  assert eval_bool(
    "function id(s) { return s } function f() { return id`x` } f() === f()",
  )
  assert eval_bool(
    "function id(s) { return s } Object.isFrozen(id`q`) && Object.isFrozen(id`q`.raw)",
  )
}

// -- native ⇄ bytecode re-entry --------------------------------------------------------

pub fn mutual_recursion_through_natives_test() {
  // interp → Array.prototype.map (native) → bytecode callback → map again …
  assert eval_string(
      "function deep(xs, d) { return d === 0 ? xs : xs.map(x => deep([x + 1], d - 1)[0]) }\n"
      <> "deep([1, 2, 3], 5).join(',')",
    )
    == "6,7,8"
  assert eval_int(
      "[3, 1, 2].sort((a, b) => [a, b].reduce((x, y) => x - y)).reduce((a, b) => a * 10 + b, 0)",
    )
    == 123
  // A throw inside the nested activation unwinds through the native and is
  // caught by the outer bytecode frame.
  assert eval_string(
      "function boom() { throw new Error('inner') }\n"
      <> "try { [1].forEach(() => [2].forEach(boom)); 'no' } catch (e) { e.message }",
    )
    == "inner"
  // And the other way: a native TypeError raised under a bytecode callback.
  assert eval_string(
      "try { [1].map(function () { return null.x }); 'no' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
}

pub fn accessors_run_bytecode_test() {
  assert eval_int("let o = { get v() { return 21 * 2 } }; o.v") == 42
  assert eval_int(
      "let o = { _x: 0, set x(v) { this._x = v + 1 }, get x() { return this._x } }; o.x = 4; o.x",
    )
    == 5
  // A native reading the property invokes the bytecode getter.
  assert eval_string("JSON.stringify({ get a() { return [1, 2].length } })")
    == "{\"a\":2}"
  assert eval_int(
      "let o = {}; Object.defineProperty(o, 'p', { get() { return this.q * 2 } }); o.q = 8; o.p",
    )
    == 16
  assert eval_string(
      "class T { get k() { return 'K' } static get s() { return 'S' } } new T().k + T.s",
    )
    == "KS"
}

pub fn embedder_calls_bytecode_value_test() {
  let #(_, st) =
    run_on(
      agent(),
      "function twice(f, x) { return f(f(x)) } function inc(n) { return n + 1 }",
    )
  let #(twice, st) = rt_helpers.global(st, "twice")
  let #(inc, st) = rt_helpers.global(st, "inc")
  let #(v, _) =
    rt_call.t_call_checked(st, twice, mk_undefined(), [inc, mk_number(JInt(40))])
  assert classify(v) == KNum(JInt(42))
}

// -- depth -------------------------------------------------------------------------

pub fn deep_recursion_is_a_catchable_range_error_test() {
  assert eval_string(
      "function dive(n) { return 1 + dive(n + 1) }\n"
      <> "try { dive(0); 'no' } catch (e) { (e instanceof RangeError) + ':' + e.message }",
    )
    == "true:Maximum call stack size exceeded"
  // Through natives on every level too.
  assert eval_bool(
    "function viaNative() { return [0].map(viaNative) }\n"
    <> "let caught; try { viaNative() } catch (e) { caught = e } caught instanceof RangeError",
  )
  // The interpreter is usable afterwards: depth was unwound with the frames.
  let #(_, st) =
    run_on(
      agent(),
      "function dive(n) { return 1 + dive(n + 1) } try { dive(0) } catch (e) {}",
    )
  let #(comp, _) =
    run_on(st, "(function f(n) { return n ? f(n - 1) + 1 : 0 })(500)")
  let assert NormalCompletion(v) = comp
  assert classify(v) == KNum(JInt(500))
}

pub fn strict_tail_calls_run_in_constant_depth_test() {
  assert eval_int(
      "'use strict'; function loop(n, acc) { return n === 0 ? acc : loop(n - 1, acc + 1) }\n"
      <> "loop(50000, 0)",
    )
    == 50_000
}

// -- constructors and classes ---------------------------------------------------------

pub fn function_constructors_test() {
  assert eval_int("function P(x) { this.x = x } new P(5).x") == 5
  assert eval_string("function Q() { return {tag: 'override'} } new Q().tag")
    == "override"
  assert eval_int("function R() { this.a = 1; return 7 } new R().a") == 1
  assert eval_bool("function S() { this.nt = new.target } new S().nt === S")
  assert eval_string("function S() { return typeof new.target } S()")
    == "undefined"
  assert eval_bool(
    "function T() {} new T() instanceof T && Object.getPrototypeOf(new T()) === T.prototype",
  )
  assert string.contains(thrown("new (() => 1)()"), "is not a constructor")
  assert string.contains(
    thrown("let o = {m() {}}; new o.m()"),
    "is not a constructor",
  )
  assert eval_int(
      "new (class { constructor(a, b) { this.v = a - b } })(...[9, 4]).v",
    )
    == 5
}

pub fn class_basics_test() {
  let src =
    "class Point {\n"
    <> "  #x; y = 2; static count = 0;\n"
    <> "  constructor(x) { this.#x = x; Point.count++ }\n"
    <> "  get x() { return this.#x }\n"
    <> "  set x(v) { this.#x = v }\n"
    <> "  norm() { return this.#x * this.#x + this.y * this.y }\n"
    <> "  static make(x) { return new Point(x) }\n"
    <> "  #secret() { return 's' }\n"
    <> "  reveal() { return this.#secret() + (#x in this) }\n"
    <> "}\n"
  assert eval_int(
      src <> "let p = new Point(3); p.x = 4; p.norm() + Point.count",
    )
    == 21
  assert eval_string(src <> "Point.make(1).reveal()") == "strue"
  assert eval_string(
      src <> "typeof Point + ':' + Point.name + ':' + Point.length",
    )
    == "function:Point:1"
  assert string.contains(
    thrown(src <> "Point(1)"),
    "cannot be invoked without 'new'",
  )
  assert string.contains(
    thrown(src <> "[1].map(Point)"),
    "cannot be invoked without 'new'",
  )
  assert eval_bool(
    src
    <> "Object.getOwnPropertyDescriptor(Point.prototype, 'norm').enumerable === false",
  )
  assert eval_string(
      "let k = 'dyn'; class C { [k]() { return 1 } static [k + 'S']() {} get [Symbol.iterator]() {} }\n"
      <> "C.prototype.dyn.name + ',' + C.dynS.name",
    )
    == "dyn,dynS"
}

pub fn derived_classes_test() {
  let src =
    "class A { constructor(v) { this.v = v } who() { return 'A' + this.v } static s() { return 'sA' } }\n"
    <> "class B extends A {\n"
    <> "  w = 10;\n"
    <> "  constructor(v) { super(v * 2); this.after = this.v + this.w }\n"
    <> "  who() { return 'B>' + super.who() }\n"
    <> "  static s() { return 'sB>' + super.s() }\n"
    <> "}\n"
  assert eval_string(
      src <> "new B(2).who() + '|' + B.s() + '|' + new B(1).after",
    )
    == "B>A4|sB>sA|12"
  assert eval_bool(
    src <> "new B(1) instanceof A && Object.getPrototypeOf(B) === A",
  )
  // this before super() is a ReferenceError; so is never calling it.
  assert string.contains(
    thrown(
      "class A {} class C extends A { constructor() { this.x = 1 } } new C()",
    ),
    "ReferenceError",
  )
  assert string.contains(
    thrown("class A {} class D extends A { constructor() {} } new D()"),
    "ReferenceError",
  )
  assert string.contains(
    thrown(
      "class A {} class E extends A { constructor() { return 1 } } new E()",
    ),
    "TypeError",
  )
  assert eval_int(
      "class A {} class F extends A { constructor() { return {n: 3} } } new F().n",
    )
    == 3
  // new.target flows through super() so the base allocates from the derived
  // prototype; extending builtins works through the runtime's construct.
  assert eval_bool(
    "class A { constructor() { this.nt = new.target } } class G extends A {} new G().nt === G",
  )
  assert eval_int(
      "class L extends Array { sum() { return this.reduce((a, b) => a + b, 0) } }\n"
      <> "let l = new L(); l.push(4, 5); l.sum() + (l instanceof Array ? 100 : 0) + l.length",
    )
    == 111
  assert eval_string(
      "class M extends null { static k() { return 'ok' } } M.k() + typeof Object.getPrototypeOf(M.prototype)",
    )
    == "okobject"
  assert string.contains(thrown("class N extends 3 {}"), "not a constructor")
  assert eval_string(
      "let base = { greet() { return 'hi ' + this.n } };\n"
      <> "let o = { __proto__: base, n: 'o', greet() { return super.greet() + '!' } }; o.greet()",
    )
    == "hi o!"
}

pub fn getters_setters_and_super_property_writes_test() {
  assert eval_int(
      "class A { set v(x) { this._v = x * 2 } } class B extends A { put() { super.v = 5; return this._v } }\n"
      <> "new B().put()",
    )
    == 10
  assert eval_int(
      "class A {} A.prototype.n = 1; class B extends A { bump() { return super.n += 4 } } new B().bump()",
    )
    == 5
}

// -- Error.stack frames ---------------------------------------------------------------

pub fn error_stack_names_bytecode_frames_test() {
  let s =
    eval_string(
      "function inner() { throw new Error('boom') }\n"
      <> "function outer() { inner() }\n"
      <> "try { outer() } catch (e) { e.stack }",
    )
  assert string.starts_with(s, "Error: boom")
  assert string.contains(s, "at inner (script:1)")
  assert string.contains(s, "at outer (script:2)")
  // Runtime-raised errors see the same frames.
  let t =
    eval_string(
      "function bad() {\n return null.x }\ntry { bad() } catch (e) { e.stack }",
    )
  assert string.starts_with(t, "TypeError")
  assert string.contains(t, "at bad (script:2)")
  // Frames are popped again on return: a later error does not name `bad`.
  let u =
    eval_string(
      "function ok() { return 1 }\nok();\ntry { null.x } catch (e) { e.stack }",
    )
  assert !string.contains(u, "at ok")
}
