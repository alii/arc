//// Milestone programs run through the new interpreter's entry points:
//// arithmetic, locals, globals, control flow, strings, literals, property
//// access, template literals, console output through the host print hook,
//// and native → bytecode re-entry through the linked `JsOps`.

import arc/compiler
import arc/host_hooks.{type ConsoleLevel, HostHooks}
import arc/interp/entry
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/types.{
  type Agent, type JsVal, Agent, JFloat, JInt, JsStore, KHandle, KNum, KStr,
  classify,
}
import gleam/int
import gleam/list
import gleam/string
import rt_helpers

fn agent() -> Agent {
  let hooks =
    HostHooks(..rt_helpers.quiet_hooks(), print: fn(level, line) {
      rt_helpers.record(#(level, line))
    })
  rt_builtins.new_agent(hooks) |> entry.link
}

fn printed() -> List(#(ConsoleLevel, String)) {
  rt_helpers.recorded()
}

/// Parse, compile and run `source` as a script on a fresh linked agent.
fn run(source: String) -> #(rt_call.Completion, Agent) {
  run_on(agent(), source)
}

fn run_on(st: Agent, source: String) -> #(rt_call.Completion, Agent) {
  run_with(st, source, compiler.compile)
}

/// REPL-style: top-level `let`/`const` become lexical GLOBALS, so they are
/// visible to later scripts run on the same agent.
fn repl_on(st: Agent, source: String) -> #(rt_call.Completion, Agent) {
  run_with(st, source, compiler.compile_repl)
}

fn run_with(
  st: Agent,
  source: String,
  compile,
) -> #(rt_call.Completion, Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compile(body, sb)
    as { "compile failed: " <> source }
  entry.run_script(st, template)
}

/// Run and return the normal completion value; a throw fails the test with
/// the thrown value rendered.
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

fn eval_float(source: String) -> Float {
  let #(v, st) = eval(source)
  case classify(v) {
    KNum(JInt(n)) -> int.to_float(n)
    KNum(JFloat(f)) -> f
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

pub fn arithmetic_test() {
  assert eval_int("1 + 2 * 3") == 7
  assert eval_int("(10 - 4) / 2") == 3
  assert eval_int("7 % 4") == 3
  assert eval_float("2 ** 10") == 1024.0
  assert eval_float("1.5 * 3") == 4.5
  assert eval_float("-5 + +'8'") == 3.0
  assert eval_string("String(0.1 + 0.2)") == "0.30000000000000004"
  assert eval_string("String(1 / 0)") == "Infinity"
}

pub fn locals_and_loops_test() {
  assert eval_int("let s = 0; for (let i = 0; i < 10; i++) { s += i } s") == 45
  assert eval_int("let n = 5, f = 1; while (n > 1) { f *= n; n-- } f") == 120
  assert eval_int("let k = 0; do { k += 3 } while (k < 10); k") == 12
  assert eval_int(
      "let t = 0; for (const x of [1, 2, 3, 4]) { if (x % 2) continue; t += x } t",
    )
    == 6
}

pub fn conditionals_test() {
  assert eval_int("let x = 3; if (x > 2) { x = 100 } else { x = 0 } x") == 100
  assert eval_int("let y = 1; if (y > 2) { y = 100 } else { y = 0 } y") == 0
  assert eval_string("typeof undefined === 'undefined' ? 'yes' : 'no'") == "yes"
  assert eval_int("null ?? 7") == 7
  assert eval_int("0 || 9") == 9
  assert eval_int("4 && 5") == 5
  assert eval_string(
      "let r; switch (2) { case 1: r = 'one'; break; case 2: r = 'two'; break; default: r = 'many' } r",
    )
    == "two"
}

pub fn globals_test() {
  assert eval_int("var g = 5; g * 2") == 10
  assert eval_int("var g = 5; globalThis.g + 1") == 6
  assert eval_int("h = 41; h + 1") == 42
  assert eval_string("let l = 'lex'; typeof globalThis.l") == "undefined"
  assert eval_string("const c = 'C'; c + c") == "CC"
  // A global read of an undeclared name is a ReferenceError.
  let #(comp, st) = run("nope + 1")
  let assert ThrowCompletion(e) = comp
  assert string.contains(rt_inspect.inspect(st, e), "ReferenceError")
}

pub fn globals_persist_across_scripts_test() {
  // Object-record bindings (var, function, implicit) live on globalThis.
  let #(_, st) =
    run_on(agent(), "var counter = 1; function bump() { return ++counter }")
  let #(comp, st) = run_on(st, "bump(); bump(); counter")
  let assert NormalCompletion(v) = comp
  assert classify(v) == KNum(JInt(3))
  // Declarative-record bindings (let/const) live on the realm.
  let #(_, st) = repl_on(st, "let step = 10; const base = counter")
  let #(comp, _) = repl_on(st, "step = step + base; step")
  let assert NormalCompletion(v) = comp
  assert classify(v) == KNum(JInt(13))
}

pub fn strings_and_templates_test() {
  assert eval_string("'con' + 'cat'") == "concat"
  assert eval_string("let n = 'world'; `hello ${n}!`") == "hello world!"
  assert eval_string("`${1 + 1} and ${'x'.toUpperCase()}`") == "2 and X"
  assert eval_int("'abc'.length") == 3
  assert eval_string("'abc'[1]") == "b"
  assert eval_string("`a${`b${'c'}`}`") == "abc"
}

pub fn object_and_array_literals_test() {
  assert eval_int("let o = {a: 1, b: [10, 20]}; o.a + o.b[1] + o.b.length")
    == 23
  assert eval_int("let a = [1, 2, 3]; a[1] = 7; a[0] + a[1] + a[2]") == 11
  assert eval_string("let o = {x: 'y'}; o.z = 'w'; o.x + o.z + o['x']") == "ywy"
  assert eval_int("let k = 'p'; let o = {[k]: 3, q: 4}; o.p + o.q") == 7
  assert eval_string("let o = {a: {b: {c: 'deep'}}}; o.a.b.c") == "deep"
  assert eval_string("typeof {}.missing") == "undefined"
  assert eval_int("[5, 6, 7].length") == 3
  assert eval_string("let a = [1, , 3]; String(a)") == "1,,3"
}

pub fn console_print_test() {
  let _ = printed()
  let #(comp, _) = run("console.log('hi', 1 + 1); console.error(`bad ${3}`)")
  let assert NormalCompletion(_) = comp
  let lines = printed()
  assert list.map(lines, fn(l) { l.1 }) == ["hi 2", "bad 3"]
  assert list.map(lines, fn(l) { l.0 })
    == [host_hooks.LogLevel, host_hooks.ErrorLevel]
}

pub fn thrown_value_is_a_completion_test() {
  let #(comp, st) = run("throw {code: 42}")
  let assert ThrowCompletion(e) = comp
  let assert KHandle(_) = classify(e)
  let #(code, _) = rt_helpers.get(st, e, "code")
  assert classify(code) == KNum(JInt(42))
}

pub fn native_calls_back_into_bytecode_test() {
  // Array.prototype.map is a runtime native: each callback invocation goes
  // rt/call → JsOps.call_bytecode → entry.run_bytecode.
  assert eval_string("[1, 2, 3].map(x => x * 2).join(',')") == "2,4,6"
  assert eval_int(
      "function add(a, b) { return a + b } [1, 2, 3, 4].reduce(add, 0)",
    )
    == 10
}

pub fn construct_through_the_runtime_test() {
  // Reflect.construct is a native: it reaches the bytecode constructor via
  // JsOps.construct_bytecode.
  assert eval_int("function P(x) { this.x = x } Reflect.construct(P, [9]).x")
    == 9
  assert eval_string(
      "function Q() { return {tag: 'override'} } Reflect.construct(Q, []).tag",
    )
    == "override"
}

pub fn indirect_eval_and_function_constructor_test() {
  assert eval_int("(0, eval)('var ev = 20; ev + 1')") == 21
  assert eval_int("var ev2 = 2; (0, eval)('ev2 * 21')") == 42
  assert eval_int("Function('a', 'b', 'return a * b')(6, 7)") == 42
  assert eval_string("Function('return 1').name") == "anonymous"
  let #(comp, st) = run("(0, eval)('let (')")
  let assert ThrowCompletion(e) = comp
  assert string.contains(rt_inspect.inspect(st, e), "SyntaxError")
}

pub fn run_bytecode_from_gleam_test() {
  // An embedder holding a bytecode function value calls it through the
  // runtime's one call entry, which lands in run_bytecode.
  let #(_, st) = run_on(agent(), "function triple(n) { return n * 3 }")
  let #(f, st) = rt_helpers.global(st, "triple")
  let #(v, _) =
    rt_call.t_call_checked(st, f, types.mk_undefined(), [
      types.mk_number(JInt(14)),
    ])
  assert classify(v) == KNum(JInt(42))
  let #(g, st) = rt_helpers.global(st, "globalThis")
  let #(t, _) = rt_helpers.get(st, g, "triple")
  assert classify(t) == classify(f)
}

/// After a script's synchronous part, run the microtask checkpoint the
/// engine epilogue would, then read `expr`.
fn drained(source: String, expr: String) -> String {
  let #(comp, st) = run_on(agent(), source)
  let assert NormalCompletion(_) = comp
  let st = rt_async.drain(st)
  let #(comp, st) = run_on(st, expr)
  case comp {
    NormalCompletion(v) ->
      case classify(v) {
        KStr(s) -> s
        _ -> panic as { expr <> " gave " <> rt_inspect.inspect(st, v) }
      }
    ThrowCompletion(e) ->
      panic as { expr <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

pub fn generators_resume_parked_frames_test() {
  // The runtime's generator driver resumes the parked body through
  // JsOps.resume_frame: next / return-through-finally / throw-into-catch.
  assert eval_string(
      "function* g() { yield 1; yield 2; return 3 } const it = g(); [it.next().value, it.next().value, it.next().value, it.next().done].join()",
    )
    == "1,2,3,true"
  assert eval_string(
      "function* g() { try { yield 1; yield 2 } finally { globalThis.fin = 'ran' } } const it = g(); it.next(); [it.return(9).value, globalThis.fin, it.next().done].join()",
    )
    == "9,ran,true"
  assert eval_string(
      "function* g() { try { yield 1 } catch (e) { yield 'caught ' + e } } const it = g(); it.next(); it.throw('x').value",
    )
    == "caught x"
  assert eval_string(
      "function* g() { for (const x of [1, 2, 3]) yield x * 2 } [...g()].join()",
    )
    == "2,4,6"
  assert eval_string(
      "function* inner() { yield 'a'; yield 'b' } function* outer() { yield* inner(); yield 'c' } [...outer()].join()",
    )
    == "a,b,c"
  // A generator called from a native goes through run_bytecode's coroutine
  // start rather than the in-loop one.
  assert eval_string(
      "function* g(n) { yield n; yield n + 1 } [10, 20].flatMap(n => [...Reflect.apply(g, null, [n])]).join()",
    )
    == "10,11,20,21"
}

pub fn async_functions_resume_parked_frames_test() {
  assert drained(
      "var log = []; async function f() { log.push(1); await null; log.push(3); return 'done' } f().then(v => log.push(v)); log.push(2)",
      "log.join()",
    )
    == "1,2,3,done"
  assert drained(
      "var out; async function f() { await 0; throw new Error('nope') } f().catch(e => { out = e.message })",
      "out",
    )
    == "nope"
  assert drained(
      "var out; async function f() { try { await Promise.reject('r') } catch (e) { return 'caught ' + e } } f().then(v => { out = v })",
      "out",
    )
    == "caught r"
  assert drained(
      "var acc = []; async function* ag() { yield 1; await null; yield 2 } (async () => { for await (const v of ag()) acc.push(v) })()",
      "acc.join()",
    )
    == "1,2"
}

pub fn async_bodies_see_their_arguments_test() {
  // An async function parks before its prologue runs, so `arguments` and a
  // rest parameter are built from the frame's saved argument list on the
  // first turn: in-loop, as an arrow, and entered from a native.
  assert drained(
      "var out = []; async function f(a, ...rest) { return [arguments.length, rest.length, rest.join('')].join('/') } f(1, 2, 3).then(v => out.push(v)); (async function () { return arguments[0] })('x').then(v => out.push(v)); (async (...r) => r.length)(1, 2, 3, 4).then(v => out.push(v)); [0].map(async function (a, ...r) { return arguments.length + ':' + r.length })[0].then(v => out.push(v))",
      "out.join()",
    )
    == "3/2/23,x,4,3:2"
}

/// One run of `source` on an agent whose collector trips after a few dozen
/// allocations, so a safepoint inside the script really collects.
fn eval_small_heap(source: String) -> String {
  let st = rt_gc.t_collect(agent(), [])
  let st = Agent(..st, store: JsStore(..st.store, gc_threshold: 64))
  case run_on(st, source) {
    #(NormalCompletion(v), st) ->
      case classify(v) {
        KStr(s) -> s
        _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
      }
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

pub fn nested_starts_keep_the_caller_rooted_test() {
  // A generator prologue, an async function's first turn and a proxied
  // constructor body all run as nested activations straight out of the
  // top-level frame. Each allocates past the threshold and returns through
  // its own root; the caller's locals (and the async result promise) must
  // survive.
  let prelude =
    "let keep = {tag: 'kept'}; let onstack = {o: 1}; function churn() { for (let i = 0; i < 300; i++) { let x = {i, a: [i]} } } "
  assert eval_small_heap(
      prelude
      <> "function* g(a = churn()) { yield a } g(); [onstack.o, keep.tag].join()",
    )
    == "1,kept"
  assert eval_small_heap(
      prelude
      <> "async function f() { churn(); return 1 } f(); [onstack.o, keep.tag].join()",
    )
    == "1,kept"
  assert eval_small_heap(
      prelude
      <> "async function* ag(a = churn()) { yield a } ag(); [onstack.o, keep.tag].join()",
    )
    == "1,kept"
  assert eval_small_heap(
      prelude
      <> "function F() { churn(); this.v = 1 } const P = new Proxy(F, {}); new P(); [onstack.o, keep.tag].join()",
    )
    == "1,kept"
}

pub fn nested_starts_are_depth_bounded_test() {
  // Recursion through a coroutine start or a proxied [[Construct]] counts
  // against the call-depth limit like any other call.
  assert eval_string(
      "var out; function* g(x = g()) { yield 1 } try { g(); out = 'unbounded' } catch (e) { out = e.constructor.name } out",
    )
    == "RangeError"
  assert eval_string(
      "var out; function F() { new P() } var P = new Proxy(F, {}); try { new P(); out = 'unbounded' } catch (e) { out = e.constructor.name } out",
    )
    == "RangeError"
}

pub fn coroutine_roots_push_one_stack_frame_test() {
  // A generator / async body entered from a native names itself once in
  // `Error.stack`, the same as when it is called in-loop.
  let count = fn(stack: String, name: String) {
    list.length(string.split(stack, "at " <> name <> " ")) - 1
  }
  let #(v, _) =
    eval("var s; [0].map(async function af() { s = new Error('e').stack }); s")
  let assert KStr(s) = classify(v)
  assert count(s, "af") == 1
  let #(v, _) =
    eval("var s; (async function af() { s = new Error('e').stack })(); s")
  let assert KStr(s) = classify(v)
  assert count(s, "af") == 1
  assert eval_int(
      "[undefined].map(function* gd(x = new Error('e').stack) { yield x })[0].next().value.split('at gd ').length - 1",
    )
    == 1
  assert eval_int(
      "var s; new Promise(async function ex(res) { s = new Error('e').stack; res() }); s.split('at ex ').length - 1",
    )
    == 1
}
