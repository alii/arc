//// Exceptions on the new interpreter: try/catch/finally completion records,
//// unwinding across bytecode frames, throws crossing the native boundary in
//// both directions, and `Error.stack` built from `Agent.frames`.

import arc/compiler
import arc/interp/entry
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/types.{type Agent, type JsVal, JInt, KNum, KStr, classify}
import arc/rt/val as rt_val
import gleam/int
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

fn eval_on(st: Agent, source: String) -> #(JsVal, Agent) {
  case run_on(st, source) {
    #(NormalCompletion(v), st) -> #(v, st)
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

fn eval(source: String) -> #(JsVal, Agent) {
  eval_on(agent(), source)
}

fn as_string(st: Agent, v: JsVal, source: String) -> String {
  case classify(v) {
    KStr(s) -> s
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

fn eval_string(source: String) -> String {
  let #(v, st) = eval(source)
  as_string(st, v, source)
}

fn eval_string_on(st: Agent, source: String) -> String {
  let #(v, st) = eval_on(st, source)
  as_string(st, v, source)
}

fn eval_int(source: String) -> Int {
  let #(v, st) = eval(source)
  case classify(v) {
    KNum(JInt(n)) -> n
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

/// The thrown value of a script that must throw, rendered.
fn thrown(source: String) -> String {
  case run(source) {
    #(ThrowCompletion(e), st) -> rt_inspect.inspect(st, e)
    #(NormalCompletion(v), st) ->
      panic as {
        source <> " did not throw, gave " <> rt_inspect.inspect(st, v)
      }
  }
}

// -- try / catch / throw ---------------------------------------------------------

pub fn catch_binds_thrown_value_test() {
  assert eval_int("try { throw 41 } catch (e) { e + 1 }") == 42
  assert eval_string("try { throw new Error('boom') } catch (e) { e.message }")
    == "boom"
  assert eval_string("try { throw 'x' } catch { 'no binding' }") == "no binding"
  assert eval_string("try { throw {a: 1, b: 'two'} } catch ({a, b}) { a + b }")
    == "1two"
}

pub fn uncaught_throw_is_throw_completion_test() {
  assert thrown("throw 7") == "7"
  assert string.contains(thrown("undefinedName.x"), "ReferenceError")
  assert string.contains(thrown("null.x"), "TypeError")
  assert string.contains(thrown("(void 0)()"), "TypeError")
  assert string.contains(thrown("let a = a"), "ReferenceError")
  assert string.contains(thrown("const k = 1; k = 2"), "TypeError")
}

pub fn uncaught_throw_restores_agent_bookkeeping_test() {
  let src =
    "function inner() { throw new Error('deep') }\n"
    <> "function outer() { inner() }\n"
    <> "outer()"
  let #(comp, st) = run(src)
  let assert ThrowCompletion(_) = comp
  assert st.frames == []
  assert st.store.call_depth == 0
  // The agent is still usable and its globals survived.
  assert eval_string_on(st, "typeof outer") == "function"
}

pub fn throw_unwinds_across_bytecode_frames_test() {
  let src =
    "function c() { throw new Error('from c') }\n"
    <> "function b() { c(); return 'b finished' }\n"
    <> "function a() { try { b() } catch (e) { return 'a caught ' + e.message } }\n"
    <> "a()"
  assert eval_string(src) == "a caught from c"
}

pub fn catch_sees_only_live_frames_test() {
  // After unwinding out of inner/outer their stack frames are gone: an Error
  // made in the catch block names only the script frame, and depth is back.
  let src =
    "function inner() { throw 1 }\n"
    <> "function outer() { inner() }\n"
    <> "var s; try { outer() } catch (e) { s = new Error('after').stack }\n"
    <> "s"
  let #(v, st) = eval(src)
  let s = as_string(st, v, src)
  assert !string.contains(s, "inner")
  assert !string.contains(s, "outer")
  assert string.contains(s, "at script:3")
  assert st.frames == []
  assert st.store.call_depth == 0
}

pub fn operand_stack_is_truncated_on_catch_test() {
  // The throw happens mid-expression with operands pushed; the handler must
  // resume on the PushTry-time stack.
  assert eval_int(
      "function boom() { throw 5 } var r; try { r = 1 + (2 * boom()) } catch (e) { r = e * 10 } r",
    )
    == 50
  assert eval_string(
      "var out = []; for (var i = 0; i < 3; i++) { try { out.push([i, (() => { throw i })()]) } catch (e) { out.push('c' + e) } } out.join()",
    )
    == "c0,c1,c2"
}

pub fn rethrow_and_nested_try_test() {
  assert eval_string(
      "var log = []; try { try { throw 'a' } catch (e) { log.push('inner ' + e); throw e + 'b' } } catch (e) { log.push('outer ' + e) } log.join()",
    )
    == "inner a,outer ab"
  assert eval_string(
      "var log = []; try { try { throw 1 } finally { log.push('f') } } catch (e) { log.push('c' + e) } log.join()",
    )
    == "f,c1"
}

// -- finally ordering matrix ------------------------------------------------------

pub fn finally_runs_on_normal_exit_test() {
  assert eval_string(
      "var l = []; try { l.push('t') } finally { l.push('f') } l.join()",
    )
    == "t,f"
  assert eval_string(
      "var l = []; try { l.push('t') } catch (e) { l.push('c') } finally { l.push('f') } l.join()",
    )
    == "t,f"
}

pub fn finally_runs_after_catch_test() {
  assert eval_string(
      "var l = []; try { throw 0 } catch (e) { l.push('c') } finally { l.push('f') } l.join()",
    )
    == "c,f"
  // A throw from the catch block still runs finally, then propagates.
  assert eval_string(
      "var l = []; try { try { throw 0 } catch (e) { l.push('c'); throw 1 } finally { l.push('f') } } catch (e) { l.push('o' + e) } l.join()",
    )
    == "c,f,o1"
}

pub fn finally_return_completion_records_test() {
  // return in try: finally runs, the try's value is returned.
  assert eval_string(
      "var l = []; function f() { try { l.push('t'); return 'rt' } finally { l.push('f') } } [f()].concat(l).join()",
    )
    == "rt,t,f"
  // return in finally overrides return in try (§14.15.3).
  assert eval_string(
      "function f() { try { return 'try' } finally { return 'fin' } } f()",
    )
    == "fin"
  // return in finally swallows a throw.
  assert eval_string(
      "function f() { try { throw 1 } finally { return 'fin' } } f()",
    )
    == "fin"
  // throw in finally replaces the pending return.
  assert eval_string(
      "function f() { try { return 1 } finally { throw 'tf' } } try { f() } catch (e) { e }",
    )
    == "tf"
  // throw in finally replaces the pending throw.
  assert eval_string(
      "try { try { throw 'first' } finally { throw 'second' } } catch (e) { e }",
    )
    == "second"
  // The return value is captured before finally runs.
  assert eval_int(
      "function f() { var x = 1; try { return x } finally { x = 2 } } f()",
    )
    == 1
}

pub fn finally_break_continue_completion_records_test() {
  assert eval_string(
      "var l = []; for (var i = 0; i < 3; i++) { try { if (i === 1) break; l.push('b' + i) } finally { l.push('f' + i) } } l.join()",
    )
    == "b0,f0,f1"
  assert eval_string(
      "var l = []; for (var i = 0; i < 3; i++) { try { if (i === 1) continue; l.push('b' + i) } finally { l.push('f' + i) } } l.join()",
    )
    == "b0,f0,f1,b2,f2"
  // break out of finally cancels the pending throw.
  assert eval_string(
      "var r = 'none'; out: { try { throw 'lost' } finally { break out } } r",
    )
    == "none"
  // labelled break through two finally blocks runs both, innermost first.
  assert eval_string(
      "var l = []; out: for (;;) { try { try { break out } finally { l.push('inner') } } finally { l.push('outer') } } l.join()",
    )
    == "inner,outer"
}

pub fn nested_finally_order_test() {
  assert eval_string(
      "var l = []; function f() { try { try { return 'r' } finally { l.push(1) } } finally { l.push(2) } } l.push(f()); l.join()",
    )
    == "1,2,r"
  assert eval_string(
      "var l = []; try { try { try { throw 't' } finally { l.push(1) } } finally { l.push(2) } } catch (e) { l.push(e) } finally { l.push(3) } l.join()",
    )
    == "1,2,t,3"
}

pub fn finally_in_loops_and_callee_frames_test() {
  // finally inside a called function runs before the caller's catch.
  assert eval_string(
      "var l = []; function g() { try { throw 'g' } finally { l.push('gf') } } try { g() } catch (e) { l.push('c' + e) } l.join()",
    )
    == "gf,cg"
  // for-of: break runs the iterator's return() and the finally.
  assert eval_string(
      "var l = []; var it = { [Symbol.iterator]() { return { next() { return { value: 1, done: false } }, return() { l.push('closed'); return {} } } } };"
      <> " for (var x of it) { try { break } finally { l.push('f') } } l.join()",
    )
    == "f,closed"
  // A throw out of a for-of body closes the iterator before the outer catch.
  assert eval_string(
      "var l = []; var it = { [Symbol.iterator]() { return { next() { return { value: 1, done: false } }, return() { l.push('closed'); return {} } } } };"
      <> " try { for (var x of it) { throw 'body' } } catch (e) { l.push(e) } l.join()",
    )
    == "closed,body"
}

// -- throws crossing the native boundary --------------------------------------

pub fn native_throw_caught_in_bytecode_test() {
  assert eval_string(
      "try { null.x } catch (e) { e instanceof TypeError ? 'TE' : 'no' }",
    )
    == "TE"
  assert eval_string("try { JSON.parse('{') } catch (e) { e.name }")
    == "SyntaxError"
  assert eval_string("try { new Array(-1) } catch (e) { e.name }")
    == "RangeError"
  assert eval_string("try { decodeURIComponent('%') } catch (e) { e.name }")
    == "URIError"
  // A getter invoked by a native ([[Get]] inside Object.keys' caller) throws.
  assert eval_string(
      "var o = { get k() { throw 'getter' } }; try { JSON.stringify(o) } catch (e) { e }",
    )
    == "getter"
}

pub fn bytecode_throw_through_native_frame_test() {
  // bytecode → Array.prototype.map (native) → bytecode callback throws; the
  // outer bytecode try catches it and the callback's frames are gone.
  let src =
    "function cb(x) { if (x === 2) throw new Error('cb ' + x); return x }\n"
    <> "var r; try { [1, 2, 3].map(cb) } catch (e) { r = e.message + '|' + e.stack.split('\\n')[1].trim() }\n"
    <> "r"
  let #(v, st) = eval(src)
  assert as_string(st, v, src) == "cb 2|at cb (script:1)"
  assert st.frames == []
  assert st.store.call_depth == 0
  // Several native frames deep: sort comparator → forEach callback → throw.
  assert eval_string(
      "var r = []; try { [3, 1, 2].sort(function (a, b) { [0].forEach(function () { if (a === 1) throw 'deep' }); return a - b }) } catch (e) { r.push(e) } r.join()",
    )
    == "deep"
}

pub fn native_catches_bytecode_throw_test() {
  // The Promise constructor catches a throwing executor and rejects.
  assert eval_string(
      "var p = new Promise(function () { throw new Error('exec') }); typeof p.then",
    )
    == "function"
  // Execution continues normally in bytecode after the native caught it.
  assert eval_int("new Promise(function () { throw 1 }); 2 + 3") == 5
}

/// A string or integer argument as bare text (no quoting).
fn show(v: JsVal) -> String {
  case classify(v) {
    KStr(s) -> s
    KNum(JInt(n)) -> int.to_string(n)
    _ -> "?"
  }
}

pub fn host_native_throw_and_reentry_test() {
  let st = agent()
  // `thrower(msg)`: a native that raises TypeError(msg).
  let #(thrower, st) =
    rt_helpers.func(st, fn(st, args) {
      let msg = case args {
        [m, ..] -> show(m)
        [] -> "no message"
      }
      rt_val.t_throw_type_error(st, "host: " <> msg)
    })
  let st = rt_obj.t_global_set(st, <<"thrower":utf8>>, thrower)
  // `reenter(f, x)`: a native that calls back into `f(x)` and lets a throw
  // propagate through it.
  let #(reenter, st) =
    rt_helpers.func(st, fn(st, args) {
      case args {
        [f, x, ..] -> rt_call.t_call_checked(st, f, types.mk_undefined(), [x])
        _ -> #(types.mk_undefined(), st)
      }
    })
  let st = rt_obj.t_global_set(st, <<"reenter":utf8>>, reenter)
  // `settle(f)`: a native that calls `f()` and reports the completion kind
  // instead of propagating.
  let #(settle, st) =
    rt_helpers.func(st, fn(st, args) {
      case args {
        [f, ..] ->
          case rt_call.t_call(st, f, types.mk_undefined(), []) {
            #(NormalCompletion(v), st) -> #(
              types.mk_string("ok:" <> show(v)),
              st,
            )
            #(ThrowCompletion(e), st) -> #(
              types.mk_string("threw:" <> show(e)),
              st,
            )
          }
        _ -> #(types.mk_undefined(), st)
      }
    })
  let st = rt_obj.t_global_set(st, <<"settle":utf8>>, settle)

  assert eval_string_on(st, "try { thrower('a') } catch (e) { e.message }")
    == "host: a"
  assert eval_string_on(
      st,
      "try { thrower('t') } catch (e) { e instanceof TypeError ? 'TE' : 'no' }",
    )
    == "TE"
  // finally runs for a native throw too.
  assert eval_string_on(
      st,
      "var l = []; try { try { thrower(1) } finally { l.push('f') } } catch (e) { l.push('c') } l.join()",
    )
    == "f,c"
  // bytecode → native → bytecode throw, caught two boundaries up.
  assert eval_string_on(
      st,
      "function inner(x) { throw 'inner ' + x } try { reenter(inner, 9) } catch (e) { e }",
    )
    == "inner 9"
  // bytecode → native → bytecode → native throw.
  assert eval_string_on(
      st,
      "function inner(x) { thrower(x) } try { reenter(inner, 'z') } catch (e) { e.message }",
    )
    == "host: z"
  // A native observing a bytecode throw as a completion.
  assert eval_string_on(st, "settle(function () { throw 'boom' })")
    == "threw:boom"
  assert eval_string_on(st, "settle(function () { return 4 })") == "ok:4"
  // The callee's finally still runs before the native sees the throw.
  assert eval_string_on(
      st,
      "var l = []; l.push(settle(function () { try { throw 'x' } finally { l.push('f') } })); l.join()",
    )
    == "f,threw:x"
  let #(_, st) =
    eval_on(st, "try { reenter(function () { thrower(0) }) } catch (e) {}")
  assert st.frames == []
  assert st.store.call_depth == 0
}

pub fn deep_recursion_range_error_is_catchable_test() {
  assert eval_string(
      "function f() { return f() } try { f() } catch (e) { e instanceof RangeError ? e.message : 'other' }",
    )
    == "Maximum call stack size exceeded"
  // Through the native boundary on every level.
  assert eval_string(
      "function f() { return [0].map(f) } try { f() } catch (e) { e instanceof RangeError ? 'RE' : 'other' }",
    )
    == "RE"
}

// -- Error.stack from Agent.frames -----------------------------------------------

pub fn basic_error_stack_header_test() {
  assert string.starts_with(
    eval_string("new Error('boom').stack"),
    "Error: boom",
  )
  assert eval_string("new Error().stack.split('\\n')[0]") == "Error"
}

pub fn type_error_stack_header_test() {
  let s = eval_string("try { null.x } catch (e) { e.stack }")
  assert string.starts_with(s, "TypeError")
  assert string.contains(s, "at ")
}

pub fn nested_frames_have_lines_test() {
  let src =
    "function inner() { throw new Error('boom'); }\n"
    <> "function outer() { inner(); }\n"
    <> "try { outer(); } catch (e) { e.stack }"
  let s = eval_string(src)
  assert string.starts_with(s, "Error: boom")
  assert string.contains(s, "at inner (script:1)")
  assert string.contains(s, "at outer (script:2)")
  assert string.contains(s, "at script:3")
  // Innermost first.
  let assert [_, f1, f2, f3] = string.split(s, "\n")
  assert string.contains(f1, "inner")
  assert string.contains(f2, "outer")
  assert string.contains(f3, "script:3")
}

pub fn interpreter_originated_errors_have_frames_test() {
  // TDZ, not-callable and null property reads are allocated by the
  // interpreter through JsOps.new_error: they carry the same frames.
  let src =
    "function f() {\n"
    <> "  return notDefined;\n"
    <> "}\n"
    <> "try { f() } catch (e) { e.stack }"
  let s = eval_string(src)
  assert string.starts_with(s, "ReferenceError")
  assert string.contains(s, "at f (script:2)")
  assert string.contains(s, "at script:4")
  let src2 = "function g() {\n  (0)();\n}\ntry { g() } catch (e) { e.stack }"
  let s2 = eval_string(src2)
  assert string.starts_with(s2, "TypeError")
  assert string.contains(s2, "at g (script:2)")
}

pub fn frames_through_native_reentry_test() {
  // The native map frame is invisible; both bytecode frames keep their lines.
  let src =
    "function cb() {\n"
    <> "  return new Error('in cb').stack;\n"
    <> "}\n"
    <> "function run() { return [1].map(cb)[0] }\n"
    <> "run()"
  let s = eval_string(src)
  assert string.contains(s, "at cb (script:2)")
  assert string.contains(s, "at run (script:4)")
  assert string.contains(s, "at script:5")
}

pub fn caller_line_restored_after_return_test() {
  // After `helper()` returns, a throw later on the caller's line 4 reports
  // line 4, not the callee's last line.
  let src =
    "function helper() {\n"
    <> "  return 1;\n"
    <> "}\n"
    <> "helper(); try { null.x } catch (e) { e.stack }"
  let s = eval_string(src)
  assert string.contains(s, "at script:4")
  assert !string.contains(s, "helper")
}

pub fn stack_is_non_enumerable_test() {
  assert eval_string(
      "var e = new Error('x'); Object.keys(e).indexOf('stack') === -1 ? 'absent' : 'present'",
    )
    == "absent"
}

pub fn stack_trace_limit_test() {
  assert eval_string("'' + Error.stackTraceLimit") == "10"
  assert eval_string("Error.stackTraceLimit = 0; new Error('x').stack")
    == "Error: x"
  let src =
    "Error.stackTraceLimit = 1;\n"
    <> "function inner() { return new Error('x').stack; }\n"
    <> "function outer() { return inner(); }\n"
    <> "outer()"
  let s = eval_string(src)
  assert string.contains(s, "at inner")
  assert !string.contains(s, "at outer")
}

pub fn capture_stack_trace_test() {
  assert eval_string("var o = {}; Error.captureStackTrace(o); typeof o.stack")
    == "string"
  assert string.starts_with(
    eval_string(
      "var o = { name: 'Boom', message: 'kaboom' }; Error.captureStackTrace(o); o.stack",
    ),
    "Boom: kaboom",
  )
  assert eval_string(
      "try { Error.captureStackTrace(42); 'no throw' } catch (e) { e instanceof TypeError ? 'TypeError' : 'other' }",
    )
    == "TypeError"
}

pub fn subclass_error_has_stack_test() {
  let s =
    eval_string("class MyError extends Error {}\nnew MyError('boom').stack")
  assert string.contains(s, "boom")
  assert string.contains(s, "at ")
}
