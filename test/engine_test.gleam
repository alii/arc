import arc/engine.{Returned}
import arc/module/load_error
import arc/vm/builtins/console
import arc/vm/value.{Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined}
import gleam/option.{Some}

// ----------------------------------------------------------------------------
// Serialization — roundtrip
// ----------------------------------------------------------------------------

/// Helper: eval on engine, assert normal completion, return value.
fn assert_eval(eng: engine.Engine(host), source: String) -> value.JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, source)
  value
}

/// Helper: serialize then deserialize an engine.
fn roundtrip(eng: engine.Engine(host)) -> engine.Engine(host) {
  let assert Ok(eng) = eng |> engine.serialize |> engine.deserialize
  eng
}

pub fn deserialize_rejects_garbage_bytes_test() {
  let result: Result(engine.Engine(Nil), _) =
    engine.deserialize(<<"definitely not a snapshot":utf8>>)
  assert result == Error(engine.MalformedBinary)
}

pub fn deserialize_rejects_unaligned_bits_test() {
  // A BitArray that isn't even byte-aligned, let alone a serialized term.
  let result: Result(engine.Engine(Nil), _) = engine.deserialize(<<1:size(3)>>)
  assert result == Error(engine.MalformedBinary)
}

pub fn deserialize_rejects_foreign_term_test() {
  // A valid Erlang external term (`term_to_binary({1, 2, 3})`), but with no
  // snapshot header in front of it — this is also the shape of a pre-header
  // snapshot. Rejected on the bytes; nothing is ever decoded.
  let result: Result(engine.Engine(Nil), _) =
    engine.deserialize(<<131, 104, 3, 97, 1, 97, 2, 97, 3>>)
  assert result == Error(engine.MalformedBinary)
}

pub fn deserialize_rejects_wrong_version_test() {
  // Our tag, a version no build will ever carry.
  let result: Result(engine.Engine(Nil), _) =
    engine.deserialize(<<"arc-engine":utf8, 999_999:32, 131, 106>>)
  assert result == Error(engine.IncompatibleSnapshot)
}

pub fn deserialize_rejects_corrupt_payload_behind_header_test() {
  // A well-formed header whose payload is not an external term at all: the
  // decoder must fail closed rather than crash out of `binary_to_term`.
  let result: Result(engine.Engine(Nil), _) =
    engine.deserialize(<<"arc-engine":utf8, 3:32, 1, 2, 3>>)
  assert result == Error(engine.IncompatibleSnapshot)
}

pub fn serialize_roundtrip_number_test() {
  let eng =
    engine.new()
    |> engine.define_global("x", JsNumber(Finite(42.0)))

  assert assert_eval(roundtrip(eng), "x") == JsNumber(Finite(42.0))
}

pub fn serialize_roundtrip_string_test() {
  let eng =
    engine.new()
    |> engine.define_global("s", JsString("hello"))

  assert assert_eval(roundtrip(eng), "s") == JsString("hello")
}

pub fn serialize_roundtrip_bool_test() {
  let eng =
    engine.new()
    |> engine.define_global("b", JsBool(True))

  assert assert_eval(roundtrip(eng), "b") == JsBool(True)
}

pub fn serialize_roundtrip_null_test() {
  let eng =
    engine.new()
    |> engine.define_global("n", JsNull)

  assert assert_eval(roundtrip(eng), "n") == JsNull
}

pub fn serialize_roundtrip_undefined_test() {
  let eng =
    engine.new()
    |> engine.define_global("u", JsUndefined)

  assert assert_eval(roundtrip(eng), "u") == JsUndefined
}

pub fn serialize_preserves_object_properties_test() {
  let eng = engine.new()
  let assert Ok(#(_, eng)) =
    engine.eval(eng, "var obj = { a: 1, b: 'two', c: true }")

  let restored = roundtrip(eng)
  assert assert_eval(restored, "obj.a") == JsNumber(Finite(1.0))
  assert assert_eval(restored, "obj.b") == JsString("two")
  assert assert_eval(restored, "obj.c") == JsBool(True)
}

pub fn serialize_preserves_array_test() {
  let eng = engine.new()
  let assert Ok(#(_, eng)) = engine.eval(eng, "var arr = [10, 20, 30]")

  let restored = roundtrip(eng)
  assert assert_eval(restored, "arr[0]") == JsNumber(Finite(10.0))
  assert assert_eval(restored, "arr[2]") == JsNumber(Finite(30.0))
  assert assert_eval(restored, "arr.length") == JsNumber(Finite(3.0))
}

pub fn serialize_preserves_nested_objects_test() {
  let eng = engine.new()
  let assert Ok(#(_, eng)) =
    engine.eval(eng, "var deep = { a: { b: { c: 99 } } }")

  assert assert_eval(roundtrip(eng), "deep.a.b.c") == JsNumber(Finite(99.0))
}

pub fn serialize_preserves_closure_test() {
  // A closure captures a variable from its defining scope.
  // After roundtrip the closure should still see the captured value.
  let eng = engine.new()
  let assert Ok(#(_, eng)) =
    engine.eval(
      eng,
      "var captured = 100;
       var getCaptured = function() { return captured; }",
    )

  assert assert_eval(roundtrip(eng), "getCaptured()") == JsNumber(Finite(100.0))
}

pub fn serialize_preserves_mutable_closure_test() {
  // Closure over a mutable variable — the mutation should survive roundtrip.
  let eng = engine.new()
  let assert Ok(#(_, eng)) =
    engine.eval(
      eng,
      "var count = 0;
       var inc = function() { count++; return count; };
       inc(); inc(); inc();",
    )

  // count is now 3 after three inc() calls
  let restored = roundtrip(eng)
  assert assert_eval(restored, "count") == JsNumber(Finite(3.0))
  // calling inc again on the restored engine should continue from 3
  let assert Ok(#(Returned(value:), _)) = engine.eval(restored, "inc()")
  assert value == JsNumber(Finite(4.0))
}

pub fn serialize_preserves_prototype_chain_test() {
  let eng = engine.new()
  let assert Ok(#(_, eng)) =
    engine.eval(
      eng,
      "var proto = { greet: function() { return 'hi'; } };
       var child = Object.create(proto);
       child.name = 'arc';",
    )

  let restored = roundtrip(eng)
  // Property on child itself
  assert assert_eval(restored, "child.name") == JsString("arc")
  // Method inherited via prototype
  assert assert_eval(restored, "child.greet()") == JsString("hi")
}

pub fn serialize_preserves_state_across_evals_test() {
  // Accumulate state across multiple eval calls, then roundtrip
  let eng = engine.new()
  let assert Ok(#(_, eng)) = engine.eval(eng, "var log = []")
  let assert Ok(#(_, eng)) = engine.eval(eng, "log.push('a')")
  let assert Ok(#(_, eng)) = engine.eval(eng, "log.push('b')")
  let assert Ok(#(_, eng)) = engine.eval(eng, "log.push('c')")

  let restored = roundtrip(eng)
  assert assert_eval(restored, "log.length") == JsNumber(Finite(3.0))
  assert assert_eval(restored, "log[0]") == JsString("a")
  assert assert_eval(restored, "log[2]") == JsString("c")
}

pub fn serialize_chained_roundtrips_test() {
  // serialize → deserialize → mutate → serialize → deserialize
  let eng = engine.new()
  let assert Ok(#(_, eng)) = engine.eval(eng, "var x = 1")

  let restored1 = roundtrip(eng)
  let assert Ok(#(_, restored1)) = engine.eval(restored1, "x = x + 10")

  let restored2 = roundtrip(restored1)
  assert assert_eval(restored2, "x") == JsNumber(Finite(11.0))
}

pub fn serialize_builtins_survive_test() {
  // Built-in globals (Math, Object, Array, etc.) should work after roundtrip
  let restored = roundtrip(engine.new())
  assert assert_eval(restored, "Math.max(1, 5, 3)") == JsNumber(Finite(5.0))
  assert assert_eval(restored, "Array.isArray([])") == JsBool(True)
  assert assert_eval(restored, "typeof Object") == JsString("function")
}

pub fn serialize_host_fn_reregister_test() {
  // Host functions don't survive serialization, but re-registering them
  // on the restored engine should work.
  let eng =
    engine.new()
    |> engine.define_fn("double", 1, fn(args, _this, state) {
      case args {
        [JsNumber(Finite(n)), ..] -> #(state, Ok(JsNumber(Finite(n *. 2.0))))
        _ -> #(state, Ok(JsUndefined))
      }
    })

  // Verify it works before serialization
  assert assert_eval(eng, "double(5)") == JsNumber(Finite(10.0))

  // After roundtrip, re-register the host function
  let restored =
    roundtrip(eng)
    |> engine.define_fn("double", 1, fn(args, _this, state) {
      case args {
        [JsNumber(Finite(n)), ..] -> #(state, Ok(JsNumber(Finite(n *. 2.0))))
        _ -> #(state, Ok(JsUndefined))
      }
    })

  assert assert_eval(restored, "double(5)") == JsNumber(Finite(10.0))
}

pub fn serialize_constructor_and_instances_test() {
  let eng = engine.new()
  let assert Ok(#(_, eng)) =
    engine.eval(
      eng,
      "function Point(x, y) { this.x = x; this.y = y; }
       Point.prototype.sum = function() { return this.x + this.y; };
       var p = new Point(3, 4);",
    )

  let restored = roundtrip(eng)
  assert assert_eval(restored, "p.x") == JsNumber(Finite(3.0))
  assert assert_eval(restored, "p.sum()") == JsNumber(Finite(7.0))
  // Can still construct new instances from the restored constructor
  let assert Ok(#(_, restored)) =
    engine.eval(restored, "var q = new Point(10, 20)")
  assert assert_eval(restored, "q.sum()") == JsNumber(Finite(30.0))
}

// ----------------------------------------------------------------------------
// Host FFI — define_fn / define_namespace / define_global
// ----------------------------------------------------------------------------

pub fn define_fn_callable_from_js_test() {
  let eng =
    engine.new()
    |> engine.define_fn("double", 1, fn(args, _this, state) {
      case args {
        [JsNumber(Finite(n)), ..] -> #(state, Ok(JsNumber(Finite(n *. 2.0))))
        _ -> #(state, Ok(JsUndefined))
      }
    })

  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, "double(21)")
  assert value == JsNumber(Finite(42.0))
}

pub fn define_fn_has_name_and_length_test() {
  let eng =
    engine.new()
    |> engine.define_fn("myFunc", 3, fn(_args, _this, state) {
      #(state, Ok(JsUndefined))
    })

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "myFunc.name + ':' + myFunc.length")
  assert value == JsString("myFunc:3")
}

pub fn define_namespace_creates_object_with_methods_test() {
  let eng =
    engine.new()
    |> engine.define_namespace("math2", [
      #("square", 1, fn(args, _this, state) {
        case args {
          [JsNumber(Finite(n)), ..] -> #(state, Ok(JsNumber(Finite(n *. n))))
          _ -> #(state, Ok(JsUndefined))
        }
      }),
      #("cube", 1, fn(args, _this, state) {
        case args {
          [JsNumber(Finite(n)), ..] -> #(
            state,
            Ok(JsNumber(Finite(n *. n *. n))),
          )
          _ -> #(state, Ok(JsUndefined))
        }
      }),
    ])

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "math2.square(4) + math2.cube(2)")
  assert value == JsNumber(Finite(24.0))
}

pub fn define_namespace_has_tostringtag_test() {
  // A namespace built via the facade must carry @@toStringTag, matching every
  // built-in namespace (Math/JSON/console) — not report "[object Object]".
  let eng =
    engine.new()
    |> engine.define_namespace("widgets", [
      #("noop", 0, fn(_args, _this, state) { #(state, Ok(JsUndefined)) }),
    ])

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "Object.prototype.toString.call(widgets)")
  assert value == JsString("[object widgets]")
}

pub fn define_global_installs_value_test() {
  let eng =
    engine.new()
    |> engine.define_global("MY_CONST", JsString("hello"))

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "MY_CONST + ' world'")
  assert value == JsString("hello world")
}

pub fn host_fn_receives_this_test() {
  let eng =
    engine.new()
    |> engine.define_fn("whoami", 0, fn(_args, this, state) {
      case this {
        JsString(s) -> #(state, Ok(JsString("this=" <> s)))
        _ -> #(state, Ok(JsString("this=other")))
      }
    })

  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, "whoami.call('abc')")
  assert value == JsString("this=abc")
}

pub fn host_fn_can_throw_test() {
  let eng =
    engine.new()
    |> engine.define_fn("boom", 0, fn(_args, _this, state) {
      #(state, Error(JsString("kaboom")))
    })

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "try { boom() } catch (e) { 'caught:' + e }")
  assert value == JsString("caught:kaboom")
}

// ----------------------------------------------------------------------------
// Module evaluation — eval_module / read_export / call
// ----------------------------------------------------------------------------

/// Single self-contained module — reject every import.
fn reject_imports(_raw: String, _parent: String) {
  Error(load_error.ResolveForbidden)
}

fn reject_loads(_resolved: String) {
  Error(load_error.LoadForbidden)
}

pub fn eval_module_reads_export_test() {
  let eng = engine.new()
  let assert Ok(#(evaluated, eng)) =
    engine.eval_module(
      eng,
      "test:mod",
      "export const answer = 42; export function noop() {}",
      reject_imports,
      reject_loads,
    )
  let assert Some(ns) = evaluated.namespace
  assert engine.read_export(eng, ns, "answer") == Some(JsNumber(Finite(42.0)))
  // Missing exports are None, not an error.
  assert engine.read_export(eng, ns, "missing") == option.None
}

pub fn call_export_threads_module_state_test() {
  // The otters lifecycle: evaluate a module, read a function export, then call
  // it repeatedly — module-scoped state in the closure must persist across
  // calls because each call threads the heap forward via the returned engine.
  let eng = engine.new()
  let assert Ok(#(evaluated, eng)) =
    engine.eval_module(
      eng,
      "test:counter",
      "let count = 0;
       export function bump(n) { count += n; return count; }",
      reject_imports,
      reject_loads,
    )
  let assert Some(ns) = evaluated.namespace
  let assert Some(bump) = engine.read_export(eng, ns, "bump")

  let assert Ok(#(Returned(value:), eng)) =
    engine.call(eng, bump, JsUndefined, [JsNumber(Finite(5.0))])
  assert value == JsNumber(Finite(5.0))

  let assert Ok(#(Returned(value:), _eng)) =
    engine.call(eng, bump, JsUndefined, [JsNumber(Finite(3.0))])
  assert value == JsNumber(Finite(8.0))
}

pub fn destructured_declaration_exports_test() {
  // `export const { … } = o` / `export let [ … ] = a` export EVERY name the
  // pattern binds (§16.2.3.3 ExportedNames = BoundNames). Import them all by
  // name AND through a namespace object from a second module.
  let dep =
    "const o = { a: 1, b: 2, extra: 3 };
     const arr = [10, 20];
     export const { a, b: c, ...r } = o;
     export let [x, , y = 1] = arr;"
  let resolve = fn(raw: String, _referrer: String) { Ok(raw) }
  let load = fn(resolved: String) {
    case resolved {
      "dep" -> Ok(dep)
      _ -> Error(load_error.LoadNotFound)
    }
  }
  let assert Ok(#(evaluated, eng)) =
    engine.eval_module(
      engine.new(),
      "entry",
      "import { a, c, r, x, y } from 'dep';
       import * as ns from 'dep';
       export const sum = a + c + x;
       export const rest = r.extra;
       export const gap = y;
       export const keys = Object.keys(ns).join(',');",
      resolve,
      load,
    )
  let assert Some(ns) = evaluated.namespace
  // a=1, c (renamed from b)=2, x=10 — every destructured binding linked.
  assert engine.read_export(eng, ns, "sum") == Some(JsNumber(Finite(13.0)))
  // Rest element: `r` is `{ extra: 3 }`.
  assert engine.read_export(eng, ns, "rest") == Some(JsNumber(Finite(3.0)))
  // Elision + default: `[x, , y = 1]` over a 2-element array leaves y = 1.
  assert engine.read_export(eng, ns, "gap") == Some(JsNumber(Finite(1.0)))
  // §10.4.6.11 [[OwnPropertyKeys]]: exported names in sorted code-unit order.
  assert engine.read_export(eng, ns, "keys") == Some(JsString("a,c,r,x,y"))
}

pub fn eval_module_syntax_error_test() {
  let assert Error(err) =
    engine.eval_module(
      engine.new(),
      "test:bad",
      "export const = ;",
      reject_imports,
      reject_loads,
    )
  // Surfaces a formatted SyntaxError, not a panic.
  assert engine.eval_error_message(err) != ""
}

// ----------------------------------------------------------------------------
// console — WHATWG Console §2 Logger / Formatter
// ----------------------------------------------------------------------------

/// Host fn that returns what `console.log(...args)` would print, so the
/// formatter is assertable from JS without touching stdout. A throw from a
/// user `toString`/`valueOf` during %s/%d/%i/%f coercion propagates, exactly
/// as it does out of `console.log`.
fn fmt_engine() -> engine.Engine(host) {
  engine.new()
  |> engine.define_fn("fmt", 0, fn(args, _this, s) {
    case console.format(args, s) {
      Ok(#(line, s)) -> #(s, Ok(JsString(line)))
      Error(#(thrown, s)) -> #(s, Error(thrown))
    }
  })
}

pub fn console_shape_test() {
  let eng = engine.new()
  assert assert_eval(eng, "typeof console.log") == JsString("function")
  assert assert_eval(eng, "console.log.length") == JsNumber(Finite(0.0))
  assert assert_eval(eng, "Object.prototype.toString.call(console)")
    == JsString("[object console]")
}

pub fn console_inspect_objects_test() {
  let eng = fmt_engine()
  assert assert_eval(eng, "fmt({a: 1}, [1, 2])")
    == JsString("{ a: 1 } [ 1, 2 ]")
  // Top-level strings are unquoted.
  assert assert_eval(eng, "fmt('raw', 1)") == JsString("raw 1")
}

pub fn console_format_specifiers_test() {
  let eng = fmt_engine()
  assert assert_eval(eng, "fmt('hi %s!', 'world')") == JsString("hi world!")
  // %d renders the WHOLE number (Node's formatNumber), it does not truncate:
  // only %i truncates, and it does so in parseInt, not in the renderer.
  assert assert_eval(eng, "fmt('%d/%i', 42.9, '7.5')") == JsString("42.9/7")
  assert assert_eval(eng, "fmt('%f', 3.14)") == JsString("3.14")
  assert assert_eval(eng, "fmt('%o', {x: 1})") == JsString("{ x: 1 }")
  // `%%` only collapses inside Formatter, i.e. with 2+ args.
  assert assert_eval(eng, "fmt('%% done', 0)") == JsString("% done 0")
}

/// %d/%i/%f share ONE renderer, so each non-finite prints under its own name
/// and a -0 Number prints "-0". The truncating `int_substitution` this
/// replaced collapsed all three of ±Infinity/NaN into "NaN" and printed -0
/// as "0".
///
/// The three still DIFFER in how they coerce, and only there: %d is
/// ToNumber, so it sees a real -0; %i/%f go through parseInt/parseFloat,
/// which coerce via ToString first, and String(-0) is "0" — so -0 is already
/// gone before the renderer runs. Likewise parseFloat("-Infinity") is
/// -Infinity but parseInt("-Infinity") is NaN.
pub fn console_format_number_edge_values_test() {
  let eng = fmt_engine()
  assert assert_eval(eng, "fmt('%d', -0)") == JsString("-0")
  assert assert_eval(eng, "fmt('%d', Infinity)") == JsString("Infinity")
  assert assert_eval(eng, "fmt('%d', -Infinity)") == JsString("-Infinity")
  assert assert_eval(eng, "fmt('%d', NaN)") == JsString("NaN")
  assert assert_eval(eng, "fmt('%f', -0)") == JsString("0")
  assert assert_eval(eng, "fmt('%i', -0)") == JsString("0")
  assert assert_eval(eng, "fmt('%f', Infinity)") == JsString("Infinity")
  assert assert_eval(eng, "fmt('%f', -Infinity)") == JsString("-Infinity")
  assert assert_eval(eng, "fmt('%i', -Infinity)") == JsString("NaN")
}

pub fn console_format_edge_cases_test() {
  let eng = fmt_engine()
  // §2.1 step 4: single string arg never enters Formatter.
  assert assert_eval(eng, "fmt('100%')") == JsString("100%")
  // Trailing lone `%` stays literal, leftover args still appended.
  assert assert_eval(eng, "fmt('100%', 'x')") == JsString("100% x")
  // §2.2.1 step 2: out-of-args specifier stays literal (Node behaviour).
  assert assert_eval(eng, "fmt('%d %d %d', 'abc', 5)") == JsString("NaN 5 %d")
  // Unknown specifier left as-is.
  assert assert_eval(eng, "fmt('a%zb', 1)") == JsString("a%zb 1")
  // %c consumes its arg, emits nothing.
  assert assert_eval(eng, "fmt('a%cb', 'color:red')") == JsString("ab")
  // §2.2 step 5: leftover args after format string.
  assert assert_eval(eng, "fmt('x=%d', 1, {y: 2})") == JsString("x=1 { y: 2 }")
}

pub fn console_format_specifier_throw_propagates_test() {
  // A USER throw raised by a specifier's coercion must escape console.log as
  // an abrupt completion (Node behaviour), not be swallowed into a fallback
  // string. Which user hook a specifier can reach follows its spec function:
  // %s = String() and %d = Number(), so toString (%s) / valueOf (%d) throws
  // escape; %i/%f = parseInt/parseFloat, which coerce via ToString only, so
  // a toString throw escapes there too.
  let eng = fmt_engine()
  assert assert_eval(
      eng,
      "try { fmt('%s', {toString(){ throw 1 }}); 'no throw' } catch (e) { e }",
    )
    == JsNumber(Finite(1.0))
  assert assert_eval(
      eng,
      "try { fmt('%d', {valueOf(){ throw 2 }}); 'no throw' } catch (e) { e }",
    )
    == JsNumber(Finite(2.0))
  assert assert_eval(
      eng,
      "try { fmt('%f', {toString(){ throw 3 }}); 'no throw' } catch (e) { e }",
    )
    == JsNumber(Finite(3.0))
  assert assert_eval(
      eng,
      "try { fmt('%i', {toString(){ throw 5 }}); 'no throw' } catch (e) { e }",
    )
    == JsNumber(Finite(5.0))
  // The real console.log path (Logger -> Formatter) propagates too.
  assert assert_eval(
      eng,
      "try { console.log('%s', {toString(){ throw 4 }}); 'no throw' }
       catch (e) { e }",
    )
    == JsNumber(Finite(4.0))
  // %o / %O never coerce, so a throwing toString is never invoked.
  assert assert_eval(
      eng,
      "try { fmt('%O', {toString(){ throw 1 }}); 'no throw' } catch (e) { e }",
    )
    == JsString("no throw")
  // %i/%f are %parseInt%/%parseFloat% (WHATWG Console §2.2.1 step 4.2/4.3),
  // which coerce with ToString — NOT ToNumber. A valueOf-thrower is never
  // invoked: Node prints "NaN" for both. Only %d (Number()) reaches valueOf.
  assert assert_eval(
      eng,
      "fmt('%i %f', {valueOf(){ throw 6 }}, {valueOf(){ throw 7 }})",
    )
    == JsString("NaN NaN")
}

pub fn console_format_symbol_never_throws_test() {
  // A Symbol makes ToString/ToNumber throw a spec TypeError — but that's
  // engine machinery, not user code, and Node/WHATWG never let it escape a
  // specifier: %s is Call(%String%, «sym») (its descriptive string) and
  // %d/%i/%f are "NaN". Only USER throws (toString/valueOf) may propagate.
  let eng = fmt_engine()
  assert assert_eval(eng, "fmt('<%s>', Symbol('x'))") == JsString("<Symbol(x)>")
  assert assert_eval(eng, "fmt('<%s>', Symbol())") == JsString("<Symbol()>")
  assert assert_eval(eng, "fmt('<%s>', Symbol.iterator)")
    == JsString("<Symbol(Symbol.iterator)>")
  assert assert_eval(eng, "fmt('%d %i %f', Symbol(), Symbol(), Symbol())")
    == JsString("NaN NaN NaN")
}

pub fn console_format_bigint_never_throws_test() {
  // BigInt is the OTHER value whose ToNumber raises an engine TypeError
  // ("Cannot convert BigInt to number"). Like Symbol, that must never escape
  // a specifier: Node renders %d/%i of a bigint as "<n>n", %f goes through
  // parseFloat(String(bigint)), and %s is String(bigint). Nothing throws.
  let eng = fmt_engine()
  assert assert_eval(eng, "fmt('%d %i %f', 1n, 2n, 3n)") == JsString("1n 2n 3")
  assert assert_eval(eng, "fmt('%s', -42n)") == JsString("-42")
  assert assert_eval(eng, "fmt('%d', -42n)") == JsString("-42n")
}

// ----------------------------------------------------------------------------
// Promise jobs — a throwing user-capability resolve must not derail the drain
// ----------------------------------------------------------------------------

pub fn promise_job_user_capability_resolve_throw_test() {
  // `Promise.prototype.then` builds the child capability with
  // NewPromiseCapability(SpeciesConstructor(this)), so `cap.resolve` can be
  // an arbitrary user function. When it throws inside the reaction job there
  // is no caller to catch it: the drain reports it to stderr
  // (event_loop.call_for_job, "Uncaught (in promise job) ...") and MUST keep
  // draining — the second, independent chain below still has to run. stderr
  // isn't capturable here, so this pins the "keeps draining" half.
  let eng = engine.new()
  let assert Ok(#(Returned(_), eng)) =
    engine.eval(
      eng,
      "var log = [];
       function C(executor) {
         executor(
           function (v) { log.push('resolve:' + v); throw new Error('boom'); },
           function (e) {},
         );
       }
       Object.defineProperty(C, Symbol.species, { value: C });
       var p = Promise.resolve(42);
       p.constructor = C;
       p.then(v => v * 2);
       Promise.resolve('x').then(v => log.push('after:' + v));",
    )
  assert assert_eval(eng, "log.join(',')") == JsString("resolve:84,after:x")
}

// ----------------------------------------------------------------------------
// No host/process model in core — `Arc` is never defined.
// ----------------------------------------------------------------------------

pub fn no_arc_global_by_default_test() {
  let eng = engine.new()
  assert assert_eval(eng, "typeof Arc") == JsString("undefined")
}

// ----------------------------------------------------------------------------
// Return — caller's call_args restored after a nested call
// ----------------------------------------------------------------------------

pub fn rest_params_survive_call_in_default_expr_test() {
  // A function call inside a default-value expression runs before the rest
  // array is built (IrCreateRestArray reads state.call_args). Returning from
  // that call must restore the caller's call_args, or the rest param is built
  // from the callee's args instead.
  let eng = engine.new()
  assert assert_eval(
      eng,
      "function g() { return 1; }
       function f(a = g(), ...rest) { return rest.join(','); }
       f(undefined, 2, 3)",
    )
    == JsString("2,3")
}

// ----------------------------------------------------------------------------
// RegExp — flags canonical order (ES §22.2.6.4)
// ----------------------------------------------------------------------------

pub fn regexp_flags_canonical_order_test() {
  let eng = engine.new()
  // Reported bug: source order 'gi' must return canonical 'gi', not 'ig'.
  assert assert_eval(eng, "(/abc/gi).flags") == JsString("gi")
  // Comprehensive: scrambled source order → spec order (d,g,i,m,s,u,y).
  assert assert_eval(eng, "(/abc/yusmigd).flags") == JsString("dgimsuy")
  // Constructor path matches literal path.
  assert assert_eval(eng, "new RegExp('abc', 'mig').flags") == JsString("gim")
}

// ----------------------------------------------------------------------------
// AsyncGenerator return completions run finally blocks (§27.6.3.8).
// `engine.eval` drains the microtask queue, so the trailing eval observes the
// fully settled log.
// ----------------------------------------------------------------------------

/// Run a script (draining microtasks), then read the global `log` array.
fn eval_then_read_log(source: String) -> value.JsValue {
  let eng = engine.new()
  let assert Ok(#(Returned(_), eng)) = engine.eval(eng, source)
  assert_eval(eng, "log.join('|')")
}

pub fn async_generator_return_runs_finally_test() {
  // .return() on a generator suspended at `yield` must run the enclosing
  // finally block, and still resolve with the requested {value, done: true}.
  let log =
    eval_then_read_log(
      "var log = [];
       async function* g() {
         try { yield 1; log.push('after'); } finally { log.push('finally'); }
       }
       var it = g();
       it.next().then(function (r) {
         log.push('n:' + r.value + ',' + r.done);
         return it.return(42);
       }).then(function (r) {
         log.push('r:' + r.value + ',' + r.done);
       });",
    )
  assert log == JsString("n:1,false|finally|r:42,true")
}

pub fn for_await_break_runs_finally_test() {
  // Breaking out of `for await` closes the async generator via .return(),
  // which must run its finally block before the loop's completion settles.
  let log =
    eval_then_read_log(
      "var log = [];
       async function* g() {
         try { yield 'a'; yield 'b'; } finally { log.push('cleanup'); }
       }
       (async function () {
         for await (var x of g()) { log.push('got:' + x); break; }
         log.push('done');
       })();",
    )
  assert log == JsString("got:a|cleanup|done")
}

pub fn async_yield_star_missing_return_runs_finally_test() {
  // yield* over an async iterator with no .return: the return completion
  // still propagates out of the yield*, through the outer finally.
  let log =
    eval_then_read_log(
      "var log = [];
       var inner = {};
       inner[Symbol.asyncIterator] = function () {
         return {
           next: function () {
             return Promise.resolve({ value: 'x', done: false });
           }
         };
       };
       async function* g() {
         try { yield* inner; } finally { log.push('fin'); }
       }
       var it = g();
       it.next().then(function (r) {
         log.push('n:' + r.value + ',' + r.done);
         return it.return('rv');
       }).then(function (r) {
         log.push('r:' + r.value + ',' + r.done);
       });",
    )
  assert log == JsString("n:x,false|fin|r:rv,true")
}

pub fn async_yield_star_missing_return_awaits_value_test() {
  // §27.5.3.8 step 7.c.ii.2: when the inner iterator has no .return, the
  // .return() argument is Await-ed BEFORE the return completion unwinds the
  // outer generator, so a thenable is unwrapped (value 42, not a Promise) and
  // the enclosing finally still runs.
  let log =
    eval_then_read_log(
      "var log = [];
       var inner = {};
       inner[Symbol.asyncIterator] = function () {
         return {
           next: function () {
             return Promise.resolve({ value: 'x', done: false });
           }
         };
       };
       async function* g() {
         try { yield* inner; } finally { log.push('fin'); }
       }
       var it = g();
       it.next().then(function (r) {
         log.push('n:' + r.value + ',' + r.done);
         return it.return(Promise.resolve(42));
       }).then(function (r) {
         log.push('r:' + r.value + ',' + r.done);
       });",
    )
  assert log == JsString("n:x,false|fin|r:42,true")
}

pub fn async_yield_star_missing_return_rejected_value_test() {
  // §27.5.3.8 step 7.c.ii.2: if the Await of the .return() argument rejects,
  // the abrupt completion replaces the return completion — it is thrown into
  // the body (running the finally) and the .return() promise REJECTS.
  let log =
    eval_then_read_log(
      "var log = [];
       var inner = {};
       inner[Symbol.asyncIterator] = function () {
         return {
           next: function () {
             return Promise.resolve({ value: 'x', done: false });
           }
         };
       };
       async function* g() {
         try { yield* inner; } finally { log.push('fin'); }
       }
       var it = g();
       it.next().then(function (r) {
         log.push('n:' + r.value + ',' + r.done);
         return it.return(Promise.reject('boom'));
       }).then(function (r) {
         log.push('unexpected:' + r.value);
       }, function (e) {
         log.push('rej:' + e);
       });",
    )
  assert log == JsString("n:x,false|fin|rej:boom")
}

pub fn async_yield_star_delegated_return_runs_outer_finally_test() {
  // Inner iterator HAS a .return: after the delegated return reports done,
  // the outer generator's own finally must still run (§27.5.3.8 7.c.viii).
  let log =
    eval_then_read_log(
      "var log = [];
       var inner = {};
       inner[Symbol.asyncIterator] = function () {
         return {
           next: function () {
             return Promise.resolve({ value: 'i', done: false });
           },
           return: function (v) {
             log.push('inner-return:' + v);
             return Promise.resolve({ value: v, done: true });
           }
         };
       };
       async function* g() {
         try { yield* inner; } finally { log.push('outer-fin'); }
       }
       var it = g();
       it.next().then(function (r) {
         return it.return('z');
       }).then(function (r) {
         log.push('r:' + r.value + ',' + r.done);
       });",
    )
  assert log == JsString("inner-return:z|outer-fin|r:z,true")
}

pub fn async_generator_finally_can_yield_test() {
  // A yield inside the finally suspends the return-unwind; the next .next()
  // resumes inside the finally and then completes with the original
  // .return() value.
  let log =
    eval_then_read_log(
      "var log = [];
       async function* g() {
         try { yield 1; }
         finally { log.push('fs'); yield 2; log.push('fe'); }
       }
       var it = g();
       (async function () {
         var r = await it.next(); log.push('n:' + r.value + ',' + r.done);
         r = await it.return(9); log.push('r1:' + r.value + ',' + r.done);
         r = await it.next(); log.push('r2:' + r.value + ',' + r.done);
       })();",
    )
  assert log == JsString("n:1,false|fs|r1:2,false|fe|r2:9,true")
}

pub fn sync_generator_return_runs_finally_test() {
  // The sync driver shares the same return-unwinder — keep it covered.
  let log =
    eval_then_read_log(
      "var log = [];
       function* g() {
         try { yield 1; } finally { log.push('sfin'); }
       }
       var it = g();
       var r = it.next(); log.push('n:' + r.value + ',' + r.done);
       r = it.return(7); log.push('r:' + r.value + ',' + r.done);",
    )
  assert log == JsString("n:1,false|sfin|r:7,true")
}

// ----------------------------------------------------------------------------
// Agent-wide symbol tables survive child executions (state.merge_globals).
// Symbol descriptions and the Symbol.for registry are agent-wide: a Symbol
// created (or registered) inside an eval / generator child execution must keep
// its description and registry entry after control returns to the caller.
// ----------------------------------------------------------------------------

pub fn eval_child_preserves_symbol_tables_test() {
  let eng = engine.new()
  // The Symbol is created (and Symbol.for-registered) inside the eval child;
  // the caller must still see its description and registry key afterwards
  // (Symbol.for in the caller must find the child's registration).
  assert assert_eval(
      eng,
      "eval('globalThis.s = Symbol(\"x\"); globalThis.reg = Symbol.for(\"k\")');
       [
         s.description,
         Symbol.keyFor(reg),
         Symbol.keyFor(Symbol.for('k')),
       ].join('|')",
    )
    == JsString("x|k|k")
}

pub fn generator_child_preserves_symbol_tables_test() {
  let eng = engine.new()
  // Symbols created inside the generator body (a child execution merged back
  // via merge_globals on each resume) must keep their description / registry
  // entry in the outer frame.
  assert assert_eval(
      eng,
      "function* g() { yield Symbol('gd'); yield Symbol.for('gk'); }
       var it = g();
       var s1 = it.next().value;
       var s2 = it.next().value;
       s1.description + '|' + Symbol.keyFor(s2)",
    )
    == JsString("gd|gk")
}
