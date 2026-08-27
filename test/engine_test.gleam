import arc/engine.{
  type JsValueKind, Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined,
  ModuleReturned, Returned,
}
import arc/host.{State}
import arc/module/load_error
import arc/module_host
import arc/rt/builtins/console
import arc/rt/snapshot.{IncompatibleSnapshot, MalformedBinary}
import arc/rt/types.{
  type JsVal, JFloat, JInt, mk_bool, mk_null, mk_number, mk_string, mk_undefined,
}
import gleam/int
import gleam/list
import gleam/option.{Some}

fn num(f: Float) -> JsVal {
  let i = truncate(f)
  case int.to_float(i) == f {
    True -> mk_number(JInt(i))
    False -> mk_number(JFloat(f))
  }
}

@external(erlang, "erlang", "trunc")
fn truncate(f: Float) -> Int

fn kinds(args: List(JsVal)) -> List(JsValueKind) {
  list.map(args, engine.classify)
}

fn assert_eval(eng: engine.Engine(host), source: String) -> JsValueKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, source)
  engine.classify(value)
}

fn roundtrip(eng: engine.Engine(host)) -> engine.Engine(host) {
  let assert Ok(bin) = engine.serialize(eng)
  let assert Ok(eng) = engine.deserialize(bin)
  eng
}

pub fn deserialize_rejects_garbage_bytes_test() {
  let result: Result(engine.Engine(Nil), _) =
    engine.deserialize(<<"definitely not a snapshot":utf8>>)
  assert result == Error(MalformedBinary)
}

pub fn deserialize_rejects_unaligned_bits_test() {
  let result: Result(engine.Engine(Nil), _) = engine.deserialize(<<1:size(3)>>)
  assert result == Error(MalformedBinary)
}

pub fn deserialize_rejects_foreign_term_test() {
  let result: Result(engine.Engine(Nil), _) =
    engine.deserialize(<<131, 104, 3, 97, 1, 97, 2, 97, 3>>)
  assert result == Error(MalformedBinary)
}

pub fn deserialize_rejects_wrong_version_test() {
  let result: Result(engine.Engine(Nil), _) =
    engine.deserialize(<<"arc-engine":utf8, 999_999:32, 131, 106>>)
  assert result == Error(IncompatibleSnapshot)
}

pub fn deserialize_rejects_corrupt_payload_behind_header_test() {
  let result: Result(engine.Engine(Nil), _) =
    engine.deserialize(<<"arc-engine":utf8, 4:32, 1, 2, 3>>)
  assert result == Error(IncompatibleSnapshot)
}

pub fn serialize_roundtrip_number_test() {
  let eng =
    engine.new()
    |> engine.define_global("x", num(42.0))

  assert assert_eval(roundtrip(eng), "x") == JsNumber(Finite(42.0))
}

pub fn serialize_roundtrip_string_test() {
  let eng =
    engine.new()
    |> engine.define_global("s", mk_string("hello"))

  assert assert_eval(roundtrip(eng), "s") == JsString("hello")
}

pub fn serialize_roundtrip_bool_test() {
  let eng =
    engine.new()
    |> engine.define_global("b", mk_bool(True))

  assert assert_eval(roundtrip(eng), "b") == JsBool(True)
}

pub fn serialize_roundtrip_null_test() {
  let eng =
    engine.new()
    |> engine.define_global("n", mk_null())

  assert assert_eval(roundtrip(eng), "n") == JsNull
}

pub fn serialize_roundtrip_undefined_test() {
  let eng =
    engine.new()
    |> engine.define_global("u", mk_undefined())

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
  let eng = engine.new()
  let assert Ok(#(_, eng)) =
    engine.eval(
      eng,
      "var count = 0;
       var inc = function() { count++; return count; };
       inc(); inc(); inc();",
    )

  let restored = roundtrip(eng)
  assert assert_eval(restored, "count") == JsNumber(Finite(3.0))
  let assert Ok(#(Returned(value:), _)) = engine.eval(restored, "inc()")
  assert engine.classify(value) == JsNumber(Finite(4.0))
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
  assert assert_eval(restored, "child.name") == JsString("arc")
  assert assert_eval(restored, "child.greet()") == JsString("hi")
}

pub fn serialize_preserves_state_across_evals_test() {
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
  let eng = engine.new()
  let assert Ok(#(_, eng)) = engine.eval(eng, "var x = 1")

  let restored1 = roundtrip(eng)
  let assert Ok(#(_, restored1)) = engine.eval(restored1, "x = x + 10")

  let restored2 = roundtrip(restored1)
  assert assert_eval(restored2, "x") == JsNumber(Finite(11.0))
}

pub fn serialize_builtins_survive_test() {
  let restored = roundtrip(engine.new())
  assert assert_eval(restored, "Math.max(1, 5, 3)") == JsNumber(Finite(5.0))
  assert assert_eval(restored, "Array.isArray([])") == JsBool(True)
  assert assert_eval(restored, "typeof Object") == JsString("function")
}

pub fn serialize_host_fn_reregister_test() {
  let eng =
    engine.new()
    |> engine.define_fn("double", 1, fn(args, _this, state) {
      case kinds(args) {
        [JsNumber(Finite(n)), ..] -> #(state, Ok(num(n *. 2.0)))
        _ -> #(state, Ok(mk_undefined()))
      }
    })

  assert assert_eval(eng, "double(5)") == JsNumber(Finite(10.0))

  let restored =
    roundtrip(eng)
    |> engine.define_fn("double", 1, fn(args, _this, state) {
      case kinds(args) {
        [JsNumber(Finite(n)), ..] -> #(state, Ok(num(n *. 2.0)))
        _ -> #(state, Ok(mk_undefined()))
      }
    })

  assert assert_eval(restored, "double(5)") == JsNumber(Finite(10.0))
}

fn with_import_hook(
  eng: engine.Engine(host),
  source: String,
) -> engine.Engine(host) {
  let #(eng, Nil) =
    engine.with_state(eng, fn(s) {
      let agent =
        module_host.install_import_hook(
          s.agent,
          "/main.js",
          fn(raw, _referrer) { Ok(raw) },
          fn(_resolved) { Ok(source) },
        )
      #(State(..s, agent:), Nil)
    })
  eng
}

pub fn serialize_host_fn_reregister_around_import_hook_test() {
  let double = fn(args, _this, state) {
    case kinds(args) {
      [JsNumber(Finite(n)), ..] -> #(state, Ok(num(n *. 2.0)))
      _ -> #(state, Ok(mk_undefined()))
    }
  }
  let negate = fn(args, _this, state) {
    case kinds(args) {
      [JsNumber(Finite(n)), ..] -> #(state, Ok(num(0.0 -. n)))
      _ -> #(state, Ok(mk_undefined()))
    }
  }
  let eng =
    engine.new()
    |> engine.define_fn("double", 1, double)
    |> with_import_hook("export const tag = 'before';")
    |> engine.define_fn("negate", 1, negate)
  let assert Ok(#(_, eng)) = engine.eval(eng, "var d = double, n = negate;")
  assert assert_eval(eng, "d(5)") == JsNumber(Finite(10.0))
  assert assert_eval(eng, "n(5)") == JsNumber(Finite(-5.0))

  let restored =
    roundtrip(eng)
    |> with_import_hook("export const tag = 'after';")
    |> engine.define_fn("double", 1, double)
    |> engine.define_fn("negate", 1, negate)
  assert assert_eval(restored, "d(5)") == JsNumber(Finite(10.0))
  assert assert_eval(restored, "n(5)") == JsNumber(Finite(-5.0))
  let assert Ok(#(Returned(_), restored)) =
    engine.eval(
      restored,
      "var tag = 'unset'; import('/m.js').then(ns => { tag = ns.tag; });",
    )
  assert assert_eval(restored, "tag") == JsString("after")
}

pub fn import_without_hook_rejects_after_deserialize_test() {
  let eng = with_import_hook(engine.new(), "export const tag = 'x';")
  let restored = roundtrip(eng)
  let assert Ok(#(Returned(_), restored)) =
    engine.eval(
      restored,
      "var msg = 'unset'; import('/m.js').catch(e => { msg = e.message; });",
    )
  assert assert_eval(restored, "msg")
    == JsString("Dynamic import is not supported in this context")
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
  let assert Ok(#(_, restored)) =
    engine.eval(restored, "var q = new Point(10, 20)")
  assert assert_eval(restored, "q.sum()") == JsNumber(Finite(30.0))
}

pub fn define_fn_callable_from_js_test() {
  let eng =
    engine.new()
    |> engine.define_fn("double", 1, fn(args, _this, state) {
      case kinds(args) {
        [JsNumber(Finite(n)), ..] -> #(state, Ok(num(n *. 2.0)))
        _ -> #(state, Ok(mk_undefined()))
      }
    })

  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, "double(21)")
  assert engine.classify(value) == JsNumber(Finite(42.0))
}

pub fn define_fn_has_name_and_length_test() {
  let eng =
    engine.new()
    |> engine.define_fn("myFunc", 3, fn(_args, _this, state) {
      #(state, Ok(mk_undefined()))
    })

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "myFunc.name + ':' + myFunc.length")
  assert engine.classify(value) == JsString("myFunc:3")
}

pub fn define_namespace_creates_object_with_methods_test() {
  let eng =
    engine.new()
    |> engine.define_namespace("math2", [
      #("square", 1, fn(args, _this, state) {
        case kinds(args) {
          [JsNumber(Finite(n)), ..] -> #(state, Ok(num(n *. n)))
          _ -> #(state, Ok(mk_undefined()))
        }
      }),
      #("cube", 1, fn(args, _this, state) {
        case kinds(args) {
          [JsNumber(Finite(n)), ..] -> #(state, Ok(num(n *. n *. n)))
          _ -> #(state, Ok(mk_undefined()))
        }
      }),
    ])

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "math2.square(4) + math2.cube(2)")
  assert engine.classify(value) == JsNumber(Finite(24.0))
}

pub fn define_namespace_has_tostringtag_test() {
  let eng =
    engine.new()
    |> engine.define_namespace("widgets", [
      #("noop", 0, fn(_args, _this, state) { #(state, Ok(mk_undefined())) }),
    ])

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "Object.prototype.toString.call(widgets)")
  assert engine.classify(value) == JsString("[object widgets]")
}

pub fn define_global_installs_value_test() {
  let eng =
    engine.new()
    |> engine.define_global("MY_CONST", mk_string("hello"))

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "MY_CONST + ' world'")
  assert engine.classify(value) == JsString("hello world")
}

pub fn host_fn_receives_this_test() {
  let eng =
    engine.new()
    |> engine.define_fn("whoami", 0, fn(_args, this, state) {
      case engine.classify(this) {
        JsString(s) -> #(state, Ok(mk_string("this=" <> s)))
        _ -> #(state, Ok(mk_string("this=other")))
      }
    })

  let assert Ok(#(Returned(value:), _)) = engine.eval(eng, "whoami.call('abc')")
  assert engine.classify(value) == JsString("this=abc")
}

pub fn host_fn_can_throw_test() {
  let eng =
    engine.new()
    |> engine.define_fn("boom", 0, fn(_args, _this, state) {
      #(state, Error(mk_string("kaboom")))
    })

  let assert Ok(#(Returned(value:), _)) =
    engine.eval(eng, "try { boom() } catch (e) { 'caught:' + e }")
  assert engine.classify(value) == JsString("caught:kaboom")
}

fn reject_imports(_raw: String, _parent: String) {
  Error(load_error.ResolveForbidden)
}

fn reject_loads(_resolved: String) {
  Error(load_error.LoadForbidden)
}

fn read_export(eng, ns, name: String) -> option.Option(JsValueKind) {
  engine.read_export(eng, ns, name) |> option.map(engine.classify)
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
  let assert ModuleReturned(namespace: ns, ..) = evaluated
  assert read_export(eng, ns, "answer") == Some(JsNumber(Finite(42.0)))
  assert read_export(eng, ns, "missing") == option.None
}

pub fn call_export_threads_module_state_test() {
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
  let assert ModuleReturned(namespace: ns, ..) = evaluated
  let assert Some(bump) = engine.read_export(eng, ns, "bump")

  let assert #(Returned(value:), eng) =
    engine.call(eng, bump, mk_undefined(), [num(5.0)])
  assert engine.classify(value) == JsNumber(Finite(5.0))

  let assert #(Returned(value:), _eng) =
    engine.call(eng, bump, mk_undefined(), [num(3.0)])
  assert engine.classify(value) == JsNumber(Finite(8.0))
}

pub fn destructured_declaration_exports_test() {
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
  let assert ModuleReturned(namespace: ns, ..) = evaluated
  assert read_export(eng, ns, "sum") == Some(JsNumber(Finite(13.0)))
  assert read_export(eng, ns, "rest") == Some(JsNumber(Finite(3.0)))
  assert read_export(eng, ns, "gap") == Some(JsNumber(Finite(1.0)))
  assert read_export(eng, ns, "keys") == Some(JsString("a,c,r,x,y"))
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
  assert engine.eval_error_message(err) != ""
}

fn fmt_engine() -> engine.Engine(host) {
  engine.new()
  |> engine.define_fn("fmt", 0, fn(args, _this, s) {
    let #(line, agent) = console.format(s.agent, args)
    #(State(..s, agent:), Ok(mk_string(line)))
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
  assert assert_eval(eng, "fmt('raw', 1)") == JsString("raw 1")
}

pub fn console_format_specifiers_test() {
  let eng = fmt_engine()
  assert assert_eval(eng, "fmt('hi %s!', 'world')") == JsString("hi world!")
  assert assert_eval(eng, "fmt('%d/%i', 42.9, '7.5')") == JsString("42.9/7")
  assert assert_eval(eng, "fmt('%f', 3.14)") == JsString("3.14")
  assert assert_eval(eng, "fmt('%o', {x: 1})") == JsString("{ x: 1 }")
  assert assert_eval(eng, "fmt('%% done', 0)") == JsString("% done 0")
}

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
  assert assert_eval(eng, "fmt('100%')") == JsString("100%")
  assert assert_eval(eng, "fmt('100%', 'x')") == JsString("100% x")
  assert assert_eval(eng, "fmt('%d %d %d', 'abc', 5)") == JsString("NaN 5 %d")
  assert assert_eval(eng, "fmt('a%zb', 1)") == JsString("a%zb 1")
  assert assert_eval(eng, "fmt('a%cb', 'color:red')") == JsString("ab")
  assert assert_eval(eng, "fmt('x=%d', 1, {y: 2})") == JsString("x=1 { y: 2 }")
}

pub fn console_format_specifier_throw_propagates_test() {
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
  assert assert_eval(
      eng,
      "try { console.log('%s', {toString(){ throw 4 }}); 'no throw' }
       catch (e) { e }",
    )
    == JsNumber(Finite(4.0))
  assert assert_eval(
      eng,
      "try { fmt('%O', {toString(){ throw 1 }}); 'no throw' } catch (e) { e }",
    )
    == JsString("no throw")
  // %i/%f coerce via tostring, valueof never runs
  assert assert_eval(
      eng,
      "fmt('%i %f', {valueOf(){ throw 6 }}, {valueOf(){ throw 7 }})",
    )
    == JsString("NaN NaN")
}

pub fn console_format_symbol_never_throws_test() {
  let eng = fmt_engine()
  assert assert_eval(eng, "fmt('<%s>', Symbol('x'))") == JsString("<Symbol(x)>")
  assert assert_eval(eng, "fmt('<%s>', Symbol())") == JsString("<Symbol()>")
  assert assert_eval(eng, "fmt('<%s>', Symbol.iterator)")
    == JsString("<Symbol(Symbol.iterator)>")
  assert assert_eval(eng, "fmt('%d %i %f', Symbol(), Symbol(), Symbol())")
    == JsString("NaN NaN NaN")
}

pub fn console_format_bigint_never_throws_test() {
  let eng = fmt_engine()
  assert assert_eval(eng, "fmt('%d %i %f', 1n, 2n, 3n)") == JsString("1n 2n 3")
  assert assert_eval(eng, "fmt('%s', -42n)") == JsString("-42")
  assert assert_eval(eng, "fmt('%d', -42n)") == JsString("-42n")
}

pub fn promise_job_user_capability_resolve_throw_test() {
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

pub fn no_arc_global_by_default_test() {
  let eng = engine.new()
  assert assert_eval(eng, "typeof Arc") == JsString("undefined")
}

pub fn rest_params_survive_call_in_default_expr_test() {
  let eng = engine.new()
  assert assert_eval(
      eng,
      "function g() { return 1; }
       function f(a = g(), ...rest) { return rest.join(','); }
       f(undefined, 2, 3)",
    )
    == JsString("2,3")
}

pub fn regexp_flags_canonical_order_test() {
  let eng = engine.new()
  assert assert_eval(eng, "(/abc/gi).flags") == JsString("gi")
  assert assert_eval(eng, "(/abc/yusmigd).flags") == JsString("dgimsuy")
  assert assert_eval(eng, "new RegExp('abc', 'mig').flags") == JsString("gim")
}

fn eval_then_read_log(source: String) -> JsValueKind {
  let eng = engine.new()
  let assert Ok(#(Returned(_), eng)) = engine.eval(eng, source)
  assert_eval(eng, "log.join('|')")
}

pub fn async_generator_return_runs_finally_test() {
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

// rejects with typeerror per jsc/engine262; v8 differs
pub fn async_yield_star_missing_return_rejected_value_test() {
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
         log.push('rej:' + (e instanceof TypeError));
       });",
    )
  assert log == JsString("n:x,false|fin|rej:true")
}

pub fn async_yield_star_delegated_return_runs_outer_finally_test() {
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

pub fn sync_yield_star_delegated_return_runs_outer_finally_test() {
  let log =
    eval_then_read_log(
      "var log = [];
       var inner = {};
       inner[Symbol.iterator] = function () {
         return {
           next: function () { return { value: 'i', done: false }; },
           return: function (v) {
             log.push('inner-return:' + v);
             return { value: v, done: true };
           }
         };
       };
       function* g() {
         try { yield* inner; } finally { log.push('outer-fin'); }
       }
       var it = g();
       it.next();
       var r = it.return('z');
       log.push('r:' + r.value + ',' + r.done);",
    )
  assert log == JsString("inner-return:z|outer-fin|r:z,true")
}

pub fn eval_child_preserves_symbol_tables_test() {
  let eng = engine.new()
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
