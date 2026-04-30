import arc/beam
import arc/engine
import arc/vm/builtins/console
import arc/vm/completion.{NormalCompletion}
import arc/vm/value.{Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined}

// ----------------------------------------------------------------------------
// Serialization — roundtrip
// ----------------------------------------------------------------------------

/// Helper: eval on engine, assert normal completion, return value.
fn assert_eval(eng: engine.Engine, source: String) -> value.JsValue {
  let assert Ok(#(NormalCompletion(value:, ..), _)) = engine.eval(eng, source)
  value
}

/// Helper: serialize then deserialize an engine.
fn roundtrip(eng: engine.Engine) -> engine.Engine {
  eng |> engine.serialize |> engine.deserialize
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
  let assert Ok(#(NormalCompletion(value:, ..), _)) =
    engine.eval(restored, "inc()")
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

  let assert Ok(#(NormalCompletion(value:, ..), _)) =
    engine.eval(eng, "double(21)")
  assert value == JsNumber(Finite(42.0))
}

pub fn define_fn_has_name_and_length_test() {
  let eng =
    engine.new()
    |> engine.define_fn("myFunc", 3, fn(_args, _this, state) {
      #(state, Ok(JsUndefined))
    })

  let assert Ok(#(NormalCompletion(value:, ..), _)) =
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

  let assert Ok(#(NormalCompletion(value:, ..), _)) =
    engine.eval(eng, "math2.square(4) + math2.cube(2)")
  assert value == JsNumber(Finite(24.0))
}

pub fn define_global_installs_value_test() {
  let eng =
    engine.new()
    |> engine.define_global("MY_CONST", JsString("hello"))

  let assert Ok(#(NormalCompletion(value:, ..), _)) =
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

  let assert Ok(#(NormalCompletion(value:, ..), _)) =
    engine.eval(eng, "whoami.call('abc')")
  assert value == JsString("this=abc")
}

pub fn host_fn_can_throw_test() {
  let eng =
    engine.new()
    |> engine.define_fn("boom", 0, fn(_args, _this, state) {
      #(state, Error(JsString("kaboom")))
    })

  let assert Ok(#(NormalCompletion(value:, ..), _)) =
    engine.eval(eng, "try { boom() } catch (e) { 'caught:' + e }")
  assert value == JsString("caught:kaboom")
}

// ----------------------------------------------------------------------------
// console — WHATWG Console §2 Logger / Formatter
// ----------------------------------------------------------------------------

/// Host fn that returns what `console.log(...args)` would print, so the
/// formatter is assertable from JS without touching stdout.
fn fmt_engine() -> engine.Engine {
  engine.new()
  |> engine.define_fn("fmt", 0, fn(args, _this, s) {
    let #(s, line) = console.format(args, s)
    #(s, Ok(JsString(line)))
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
  assert assert_eval(eng, "fmt('%d/%i', 42.9, '7.5')") == JsString("42/7")
  assert assert_eval(eng, "fmt('%f', 3.14)") == JsString("3.14")
  assert assert_eval(eng, "fmt('%o', {x: 1})") == JsString("{ x: 1 }")
  // `%%` only collapses inside Formatter, i.e. with 2+ args.
  assert assert_eval(eng, "fmt('%% done', 0)") == JsString("% done 0")
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

// ----------------------------------------------------------------------------
// arc/beam — opt-in BEAM primitives as host fns
// ----------------------------------------------------------------------------

pub fn beam_not_installed_by_default_test() {
  let eng = engine.new()
  assert assert_eval(eng, "typeof Arc") == JsString("undefined")
}

pub fn beam_install_test() {
  let eng = engine.new() |> beam.install("Arc")
  assert assert_eval(eng, "typeof Arc.spawn") == JsString("function")
  assert assert_eval(eng, "typeof Arc.subject") == JsString("function")
  assert assert_eval(eng, "Arc.peek(Promise.resolve(1)).type")
    == JsString("resolved")
}

pub fn beam_install_custom_name_test() {
  let eng = engine.new() |> beam.install("proc")
  assert assert_eval(eng, "typeof proc.self") == JsString("function")
  assert assert_eval(eng, "typeof Arc") == JsString("undefined")
}

pub fn beam_subject_roundtrip_test() {
  let eng = engine.new() |> beam.install("Arc")
  assert assert_eval(eng, "var s = Arc.subject(); s.send(42); s.receive()")
    == JsNumber(Finite(42.0))
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
