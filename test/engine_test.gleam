import arc/engine
import arc/vm/completion.{NormalCompletion}
import arc/vm/value.{Finite, JsNumber, JsString, JsUndefined}

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
