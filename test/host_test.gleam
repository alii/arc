import arc/engine
import arc/host
import arc/vm/completion.{NormalCompletion, ThrowCompletion}
import arc/vm/ops/coerce
import arc/vm/state
import arc/vm/value.{Finite, JsNumber, JsString, JsUndefined}
import gleam/int
import gleam/string

fn extract_error_message(eng, source) -> String {
  let assert Ok(#(NormalCompletion(value: JsString(msg), ..), _)) =
    engine.eval(eng, "try { " <> source <> " } catch (e) { e.message }")
  msg
}

/// Engine with a single host fn `name` that passes its first argument to
/// `validate`, returning `undefined` when called with no arguments.
fn engine_with_validator(name, validate) {
  engine.new()
  |> engine.define_fn(name, 1, fn(args, _, s) {
    case args {
      [v, ..] -> validate(v, s)
      _ -> #(s, Ok(JsUndefined))
    }
  })
}

fn eval_value(eng, source) {
  let assert Ok(#(NormalCompletion(value:, ..), _)) = engine.eval(eng, source)
  value
}

// -- validate_string ---------------------------------------------------------

pub fn validate_string_accepts_string_test() {
  let eng =
    engine_with_validator("upper", fn(v, s) {
      use str, s <- host.validate_string(s, v, "input")
      #(s, Ok(JsString(string.uppercase(str))))
    })
  assert eval_value(eng, "upper('abc')") == JsString("ABC")
}

pub fn validate_string_rejects_number_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use _, s <- host.validate_string(s, v, "name")
      #(s, Ok(JsUndefined))
    })
  assert extract_error_message(eng, "f(42)")
    == "The \"name\" argument must be of type string. Received type number"
}

pub fn validate_string_rejects_null_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use _, s <- host.validate_string(s, v, "name")
      #(s, Ok(JsUndefined))
    })
  assert extract_error_message(eng, "f(null)")
    == "The \"name\" argument must be of type string. Received type object"
}

// -- validate_function -------------------------------------------------------

pub fn validate_function_accepts_arrow_test() {
  let eng =
    engine_with_validator("callIt", fn(v, s) {
      use cb, s <- host.validate_function(s, v, "callback")
      state.try_call(s, cb, JsUndefined, [], fn(r, s) { #(s, Ok(r)) })
    })
  assert eval_value(eng, "callIt(() => 42)") == JsNumber(Finite(42.0))
}

pub fn validate_function_rejects_string_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use _, s <- host.validate_function(s, v, "callback")
      #(s, Ok(JsUndefined))
    })
  assert extract_error_message(eng, "f('nope')")
    == "The \"callback\" argument must be of type function. Received type string"
}

pub fn validate_function_accepts_builtin_test() {
  let eng =
    engine_with_validator("check", fn(v, s) {
      use _, s <- host.validate_function(s, v, "fn")
      #(s, Ok(JsString("ok")))
    })
  assert eval_value(eng, "check(Math.abs)") == JsString("ok")
}

// -- validate_integer --------------------------------------------------------

pub fn validate_integer_accepts_in_range_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use n, s <- host.validate_integer(s, v, "port", 0, 65_535)
      #(s, Ok(JsNumber(Finite(int.to_float(n)))))
    })
  assert eval_value(eng, "f(8080)") == JsNumber(Finite(8080.0))
}

pub fn validate_integer_rejects_out_of_range_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use _, s <- host.validate_integer(s, v, "port", 0, 65_535)
      #(s, Ok(JsUndefined))
    })
  assert extract_error_message(eng, "f(70000)")
    == "The value of \"port\" is out of range. It must be >= 0 and <= 65535. Received 70000"
}

pub fn validate_integer_rejects_float_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use _, s <- host.validate_integer(s, v, "n", 0, 100)
      #(s, Ok(JsUndefined))
    })
  assert extract_error_message(eng, "f(3.14)")
    == "The \"n\" argument must be of type integer. Received type number"
}

pub fn validate_integer_range_error_is_rangeerror_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use _, s <- host.validate_integer(s, v, "n", 0, 10)
      #(s, Ok(JsUndefined))
    })
  assert eval_value(
      eng,
      "try { f(99) } catch (e) { e instanceof RangeError ? 'range' : 'other' }",
    )
    == JsString("range")
}

// -- try_call ----------------------------------------------------------------

pub fn try_call_invokes_callable_test() {
  let eng =
    engine.new()
    |> engine.define_fn("apply", 2, fn(args, _, s) {
      case args {
        [cb, x, ..] -> {
          use result, s <- host.try_call(s, cb, "fn", JsUndefined, [x])
          #(s, Ok(result))
        }
        _ -> #(s, Ok(JsUndefined))
      }
    })
  assert eval_value(eng, "apply(x => x + 1, 9)") == JsNumber(Finite(10.0))
}

pub fn try_call_rejects_noncallable_with_arg_name_test() {
  let eng =
    engine.new()
    |> engine.define_fn("apply", 2, fn(args, _, s) {
      case args {
        [cb, x, ..] -> {
          use result, s <- host.try_call(s, cb, "fn", JsUndefined, [x])
          #(s, Ok(result))
        }
        _ -> #(s, Ok(JsUndefined))
      }
    })
  assert extract_error_message(eng, "apply(42, 1)")
    == "The \"fn\" argument must be of type function. Received type number"
}

pub fn try_call_propagates_callback_throw_test() {
  let eng =
    engine_with_validator("apply", fn(cb, s) {
      use result, s <- host.try_call(s, cb, "fn", JsUndefined, [])
      #(s, Ok(result))
    })
  assert eval_value(
      eng,
      "try { apply(() => { throw new Error('from cb') }) } catch (e) { e.message }",
    )
    == JsString("from cb")
}

// -- validate_boolean --------------------------------------------------------

pub fn validate_boolean_accepts_true_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use b, s <- host.validate_boolean(s, v, "flag")
      #(
        s,
        Ok(
          JsString(case b {
            True -> "yes"
            False -> "no"
          }),
        ),
      )
    })
  assert eval_value(eng, "f(true)") == JsString("yes")
}

pub fn validate_boolean_rejects_truthy_test() {
  let eng =
    engine_with_validator("f", fn(v, s) {
      use _, s <- host.validate_boolean(s, v, "flag")
      #(s, Ok(JsUndefined))
    })
  assert extract_error_message(eng, "f(1)")
    == "The \"flag\" argument must be of type boolean. Received type number"
}

// -- host.array --------------------------------------------------------------

pub fn array_builds_real_js_array_test() {
  let eng =
    engine_with_validator("triple", fn(v, s) {
      let #(s, arr) = host.array(s, [v, v, v])
      #(s, Ok(arr))
    })
  assert eval_value(eng, "Array.isArray(triple(7)) && triple(7).join('-')")
    == JsString("7-7-7")
}

// -- host.object -------------------------------------------------------------

pub fn object_builds_plain_object_test() {
  let eng =
    engine.new()
    |> engine.define_fn("point", 2, fn(args, _, s) {
      case args {
        [x, y, ..] -> {
          let #(s, obj) = host.object(s, [#("x", x), #("y", y)])
          #(s, Ok(obj))
        }
        _ -> #(s, Ok(JsUndefined))
      }
    })
  assert eval_value(eng, "let p = point(3, 4); p.x + ',' + p.y")
    == JsString("3,4")
}

// -- to_string (coercing) ----------------------------------------------------

pub fn to_string_coerces_number_test() {
  let eng =
    engine_with_validator("str", fn(v, s) {
      use str, s <- coerce.try_to_string(s, v)
      #(s, Ok(JsString("got:" <> str)))
    })
  assert eval_value(eng, "str(42)") == JsString("got:42")
}

pub fn to_string_calls_user_tostring_test() {
  let eng =
    engine_with_validator("str", fn(v, s) {
      use str, s <- coerce.try_to_string(s, v)
      #(s, Ok(JsString(str)))
    })
  assert eval_value(eng, "str({ toString() { return 'custom' } })")
    == JsString("custom")
}

pub fn to_string_propagates_throw_test() {
  let eng =
    engine_with_validator("str", fn(v, s) {
      use str, s <- coerce.try_to_string(s, v)
      #(s, Ok(JsString(str)))
    })
  let assert Ok(#(ThrowCompletion(..), _)) =
    engine.eval(eng, "str({ toString() { throw new Error('nope') } })")
}
