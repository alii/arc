import arc/compiler
import arc/host
import arc/interp/entry
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion}
import arc/rt/inspect as rt_inspect
import arc/rt/types.{
  type Agent, type JsVal, JFloat, JInt, KBool, KNum, KStr, classify, mk_number,
  mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/option
import gleam/string
import rt_helpers

fn new_state() -> host.State(host) {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
  |> entry.link
  |> host.from_agent(host.new_key())
}

fn run(s: host.State(host), source: String) -> #(Completion, Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compiler.compile(body, sb)
    as { "compile failed: " <> source }
  let #(completion, st) = entry.run_script(s.agent, template)
  #(completion, rt_async.drain(st))
}

fn eval_value(s: host.State(host), source: String) -> JsVal {
  case run(s, source) {
    #(NormalCompletion(v), _) -> v
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

fn eval_string(s: host.State(host), source: String) -> String {
  let v = eval_value(s, source)
  case classify(v) {
    KStr(str) -> str
    other -> panic as { source <> " gave " <> string.inspect(other) }
  }
}

fn eval_number(s: host.State(host), source: String) -> Float {
  let v = eval_value(s, source)
  case classify(v) {
    KNum(JInt(i)) -> int.to_float(i)
    KNum(JFloat(f)) -> f
    other -> panic as { source <> " gave " <> string.inspect(other) }
  }
}

fn eval_bool(s: host.State(host), source: String) -> Bool {
  let v = eval_value(s, source)
  case classify(v) {
    KBool(b) -> b
    other -> panic as { source <> " gave " <> string.inspect(other) }
  }
}

fn extract_error_message(s: host.State(host), source: String) -> String {
  eval_string(s, "try { " <> source <> " } catch (e) { e.message }")
}

fn state_with_validator(name, validate) -> host.State(host) {
  new_state()
  |> host.define_fn(name, 1, fn(args, _, s) {
    case args {
      [v, ..] -> validate(v, s)
      _ -> #(s, Ok(mk_undefined()))
    }
  })
}

pub fn validate_string_accepts_string_test() {
  let s =
    state_with_validator("upper", fn(v, s) {
      use str, s <- host.validate_string(s, v, "input")
      #(s, Ok(mk_string(string.uppercase(str))))
    })
  assert eval_string(s, "upper('abc')") == "ABC"
}

pub fn validate_string_rejects_number_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use _, s <- host.validate_string(s, v, "name")
      #(s, Ok(mk_undefined()))
    })
  assert extract_error_message(s, "f(42)")
    == "The \"name\" argument must be of type string. Received type number"
}

pub fn validate_string_rejects_null_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use _, s <- host.validate_string(s, v, "name")
      #(s, Ok(mk_undefined()))
    })
  assert extract_error_message(s, "f(null)")
    == "The \"name\" argument must be of type string. Received type object"
}

pub fn validate_function_accepts_arrow_test() {
  let s =
    state_with_validator("callIt", fn(v, s) {
      use cb, s <- host.validate_function(s, v, "callback")
      host.try_call(s, cb, "callback", mk_undefined(), [], fn(r, s) {
        #(s, Ok(r))
      })
    })
  assert eval_number(s, "callIt(() => 42)") == 42.0
}

pub fn validate_function_rejects_string_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use _, s <- host.validate_function(s, v, "callback")
      #(s, Ok(mk_undefined()))
    })
  assert extract_error_message(s, "f('nope')")
    == "The \"callback\" argument must be of type function. Received type string"
}

pub fn validate_function_accepts_builtin_test() {
  let s =
    state_with_validator("check", fn(v, s) {
      use _, s <- host.validate_function(s, v, "fn")
      #(s, Ok(mk_string("ok")))
    })
  assert eval_string(s, "check(Math.abs)") == "ok"
}

pub fn validate_integer_accepts_in_range_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use n, s <- host.validate_integer(s, v, "port", 0, 65_535)
      #(s, Ok(mk_number(JInt(n))))
    })
  assert eval_number(s, "f(8080)") == 8080.0
}

pub fn validate_integer_rejects_out_of_range_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use _, s <- host.validate_integer(s, v, "port", 0, 65_535)
      #(s, Ok(mk_undefined()))
    })
  assert extract_error_message(s, "f(70000)")
    == "The value of \"port\" is out of range. It must be >= 0 and <= 65535. Received 70000"
}

pub fn validate_integer_rejects_float_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use _, s <- host.validate_integer(s, v, "n", 0, 100)
      #(s, Ok(mk_undefined()))
    })
  assert extract_error_message(s, "f(3.14)")
    == "The value of \"n\" is out of range. It must be an integer. Received 3.14"
  assert extract_error_message(s, "f(NaN)")
    == "The value of \"n\" is out of range. It must be an integer. Received NaN"
  assert extract_error_message(s, "f(Infinity)")
    == "The value of \"n\" is out of range. It must be an integer. Received Infinity"
  assert eval_string(
      s,
      "try { f(3.14) } catch (e) { e instanceof RangeError ? 'range' : 'other' }",
    )
    == "range"
}

pub fn validate_integer_rejects_non_number_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use _, s <- host.validate_integer(s, v, "n", 0, 100)
      #(s, Ok(mk_undefined()))
    })
  assert extract_error_message(s, "f('3')")
    == "The \"n\" argument must be of type integer. Received type string"
  assert eval_string(
      s,
      "try { f('3') } catch (e) { e instanceof TypeError ? 'type' : 'other' }",
    )
    == "type"
}

pub fn validate_integer_range_error_is_rangeerror_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use _, s <- host.validate_integer(s, v, "n", 0, 10)
      #(s, Ok(mk_undefined()))
    })
  assert eval_string(
      s,
      "try { f(99) } catch (e) { e instanceof RangeError ? 'range' : 'other' }",
    )
    == "range"
}

pub fn try_call_invokes_callable_test() {
  let s =
    new_state()
    |> host.define_fn("apply", 2, fn(args, _, s) {
      case args {
        [cb, x, ..] -> {
          use result, s <- host.try_call(s, cb, "fn", mk_undefined(), [x])
          #(s, Ok(result))
        }
        _ -> #(s, Ok(mk_undefined()))
      }
    })
  assert eval_number(s, "apply(x => x + 1, 9)") == 10.0
}

pub fn try_call_rejects_noncallable_with_arg_name_test() {
  let s =
    new_state()
    |> host.define_fn("apply", 2, fn(args, _, s) {
      case args {
        [cb, x, ..] -> {
          use result, s <- host.try_call(s, cb, "fn", mk_undefined(), [x])
          #(s, Ok(result))
        }
        _ -> #(s, Ok(mk_undefined()))
      }
    })
  assert extract_error_message(s, "apply(42, 1)")
    == "The \"fn\" argument must be of type function. Received type number"
}

pub fn try_call_propagates_callback_throw_test() {
  let s =
    state_with_validator("apply", fn(cb, s) {
      use result, s <- host.try_call(s, cb, "fn", mk_undefined(), [])
      #(s, Ok(result))
    })
  assert eval_string(
      s,
      "try { apply(() => { throw new Error('from cb') }) } catch (e) { e.message }",
    )
    == "from cb"
}

pub fn validate_boolean_accepts_true_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use b, s <- host.validate_boolean(s, v, "flag")
      #(
        s,
        Ok(
          mk_string(case b {
            True -> "yes"
            False -> "no"
          }),
        ),
      )
    })
  assert eval_string(s, "f(true)") == "yes"
}

pub fn validate_boolean_rejects_truthy_test() {
  let s =
    state_with_validator("f", fn(v, s) {
      use _, s <- host.validate_boolean(s, v, "flag")
      #(s, Ok(mk_undefined()))
    })
  assert extract_error_message(s, "f(1)")
    == "The \"flag\" argument must be of type boolean. Received type number"
}

pub fn array_builds_real_js_array_test() {
  let s =
    state_with_validator("triple", fn(v, s) {
      let #(s, arr) = host.array(s, [v, v, v])
      #(s, Ok(arr))
    })
  assert eval_string(s, "Array.isArray(triple(7)) && triple(7).join('-')")
    == "7-7-7"
}

pub fn object_builds_plain_object_test() {
  let s =
    new_state()
    |> host.define_fn("point", 2, fn(args, _, s) {
      case args {
        [x, y, ..] -> {
          let #(s, obj) = host.object(s, [#("x", x), #("y", y)])
          #(s, Ok(obj))
        }
        _ -> #(s, Ok(mk_undefined()))
      }
    })
  assert eval_string(s, "let p = point(3, 4); p.x + ',' + p.y") == "3,4"
}

fn to_string(s: host.State(host), v: JsVal) -> #(String, host.State(host)) {
  let #(str, st) = rt_val.t_to_string(s.agent, v)
  #(str, host.State(..s, agent: st))
}

pub fn to_string_coerces_number_test() {
  let s =
    state_with_validator("str", fn(v, s) {
      let #(str, s) = to_string(s, v)
      #(s, Ok(mk_string("got:" <> str)))
    })
  assert eval_string(s, "str(42)") == "got:42"
}

pub fn to_string_calls_user_tostring_test() {
  let s =
    state_with_validator("str", fn(v, s) {
      let #(str, s) = to_string(s, v)
      #(s, Ok(mk_string(str)))
    })
  assert eval_string(s, "str({ toString() { return 'custom' } })") == "custom"
}

pub fn to_string_propagates_throw_test() {
  let s =
    state_with_validator("str", fn(v, s) {
      let #(str, s) = to_string(s, v)
      #(s, Ok(mk_string(str)))
    })
  let assert #(ThrowCompletion(_), _) =
    run(s, "str({ toString() { throw new Error('nope') } })")
}

type MyHost {
  Pid(Int)
  Socket(String)
}

pub fn host_object_typed_roundtrip_test() {
  let s: host.State(MyHost) =
    new_state()
    |> host.define_fn("makePid", 0, fn(_args, _this, s) {
      let #(s, val) = host.alloc_host_object(s, Pid(42), option.None)
      #(s, Ok(val))
    })
    |> host.define_fn("readHost", 1, fn(args, _this, s) {
      case host.read_host(s, host.first_arg(args)) {
        option.Some(Pid(n)) -> #(s, Ok(mk_number(JInt(n))))
        option.Some(Socket(name)) -> #(s, Ok(mk_string("socket:" <> name)))
        option.None -> #(s, Ok(mk_string("not-a-host-object")))
      }
    })

  assert eval_number(s, "readHost(makePid())") == 42.0
  assert eval_string(s, "readHost({})") == "not-a-host-object"
  assert eval_bool(s, "var p = makePid(); p === p") == True
}
