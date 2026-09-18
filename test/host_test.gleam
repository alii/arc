import arc/compiler
import arc/host
import arc/interp/entry
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion}
import arc/rt/inspect as rt_inspect
import arc/rt/types.{
  type Agent, type JsVal, JFloat, JInt, KBool, KNum, KStr, classify, mk_int,
  mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/option
import gleam/string
import rt_helpers

fn new_state() -> host.Context(host) {
  rt_builtins.new_agent(rt_helpers.quiet_hooks())
  |> entry.link
  |> host.from_agent(host.new_brand())
}

fn run(ctx: host.Context(host), source: String) -> #(Completion(JsVal), Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compiler.compile_script(body, sb)
    as { "compile failed: " <> source }
  let #(completion, st) = entry.run_script(ctx.agent, template)
  #(completion, rt_async.drain(st))
}

fn eval_value(ctx: host.Context(host), source: String) -> JsVal {
  case run(ctx, source) {
    #(NormalCompletion(v), _) -> v
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

fn eval_string(ctx: host.Context(host), source: String) -> String {
  let v = eval_value(ctx, source)
  case classify(v) {
    KStr(str) -> str
    other -> panic as { source <> " gave " <> string.inspect(other) }
  }
}

fn eval_number(ctx: host.Context(host), source: String) -> Float {
  let v = eval_value(ctx, source)
  case classify(v) {
    KNum(JInt(i)) -> int.to_float(i)
    KNum(JFloat(f)) -> f
    other -> panic as { source <> " gave " <> string.inspect(other) }
  }
}

fn eval_bool(ctx: host.Context(host), source: String) -> Bool {
  let v = eval_value(ctx, source)
  case classify(v) {
    KBool(b) -> b
    other -> panic as { source <> " gave " <> string.inspect(other) }
  }
}

fn extract_error_message(ctx: host.Context(host), source: String) -> String {
  eval_string(ctx, "try { " <> source <> " } catch (e) { e.message }")
}

fn state_with_validator(name, validate) -> host.Context(host) {
  new_state()
  |> host.define_fn(name, 1, fn(ctx, args, _) {
    case args {
      [v, ..] -> validate(v, ctx)
      _ -> #(Ok(mk_undefined()), ctx)
    }
  })
}

pub fn validate_string_accepts_string_test() {
  let ctx =
    state_with_validator("upper", fn(v, ctx) {
      use str, ctx <- host.validate_string(ctx, v, "input")
      #(Ok(mk_string(string.uppercase(str))), ctx)
    })
  assert eval_string(ctx, "upper('abc')") == "ABC"
}

pub fn validate_string_rejects_number_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use _, ctx <- host.validate_string(ctx, v, "name")
      #(Ok(mk_undefined()), ctx)
    })
  assert extract_error_message(ctx, "f(42)")
    == "The \"name\" argument must be of type string. Received type number"
}

pub fn validate_string_rejects_null_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use _, ctx <- host.validate_string(ctx, v, "name")
      #(Ok(mk_undefined()), ctx)
    })
  assert extract_error_message(ctx, "f(null)")
    == "The \"name\" argument must be of type string. Received type object"
}

pub fn validate_function_accepts_arrow_test() {
  let ctx =
    state_with_validator("callIt", fn(v, ctx) {
      use cb, ctx <- host.validate_function(ctx, v, "callback")
      host.try_call(ctx, cb, "callback", mk_undefined(), [], fn(r, ctx) {
        #(Ok(r), ctx)
      })
    })
  assert eval_number(ctx, "callIt(() => 42)") == 42.0
}

pub fn validate_function_rejects_string_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use _, ctx <- host.validate_function(ctx, v, "callback")
      #(Ok(mk_undefined()), ctx)
    })
  assert extract_error_message(ctx, "f('nope')")
    == "The \"callback\" argument must be of type function. Received type string"
}

pub fn validate_function_accepts_builtin_test() {
  let ctx =
    state_with_validator("check", fn(v, ctx) {
      use _, ctx <- host.validate_function(ctx, v, "fn")
      #(Ok(mk_string("ok")), ctx)
    })
  assert eval_string(ctx, "check(Math.abs)") == "ok"
}

pub fn validate_integer_accepts_in_range_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use n, ctx <- host.validate_integer(ctx, v, "port", 0, 65_535)
      #(Ok(mk_int(n)), ctx)
    })
  assert eval_number(ctx, "f(8080)") == 8080.0
}

pub fn validate_integer_rejects_out_of_range_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use _, ctx <- host.validate_integer(ctx, v, "port", 0, 65_535)
      #(Ok(mk_undefined()), ctx)
    })
  assert extract_error_message(ctx, "f(70000)")
    == "The value of \"port\" is out of range. It must be >= 0 and <= 65535. Received 70000"
}

pub fn validate_integer_rejects_float_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use _, ctx <- host.validate_integer(ctx, v, "n", 0, 100)
      #(Ok(mk_undefined()), ctx)
    })
  assert extract_error_message(ctx, "f(3.14)")
    == "The value of \"n\" is out of range. It must be an integer. Received 3.14"
  assert extract_error_message(ctx, "f(NaN)")
    == "The value of \"n\" is out of range. It must be an integer. Received NaN"
  assert extract_error_message(ctx, "f(Infinity)")
    == "The value of \"n\" is out of range. It must be an integer. Received Infinity"
  assert eval_string(
      ctx,
      "try { f(3.14) } catch (e) { e instanceof RangeError ? 'range' : 'other' }",
    )
    == "range"
}

pub fn validate_integer_rejects_non_number_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use _, ctx <- host.validate_integer(ctx, v, "n", 0, 100)
      #(Ok(mk_undefined()), ctx)
    })
  assert extract_error_message(ctx, "f('3')")
    == "The \"n\" argument must be of type integer. Received type string"
  assert eval_string(
      ctx,
      "try { f('3') } catch (e) { e instanceof TypeError ? 'type' : 'other' }",
    )
    == "type"
}

pub fn validate_integer_range_error_is_rangeerror_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use _, ctx <- host.validate_integer(ctx, v, "n", 0, 10)
      #(Ok(mk_undefined()), ctx)
    })
  assert eval_string(
      ctx,
      "try { f(99) } catch (e) { e instanceof RangeError ? 'range' : 'other' }",
    )
    == "range"
}

pub fn try_call_invokes_callable_test() {
  let ctx =
    new_state()
    |> host.define_fn("apply", 2, fn(ctx, args, _) {
      case args {
        [cb, x, ..] -> {
          use result, ctx <- host.try_call(ctx, cb, "fn", mk_undefined(), [x])
          #(Ok(result), ctx)
        }
        _ -> #(Ok(mk_undefined()), ctx)
      }
    })
  assert eval_number(ctx, "apply(x => x + 1, 9)") == 10.0
}

pub fn try_call_rejects_noncallable_with_arg_name_test() {
  let ctx =
    new_state()
    |> host.define_fn("apply", 2, fn(ctx, args, _) {
      case args {
        [cb, x, ..] -> {
          use result, ctx <- host.try_call(ctx, cb, "fn", mk_undefined(), [x])
          #(Ok(result), ctx)
        }
        _ -> #(Ok(mk_undefined()), ctx)
      }
    })
  assert extract_error_message(ctx, "apply(42, 1)")
    == "The \"fn\" argument must be of type function. Received type number"
}

pub fn try_call_propagates_callback_throw_test() {
  let ctx =
    state_with_validator("apply", fn(cb, ctx) {
      use result, ctx <- host.try_call(ctx, cb, "fn", mk_undefined(), [])
      #(Ok(result), ctx)
    })
  assert eval_string(
      ctx,
      "try { apply(() => { throw new Error('from cb') }) } catch (e) { e.message }",
    )
    == "from cb"
}

pub fn validate_boolean_accepts_true_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use b, ctx <- host.validate_boolean(ctx, v, "flag")
      #(
        Ok(
          mk_string(case b {
            True -> "yes"
            False -> "no"
          }),
        ),
        ctx,
      )
    })
  assert eval_string(ctx, "f(true)") == "yes"
}

pub fn validate_boolean_rejects_truthy_test() {
  let ctx =
    state_with_validator("f", fn(v, ctx) {
      use _, ctx <- host.validate_boolean(ctx, v, "flag")
      #(Ok(mk_undefined()), ctx)
    })
  assert extract_error_message(ctx, "f(1)")
    == "The \"flag\" argument must be of type boolean. Received type number"
}

pub fn array_builds_real_js_array_test() {
  let ctx =
    state_with_validator("triple", fn(v, ctx) {
      let #(arr, ctx) = host.array(ctx, [v, v, v])
      #(Ok(arr), ctx)
    })
  assert eval_string(ctx, "Array.isArray(triple(7)) && triple(7).join('-')")
    == "7-7-7"
}

pub fn object_builds_plain_object_test() {
  let ctx =
    new_state()
    |> host.define_fn("point", 2, fn(ctx, args, _) {
      case args {
        [x, y, ..] -> {
          let #(obj, ctx) = host.object(ctx, [#("x", x), #("y", y)])
          #(Ok(obj), ctx)
        }
        _ -> #(Ok(mk_undefined()), ctx)
      }
    })
  assert eval_string(ctx, "let p = point(3, 4); p.x + ',' + p.y") == "3,4"
}

fn to_string(
  ctx: host.Context(host),
  v: JsVal,
) -> #(String, host.Context(host)) {
  let #(str, st) = rt_val.to_string(ctx.agent, v)
  #(str, host.Context(..ctx, agent: st))
}

pub fn to_string_coerces_number_test() {
  let ctx =
    state_with_validator("str", fn(v, ctx) {
      let #(str, ctx) = to_string(ctx, v)
      #(Ok(mk_string("got:" <> str)), ctx)
    })
  assert eval_string(ctx, "str(42)") == "got:42"
}

pub fn to_string_calls_user_tostring_test() {
  let ctx =
    state_with_validator("str", fn(v, ctx) {
      let #(str, ctx) = to_string(ctx, v)
      #(Ok(mk_string(str)), ctx)
    })
  assert eval_string(ctx, "str({ toString() { return 'custom' } })") == "custom"
}

pub fn to_string_propagates_throw_test() {
  let ctx =
    state_with_validator("str", fn(v, ctx) {
      let #(str, ctx) = to_string(ctx, v)
      #(Ok(mk_string(str)), ctx)
    })
  let assert #(ThrowCompletion(_), _) =
    run(ctx, "str({ toString() { throw new Error('nope') } })")
}

type MyHost {
  Pid(Int)
  Socket(String)
}

pub fn host_object_typed_roundtrip_test() {
  let ctx: host.Context(MyHost) =
    new_state()
    |> host.define_fn("makePid", 0, fn(ctx, _args, _this) {
      let #(val, ctx) = host.alloc_host_object(ctx, Pid(42), option.None)
      #(Ok(val), ctx)
    })
    |> host.define_fn("readHost", 1, fn(ctx, args, _this) {
      case host.read_host(ctx, host.first_arg(args)) {
        option.Some(Pid(n)) -> #(Ok(mk_int(n)), ctx)
        option.Some(Socket(name)) -> #(Ok(mk_string("socket:" <> name)), ctx)
        option.None -> #(Ok(mk_string("not-a-host-object")), ctx)
      }
    })

  assert eval_number(ctx, "readHost(makePid())") == 42.0
  assert eval_string(ctx, "readHost({})") == "not-a-host-object"
  assert eval_bool(ctx, "var p = makePid(); p === p") == True
}
