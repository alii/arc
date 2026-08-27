import arc/host_hooks.{
  type ConsoleLevel, DebugLevel, ErrorLevel, InfoLevel, LogLevel, WarnLevel,
}
import arc/rt/builtins/common
import arc/rt/builtins/global_fns
import arc/rt/inspect
import arc/rt/types.{
  type Agent, type ConsoleNative, type Handle, type JsNum, type JsVal, ConsoleN,
  ConsolePrint, JFloat, JInt, KBig, KStr, KSym, classify, mk_number,
  mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn init(
  st: Agent,
  object_proto: Handle,
  function_proto: Handle,
) -> #(Handle, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, function_proto, [
      #("log", ConsoleN(ConsolePrint(LogLevel)), 0),
      #("info", ConsoleN(ConsolePrint(InfoLevel)), 0),
      #("debug", ConsoleN(ConsolePrint(DebugLevel)), 0),
      #("warn", ConsoleN(ConsolePrint(WarnLevel)), 0),
      #("error", ConsoleN(ConsolePrint(ErrorLevel)), 0),
    ])
  common.init_namespace(st, object_proto, "console", methods)
}

pub fn dispatch(
  st: Agent,
  native: ConsoleNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let ConsolePrint(level:) = native
  print(st, level, args)
}

pub fn print(
  st: Agent,
  level: ConsoleLevel,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(line, st) = format(st, args)
  st.hooks.print(level, line)
  #(mk_undefined(), st)
}

pub fn format(st: Agent, args: List(JsVal)) -> #(String, Agent) {
  case args {
    [first, next, ..rest] ->
      case classify(first) {
        KStr(fmt) -> formatter(st, fmt, [next, ..rest], "")
        _ -> #(list.map(args, display(st, _)) |> string.join(" "), st)
      }
    _ -> #(list.map(args, display(st, _)) |> string.join(" "), st)
  }
}

// whatwg console §2.2.1 formatter
fn formatter(
  st: Agent,
  fmt: String,
  args: List(JsVal),
  acc: String,
) -> #(String, Agent) {
  case string.pop_grapheme(fmt) {
    Error(Nil) -> {
      let trailing = list.map(args, display(st, _))
      #(string.join([acc, ..trailing], " "), st)
    }
    Ok(#("%", rest)) ->
      case string.pop_grapheme(rest) {
        Error(Nil) -> formatter(st, "", args, acc <> "%")
        Ok(#(sp, rest)) ->
          case spec(st, sp, args) {
            Some(#(sub, args, st)) -> formatter(st, rest, args, acc <> sub)
            None -> formatter(st, rest, args, acc <> "%" <> sp)
          }
      }
    Ok(#(ch, rest)) -> formatter(st, rest, args, acc <> ch)
  }
}

fn spec(
  st: Agent,
  sp: String,
  args: List(JsVal),
) -> Option(#(String, List(JsVal), Agent)) {
  case sp, args {
    "%", _ -> Some(#("%", args, st))
    _, [] -> None
    "s", [head, ..rest] ->
      case classify(head) {
        KSym(id) -> Some(#(rt_types.symbol_descriptive_string(id), rest, st))
        _ -> {
          let #(s, st) = rt_val.t_to_string(st, head)
          Some(#(s, rest, st))
        }
      }
    "d", [head, ..rest] | "i", [head, ..rest] ->
      case classify(head) {
        KSym(_) -> Some(#("NaN", rest, st))
        KBig(n) -> Some(#(int.to_string(n) <> "n", rest, st))
        _ -> {
          let #(n, st) = case sp {
            "i" -> global_fns.parse_int_value(st, head, mk_number(JInt(10)))
            _ -> rt_val.t_to_number(st, head)
          }
          Some(#(number_substitution(n), rest, st))
        }
      }
    "f", [head, ..rest] ->
      case classify(head) {
        KSym(_) -> Some(#("NaN", rest, st))
        _ -> {
          let #(n, st) = global_fns.parse_float_value(st, head)
          Some(#(number_substitution(n), rest, st))
        }
      }
    "o", [head, ..rest] | "O", [head, ..rest] ->
      Some(#(inspect.inspect(st, head), rest, st))
    "c", [_, ..rest] -> Some(#("", rest, st))
    _, _ -> None
  }
}

fn number_substitution(n: JsNum) -> String {
  case n {
    JFloat(f) ->
      case rt_val.is_neg_zero(f) {
        True -> "-0"
        False -> rt_val.jsnum_to_string(n)
      }
    _ -> rt_val.jsnum_to_string(n)
  }
}

fn display(st: Agent, val: JsVal) -> String {
  case classify(val) {
    KStr(s) -> s
    _ -> inspect.inspect(st, val)
  }
}
