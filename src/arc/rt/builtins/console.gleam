//// The `console` global (WHATWG Console).
////
//// Faithful port of arc/vm/builtins/console.gleam over the threaded Agent.
//// Output goes through `HostHooks.print` with the method's level; the
//// default hook writes log/info/debug to stdout and warn/error to stderr.
//// Return-tuple order is `#(JsVal, Agent)` (R1); a user
//// `toString`/`valueOf` throw from a %s/%d specifier diverges via `t_throw`
//// inside `t_to_string`/`t_to_number` (D7) — nothing is written.

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

/// Build the `console` global per WHATWG Console.
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

/// Per-module dispatch for the `console` global.
pub fn dispatch(
  st: Agent,
  native: ConsoleNative,
  _this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let ConsolePrint(level:) = native
  print(st, level, args)
}

/// WHATWG Console §2.1 Logger — format `args` then hand the line to
/// `HostHooks.print`. Formatting runs user code (`toString`/`valueOf` via
/// %s/%d/%i/%f), so it can throw; a throw aborts the log — nothing is
/// written — and diverges out of `console.log`, matching Node.
pub fn print(
  st: Agent,
  level: ConsoleLevel,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(line, st) = format(st, args)
  st.hooks.print(level, line)
  #(mk_undefined(), st)
}

/// Format `args` to the string a console method would print, without the I/O.
/// Public so tests can assert formatting independent of the print hook.
pub fn format(st: Agent, args: List(JsVal)) -> #(String, Agent) {
  case args {
    // §2.1 step 4: only run Formatter if first is a string AND there are more
    // args. `console.log("100%")` must print `100%`, not consume the `%`.
    [first, next, ..rest] ->
      case classify(first) {
        KStr(fmt) -> formatter(st, fmt, [next, ..rest], "")
        _ -> #(list.map(args, display(st, _)) |> string.join(" "), st)
      }
    _ -> #(list.map(args, display(st, _)) |> string.join(" "), st)
  }
}

/// WHATWG Console §2.2.1 Formatter. Walk `fmt` consuming one arg per
/// specifier, then append leftover args space-separated. Supports
/// `%s %d %i %f %o %O %c %%`; unknown `%x` is left literal.
fn formatter(
  st: Agent,
  fmt: String,
  args: List(JsVal),
  acc: String,
) -> #(String, Agent) {
  case string.pop_grapheme(fmt) {
    Error(Nil) -> {
      // §2.2 step 5: leftover args are Printer'd after the formatted string.
      let trailing = list.map(args, display(st, _))
      #(string.join([acc, ..trailing], " "), st)
    }
    Ok(#("%", rest)) ->
      case string.pop_grapheme(rest) {
        // Trailing lone `%` — emit literally, keep going so leftover args
        // still get appended.
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

/// Apply one format specifier. `Some(#(sub, rest_args, st))` when recognised
/// and an arg was consumed; `None` when unknown or no arg left.
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
        // %s on a Symbol is Call(%String%) — descriptive string, never throw.
        KSym(id) -> Some(#(rt_types.symbol_descriptive_string(id), rest, st))
        _ -> {
          let #(s, st) = rt_val.t_to_string(st, head)
          Some(#(s, rest, st))
        }
      }
    "d", [head, ..rest] | "i", [head, ..rest] ->
      case classify(head) {
        KSym(_) -> Some(#("NaN", rest, st))
        // BigInt under %d/%i renders as "<n>n" (Node), never throws.
        KBig(n) -> Some(#(int.to_string(n) <> "n", rest, st))
        _ -> {
          let #(n, st) = case sp {
            // %i is %parseInt% — ToString-only coercion.
            "i" -> global_fns.parse_int_value(st, head, mk_number(JInt(10)))
            // %d is Number() — ToNumber; user valueOf runs.
            _ -> rt_val.t_to_number(st, head)
          }
          Some(#(number_substitution(n), rest, st))
        }
      }
    "f", [head, ..rest] ->
      case classify(head) {
        KSym(_) -> Some(#("NaN", rest, st))
        _ -> {
          // %f is %parseFloat% — ToString-only coercion.
          let #(n, st) = global_fns.parse_float_value(st, head)
          Some(#(number_substitution(n), rest, st))
        }
      }
    "o", [head, ..rest] | "O", [head, ..rest] ->
      Some(#(inspect.inspect(st, head), rest, st))
    // %c is CSS styling — meaningless on a terminal, so it consumes its arg
    // and emits nothing, like Node.
    "c", [_, ..rest] -> Some(#("", rest, st))
    _, _ -> None
  }
}

/// Render the number a %d/%i/%f specifier coerced to — Node's `formatNumber`.
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

/// "Optimally useful" rendering for one Printer arg. Top-level strings are
/// raw (no quotes — `console.log("a")` prints `a`); everything else uses the
/// REPL inspector so objects/arrays read as `{ a: 1 }` / `[1, 2]` instead of
/// `[object Object]`.
fn display(st: Agent, val: JsVal) -> String {
  case classify(val) {
    KStr(s) -> s
    _ -> inspect.inspect(st, val)
  }
}
