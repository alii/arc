//// `console` global per WHATWG Console.

import arc/vm/builtins/common
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type ConsoleNativeFn, type JsValue, type Ref, ConsoleLog, ConsoleLogError,
  ConsoleNative, JsNumber, JsString, JsUndefined,
}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Build the `console` global per WHATWG Console.
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [
      #("log", ConsoleNative(ConsoleLog), 0),
      #("info", ConsoleNative(ConsoleLog), 0),
      #("debug", ConsoleNative(ConsoleLog), 0),
      #("warn", ConsoleNative(ConsoleLogError), 0),
      #("error", ConsoleNative(ConsoleLogError), 0),
    ])
  common.init_namespace(h, object_proto, "console", methods)
}

/// Per-module dispatch for the `console` global.
pub fn dispatch(
  native: ConsoleNativeFn,
  args: List(JsValue),
  _this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case native {
    ConsoleLog -> print(args, state, io.println)
    ConsoleLogError -> print(args, state, io.println_error)
  }
}

/// WHATWG Console §2.1 Logger — format `args` then hand the line to `write`
/// (`io.println` for log/info/debug, `io.println_error` for warn/error).
pub fn print(
  args: List(JsValue),
  state: State,
  write: fn(String) -> Nil,
) -> #(State, Result(JsValue, JsValue)) {
  let #(state, line) = format(args, state)
  write(line)
  #(state, Ok(JsUndefined))
}

/// Format `args` to the string a console method would print, without the
/// I/O. Public so tests can assert formatting independent of stdout/stderr.
pub fn format(args: List(JsValue), state: State) -> #(State, String) {
  case args {
    // §2.1 step 4: only run Formatter if first is a string AND there are more
    // args. `console.log("100%")` must print `100%`, not consume the `%`.
    [JsString(fmt), next, ..rest] -> formatter(state, fmt, [next, ..rest], "")
    _ -> #(state, list.map(args, display(state.heap, _)) |> string.join(" "))
  }
}

/// WHATWG Console §2.2.1 Formatter. Walk `fmt` consuming one arg per
/// specifier, then append leftover args space-separated. Supports
/// `%s %d %i %f %o %O %c %%`; unknown `%x` is left literal (matches
/// Node/Chrome, spec says skip).
fn formatter(
  state: State,
  fmt: String,
  args: List(JsValue),
  acc: String,
) -> #(State, String) {
  case string.pop_grapheme(fmt) {
    Error(Nil) -> {
      // §2.2 step 5: leftover args are Printer'd after the formatted string.
      let trailing = list.map(args, display(state.heap, _))
      #(state, string.join([acc, ..trailing], " "))
    }
    Ok(#("%", rest)) ->
      case string.pop_grapheme(rest) {
        // Trailing lone `%` — emit literally, keep going so leftover args
        // still get appended.
        Error(Nil) -> formatter(state, "", args, acc <> "%")
        Ok(#(sp, rest)) ->
          case spec(state, sp, args) {
            Some(#(state, sub, args)) ->
              formatter(state, rest, args, acc <> sub)
            None -> formatter(state, rest, args, acc <> "%" <> sp)
          }
      }
    Ok(#(ch, rest)) -> formatter(state, rest, args, acc <> ch)
  }
}

/// Apply one format specifier. Returns `Some(state, substitution, rest_args)`
/// or `None` for an unknown specifier (caller emits it literally). `%c` is
/// CSS styling — meaningless on a terminal, so it consumes its arg and
/// emits nothing, like Node.
fn spec(
  state: State,
  sp: String,
  args: List(JsValue),
) -> Option(#(State, String, List(JsValue))) {
  case sp, args {
    "%", _ -> Some(#(state, "%", args))
    // §2.2.1 step 2: out of args → leave the specifier literal.
    _, [] -> None
    "s", [head, ..rest]
    | "d", [head, ..rest]
    | "i", [head, ..rest]
    | "f", [head, ..rest]
    | "o", [head, ..rest]
    | "O", [head, ..rest]
    | "c", [head, ..rest]
    -> {
      let #(state, sub) = case sp {
        "s" ->
          case coerce.js_to_string(state, head) {
            Ok(#(s, state)) -> #(state, s)
            Error(#(_, state)) -> #(state, display(state.heap, head))
          }
        // §2.2.1 step 4.2/4.3: "%d/%i/%f shall be converted by spec function
        // %parseInt%/%parseFloat%". We feed ToNumber's result through int
        // truncation for d/i; both yield "NaN" for non-numeric.
        "d" | "i" ->
          case coerce.js_to_number(state, head) {
            Ok(#(value.Finite(n), state)) -> #(
              state,
              string.inspect(value.float_to_int(n)),
            )
            Ok(#(_, state)) | Error(#(_, state)) -> #(state, "NaN")
          }
        "f" ->
          case coerce.js_to_number(state, head) {
            Ok(#(n, state)) -> #(state, object.inspect(JsNumber(n), state.heap))
            Error(#(_, state)) -> #(state, "NaN")
          }
        "o" | "O" -> #(state, object.inspect(head, state.heap))
        // "c"
        _ -> #(state, "")
      }
      Some(#(state, sub, rest))
    }
    _, _ -> None
  }
}

/// "Optimally useful" rendering for one Printer arg. Top-level strings are
/// raw (no quotes — `console.log("a")` prints `a`); everything else uses the
/// REPL inspector so objects/arrays read as `{ a: 1 }` / `[1, 2]` instead of
/// `[object Object]`.
fn display(h: Heap, val: JsValue) -> String {
  case val {
    JsString(s) -> s
    _ -> object.inspect(val, h)
  }
}
