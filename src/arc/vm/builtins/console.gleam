//// `console` global per WHATWG Console.

import arc/vm/builtins/common
import arc/vm/builtins/symbol as builtins_symbol
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type ConsoleNativeFn, type JsValue, type Ref, ConsoleLog, ConsoleLogError,
  ConsoleNative, JsNumber, JsString, JsSymbol, JsUndefined,
}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Build the `console` global per WHATWG Console.
pub fn init(
  h: Heap(host),
  object_proto: Ref,
  function_proto: Ref,
) -> #(Heap(host), Ref) {
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
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  case native {
    ConsoleLog -> print(args, state, io.println)
    ConsoleLogError -> print(args, state, io.println_error)
  }
}

/// WHATWG Console §2.1 Logger — format `args` then hand the line to `write`
/// (`io.println` for log/info/debug, `io.println_error` for warn/error).
/// Formatting runs user code (`toString`/`valueOf` via %s/%d/%i/%f), so it
/// can throw; a throw aborts the log — nothing is written — and propagates
/// out of `console.log`, matching Node.
pub fn print(
  args: List(JsValue),
  state: State(host),
  write: fn(String) -> Nil,
) -> #(State(host), Result(JsValue, JsValue)) {
  case format(args, state) {
    Ok(#(line, state)) -> {
      write(line)
      #(state, Ok(JsUndefined))
    }
    Error(#(thrown, state)) -> #(state, Error(thrown))
  }
}

/// Format `args` to the string a console method would print, without the
/// I/O. Public so tests can assert formatting independent of stdout/stderr.
/// `Error(#(thrown, state))` is an abrupt completion from a user
/// `toString`/`valueOf` invoked by a format specifier.
pub fn format(
  args: List(JsValue),
  state: State(host),
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  case args {
    // §2.1 step 4: only run Formatter if first is a string AND there are more
    // args. `console.log("100%")` must print `100%`, not consume the `%`.
    [JsString(fmt), next, ..rest] -> formatter(state, fmt, [next, ..rest], "")
    _ ->
      Ok(#(list.map(args, display(state.heap, _)) |> string.join(" "), state))
  }
}

/// WHATWG Console §2.2.1 Formatter. Walk `fmt` consuming one arg per
/// specifier, then append leftover args space-separated. Supports
/// `%s %d %i %f %o %O %c %%`; unknown `%x` is left literal (matches
/// Node/Chrome, spec says skip). Propagates a throw from a specifier's
/// coercion (see `spec`).
fn formatter(
  state: State(host),
  fmt: String,
  args: List(JsValue),
  acc: String,
) -> Result(#(String, State(host)), #(JsValue, State(host))) {
  case string.pop_grapheme(fmt) {
    Error(Nil) -> {
      // §2.2 step 5: leftover args are Printer'd after the formatted string.
      let trailing = list.map(args, display(state.heap, _))
      Ok(#(string.join([acc, ..trailing], " "), state))
    }
    Ok(#("%", rest)) ->
      case string.pop_grapheme(rest) {
        // Trailing lone `%` — emit literally, keep going so leftover args
        // still get appended.
        Error(Nil) -> formatter(state, "", args, acc <> "%")
        Ok(#(sp, rest)) -> {
          use consumed <- result.try(spec(state, sp, args))
          case consumed {
            Some(#(sub, args, state)) ->
              formatter(state, rest, args, acc <> sub)
            None -> formatter(state, rest, args, acc <> "%" <> sp)
          }
        }
      }
    Ok(#(ch, rest)) -> formatter(state, rest, args, acc <> ch)
  }
}

/// Apply one format specifier.
///
/// - `Ok(Some(#(substitution, rest_args, state)))` — recognised, arg consumed.
/// - `Ok(None)` — unknown specifier, or a known one with no arg left
///   (§2.2.1 step 2); the caller emits `%x` literally.
/// - `Error(#(thrown, state))` — %s/%d/%i/%f ran a user `toString`/`valueOf`
///   (via `js_to_string`/`js_to_number`) and it threw. The abrupt completion
///   must propagate out of `console.log`, not be papered over with a
///   fallback string — Node throws here too.
fn spec(
  state: State(host),
  sp: String,
  args: List(JsValue),
) -> Result(
  Option(#(String, List(JsValue), State(host))),
  #(JsValue, State(host)),
) {
  case sp, args {
    "%", _ -> Ok(Some(#("%", args, state)))
    // §2.2.1 step 2: out of args → leave the specifier literal.
    _, [] -> Ok(None)
    // A Symbol makes ToString/ToNumber throw the spec TypeError. That is
    // engine machinery, not user code, and neither WHATWG Console nor Node
    // lets it escape a specifier: %s is Call(%String%, undefined, «arg»)
    // — NOT ToString — so a Symbol yields its descriptive string, and
    // %d/%i/%f yield "NaN". Match Symbols BEFORE the coercing arms below so
    // the only throws that propagate are the user's own toString/valueOf.
    "s", [JsSymbol(id), ..rest] ->
      Ok(
        Some(#(
          builtins_symbol.descriptive_string(id, state.ctx.symbol_descriptions),
          rest,
          state,
        )),
      )
    "d", [JsSymbol(_), ..rest]
    | "i", [JsSymbol(_), ..rest]
    | "f", [JsSymbol(_), ..rest]
    -> Ok(Some(#("NaN", rest, state)))
    "s", [head, ..rest] -> {
      use #(s, state) <- result.map(coerce.js_to_string(state, head))
      Some(#(s, rest, state))
    }
    // §2.2.1 step 4.2: "%d/%i shall be converted by spec function %parseInt%".
    // We feed ToNumber's result through int truncation; non-numeric → "NaN".
    "d", [head, ..rest] | "i", [head, ..rest] -> {
      use #(n, state) <- result.map(coerce.js_to_number(state, head))
      let sub = case n {
        value.Finite(f) -> string.inspect(value.float_to_int(f))
        _ -> "NaN"
      }
      Some(#(sub, rest, state))
    }
    // §2.2.1 step 4.3: "%f shall be converted by spec function %parseFloat%".
    "f", [head, ..rest] -> {
      use #(n, state) <- result.map(coerce.js_to_number(state, head))
      Some(#(object.inspect(JsNumber(n), state.heap), rest, state))
    }
    "o", [head, ..rest] | "O", [head, ..rest] ->
      Ok(Some(#(object.inspect(head, state.heap), rest, state)))
    // %c is CSS styling — meaningless on a terminal, so it consumes its arg
    // and emits nothing, like Node.
    "c", [_, ..rest] -> Ok(Some(#("", rest, state)))
    _, _ -> Ok(None)
  }
}

/// "Optimally useful" rendering for one Printer arg. Top-level strings are
/// raw (no quotes — `console.log("a")` prints `a`); everything else uses the
/// REPL inspector so objects/arrays read as `{ a: 1 }` / `[1, 2]` instead of
/// `[object Object]`.
fn display(h: Heap(host), val: JsValue) -> String {
  case val {
    JsString(s) -> s
    _ -> object.inspect(val, h)
  }
}
