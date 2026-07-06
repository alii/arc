//// `console` global per WHATWG Console.

import arc/vm/builtins/common
import arc/vm/builtins/number
import arc/vm/ops/coerce
import arc/vm/ops/numeric
import arc/vm/ops/object
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type ConsoleNativeFn, type JsValue, type Ref, ConsoleLog, ConsoleLogError,
  ConsoleNative, JsNumber, JsString, JsSymbol, JsUndefined,
}
import gleam/int
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
/// - `Error(#(thrown, state))` — the specifier's coercion ran a user
///   `toString`/`valueOf` and it threw. The abrupt completion must propagate
///   out of `console.log`, not be papered over with a fallback string — Node
///   throws here too. Which user hook can throw depends on the specifier:
///   %s is `js_to_string` and %d is `js_to_number`, so a toString-thrower
///   (%s) or valueOf-thrower (%d) escapes. %i/%f go through
///   %parseInt%/%parseFloat%, which coerce with ToString only — so under
///   %i/%f a toString-thrower escapes but a valueOf-thrower is never called
///   (Node prints "NaN" there, it does not throw).
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
    // %d/%i/%f yield "NaN" (Console §2.2.1 step 4.2/4.3 special-cases
    // Symbol). Match Symbols BEFORE the coercing arms below so the only
    // throws that propagate are the user's own toString/valueOf.
    "s", [JsSymbol(id), ..rest] ->
      Ok(Some(#(value.symbol_descriptive_string(id), rest, state)))
    "d", [JsSymbol(_), ..rest]
    | "i", [JsSymbol(_), ..rest]
    | "f", [JsSymbol(_), ..rest]
    -> Ok(Some(#("NaN", rest, state)))
    "s", [head, ..rest] -> {
      use #(s, state) <- result.map(coerce.js_to_string(state, head))
      Some(#(s, rest, state))
    }
    // BigInt is the other value ToNumber rejects with an ENGINE TypeError
    // ("Cannot convert BigInt to number"). Like Symbol, that must never
    // escape %d/%i: Node renders a bigint under %d/%i as "<n>n" and never
    // throws. Guard it BEFORE the ToNumber arm below. (%f needs no guard:
    // it goes through %parseFloat%, i.e. ToString — fine on BigInt.)
    "d", [value.JsBigInt(value.BigInt(n)), ..rest]
    | "i", [value.JsBigInt(value.BigInt(n)), ..rest]
    -> Ok(Some(#(int.to_string(n) <> "n", rest, state)))
    // %d is Number() in Node, i.e. ToNumber: a user valueOf runs and its
    // throw propagates. Non-numeric → "NaN".
    "d", [head, ..rest] -> {
      use #(n, state) <- result.map(coerce.js_to_number(state, head))
      Some(#(number_substitution(n), rest, state))
    }
    // §2.2.1 step 4.2: "%i shall be converted by Call(%parseInt%, undefined,
    // « current, 10 »)". parseInt coerces with ToString, NOT ToNumber — so a
    // user toString throw propagates, but a valueOf-thrower is never called
    // and Node prints "NaN". Do not "simplify" this into the %d arm.
    "i", [head, ..rest] -> {
      let radix = JsNumber(value.Finite(10.0))
      case number.parse_int_value(state, head, radix) {
        #(state, Ok(n)) -> Ok(Some(#(number_substitution(n), rest, state)))
        #(state, Error(thrown)) -> Error(#(thrown, state))
      }
    }
    // §2.2.1 step 4.3: "%f shall be converted by Call(%parseFloat%,
    // undefined, « current »)". Same ToString-only coercion note as %i.
    "f", [head, ..rest] ->
      case number.parse_float_value(state, head) {
        #(state, Ok(n)) -> Ok(Some(#(number_substitution(n), rest, state)))
        #(state, Error(thrown)) -> Error(#(thrown, state))
      }
    "o", [head, ..rest] | "O", [head, ..rest] ->
      Ok(Some(#(object.inspect(head, state.heap), rest, state)))
    // %c is CSS styling — meaningless on a terminal, so it consumes its arg
    // and emits nothing, like Node.
    "c", [_, ..rest] -> Ok(Some(#("", rest, state)))
    _, _ -> Ok(None)
  }
}

/// Render the number a %d/%i/%f specifier coerced its argument to — Node's
/// `formatNumber`, i.e. Number::toString of the WHOLE number, plus the "-0"
/// that §6.1.6.1.20 renders as "0".
///
/// The three specifiers differ only in HOW they coerce (%d = Number(),
/// %i = parseInt, %f = parseFloat), never in how the result prints — so they
/// share this one renderer. Truncating here (the old `%d`) would print
/// `console.log("%d", 42.9)` as "42" instead of Node's "42.9", and collapsing
/// the non-finite arms into "NaN" would print `Infinity` as "NaN".
fn number_substitution(n: value.JsNum) -> String {
  case n {
    value.Finite(f) ->
      case numeric.is_neg_zero(f) {
        True -> "-0"
        False -> value.js_format_number(f)
      }
    value.NaN -> "NaN"
    value.Infinity -> "Infinity"
    value.NegInfinity -> "-Infinity"
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
