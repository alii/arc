/// Evaluate JavaScript source code through the full Arc pipeline.
///
/// Pipeline: parse → compile → vm.run_with_globals
import arc/compiler
import arc/parser
import arc/vm/builtins.{type Builtins}
import arc/vm/heap
import arc/vm/value
import arc/vm/vm
import gleam/dict
import gleam/int
import gleam/string

/// Evaluate JS source and return a human-readable result string.
pub fn eval(source: String) -> Result(String, String) {
  case parser.parse(source, parser.Script) {
    Error(err) -> Error("SyntaxError: " <> parser.parse_error_to_string(err))
    Ok(program) ->
      case compiler.compile(program) {
        Error(compiler.Unsupported(desc)) ->
          Error("CompileError: unsupported " <> desc)
        Error(compiler.BreakOutsideLoop) ->
          Error("CompileError: break outside loop")
        Error(compiler.ContinueOutsideLoop) ->
          Error("CompileError: continue outside loop")
        Ok(template) -> {
          let h = heap.new()
          let #(h, b) = builtins.init(h)
          let globals = make_globals(b)
          case vm.run_with_globals(template, h, b, globals) {
            Ok(vm.NormalCompletion(val, _)) -> Ok(inspect_value(val))
            Ok(vm.ThrowCompletion(val, _)) ->
              Error("Uncaught " <> inspect_value(val))
            Error(vm_err) -> Error("InternalError: " <> string.inspect(vm_err))
          }
        }
      }
  }
}

/// Standard global bindings for JS execution.
fn make_globals(b: Builtins) -> dict.Dict(String, value.JsValue) {
  dict.from_list([
    #("NaN", value.JsNumber(value.NaN)),
    #("Infinity", value.JsNumber(value.Infinity)),
    #("undefined", value.JsUndefined),
    #("Object", value.JsObject(b.object.constructor)),
    #("Function", value.JsObject(b.function.constructor)),
    #("Array", value.JsObject(b.array.constructor)),
    #("Error", value.JsObject(b.error.constructor)),
    #("TypeError", value.JsObject(b.type_error.constructor)),
    #("ReferenceError", value.JsObject(b.reference_error.constructor)),
    #("RangeError", value.JsObject(b.range_error.constructor)),
    #("SyntaxError", value.JsObject(b.syntax_error.constructor)),
    #("Math", value.JsObject(b.math)),
  ])
}

/// Convert a JsValue to a human-readable string representation.
pub fn inspect_value(val: value.JsValue) -> String {
  case val {
    value.JsUndefined -> "undefined"
    value.JsNull -> "null"
    value.JsBool(True) -> "true"
    value.JsBool(False) -> "false"
    value.JsNumber(value.Finite(n)) -> format_number(n)
    value.JsNumber(value.NaN) -> "NaN"
    value.JsNumber(value.Infinity) -> "Infinity"
    value.JsNumber(value.NegInfinity) -> "-Infinity"
    value.JsString(s) -> "\"" <> s <> "\""
    value.JsObject(_) -> "[object]"
    value.JsSymbol(_) -> "Symbol()"
    value.JsBigInt(value.BigInt(n)) -> int.to_string(n) <> "n"
    value.JsUninitialized -> "<uninitialized>"
  }
}

/// Format a float nicely: 3.0 → "3", 3.14 → "3.14"
fn format_number(n: Float) -> String {
  let s = string.inspect(n)
  case string.ends_with(s, ".0") {
    True -> string.drop_end(s, 2)
    False -> s
  }
}
