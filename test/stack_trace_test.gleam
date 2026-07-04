import arc/compiler
import arc/parser
import arc/vm/builtins
import arc/vm/exec/entry
import arc/vm/heap
import arc/vm/value.{JsString}
import gleam/string

/// Parse + compile + run JS source, return the settled outcome
/// (Ok(value) / Error(thrown)).
fn run_js(
  source: String,
) -> Result(Result(value.JsValue, value.JsValue), String) {
  case parser.parse_script(source) {
    Error(err) -> Error("parse error: " <> parser.parse_error_to_string(err))
    Ok(#(body, sb)) ->
      case compiler.compile(body, sb) {
        Error(e) -> Error("compile error: " <> string.inspect(e))
        Ok(template) -> {
          let h = heap.new()
          let #(h, b) = builtins.init(h)
          let #(h, global_object) = builtins.globals(b, h)
          case entry.run(template, h, b, global_object) {
            Ok(#(settled, _heap)) -> Ok(settled)
            Error(vm_err) -> Error("vm error: " <> string.inspect(vm_err))
          }
        }
      }
  }
}

/// Run JS whose final expression is a string, return that string.
fn eval_string(source: String) -> String {
  case run_js(source) {
    Ok(Ok(JsString(s))) -> s
    other ->
      panic as { "expected string completion, got " <> string.inspect(other) }
  }
}

pub fn basic_error_stack_header_test() {
  let s = eval_string("new Error('boom').stack")
  // First line is "Error: boom"
  assert string.starts_with(s, "Error: boom")
}

pub fn type_error_stack_header_test() {
  let s = eval_string("try { null.x } catch (e) { e.stack }")
  assert string.starts_with(s, "TypeError")
  assert string.contains(s, "at ")
}

pub fn nested_frames_have_lines_test() {
  let src =
    "function inner() { throw new Error('boom'); }\n"
    <> "function outer() { inner(); }\n"
    <> "try { outer(); } catch (e) { e.stack }"
  let s = eval_string(src)
  assert string.starts_with(s, "Error: boom")
  assert string.contains(s, "at inner (script:1)")
  assert string.contains(s, "at outer (script:2)")
  assert string.contains(s, "at script:3")
}

pub fn stack_is_non_enumerable_test() {
  // Object.keys should not include "stack".
  let s =
    eval_string(
      "var e = new Error('x'); Object.keys(e).indexOf('stack') === -1 ? 'absent' : 'present'",
    )
  assert s == "absent"
}

pub fn stack_trace_limit_default_test() {
  let s = eval_string("'' + Error.stackTraceLimit")
  assert s == "10"
}

pub fn stack_trace_limit_zero_drops_frames_test() {
  // limit 0 => only the header line, no "    at " frames.
  let s = eval_string("Error.stackTraceLimit = 0; new Error('x').stack")
  assert s == "Error: x"
}

pub fn stack_trace_limit_one_keeps_one_frame_test() {
  let src =
    "Error.stackTraceLimit = 1;\n"
    <> "function inner() { return new Error('x').stack; }\n"
    <> "function outer() { return inner(); }\n"
    <> "outer()"
  let s = eval_string(src)
  // Exactly one frame: the innermost (inner).
  assert string.contains(s, "at inner")
  assert !string.contains(s, "at outer")
}

pub fn capture_stack_trace_sets_string_test() {
  let s = eval_string("var o = {}; Error.captureStackTrace(o); typeof o.stack")
  assert s == "string"
}

pub fn capture_stack_trace_uses_target_name_message_test() {
  let s =
    eval_string(
      "var o = { name: 'Boom', message: 'kaboom' };"
      <> " Error.captureStackTrace(o); o.stack",
    )
  assert string.starts_with(s, "Boom: kaboom")
}

pub fn capture_stack_trace_non_object_throws_test() {
  let s =
    eval_string(
      "try { Error.captureStackTrace(42); 'no throw' }"
      <> " catch (e) { e instanceof TypeError ? 'TypeError' : 'other' }",
    )
  assert s == "TypeError"
}

pub fn subclass_error_has_stack_test() {
  let src = "class MyError extends Error {}\n" <> "new MyError('boom').stack"
  let s = eval_string(src)
  assert string.contains(s, "boom")
  assert string.contains(s, "at ")
}
