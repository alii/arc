import arc/bytecode/lazy.{LazyFunction}
import arc/compiler
import arc/compiler/scope
import arc/internal/tuple_array
import arc/parser
import arc/rt/bytecode.{type FuncTemplate, CaptureLocal}
import gleam/bit_array
import gleam/list
import gleam/option.{type Option, Some}
import gleam/string

fn whole(source: String) -> FuncTemplate {
  let assert Ok(#(body, scopes)) = parser.parse_script(source)
  let assert Ok(template) = compiler.compile_script(body, scopes)
  template
}

fn child(template: FuncTemplate, index: Int) -> FuncTemplate {
  let assert [found, ..] =
    list.drop(tuple_array.to_list(template.functions), index)
  found
}

// the byte offset and line the first `marker` in `source` starts at
fn position(source: String, marker: String) -> #(Int, Int) {
  let assert Ok(#(before, _)) = string.split_once(source, marker)
  #(string.byte_size(before), list.length(string.split(before, "\n")))
}

fn alone(
  source: String,
  marker: String,
  form: scope.FnForm,
  strict: Bool,
  name: Option(String),
  captures: List(#(String, Int, scope.CaptureSeed)),
) -> FuncTemplate {
  let #(start, line) = position(source, marker)
  let assert Ok(template) =
    compiler.compile_function_alone(
      LazyFunction(
        bytes: bit_array.from_string(source),
        is_module: False,
        source: scope.FnSource(start:, line:, form:, strict:),
        name:,
        captures:,
      ),
      lazy: False,
    )
  template
}

pub fn a_declaration_compiled_alone_matches_the_whole_compile_test() {
  let source =
    "var seen = 1;
function add(a, b) {
  var inner = function () { return a + b + seen; };
  return inner() + this.x++;
}
"
  assert alone(
      source,
      "function add",
      scope.FnDeclaration,
      False,
      Some("add"),
      [],
    )
    == child(whole(source), 0)
}

pub fn an_expression_compiled_alone_takes_its_captures_as_seeds_test() {
  let source =
    "(function (base) {
  'use strict';
  var count = 0;
  return function next(step) { count += step; return base + count + arguments.length; };
})(1);
"
  let in_place = child(child(whole(source), 0), 0)
  let assert [CaptureLocal(base_slot), CaptureLocal(count_slot)] =
    in_place.env_descriptors
  // base is never written, so it is held by value; count is in a box
  let captures = [
    #("base", base_slot, scope.CaptureSeed(False, scope.ParamBinding)),
    #("count", count_slot, scope.CaptureSeed(True, scope.VarBinding)),
  ]
  assert alone(
      source,
      "function next",
      scope.FnExpression,
      True,
      Some("next"),
      captures,
    )
    == in_place
}
