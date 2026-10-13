import arc/bytecode/lazy.{LazyFunction}
import arc/bytecode/lexical
import arc/compiler
import arc/compiler/scope
import arc/internal/tuple_array
import arc/parser
import arc/rt/bytecode.{type FuncTemplate}
import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
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

// `free` is, for each outer name the function uses, in order, how the code
// around it hands it over, or None for a global; `parent_slots` are the
// parent's slots for the ones it does hand over
fn alone(
  source: String,
  marker: String,
  form: scope.FnForm,
  strict: Bool,
  name: Option(String),
  free: List(Option(scope.CaptureSeed)),
  parent_slots: List(Int),
) -> FuncTemplate {
  let #(start, line) = position(source, marker)
  let assert Ok(template) =
    compiler.compile_function_alone(
      LazyFunction(
        bytes: bit_array.from_string(source),
        is_module: False,
        source: scope.FnSource(start:, end: 0, line:, form:, strict:),
        name:,
        free: lazy.pack_free(free),
        lexical: 0,
        code_kind: lexical.FunctionCode,
      ),
      parent_slots:,
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
  // `seen` is a global there
  assert alone(
      source,
      "function add",
      scope.FnDeclaration,
      False,
      Some("add"),
      [None],
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
  let assert [base_slot, count_slot] = in_place.env_descriptors
  // base is never written, so it is held by value; count is in a box
  assert alone(
      source,
      "function next",
      scope.FnExpression,
      True,
      Some("next"),
      [
        Some(scope.CaptureSeed(False, scope.ParamBinding)),
        Some(scope.CaptureSeed(True, scope.VarBinding)),
      ],
      [base_slot, count_slot],
    )
    == in_place
}
