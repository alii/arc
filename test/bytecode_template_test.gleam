//// A compiled script's shared-runtime template: the constant pool holds wire
//// values that classify as the source literals (integral literals in i32
//// range as the int row, everything else finite as the float row, the same
//// rule compiled code uses), nested function templates included.

import arc/bytecode/opcode
import arc/compiler
import arc/internal/tuple_array
import arc/parser
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/types.{
  type JsValKind, JFloat, JInt, KHandle, KNum, KStr, KSym, KTdz, KUndef,
  classify,
}
import gleam/int
import gleam/list
import gleam/option.{Some}

fn compile(source: String) -> FuncTemplate {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
  let assert Ok(template) = compiler.compile(body, sb)
  template
}

fn constant_kinds(t: FuncTemplate) -> List(JsValKind) {
  tuple_array.to_list(t.constants) |> list.map(classify)
}

/// Constant kinds of `t` and every nested template, depth first.
fn all_constant_kinds(t: FuncTemplate) -> List(JsValKind) {
  list.flatten([
    constant_kinds(t),
    ..list.map(tuple_array.to_list(t.functions), all_constant_kinds)
  ])
}

fn child(t: FuncTemplate, index: Int) -> FuncTemplate {
  let assert Some(c) = tuple_array.get(index, t.functions)
  c
}

pub fn string_and_number_constants_classify_test() {
  let kinds =
    constant_kinds(compile("var s = 'hello'; var n = 42; var f = 1.5;"))
  assert list.contains(kinds, KStr("hello"))
  assert list.contains(kinds, KNum(JInt(42)))
  assert list.contains(kinds, KNum(JFloat(1.5)))
}

pub fn integral_literal_outside_i32_is_float_row_test() {
  let kinds = constant_kinds(compile("var big = 4294967296; var neg = -7;"))
  assert list.contains(kinds, KNum(JFloat(4_294_967_296.0)))
  assert !list.contains(kinds, KNum(JInt(4_294_967_296)))
  // `-7` is unary minus applied to the literal 7.
  assert list.contains(kinds, KNum(JInt(7)))
}

pub fn nested_function_template_constants_classify_test() {
  let t =
    compile(
      "function outer() { function inner() { return 'deep'; } return 3; }",
    )
  let outer = child(t, 0)
  assert outer.name == Some("outer")
  assert list.contains(constant_kinds(outer), KNum(JInt(3)))
  let inner = child(outer, 0)
  assert inner.name == Some("inner")
  assert list.contains(constant_kinds(inner), KStr("deep"))
}

pub fn binding_seeds_classify_as_undefined_and_tdz_test() {
  // The binding prologue seeds `var` slots with undefined and `let` slots
  // with the TDZ sentinel, both through the constant pool.
  let kinds = all_constant_kinds(compile("function f() { var v; let l = 1; }"))
  assert list.contains(kinds, KUndef)
  assert list.contains(kinds, KTdz)
  assert list.contains(kinds, KNum(JInt(1)))
}

pub fn constant_pool_never_holds_heap_references_test() {
  let kinds =
    all_constant_kinds(compile(
      "var o = {a: [1, 2.5, 'x']}; class C { m() { return `t${o}`; } }",
    ))
  assert kinds != []
  assert list.all(kinds, fn(k) {
    case k {
      KHandle(_) | KSym(_) -> False
      _ -> True
    }
  })
}

/// GetTemplateObject site indices of `t` and every nested template, depth
/// first.
fn all_template_sites(t: FuncTemplate) -> List(Int) {
  let own =
    tuple_array.to_list(t.bytecode)
    |> list.filter_map(fn(op) {
      case op {
        opcode.GetTemplateObject(site:, ..) -> Ok(site)
        _ -> Error(Nil)
      }
    })
  list.flatten([
    own,
    ..list.map(tuple_array.to_list(t.functions), all_template_sites)
  ])
}

const tagged_source = "
  function id(s) { return s }
  var a = id`top`;
  function f(x) { return [id`f0${x}`, () => id`arrow`] }
  class C { m() { return id`m` } static s = id`static`; #p = id`field` }
  var g = function* () { yield id`gen` }
  var h = async () => id`async${await 0}done`
"

pub fn compiling_twice_gives_equal_templates_test() {
  // Nothing in the compiler reads VM-global state: the same source yields
  // the same template tree, nested children and template sites included.
  assert compile(tagged_source) == compile(tagged_source)
  assert compile("eval('1'); new Function('return id`x`')")
    == compile("eval('1'); new Function('return id`x`')")
}

pub fn template_sites_number_the_unit_in_source_order_test() {
  // One counter threads through every nested function of the unit, so no
  // two sites share an index and the numbering is dense from 0.
  let sites = all_template_sites(compile(tagged_source))
  assert list.length(sites) == 8
  assert list.sort(sites, int.compare) == [0, 1, 2, 3, 4, 5, 6, 7]
}
