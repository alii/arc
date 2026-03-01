import arc/compiler
import arc/module
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/common
import arc/vm/heap
import arc/vm/value.{
  Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined, NaN,
}
import arc/vm/vm
import gleam/dict
import gleam/int
import gleam/option.{None, Some}
import gleam/set
import gleam/string

// ============================================================================
// Test helpers
// ============================================================================

/// Parse + compile + run JS source, return the completion value.
fn run_js(source: String) -> Result(vm.Completion, String) {
  case parser.parse(source, parser.Script) {
    Error(err) -> Error("parse error: " <> parser.parse_error_to_string(err))
    Ok(program) ->
      case compiler.compile(program) {
        Error(compiler.Unsupported(desc)) ->
          Error("compile error: unsupported " <> desc)
        Error(compiler.BreakOutsideLoop) ->
          Error("compile error: break outside loop")
        Error(compiler.ContinueOutsideLoop) ->
          Error("compile error: continue outside loop")
        Ok(template) -> {
          let h = heap.new()
          let #(h, b) = builtins.init(h)
          let #(h, globals) = builtins.globals(b, h)
          case vm.run_with_globals(template, h, b, globals) {
            Ok(#(completion, _state)) -> Ok(completion)
            Error(vm_err) -> Error("vm error: " <> inspect_vm_error(vm_err))
          }
        }
      }
  }
}

/// Like run_js but drains the promise job queue after script completes.
fn run_js_drain(source: String) -> Result(vm.Completion, String) {
  case parser.parse(source, parser.Script) {
    Error(err) -> Error("parse error: " <> parser.parse_error_to_string(err))
    Ok(program) ->
      case compiler.compile(program) {
        Error(compiler.Unsupported(desc)) ->
          Error("compile error: unsupported " <> desc)
        Error(compiler.BreakOutsideLoop) ->
          Error("compile error: break outside loop")
        Error(compiler.ContinueOutsideLoop) ->
          Error("compile error: continue outside loop")
        Ok(template) -> {
          let h = heap.new()
          let #(h, b) = builtins.init(h)
          let #(h, globals) = builtins.globals(b, h)
          case vm.run_and_drain(template, h, b, globals) {
            Ok(completion) -> Ok(completion)
            Error(vm_err) -> Error("vm error: " <> inspect_vm_error(vm_err))
          }
        }
      }
  }
}

/// Assert that the script returns a promise, drain completes, and the promise
/// is fulfilled with the expected value.
fn assert_promise_resolves(source: String, expected: value.JsValue) -> Nil {
  case run_js_drain(source) {
    Ok(vm.NormalCompletion(val, h)) ->
      case vm.promise_result(h, val) {
        Some(resolved) -> {
          assert resolved == expected
        }
        None ->
          panic as {
            "expected fulfilled promise, got: "
            <> string.inspect(val)
            <> " for: "
            <> source
          }
      }
    Ok(vm.ThrowCompletion(val, _)) ->
      panic as {
        "expected NormalCompletion, got ThrowCompletion("
        <> string.inspect(val)
        <> ") for: "
        <> source
      }
    Ok(vm.YieldCompletion(_, _)) -> panic as "unexpected YieldCompletion"
    Error(err) -> panic as { "error for: " <> source <> " — " <> err }
  }
}

fn assert_promise_rejects(source: String, expected: value.JsValue) -> Nil {
  case run_js_drain(source) {
    Ok(vm.NormalCompletion(val, h)) ->
      case vm.promise_result(h, val) {
        Some(rejected) -> {
          assert rejected == expected
        }
        None ->
          panic as {
            "expected rejected promise, got: "
            <> string.inspect(val)
            <> " for: "
            <> source
          }
      }
    Ok(vm.ThrowCompletion(val, _)) ->
      panic as {
        "expected NormalCompletion, got ThrowCompletion("
        <> string.inspect(val)
        <> ") for: "
        <> source
      }
    Ok(vm.YieldCompletion(_, _)) -> panic as "unexpected YieldCompletion"
    Error(err) -> panic as { "error for: " <> source <> " — " <> err }
  }
}

fn inspect_vm_error(err: vm.VmError) -> String {
  case err {
    vm.PcOutOfBounds(pc) -> "PcOutOfBounds(" <> pc |> int.to_string <> ")"
    vm.StackUnderflow(op) -> "StackUnderflow(" <> op <> ")"
    vm.LocalIndexOutOfBounds(i) ->
      "LocalIndexOutOfBounds(" <> i |> int.to_string <> ")"
    vm.Unimplemented(op) -> "Unimplemented(" <> op <> ")"
  }
}

fn assert_normal(source: String, expected: value.JsValue) -> Nil {
  case run_js(source) {
    Ok(vm.NormalCompletion(val, _)) -> {
      assert val == expected
    }
    Ok(vm.ThrowCompletion(_, _)) ->
      panic as {
        "expected NormalCompletion, got ThrowCompletion for: " <> source
      }
    Ok(vm.YieldCompletion(_, _)) -> panic as "unexpected YieldCompletion"
    Error(err) -> panic as { "error for: " <> source <> " — " <> err }
  }
}

fn assert_normal_number(source: String, expected: Float) -> Nil {
  assert_normal(source, JsNumber(Finite(expected)))
}

fn assert_thrown(source: String) -> Nil {
  case run_js(source) {
    Ok(vm.ThrowCompletion(_, _)) -> Nil
    Ok(vm.NormalCompletion(val, _)) ->
      panic as {
        "expected ThrowCompletion, got NormalCompletion("
        <> string.inspect(val)
        <> ") for: "
        <> source
      }
    Ok(vm.YieldCompletion(_, _)) -> panic as "unexpected YieldCompletion"
    Error(err) -> panic as { "error for: " <> source <> " — " <> err }
  }
}

// ============================================================================
// Literal tests
// ============================================================================

pub fn number_literal_test() -> Nil {
  assert_normal_number("42", 42.0)
}

pub fn string_literal_test() -> Nil {
  assert_normal("\"hello\"", JsString("hello"))
}

pub fn boolean_true_test() -> Nil {
  assert_normal("true", JsBool(True))
}

pub fn boolean_false_test() -> Nil {
  assert_normal("false", JsBool(False))
}

pub fn null_literal_test() -> Nil {
  assert_normal("null", JsNull)
}

pub fn undefined_literal_test() -> Nil {
  assert_normal("undefined", JsUndefined)
}

// ============================================================================
// Binary operator tests
// ============================================================================

pub fn addition_test() -> Nil {
  assert_normal_number("1 + 2", 3.0)
}

pub fn subtraction_test() -> Nil {
  assert_normal_number("10 - 3", 7.0)
}

pub fn multiplication_test() -> Nil {
  assert_normal_number("4 * 5", 20.0)
}

pub fn division_test() -> Nil {
  assert_normal_number("15 / 3", 5.0)
}

pub fn modulo_test() -> Nil {
  assert_normal_number("10 % 3", 1.0)
}

pub fn comparison_less_than_test() -> Nil {
  assert_normal("1 < 2", JsBool(True))
}

pub fn comparison_greater_than_test() -> Nil {
  assert_normal("2 > 1", JsBool(True))
}

pub fn strict_equal_test() -> Nil {
  assert_normal("1 === 1", JsBool(True))
}

pub fn strict_not_equal_test() -> Nil {
  assert_normal("1 !== 2", JsBool(True))
}

pub fn string_concat_test() -> Nil {
  assert_normal("\"hello\" + \" world\"", JsString("hello world"))
}

pub fn nested_arithmetic_test() -> Nil {
  assert_normal_number("(1 + 2) * (3 + 4)", 21.0)
}

// ============================================================================
// Unary operator tests
// ============================================================================

pub fn negate_test() -> Nil {
  assert_normal_number("-5", -5.0)
}

pub fn logical_not_test() -> Nil {
  assert_normal("!true", JsBool(False))
}

pub fn unary_plus_test() -> Nil {
  assert_normal_number("+42", 42.0)
}

pub fn void_test() -> Nil {
  assert_normal("void 0", JsUndefined)
}

// ============================================================================
// Variable tests
// ============================================================================

pub fn var_declaration_test() -> Nil {
  assert_normal_number("var x = 10; x", 10.0)
}

pub fn var_reassignment_test() -> Nil {
  assert_normal_number("var x = 1; x = 5; x", 5.0)
}

pub fn multiple_vars_test() -> Nil {
  assert_normal_number("var x = 1; var y = 2; x + y", 3.0)
}

pub fn let_declaration_test() -> Nil {
  assert_normal_number("let x = 42; x", 42.0)
}

pub fn const_declaration_test() -> Nil {
  assert_normal_number("const x = 99; x", 99.0)
}

pub fn compound_assignment_test() -> Nil {
  assert_normal_number("var x = 10; x += 5; x", 15.0)
}

pub fn var_no_init_test() -> Nil {
  assert_normal("var x; x", JsUndefined)
}

// ============================================================================
// Control flow tests
// ============================================================================

pub fn if_true_test() -> Nil {
  assert_normal_number("var x = 0; if (true) { x = 1; } x", 1.0)
}

pub fn if_false_test() -> Nil {
  assert_normal_number("var x = 0; if (false) { x = 1; } x", 0.0)
}

pub fn if_else_test() -> Nil {
  assert_normal_number("var x; if (true) { x = 1; } else { x = 2; } x", 1.0)
}

pub fn if_else_false_test() -> Nil {
  assert_normal_number("var x; if (false) { x = 1; } else { x = 2; } x", 2.0)
}

pub fn while_loop_test() -> Nil {
  assert_normal_number("var i = 0; while (i < 5) { i = i + 1; } i", 5.0)
}

pub fn for_loop_test() -> Nil {
  assert_normal_number(
    "var sum = 0; for (var i = 0; i < 5; i = i + 1) { sum = sum + i; } sum",
    10.0,
  )
}

pub fn do_while_test() -> Nil {
  assert_normal_number("var i = 0; do { i = i + 1; } while (i < 3); i", 3.0)
}

pub fn break_test() -> Nil {
  assert_normal_number(
    "var i = 0; while (true) { if (i === 5) { break; } i = i + 1; } i",
    5.0,
  )
}

pub fn continue_test() -> Nil {
  assert_normal_number(
    "var sum = 0; for (var i = 0; i < 5; i = i + 1) { if (i === 2) { continue; } sum = sum + i; } sum",
    8.0,
  )
}

// ============================================================================
// Block scoping tests
// ============================================================================

pub fn block_scope_let_test() -> Nil {
  // let inside block should not be visible outside
  assert_normal_number("var x = 1; { let y = 2; x = x + y; } x", 3.0)
}

// ============================================================================
// Logical operator tests
// ============================================================================

pub fn logical_and_short_circuit_test() -> Nil {
  assert_normal("false && true", JsBool(False))
}

pub fn logical_and_evaluates_both_test() -> Nil {
  assert_normal_number("1 && 2", 2.0)
}

pub fn logical_or_short_circuit_test() -> Nil {
  assert_normal("true || false", JsBool(True))
}

pub fn logical_or_evaluates_second_test() -> Nil {
  assert_normal_number("0 || 42", 42.0)
}

pub fn nullish_coalescing_null_test() -> Nil {
  assert_normal_number("null ?? 42", 42.0)
}

pub fn nullish_coalescing_value_test() -> Nil {
  assert_normal_number("5 ?? 42", 5.0)
}

// ============================================================================
// Conditional (ternary) tests
// ============================================================================

pub fn ternary_true_test() -> Nil {
  assert_normal_number("true ? 1 : 2", 1.0)
}

pub fn ternary_false_test() -> Nil {
  assert_normal_number("false ? 1 : 2", 2.0)
}

// ============================================================================
// Object tests
// ============================================================================

pub fn empty_object_test() -> Nil {
  // Just test that it doesn't crash — the result is an object ref
  case run_js("({})") {
    Ok(vm.NormalCompletion(value.JsObject(_), _)) -> Nil
    _other -> panic as { "expected object, got something else" }
  }
}

pub fn object_property_access_test() -> Nil {
  assert_normal_number("var obj = {x: 42}; obj.x", 42.0)
}

pub fn object_multiple_properties_test() -> Nil {
  assert_normal_number("var obj = {x: 1, y: 2}; obj.x + obj.y", 3.0)
}

pub fn object_property_undefined_test() -> Nil {
  assert_normal("var obj = {}; obj.x", JsUndefined)
}

// --- Computed keys ---

pub fn object_computed_key_basic_test() -> Nil {
  assert_normal_number("var k = 'x'; var o = {[k]: 42}; o.x", 42.0)
}

pub fn object_computed_key_expr_test() -> Nil {
  assert_normal_number("var o = {['a' + 'b']: 1}; o.ab", 1.0)
}

pub fn object_computed_key_number_test() -> Nil {
  // Number key ToPropertyKey → "42"
  assert_normal_number("var k = 42; var o = {[k]: 7}; o['42']", 7.0)
}

pub fn object_computed_key_undefined_test() -> Nil {
  // undefined → "undefined"
  assert_normal_number("var k; var o = {[k]: 3}; o['undefined']", 3.0)
}

pub fn object_computed_key_null_test() -> Nil {
  // null → "null"
  assert_normal_number("var o = {[null]: 9}; o['null']", 9.0)
}

pub fn object_computed_key_mixed_test() -> Nil {
  // Mix of static and computed
  assert_normal_number(
    "var k = 'b'; var o = {a: 1, [k]: 2, c: 3}; o.a + o.b + o.c",
    6.0,
  )
}

pub fn object_computed_key_to_primitive_test() -> Nil {
  // Object key goes through ToPrimitive → ToString
  assert_normal_number(
    "var k = {toString: function() { return 'foo' }}; var o = {[k]: 5}; o.foo",
    5.0,
  )
}

pub fn object_computed_key_symbol_test() -> Nil {
  // Symbol key goes to symbol_properties (not coerced to string)
  assert_normal_number("var s = Symbol('k'); var o = {[s]: 99}; o[s]", 99.0)
}

pub fn object_computed_key_symbol_not_string_key_test() -> Nil {
  // Symbol key should NOT create a string-keyed property
  assert_normal(
    "var s = Symbol('k'); var o = {[s]: 1}; Object.keys(o).length",
    JsNumber(Finite(0.0)),
  )
}

pub fn object_computed_key_overwrite_test() -> Nil {
  // Later key wins
  assert_normal_number("var o = {a: 1, ['a']: 2}; o.a", 2.0)
}

// --- Numeric literal keys (non-computed but need ToString) ---

pub fn object_numeric_key_test() -> Nil {
  assert_normal_number("var o = {1: 'a'}; o['1'] === 'a' ? 1 : 0", 1.0)
}

pub fn object_numeric_key_access_test() -> Nil {
  assert_normal("var o = {1: 'a', 2: 'b'}; o[1] + o[2]", JsString("ab"))
}

pub fn object_numeric_key_keys_test() -> Nil {
  assert_normal(
    "var o = {1: 'a', 2: 'b'}; Object.keys(o).join(',')",
    JsString("1,2"),
  )
}

// --- Object spread ---

pub fn object_spread_basic_test() -> Nil {
  assert_normal_number("var s = {a: 1}; var o = {...s}; o.a", 1.0)
}

pub fn object_spread_merge_test() -> Nil {
  assert_normal_number(
    "var s = {a: 1, b: 2}; var o = {...s, c: 3}; o.a + o.b + o.c",
    6.0,
  )
}

pub fn object_spread_override_after_test() -> Nil {
  // Later static key overrides spread
  assert_normal_number("var s = {a: 1}; var o = {...s, a: 2}; o.a", 2.0)
}

pub fn object_spread_override_before_test() -> Nil {
  // Spread overrides earlier static key
  assert_normal_number("var s = {a: 2}; var o = {a: 1, ...s}; o.a", 2.0)
}

pub fn object_spread_multiple_test() -> Nil {
  assert_normal_number(
    "var a = {x: 1}; var b = {y: 2}; var o = {...a, ...b}; o.x + o.y",
    3.0,
  )
}

pub fn object_spread_null_test() -> Nil {
  // Spreading null is a no-op per spec (unlike Object.assign target)
  assert_normal(
    "var o = {...null}; Object.keys(o).length",
    JsNumber(Finite(0.0)),
  )
}

pub fn object_spread_undefined_test() -> Nil {
  assert_normal(
    "var o = {...undefined}; Object.keys(o).length",
    JsNumber(Finite(0.0)),
  )
}

pub fn object_spread_primitive_number_test() -> Nil {
  // Number wrapper has no own enumerable props → no-op
  assert_normal("var o = {...42}; Object.keys(o).length", JsNumber(Finite(0.0)))
}

pub fn object_spread_only_own_test() -> Nil {
  // Spread copies OWN properties only, not inherited
  assert_normal(
    "var proto = {inherited: 1}; var src = Object.create(proto); src.own = 2; "
      <> "var o = {...src}; o.inherited === undefined && o.own === 2",
    JsBool(True),
  )
}

pub fn object_spread_only_enumerable_test() -> Nil {
  // Non-enumerable props are skipped
  assert_normal(
    "var src = {}; Object.defineProperty(src, 'hidden', {value: 1, enumerable: false}); "
      <> "src.visible = 2; var o = {...src}; "
      <> "o.hidden === undefined && o.visible === 2",
    JsBool(True),
  )
}

pub fn object_spread_array_test() -> Nil {
  // Spreading an array into an object → index keys
  assert_normal(
    "var o = {...[10, 20, 30]}; o['0'] + o['1'] + o['2']",
    JsNumber(Finite(60.0)),
  )
}

pub fn object_spread_array_sparse_test() -> Nil {
  // Holes in source array are skipped
  assert_normal(
    "var o = {...[1, , 3]}; o['0'] === 1 && o['1'] === undefined && o['2'] === 3",
    JsBool(True),
  )
}

pub fn object_spread_preserves_symbol_test() -> Nil {
  // Symbol-keyed enumerable props are copied too
  assert_normal_number(
    "var s = Symbol(); var src = {[s]: 7}; var o = {...src}; o[s]",
    7.0,
  )
}

pub fn object_spread_with_computed_key_test() -> Nil {
  // Interleaved spread + computed key + static
  assert_normal(
    "var s = {a: 1}; var k = 'b'; var o = {...s, [k]: 2, c: 3}; "
      <> "o.a + ',' + o.b + ',' + o.c",
    JsString("1,2,3"),
  )
}

// ============================================================================
// Array spread tests — [...x], [a, ...b, c]
// ============================================================================

pub fn array_spread_basic_test() -> Nil {
  assert_normal("[...[1,2,3]].join(',')", JsString("1,2,3"))
}

pub fn array_spread_empty_test() -> Nil {
  assert_normal_number("[...[]].length", 0.0)
}

pub fn array_spread_leading_trailing_test() -> Nil {
  assert_normal("[0, ...[1,2], 3].join(',')", JsString("0,1,2,3"))
}

pub fn array_spread_leading_only_test() -> Nil {
  // Prefix-only case exercises the IrArrayFrom(N) optimization for leading
  // non-spread elements before switching to incremental mode.
  assert_normal("[10, 20, ...[30, 40]].join(',')", JsString("10,20,30,40"))
}

pub fn array_spread_trailing_only_test() -> Nil {
  // Spread-first means IrArrayFrom(0) then pure incremental.
  assert_normal("[...[1,2], 3, 4].join(',')", JsString("1,2,3,4"))
}

pub fn array_spread_multiple_test() -> Nil {
  assert_normal(
    "[1, ...[2,3], 4, ...[5,6], 7].join(',')",
    JsString("1,2,3,4,5,6,7"),
  )
}

pub fn array_spread_length_test() -> Nil {
  assert_normal_number("[1, ...[2,3,4], 5].length", 5.0)
}

pub fn array_spread_source_holes_test() -> Nil {
  // Holes in the spread SOURCE become undefined in the result — the array
  // iterator visits all indices 0..length and Get() returns undefined for holes.
  assert_normal("[...[1,,3]].join(',')", JsString("1,,3"))
}

pub fn array_spread_source_holes_length_test() -> Nil {
  // Confirm the hole became an actual element (length 3, not 2).
  assert_normal_number("[...[1,,3]].length", 3.0)
}

pub fn array_spread_source_holes_value_test() -> Nil {
  // The filled-in value is undefined, not a hole (1 in arr is true).
  assert_normal(
    "var a = [...[1,,3]]; 1 in a && a[1] === undefined",
    JsBool(True),
  )
}

pub fn array_spread_null_throws_test() -> Nil {
  // Unlike object spread, array spread uses the iterator protocol —
  // null has no [Symbol.iterator], so TypeError.
  assert_thrown("[...null]")
}

pub fn array_spread_undefined_throws_test() -> Nil {
  assert_thrown("[...undefined]")
}

pub fn array_spread_number_throws_test() -> Nil {
  // Primitives without Symbol.iterator throw. Numbers aren't iterable.
  assert_thrown("[...42]")
}

pub fn array_spread_non_iterable_object_throws_test() -> Nil {
  // Plain objects aren't iterable even if they're array-like.
  assert_thrown("var o = {length: 2, 0: 'a', 1: 'b'}; [...o]")
}

pub fn array_spread_nested_test() -> Nil {
  // Spread inside a spread source.
  assert_normal("[...[1, ...[2, 3], 4]].join(',')", JsString("1,2,3,4"))
}

pub fn array_spread_generator_test() -> Nil {
  // Generators are iterable — spread drains them.
  assert_normal(
    "function* g() { yield 1; yield 2; yield 3; } [...g()].join(',')",
    JsString("1,2,3"),
  )
}

pub fn array_spread_generator_interleaved_test() -> Nil {
  assert_normal(
    "function* g() { yield 2; yield 3; } [1, ...g(), 4].join(',')",
    JsString("1,2,3,4"),
  )
}

pub fn array_spread_generator_empty_test() -> Nil {
  // Generator that yields nothing.
  assert_normal_number("function* g() {} [...g()].length", 0.0)
}

pub fn array_spread_does_not_mutate_source_test() -> Nil {
  // Spread should read, not consume, the source array.
  assert_normal("var s = [1,2,3]; [...s]; s.join(',')", JsString("1,2,3"))
}

pub fn array_spread_copies_not_shares_test() -> Nil {
  // The result is a new array, not the same ref.
  assert_normal(
    "var s = [1,2,3]; var t = [...s]; t.push(4); s.length",
    JsNumber(Finite(3.0)),
  )
}

// ============================================================================
// Call spread tests — f(...x), obj.m(...x), new F(...x)
// ============================================================================

pub fn call_spread_basic_test() -> Nil {
  assert_normal_number("function f(a,b,c){ return a+b+c } f(...[1,2,3])", 6.0)
}

pub fn call_spread_mixed_test() -> Nil {
  // Regular args before and after spread.
  assert_normal_number(
    "function f(a,b,c,d){ return a*1000+b*100+c*10+d } f(1, ...[2,3], 4)",
    1234.0,
  )
}

pub fn call_spread_multiple_test() -> Nil {
  assert_normal_number(
    "function f(a,b,c,d,e){ return a+b+c+d+e } f(...[1,2], 3, ...[4,5])",
    15.0,
  )
}

pub fn call_spread_empty_test() -> Nil {
  // Spreading an empty array contributes zero args.
  assert_normal_number("function f(a,b){ return a+b } f(1, ...[], 2)", 3.0)
}

pub fn call_spread_extra_args_test() -> Nil {
  // More spread args than params — extras are ignored.
  assert_normal_number("function f(a,b){ return a+b } f(...[1,2,3,4,5])", 3.0)
}

pub fn call_spread_fewer_args_test() -> Nil {
  // Fewer spread args than params — missing params are undefined.
  assert_normal("function f(a,b,c){ return c } f(...[1,2])", JsUndefined)
}

pub fn call_spread_null_throws_test() -> Nil {
  assert_thrown("function f(){} f(...null)")
}

pub fn call_spread_generator_test() -> Nil {
  assert_normal_number(
    "function* g(){ yield 1; yield 2 } "
      <> "function f(a,b,c,d){ return a*1000+b*100+c*10+d } f(0, ...g(), 3)",
    123.0,
  )
}

pub fn method_spread_this_binding_test() -> Nil {
  // obj.m(...x) must bind this=obj. CallMethodApply opcode responsibility.
  assert_normal_number(
    "var o = {v: 100, f: function(a,b){ return this.v + a + b }}; "
      <> "o.f(...[2, 3])",
    105.0,
  )
}

pub fn method_spread_mixed_test() -> Nil {
  assert_normal_number(
    "var o = {v: 10, f: function(a,b,c){ return this.v*a + b + c }}; "
      <> "o.f(2, ...[3, 4])",
    27.0,
  )
}

pub fn method_spread_native_test() -> Nil {
  // Spread into a native method (Array.prototype.push).
  assert_normal(
    "var a = [1,2]; a.push(...[3,4,5]); a.join(',')",
    JsString("1,2,3,4,5"),
  )
}

pub fn method_spread_native_returns_test() -> Nil {
  // push returns new length — verify the apply path wires return value.
  assert_normal_number("var a = [1]; a.push(...[2,3,4])", 4.0)
}

pub fn new_spread_basic_test() -> Nil {
  // new F(...args) via CallConstructorApply.
  assert_normal_number(
    "function F(a,b){ this.sum = a+b } new F(...[3,4]).sum",
    7.0,
  )
}

pub fn new_spread_mixed_test() -> Nil {
  assert_normal_number(
    "function F(a,b,c){ this.r = a*100+b*10+c } new F(1, ...[2,3]).r",
    123.0,
  )
}

pub fn new_spread_class_test() -> Nil {
  // Spread through a class constructor (derived-constructor check in do_construct).
  assert_normal_number(
    "class C { constructor(a,b){ this.p = a*b } } new C(...[6,7]).p",
    42.0,
  )
}

pub fn optional_call_spread_test() -> Nil {
  // f?.(...args) when f is defined.
  assert_normal_number(
    "var f = function(a,b){ return a+b }; f?.(...[5,6])",
    11.0,
  )
}

pub fn optional_call_spread_nullish_test() -> Nil {
  // f?.(...args) when f is undefined — short-circuits to undefined,
  // spread arg must not be evaluated (but we can't test that without a
  // side-effect tracker; just verify it doesn't throw).
  assert_normal("var f = undefined; f?.(...[1,2])", JsUndefined)
}

pub fn call_spread_catch_test() -> Nil {
  // Thrown TypeError from spread should be catchable.
  assert_normal_number(
    "function f(){} try { f(...null) } catch(e) { 99 }",
    99.0,
  )
}

pub fn array_spread_catch_test() -> Nil {
  assert_normal_number(
    "var r; try { r = [...undefined] } catch(e) { r = 42 } r",
    42.0,
  )
}

pub fn call_spread_evaluation_order_test() -> Nil {
  // Args are evaluated left-to-right including spreads.
  assert_normal(
    "var log = []; function t(x){ log.push(x); return x } "
      <> "function f(){} f(t(1), ...[t(2),t(3)], t(4)); log.join(',')",
    JsString("1,2,3,4"),
  )
}

pub fn array_spread_evaluation_order_test() -> Nil {
  assert_normal(
    "var log = []; function t(x){ log.push(x); return x } "
      <> "var a = [t(1), ...[t(2),t(3)], t(4)]; log.join(',')",
    JsString("1,2,3,4"),
  )
}

// ============================================================================
// Optional chaining tests
// ============================================================================

pub fn optional_chaining_dot_test() -> Nil {
  assert_normal_number("var obj = {x: 42}; obj?.x", 42.0)
}

pub fn optional_chaining_null_test() -> Nil {
  assert_normal("var obj = null; obj?.x", JsUndefined)
}

pub fn optional_chaining_undefined_test() -> Nil {
  assert_normal("var obj = undefined; obj?.x", JsUndefined)
}

pub fn optional_chaining_computed_test() -> Nil {
  assert_normal_number("var obj = {x: 10}; obj?.['x']", 10.0)
}

pub fn optional_chaining_computed_null_test() -> Nil {
  assert_normal("var obj = null; obj?.['x']", JsUndefined)
}

pub fn optional_call_test() -> Nil {
  assert_normal_number("var fn = function() { return 5; }; fn?.()", 5.0)
}

pub fn optional_call_null_test() -> Nil {
  assert_normal("var fn = null; fn?.()", JsUndefined)
}

// ============================================================================
// Expression statement result tests
// ============================================================================

pub fn expression_result_test() -> Nil {
  // The last expression statement's value should be the program result
  assert_normal_number("1; 2; 3", 3.0)
}

pub fn empty_program_test() -> Nil {
  assert_normal("", JsUndefined)
}

// ============================================================================
// Update expression tests
// ============================================================================

pub fn prefix_increment_test() -> Nil {
  assert_normal_number("var x = 5; ++x", 6.0)
}

pub fn postfix_increment_test() -> Nil {
  assert_normal_number("var x = 5; x++", 5.0)
}

pub fn postfix_increment_side_effect_test() -> Nil {
  assert_normal_number("var x = 5; x++; x", 6.0)
}

// ============================================================================
// Try/catch tests
// ============================================================================

pub fn try_catch_basic_test() -> Nil {
  assert_normal_number("try { throw 42; } catch(e) { e }", 42.0)
}

pub fn try_no_throw_test() -> Nil {
  assert_normal_number("var x = 0; try { x = 1; } catch(e) { x = 2; } x", 1.0)
}

pub fn try_finally_normal_test() -> Nil {
  assert_normal_number("var x = 0; try { x = 1; } finally { x = 10; } x", 10.0)
}

pub fn try_finally_throw_test() -> Nil {
  // Finally runs even when exception is thrown, then re-throws
  case run_js("var x = 0; try { x = 1; throw 42; } finally { x = 10; }") {
    Ok(vm.ThrowCompletion(JsNumber(Finite(42.0)), _)) -> Nil
    other ->
      panic as { "expected ThrowCompletion(42): " <> string.inspect(other) }
  }
}

pub fn try_catch_finally_normal_test() -> Nil {
  assert_normal_number(
    "var x = 0; try { x = 1; } catch(e) { x = 2; } finally { x = x + 10; } x",
    11.0,
  )
}

pub fn try_catch_finally_caught_test() -> Nil {
  assert_normal_number(
    "var x = 0; try { throw 5; } catch(e) { x = e; } finally { x = x + 10; } x",
    15.0,
  )
}

pub fn try_catch_finally_rethrow_test() -> Nil {
  // If catch re-throws, finally still runs, then the exception propagates
  case
    run_js(
      "var x = 0; try { throw 42; } catch(e) { throw e + 1; } finally { x = 99; }",
    )
  {
    Ok(vm.ThrowCompletion(JsNumber(Finite(43.0)), _)) -> Nil
    other ->
      panic as { "expected ThrowCompletion(43): " <> string.inspect(other) }
  }
}

// ============================================================================
// Throw as ThrowCompletion test
// ============================================================================

pub fn uncaught_throw_test() -> Nil {
  case run_js("throw 42") {
    Ok(vm.ThrowCompletion(JsNumber(Finite(42.0)), _)) -> Nil
    Ok(vm.NormalCompletion(_, _)) ->
      panic as "expected ThrowCompletion, got NormalCompletion"
    other -> panic as { "unexpected result: " <> string.inspect(other) }
  }
}

// ============================================================================
// Sequence expression test
// ============================================================================

pub fn sequence_expression_test() -> Nil {
  assert_normal_number("(1, 2, 3)", 3.0)
}

// ============================================================================
// Function tests
// ============================================================================

pub fn function_declaration_basic_test() -> Nil {
  assert_normal_number("function add(a, b) { return a + b; } add(3, 4)", 7.0)
}

pub fn function_no_args_test() -> Nil {
  assert_normal_number("function f() { return 42; } f()", 42.0)
}

pub fn function_implicit_return_test() -> Nil {
  assert_normal("function f() {} f()", JsUndefined)
}

pub fn function_expression_test() -> Nil {
  assert_normal_number("var f = function(x) { return x * 2; }; f(5)", 10.0)
}

pub fn function_multiple_calls_test() -> Nil {
  assert_normal_number(
    "function inc(x) { return x + 1; } inc(inc(inc(0)))",
    3.0,
  )
}

pub fn function_extra_args_ignored_test() -> Nil {
  assert_normal_number("function f(a) { return a; } f(1, 2, 3)", 1.0)
}

pub fn function_missing_args_undefined_test() -> Nil {
  assert_normal("function f(a, b) { return b; } f(1)", JsUndefined)
}

pub fn function_with_locals_test() -> Nil {
  assert_normal_number(
    "function f(x) { var y = x + 1; return y * 2; } f(4)",
    10.0,
  )
}

pub fn function_hoisting_test() -> Nil {
  assert_normal_number("var x = f(); function f() { return 99; } x", 99.0)
}

pub fn function_recursion_test() -> Nil {
  assert_normal_number(
    "function fact(n) { if (n <= 1) return 1; return n * fact(n - 1); } fact(5)",
    120.0,
  )
}

// ============================================================================
// Arrow function tests
// ============================================================================

pub fn arrow_expression_body_test() -> Nil {
  assert_normal_number("var f = (x) => x + 1; f(5)", 6.0)
}

pub fn arrow_two_params_test() -> Nil {
  assert_normal_number("var f = (a, b) => a + b; f(3, 4)", 7.0)
}

pub fn arrow_block_body_test() -> Nil {
  assert_normal_number("var f = (x) => { return x * 2; }; f(5)", 10.0)
}

pub fn arrow_no_params_test() -> Nil {
  assert_normal_number("var f = () => 42; f()", 42.0)
}

pub fn arrow_nested_expression_test() -> Nil {
  assert_normal_number("var f = (x) => x * x + 1; f(3)", 10.0)
}

// ============================================================================
// Closure tests
// ============================================================================

pub fn closure_basic_test() -> Nil {
  // Use function expression (not declaration) so the closure is created
  // AFTER x=10 is assigned (function declarations are hoisted above vars)
  assert_normal_number(
    "function make() { var x = 10; var inner = function() { return x; }; return inner; } make()()",
    10.0,
  )
}

pub fn closure_hoisted_declaration_test() -> Nil {
  assert_normal_number(
    "function make() { var x = 10; function inner() { return x; } return inner; } make()()",
    10.0,
  )
}

pub fn closure_param_capture_test() -> Nil {
  assert_normal_number(
    "function adder(n) { return function(x) { return x + n; }; } adder(5)(3)",
    8.0,
  )
}

pub fn closure_two_params_test() -> Nil {
  assert_normal_number(
    "function f(a, b) { return function() { return a + b; }; } f(3, 4)()",
    7.0,
  )
}

pub fn closure_arrow_test() -> Nil {
  assert_normal_number("function f(x) { return () => x; } f(42)()", 42.0)
}

pub fn closure_let_test() -> Nil {
  assert_normal_number(
    "function f() { let x = 99; return function() { return x; }; } f()()",
    99.0,
  )
}

pub fn closure_multiple_captures_test() -> Nil {
  assert_normal_number(
    "function f(a, b, c) { return function() { return a + b + c; }; } f(1, 2, 3)()",
    6.0,
  )
}

pub fn closure_factory_test() -> Nil {
  assert_normal_number(
    "function multiplier(factor) { return function(x) { return x * factor; }; } var double = multiplier(2); double(7)",
    14.0,
  )
}

pub fn closure_arrow_expression_capture_test() -> Nil {
  assert_normal_number("function f(x) { return (y) => x + y; } f(10)(20)", 30.0)
}

pub fn closure_var_capture_test() -> Nil {
  // Closures capture by reference via BoxSlot — mutations are visible.
  assert_normal_number(
    "function f() { var x = 5; var g = function() { return x; }; x = 10; return g; } f()()",
    10.0,
  )
}

pub fn closure_independent_copies_test() -> Nil {
  // Two closures from same factory get independent copies
  assert_normal_number(
    "function make(n) { return function() { return n; }; } var a = make(1); var b = make(2); a() + b()",
    3.0,
  )
}

pub fn closure_mutation_after_creation_test() -> Nil {
  // Mutation after closure creation is visible through the closure
  assert_normal_number(
    "function f() { var x = 5; var g = function() { return x; }; x = 10; return g(); } f()",
    10.0,
  )
}

pub fn closure_mutation_through_closure_test() -> Nil {
  // Mutation through the closure is visible to the parent
  assert_normal_number(
    "function f() { var x = 0; var inc = function() { x = x + 1; }; inc(); inc(); inc(); return x; } f()",
    3.0,
  )
}

pub fn closure_shared_between_siblings_test() -> Nil {
  // Two closures share the same variable
  assert_normal_number(
    "function f() { var x = 0; var inc = function() { x = x + 1; return x; }; var get = function() { return x; }; inc(); inc(); return get(); } f()",
    2.0,
  )
}

pub fn closure_counter_pattern_test() -> Nil {
  // Classic counter closure pattern
  assert_normal_number(
    "function counter() { var n = 0; return function() { n = n + 1; return n; }; } var c = counter(); c(); c(); c()",
    3.0,
  )
}

pub fn closure_hoisted_fn_mutation_test() -> Nil {
  // Hoisted function declaration + mutation in parent
  assert_normal_number(
    "function f() { function get() { return x; } var x = 42; return get(); } f()",
    42.0,
  )
}

pub fn closure_param_mutation_test() -> Nil {
  // Closure captures a param, parent mutates it
  assert_normal_number(
    "function f(x) { var g = function() { return x; }; x = 99; return g(); } f(1)",
    99.0,
  )
}

// ============================================================================
// Array tests
// ============================================================================

pub fn array_literal_test() -> Nil {
  // Array literal produces an object
  case run_js("[1, 2, 3]") {
    Ok(vm.NormalCompletion(value.JsObject(_), _)) -> Nil
    _other -> panic as "expected array object"
  }
}

pub fn array_index_access_test() -> Nil {
  assert_normal_number("[10, 20, 30][1]", 20.0)
}

pub fn array_index_zero_test() -> Nil {
  assert_normal_number("[42, 99][0]", 42.0)
}

pub fn array_length_test() -> Nil {
  assert_normal_number("[1, 2, 3].length", 3.0)
}

pub fn array_empty_length_test() -> Nil {
  assert_normal_number("[].length", 0.0)
}

pub fn array_out_of_bounds_test() -> Nil {
  assert_normal("[1, 2][5]", JsUndefined)
}

pub fn array_index_assignment_test() -> Nil {
  assert_normal_number("var a = [1, 2, 3]; a[0] = 42; a[0]", 42.0)
}

pub fn array_index_assignment_grows_length_test() -> Nil {
  assert_normal_number("var a = [1]; a[5] = 99; a.length", 6.0)
}

pub fn array_sparse_hole_test() -> Nil {
  // Holes in array literals preserve length
  assert_normal_number("[1, , 3].length", 3.0)
}

pub fn array_sparse_hole_value_test() -> Nil {
  // Reading a hole returns undefined
  assert_normal("[1, , 3][1]", JsUndefined)
}

pub fn array_sparse_hole_in_operator_test() -> Nil {
  // But `in` distinguishes holes from undefined: index 1 has no property.
  assert_normal("var a = [1, , 3]; 1 in a", JsBool(False))
}

pub fn array_sparse_hole_in_operator_present_test() -> Nil {
  // Non-hole indices are present.
  assert_normal("var a = [1, , 3]; 0 in a && 2 in a", JsBool(True))
}

pub fn array_sparse_hole_leading_test() -> Nil {
  // Leading hole.
  assert_normal("var a = [, 2, 3]; 0 in a", JsBool(False))
}

pub fn array_sparse_hole_all_holes_test() -> Nil {
  // All-hole array: length 2, neither index present.
  // [,,] has a trailing comma — 2 elements, both holes.
  assert_normal(
    "var a = [,,]; a.length === 2 && !(0 in a) && !(1 in a)",
    JsBool(True),
  )
}

pub fn array_sparse_hole_multiple_test() -> Nil {
  // Multiple holes.
  assert_normal(
    "var a = [1,,3,,5]; !(1 in a) && !(3 in a) && (0 in a) && (4 in a)",
    JsBool(True),
  )
}

pub fn array_sparse_hole_foreach_skips_test() -> Nil {
  // forEach skips holes (SkipHoles behavior).
  assert_normal_number(
    "var count = 0; [1,,3].forEach(function() { count++ }); count",
    2.0,
  )
}

pub fn array_sparse_hole_spread_prefix_test() -> Nil {
  // Hole before a spread: [, ...[1,2], 3] → hole at 0, then 1,2,3.
  assert_normal(
    "var a = [, ...[1,2], 3]; !(0 in a) && a.length === 4 && a[1] === 1",
    JsBool(True),
  )
}

pub fn array_sparse_hole_spread_tail_test() -> Nil {
  // Hole after a spread: [1, ...[], , 3] → length 3, hole at 1.
  assert_normal(
    "var a = [1, ...[], , 3]; a.length === 3 && !(1 in a) && a[2] === 3",
    JsBool(True),
  )
}

pub fn array_sparse_hole_spread_sandwich_test() -> Nil {
  // Holes on both sides of a spread.
  assert_normal(
    "var a = [, ...[1,2], , 3]; a.length === 5 && !(0 in a) && !(3 in a) && a[4] === 3",
    JsBool(True),
  )
}

pub fn array_string_elements_test() -> Nil {
  assert_normal("['a', 'b', 'c'][2]", JsString("c"))
}

pub fn array_nested_access_test() -> Nil {
  assert_normal_number("var a = [10, 20]; var b = [a[0] + a[1]]; b[0]", 30.0)
}

pub fn array_compound_assignment_test() -> Nil {
  assert_normal_number("var a = [10]; a[0] += 5; a[0]", 15.0)
}

pub fn array_in_variable_test() -> Nil {
  assert_normal_number("var a = [5, 10, 15]; a[2]", 15.0)
}

pub fn array_length_after_assignment_test() -> Nil {
  assert_normal_number("var a = []; a[0] = 1; a[1] = 2; a.length", 2.0)
}

// ============================================================================
// this / new / methods
// ============================================================================

pub fn new_basic_constructor_test() -> Nil {
  assert_normal_number(
    "function Foo(x) { this.x = x; }
     var o = new Foo(42);
     o.x",
    42.0,
  )
}

pub fn function_prototype_exists_test() -> Nil {
  assert_normal(
    "function Foo() {}
     typeof Foo.prototype",
    JsString("object"),
  )
}

pub fn prototype_method_with_this_test() -> Nil {
  assert_normal_number(
    "function Foo(x) { this.x = x; }
     Foo.prototype.getX = function() { return this.x; };
     var o = new Foo(5);
     o.getX()",
    5.0,
  )
}

pub fn constructor_implicit_return_test() -> Nil {
  // Constructor returns undefined → new object used
  assert_normal_number(
    "function Foo(x) { this.x = x; }
     var o = new Foo(7);
     o.x",
    7.0,
  )
}

pub fn constructor_explicit_object_return_test() -> Nil {
  // Constructor returns an object → that object is used
  assert_normal_number(
    "function Foo() { this.a = 1; return { b: 99 }; }
     var o = new Foo();
     o.b",
    99.0,
  )
}

pub fn static_property_on_function_test() -> Nil {
  assert_normal_number(
    "function Foo() {}
     Foo.bar = 42;
     Foo.bar",
    42.0,
  )
}

pub fn sta_js_pattern_test() -> Nil {
  // Full sta.js-style integration
  assert_normal(
    "function Test262Error(message) {
       this.message = message || '';
     }
     Test262Error.prototype.toString = function() {
       return 'Test262Error: ' + this.message;
     };
     Test262Error.thrower = function(message) {
       throw new Test262Error(message);
     };
     var e = new Test262Error('hello');
     e.toString()",
    JsString("Test262Error: hello"),
  )
}

pub fn prototype_chain_property_lookup_test() -> Nil {
  assert_normal_number(
    "function Foo() {}
     Foo.prototype.x = 10;
     var o = new Foo();
     o.x",
    10.0,
  )
}

pub fn this_global_in_plain_sloppy_call_test() -> Nil {
  // ES §10.2.1.2 OrdinaryCallBindThis: sloppy callee gets globalThis
  // when thisArg is undefined.
  assert_normal(
    "function f() { return this === globalThis; }
     f()",
    JsBool(True),
  )
}

pub fn this_undefined_in_plain_strict_call_test() -> Nil {
  // Strict callee keeps undefined this.
  assert_normal(
    "function f() { 'use strict'; return this; }
     f()",
    JsUndefined,
  )
}

pub fn method_call_binds_this_test() -> Nil {
  assert_normal_number(
    "var obj = { x: 99, getX: function() { return this.x; } };
     obj.getX()",
    99.0,
  )
}

// ============================================================================
// Template literals
// ============================================================================

pub fn template_literal_no_expressions_test() -> Nil {
  assert_normal("`hello world`", JsString("hello world"))
}

pub fn template_literal_with_expression_test() -> Nil {
  assert_normal("var x = 42; `value is ${x}`", JsString("value is 42"))
}

pub fn template_literal_multiple_expressions_test() -> Nil {
  assert_normal(
    "var a = 1; var b = 2; `${a} + ${b} = ${a + b}`",
    JsString("1 + 2 = 3"),
  )
}

pub fn template_literal_empty_test() -> Nil {
  assert_normal("``", JsString(""))
}

// ============================================================================
// Switch statements
// ============================================================================

pub fn switch_basic_match_test() -> Nil {
  assert_normal_number(
    "var x = 2; var r = 0;
     switch (x) {
       case 1: r = 10; break;
       case 2: r = 20; break;
       case 3: r = 30; break;
     }
     r",
    20.0,
  )
}

pub fn switch_default_test() -> Nil {
  assert_normal_number(
    "var x = 99; var r = 0;
     switch (x) {
       case 1: r = 10; break;
       default: r = -1; break;
     }
     r",
    -1.0,
  )
}

pub fn switch_fallthrough_test() -> Nil {
  assert_normal_number(
    "var x = 1; var r = 0;
     switch (x) {
       case 1: r = r + 10;
       case 2: r = r + 20; break;
       case 3: r = r + 30; break;
     }
     r",
    30.0,
  )
}

pub fn switch_no_match_no_default_test() -> Nil {
  assert_normal_number(
    "var x = 99; var r = 5;
     switch (x) {
       case 1: r = 10; break;
       case 2: r = 20; break;
     }
     r",
    5.0,
  )
}

pub fn assert_js_harness_basic_test() -> Nil {
  // Minimal sta.js + assert.js + simple assertion
  assert_normal(
    "function Test262Error(message) { this.message = message || ''; }
     Test262Error.prototype.toString = function () { return 'Test262Error: ' + this.message; };

     function assert(mustBeTrue, message) {
       if (mustBeTrue === true) { return; }
       throw new Test262Error(message);
     }
     assert._isSameValue = function (a, b) {
       if (a === b) { return a !== 0 || 1 / a === 1 / b; }
       return a !== a && b !== b;
     };
     assert.sameValue = function (actual, expected, message) {
       try {
         if (assert._isSameValue(actual, expected)) { return; }
       } catch (error) {
         throw new Test262Error('_isSameValue threw');
       }
       throw new Test262Error('not same value');
     };

     assert.sameValue(1 + 1, 2);
     assert.sameValue(typeof 42, 'number');
     42",
    JsNumber(Finite(42.0)),
  )
}

pub fn full_assert_js_compiles_test() -> Nil {
  // Test that real sta.js + assert.js + simple test compiles and runs
  assert_normal(
    "function Test262Error(message) {
       this.message = message || '';
     }
     Test262Error.prototype.toString = function () {
       return 'Test262Error: ' + this.message;
     };
     Test262Error.thrower = function (message) {
       throw new Test262Error(message);
     };
     function $DONOTEVALUATE() {
       throw 'Test262: This statement should not be evaluated.';
     }

     function assert(mustBeTrue, message) {
       if (mustBeTrue === true) { return; }
       if (message === undefined) {
         message = 'Expected true but got ' + assert._toString(mustBeTrue);
       }
       throw new Test262Error(message);
     }
     assert._isSameValue = function (a, b) {
       if (a === b) { return a !== 0 || 1 / a === 1 / b; }
       return a !== a && b !== b;
     };
     assert.sameValue = function (actual, expected, message) {
       try {
         if (assert._isSameValue(actual, expected)) { return; }
       } catch (error) {
         throw new Test262Error(message + ' (_isSameValue operation threw) ' + error);
       }
       if (message === undefined) { message = ''; } else { message += ' '; }
       message += 'Expected SameValue';
       throw new Test262Error(message);
     };
     assert.notSameValue = function (actual, unexpected, message) {
       if (!assert._isSameValue(actual, unexpected)) { return; }
       if (message === undefined) { message = ''; } else { message += ' '; }
       message += 'Expected not SameValue';
       throw new Test262Error(message);
     };
     assert.throws = function (expectedErrorConstructor, func, message) {
       if (typeof func !== 'function') {
         throw new Test262Error('assert.throws requires a function');
       }
       if (message === undefined) { message = ''; } else { message += ' '; }
       try { func(); } catch (thrown) {
         if (typeof thrown !== 'object' || thrown === null) {
           throw new Test262Error(message + 'Thrown value was not an object!');
         } else if (thrown.constructor !== expectedErrorConstructor) {
           throw new Test262Error(message + 'Wrong error constructor');
         }
         return;
       }
       throw new Test262Error(message + 'No exception thrown');
     };
     function isPrimitive(value) {
       return !value || (typeof value !== 'object' && typeof value !== 'function');
     }
     assert._formatIdentityFreeValue = function (value) {
       switch (value === null ? 'null' : typeof value) {
         case 'string': return '\"' + value + '\"';
         case 'number':
           if (value === 0 && 1 / value === -Infinity) return '-0';
         case 'boolean':
         case 'undefined':
         case 'null':
           return '' + value;
       }
     };
     assert._toString = function (value) {
       var basic = assert._formatIdentityFreeValue(value);
       if (basic) return basic;
       return '' + value;
     };

     assert.sameValue(typeof true, 'boolean');
     assert.sameValue(typeof false, 'boolean');
     assert.sameValue(1 + 1, 2);
     assert.sameValue(typeof 42, 'number');
     assert.sameValue(typeof undefined, 'undefined');
     42",
    JsNumber(Finite(42.0)),
  )
}

pub fn switch_string_cases_test() -> Nil {
  assert_normal(
    "var t = 'number'; var r = '';
     switch (typeof t) {
       case 'string': r = 'is string'; break;
       case 'number': r = 'is number'; break;
       default: r = 'other';
     }
     r",
    JsString("is string"),
  )
}

// ============================================================================
// Function .name property
// ============================================================================

pub fn function_declaration_name_test() -> Nil {
  assert_normal("function foo() {} foo.name", JsString("foo"))
}

pub fn function_expression_named_name_test() -> Nil {
  assert_normal("var f = function bar() {}; f.name", JsString("bar"))
}

pub fn function_expression_anonymous_name_test() -> Nil {
  assert_normal("var f = function() {}; f.name", JsString(""))
}

pub fn arrow_function_name_test() -> Nil {
  assert_normal("var f = () => 1; f.name", JsString(""))
}

// ============================================================================
// .prototype.constructor backlink
// ============================================================================

pub fn prototype_constructor_backlink_test() -> Nil {
  assert_normal(
    "function Foo() {}
     Foo.prototype.constructor === Foo",
    JsBool(True),
  )
}

pub fn prototype_is_object_test() -> Nil {
  assert_normal(
    "function Foo() {}
     typeof Foo.prototype",
    JsString("object"),
  )
}

pub fn constructor_via_new_test() -> Nil {
  assert_normal(
    "function Foo() {}
     var o = new Foo();
     o.constructor === Foo",
    JsBool(True),
  )
}

pub fn arrow_has_no_prototype_test() -> Nil {
  assert_normal(
    "var f = () => 1;
     typeof f.prototype",
    JsString("undefined"),
  )
}

pub fn constructor_return_object_overrides_test() -> Nil {
  // If constructor explicitly returns an object, that object is used
  assert_normal(
    "function Foo() { return {x: 99}; }
     var o = new Foo();
     o.x",
    JsNumber(Finite(99.0)),
  )
}

pub fn constructor_return_primitive_ignored_test() -> Nil {
  // If constructor returns a primitive, the constructed object is used
  assert_normal(
    "function Foo() { this.x = 42; return 5; }
     var o = new Foo();
     o.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn prototype_chain_inheritance_test() -> Nil {
  // Properties set on prototype are accessible via the chain
  assert_normal(
    "function Foo() {}
     Foo.prototype.hello = 'world';
     var o = new Foo();
     o.hello",
    JsString("world"),
  )
}

pub fn constructor_non_object_prototype_fallback_test() -> Nil {
  // When constructor.prototype is not an object, new instance gets Object.prototype
  assert_normal(
    "function Foo() { this.x = 1; }
     Foo.prototype = 42;
     var o = new Foo();
     o.x",
    JsNumber(Finite(1.0)),
  )
}

// ============================================================================
// Destructuring tests
// ============================================================================

pub fn object_destructuring_basic_test() -> Nil {
  assert_normal_number("let {a, b} = {a: 1, b: 2}; a + b", 3.0)
}

pub fn array_destructuring_basic_test() -> Nil {
  assert_normal_number("let [x, y] = [10, 20]; x + y", 30.0)
}

pub fn object_destructuring_default_used_test() -> Nil {
  assert_normal_number("let {a = 5} = {}; a", 5.0)
}

pub fn object_destructuring_default_not_used_test() -> Nil {
  assert_normal_number("let {a = 5} = {a: 42}; a", 42.0)
}

pub fn object_destructuring_rename_test() -> Nil {
  assert_normal_number("let {a: x} = {a: 99}; x", 99.0)
}

pub fn object_destructuring_nested_test() -> Nil {
  assert_normal_number("let {a: {b}} = {a: {b: 7}}; b", 7.0)
}

pub fn array_destructuring_nested_test() -> Nil {
  assert_normal_number("let [a, [b, c]] = [1, [2, 3]]; b", 2.0)
}

pub fn function_param_destructuring_test() -> Nil {
  assert_normal_number(
    "function f({x, y}) { return x + y; } f({x: 3, y: 4})",
    7.0,
  )
}

pub fn var_destructuring_hoisting_test() -> Nil {
  assert_normal_number("var {a} = {a: 1}; a", 1.0)
}

pub fn array_destructuring_hole_test() -> Nil {
  assert_normal_number("let [, b] = [10, 20]; b", 20.0)
}

pub fn array_destructuring_default_test() -> Nil {
  assert_normal_number("let [a = 99] = []; a", 99.0)
}

pub fn const_destructuring_test() -> Nil {
  assert_normal_number("const {x, y} = {x: 3, y: 7}; x + y", 10.0)
}

pub fn nested_default_test() -> Nil {
  assert_normal_number("let {a: {b} = {b: 5}} = {}; b", 5.0)
}

pub fn arrow_param_destructuring_test() -> Nil {
  assert_normal_number("var f = ({a, b}) => a * b; f({a: 3, b: 4})", 12.0)
}

// ============================================================================
// For-in loop tests
// ============================================================================

pub fn for_in_object_basic_test() -> Nil {
  // Collect keys from a plain object
  assert_normal_number(
    "var sum = 0; var obj = {a: 1, b: 2, c: 3}; for (var k in obj) { sum += obj[k]; } sum",
    6.0,
  )
}

pub fn for_in_let_binding_test() -> Nil {
  assert_normal(
    "var result = ''; var obj = {x: 1, y: 2}; for (let k in obj) { result += k; } result",
    JsString("xy"),
  )
}

pub fn for_in_null_test() -> Nil {
  // for-in on null should not iterate
  assert_normal_number("var x = 0; for (var k in null) { x = 1; } x", 0.0)
}

pub fn for_in_undefined_test() -> Nil {
  // for-in on undefined should not iterate
  assert_normal_number("var x = 0; for (var k in undefined) { x = 1; } x", 0.0)
}

pub fn for_in_array_test() -> Nil {
  // for-in on array iterates indices as strings
  assert_normal(
    "var result = ''; for (var k in [10, 20, 30]) { result += k; } result",
    JsString("012"),
  )
}

pub fn for_in_break_test() -> Nil {
  assert_normal_number(
    "var count = 0; for (var k in {a: 1, b: 2, c: 3}) { count++; if (count === 2) break; } count",
    2.0,
  )
}

pub fn for_in_continue_test() -> Nil {
  assert_normal_number(
    "var sum = 0; var obj = {a: 1, b: 2, c: 3}; for (var k in obj) { if (k === 'b') continue; sum += obj[k]; } sum",
    4.0,
  )
}

pub fn for_in_existing_var_test() -> Nil {
  // for-in with existing variable (no declaration)
  assert_normal(
    "var k; for (k in {hello: 1, world: 2}) {} k",
    JsString("world"),
  )
}

// ============================================================================
// For-of loop tests
// ============================================================================

pub fn for_of_array_basic_test() -> Nil {
  assert_normal_number(
    "var sum = 0; for (var x of [1, 2, 3]) { sum += x; } sum",
    6.0,
  )
}

pub fn for_of_let_binding_test() -> Nil {
  assert_normal_number(
    "var sum = 0; for (let x of [10, 20, 30]) { sum += x; } sum",
    60.0,
  )
}

pub fn for_of_const_binding_test() -> Nil {
  assert_normal_number(
    "var sum = 0; for (const x of [5, 10, 15]) { sum += x; } sum",
    30.0,
  )
}

pub fn for_of_break_test() -> Nil {
  assert_normal_number(
    "var sum = 0; for (var x of [1, 2, 3, 4, 5]) { if (x > 3) break; sum += x; } sum",
    6.0,
  )
}

pub fn for_of_continue_test() -> Nil {
  assert_normal_number(
    "var sum = 0; for (var x of [1, 2, 3, 4]) { if (x === 2) continue; sum += x; } sum",
    8.0,
  )
}

pub fn for_of_empty_array_test() -> Nil {
  assert_normal_number("var sum = 0; for (var x of []) { sum += x; } sum", 0.0)
}

pub fn for_of_destructuring_test() -> Nil {
  // for-of with array destructuring
  assert_normal_number(
    "var sum = 0; var arr = [[1, 2], [3, 4]]; for (var [a, b] of arr) { sum += a + b; } sum",
    10.0,
  )
}

pub fn for_of_string_values_test() -> Nil {
  assert_normal(
    "var result = ''; for (var x of ['a', 'b', 'c']) { result += x; } result",
    JsString("abc"),
  )
}

// ============================================================================
// delete operator
// ============================================================================

pub fn delete_property_test() -> Nil {
  assert_normal("var obj = {x: 1, y: 2}; delete obj.x; obj.x", JsUndefined)
}

pub fn delete_returns_true_test() -> Nil {
  assert_normal("var obj = {x: 1}; delete obj.x", JsBool(True))
}

pub fn delete_nonexistent_test() -> Nil {
  assert_normal("var obj = {}; delete obj.x", JsBool(True))
}

pub fn delete_computed_test() -> Nil {
  assert_normal(
    "var obj = {a: 10}; var k = 'a'; delete obj[k]; obj.a",
    JsUndefined,
  )
}

pub fn delete_variable_test() -> Nil {
  // delete of a plain variable returns true in sloppy mode
  assert_normal("var x = 1; delete x", JsBool(True))
}

pub fn delete_non_object_test() -> Nil {
  assert_normal("delete 42", JsBool(True))
}

// ============================================================================
// in operator
// ============================================================================

pub fn in_own_property_test() -> Nil {
  assert_normal("'x' in {x: 1}", JsBool(True))
}

pub fn in_missing_property_test() -> Nil {
  assert_normal("'y' in {x: 1}", JsBool(False))
}

pub fn in_prototype_chain_test() -> Nil {
  // "constructor" exists on the prototype (inherited)
  assert_normal("'constructor' in {}", JsBool(True))
}

pub fn in_array_index_test() -> Nil {
  assert_normal("0 in [10, 20]", JsBool(True))
}

pub fn in_array_length_test() -> Nil {
  assert_normal("'length' in []", JsBool(True))
}

pub fn in_throws_for_non_object_test() -> Nil {
  assert_thrown("'x' in 42")
}

// ============================================================================
// Property descriptor behavior
// ============================================================================

pub fn for_in_skips_non_enumerable_test() -> Nil {
  // for-in on new Foo() should NOT include "constructor" (non-enumerable)
  assert_normal(
    "function Foo() {} var result = ''; for (var k in new Foo()) { result += k; } result",
    JsString(""),
  )
}

pub fn for_in_includes_own_enumerable_test() -> Nil {
  // Own properties set via assignment ARE enumerable
  assert_normal(
    "function Foo() { this.x = 1; this.y = 2; } var result = ''; for (var k in new Foo()) { result += k; } result",
    JsString("xy"),
  )
}

pub fn for_in_prototype_enumerable_test() -> Nil {
  // User-set prototype properties are enumerable and show in for-in
  assert_normal(
    "function Foo() {} Foo.prototype.bar = 42; var result = ''; for (var k in new Foo()) { result += k; } result",
    JsString("bar"),
  )
}

pub fn function_name_not_enumerable_test() -> Nil {
  // function.name is not enumerable
  assert_normal(
    "function foo() {} var result = ''; for (var k in foo) { result += k; } result",
    JsString(""),
  )
}

pub fn function_prototype_not_enumerable_test() -> Nil {
  // function.prototype is not enumerable
  assert_normal(
    "function foo() {} var keys = []; for (var k in foo) { keys.push(k); } keys.length",
    JsNumber(Finite(0.0)),
  )
}

pub fn delete_then_in_test() -> Nil {
  assert_normal("var obj = {x: 1}; delete obj.x; 'x' in obj", JsBool(False))
}

// ============================================================================
// Class tests
// ============================================================================

pub fn class_basic_constructor_test() -> Nil {
  assert_normal(
    "class Foo { constructor(x) { this.x = x; } } var f = new Foo(42); f.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn class_instance_method_test() -> Nil {
  assert_normal(
    "class Foo { greet() { return 'hi'; } } var f = new Foo(); f.greet()",
    JsString("hi"),
  )
}

pub fn class_method_accesses_this_test() -> Nil {
  assert_normal(
    "class Foo { constructor(x) { this.x = x; } getX() { return this.x; } } var f = new Foo(10); f.getX()",
    JsNumber(Finite(10.0)),
  )
}

pub fn class_static_method_test() -> Nil {
  assert_normal(
    "class Foo { static create() { return 99; } } Foo.create()",
    JsNumber(Finite(99.0)),
  )
}

pub fn class_typeof_test() -> Nil {
  assert_normal("class Foo {} typeof Foo", JsString("function"))
}

pub fn class_instanceof_test() -> Nil {
  assert_normal(
    "class Foo {} var f = new Foo(); f instanceof Foo",
    JsBool(True),
  )
}

pub fn class_expression_test() -> Nil {
  assert_normal(
    "var Foo = class { constructor(x) { this.x = x; } }; var f = new Foo(5); f.x",
    JsNumber(Finite(5.0)),
  )
}

pub fn class_field_initializer_test() -> Nil {
  assert_normal(
    "class Foo { x = 42; } var f = new Foo(); f.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn class_field_with_constructor_test() -> Nil {
  assert_normal(
    "class Foo { x = 1; constructor(y) { this.y = y; } } var f = new Foo(2); f.x + f.y",
    JsNumber(Finite(3.0)),
  )
}

pub fn class_multiple_methods_test() -> Nil {
  assert_normal(
    "class Calc { add(a, b) { return a + b; } mul(a, b) { return a * b; } } var c = new Calc(); c.add(2, 3) + c.mul(4, 5)",
    JsNumber(Finite(25.0)),
  )
}

pub fn class_method_not_enumerable_test() -> Nil {
  assert_normal(
    "class Foo { bar() {} } var f = new Foo(); var keys = []; for (var k in f) { keys.push(k); } keys.length",
    JsNumber(Finite(0.0)),
  )
}

pub fn class_no_constructor_test() -> Nil {
  assert_normal(
    "class Empty {} var e = new Empty(); typeof e",
    JsString("object"),
  )
}

// ============================================================================
// Function.prototype.call / apply / bind
// ============================================================================

pub fn function_call_basic_test() -> Nil {
  assert_normal(
    "function greet(x) { return this.prefix + x; }
     var obj = { prefix: 'Hello ' };
     greet.call(obj, 'world')",
    JsString("Hello world"),
  )
}

pub fn function_call_no_args_test() -> Nil {
  // Sloppy callee: primitive thisArg → boxed to Number wrapper object.
  // Verify via valueOf() + instanceof.
  assert_normal(
    "function getThis() { return this; }
     let t = getThis.call(42);
     t.valueOf() === 42 && t instanceof Number && typeof t === 'object'",
    JsBool(True),
  )
}

pub fn function_call_primitive_this_strict_test() -> Nil {
  // Strict callee: primitive thisArg passes through unboxed.
  assert_normal(
    "function getThis() { 'use strict'; return this; }
     getThis.call(42)",
    JsNumber(Finite(42.0)),
  )
}

// -- Wrapper prototype method tests (thisNumberValue / thisBooleanValue etc.) --

pub fn number_tostring_radix_test() -> Nil {
  assert_normal("(255).toString(16)", JsString("ff"))
}

pub fn number_tostring_radix_negative_test() -> Nil {
  assert_normal("(-255).toString(16)", JsString("-ff"))
}

pub fn number_tostring_default_radix_test() -> Nil {
  assert_normal("(255).toString()", JsString("255"))
}

pub fn number_tostring_radix_undefined_test() -> Nil {
  // explicit undefined → radix 10 per spec
  assert_normal("(255).toString(undefined)", JsString("255"))
}

pub fn number_tostring_radix_out_of_range_throws_test() -> Nil {
  assert_thrown("(255).toString(1)")
}

pub fn number_tostring_radix_37_throws_test() -> Nil {
  assert_thrown("(255).toString(37)")
}

pub fn number_tostring_nan_ignores_radix_test() -> Nil {
  assert_normal("NaN.toString(16)", JsString("NaN"))
}

pub fn number_valueof_on_primitive_test() -> Nil {
  assert_normal("(42).valueOf()", JsNumber(Finite(42.0)))
}

pub fn number_valueof_on_wrapper_test() -> Nil {
  assert_normal("new Number(42).valueOf()", JsNumber(Finite(42.0)))
}

pub fn number_valueof_cross_type_throws_test() -> Nil {
  // thisNumberValue rejects non-Number this
  assert_thrown("Number.prototype.valueOf.call(true)")
}

pub fn boolean_tostring_on_primitive_test() -> Nil {
  assert_normal("true.toString()", JsString("true"))
}

pub fn boolean_tostring_on_wrapper_test() -> Nil {
  assert_normal("new Boolean(false).toString()", JsString("false"))
}

pub fn boolean_valueof_on_primitive_test() -> Nil {
  assert_normal("false.valueOf()", JsBool(False))
}

pub fn boolean_valueof_cross_type_throws_test() -> Nil {
  assert_thrown("Boolean.prototype.valueOf.call(42)")
}

pub fn string_valueof_cross_type_throws_test() -> Nil {
  // String.prototype.valueOf uses thisStringValue, NOT generic ToString
  assert_thrown("String.prototype.valueOf.call(42)")
}

pub fn string_valueof_on_wrapper_test() -> Nil {
  assert_normal("new String('hi').valueOf()", JsString("hi"))
}

pub fn number_primitive_computed_field_test() -> Nil {
  // GetElem on number primitive with string key → delegates to Number.prototype
  assert_normal("typeof (5)['toString']", JsString("function"))
}

// -- Computed method call this-binding tests --
// obj[key](args) must bind `this` to obj. Pre-fix, this compiled to plain
// GetElem + Call, dropping the receiver entirely.

pub fn computed_method_call_binds_this_test() -> Nil {
  assert_normal(
    "let o = { x: 42, get: function() { return this.x; } };
     o['get']()",
    JsNumber(Finite(42.0)),
  )
}

pub fn computed_method_call_on_primitive_test() -> Nil {
  assert_normal("(255)['toString'](16)", JsString("ff"))
}

pub fn computed_method_call_with_dynamic_key_test() -> Nil {
  assert_normal(
    "let k = 'get';
     let o = { x: 7, get: function() { return this.x; } };
     o[k]()",
    JsNumber(Finite(7.0)),
  )
}

pub fn computed_method_call_with_spread_test() -> Nil {
  assert_normal(
    "let o = { sum: function(a, b, c) { return this.base + a + b + c; }, base: 100 };
     let args = [1, 2, 3];
     o['sum'](...args)",
    JsNumber(Finite(106.0)),
  )
}

// -- Generic array-like tests (ES §23.1.3: Array.prototype methods are generic) --
// require_array handles: real arrays, arguments, String wrappers, primitive
// strings, and plain objects with .length + indexed props.

pub fn array_like_join_on_string_primitive_test() -> Nil {
  // ToObject("abc") → String wrapper, index props from chars.
  assert_normal("Array.prototype.join.call('abc', '-')", JsString("a-b-c"))
}

pub fn array_like_join_on_string_wrapper_test() -> Nil {
  assert_normal(
    "Array.prototype.join.call(new String('xyz'), ',')",
    JsString("x,y,z"),
  )
}

pub fn array_like_join_on_plain_object_test() -> Nil {
  // LengthOfArrayLike: reads .length, gathers "0","1","2" from properties.
  assert_normal(
    "let o = {length: 3, 0: 'a', 1: 'b', 2: 'c'};
     Array.prototype.join.call(o, '+')",
    JsString("a+b+c"),
  )
}

pub fn array_like_join_sparse_object_test() -> Nil {
  // Missing indices treated as empty string (same as real sparse arrays).
  assert_normal(
    "let o = {length: 4, 0: 'a', 2: 'c'};
     Array.prototype.join.call(o, '-')",
    JsString("a--c-"),
  )
}

pub fn array_like_join_on_number_test() -> Nil {
  // Number wrapper has no .length → length 0 → empty string.
  assert_normal("Array.prototype.join.call(42)", JsString(""))
}

pub fn array_like_indexof_on_string_test() -> Nil {
  assert_normal(
    "Array.prototype.indexOf.call('hello', 'l')",
    JsNumber(Finite(2.0)),
  )
}

pub fn array_like_includes_on_string_test() -> Nil {
  assert_normal("Array.prototype.includes.call('hello', 'e')", JsBool(True))
}

pub fn array_like_slice_on_string_returns_array_test() -> Nil {
  // slice returns a real Array (not a string).
  assert_normal(
    "let r = Array.prototype.slice.call('hello', 1, 4);
     Array.isArray(r) && r.join('') === 'ell'",
    JsBool(True),
  )
}

pub fn array_like_map_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 3, 0: 10, 1: 20, 2: 30};
     Array.prototype.map.call(o, x => x * 2).join(',')",
    JsString("20,40,60"),
  )
}

pub fn array_like_filter_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 5, 0: 1, 1: 2, 2: 3, 3: 4, 4: 5};
     Array.prototype.filter.call(o, x => x > 2).join(',')",
    JsString("3,4,5"),
  )
}

pub fn array_like_reduce_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 4, 0: 1, 1: 2, 2: 3, 3: 4};
     Array.prototype.reduce.call(o, (a, b) => a + b, 0)",
    JsNumber(Finite(10.0)),
  )
}

pub fn array_like_slice_on_arguments_test() -> Nil {
  // The classic pre-rest-params pattern.
  assert_normal(
    "function f() { return Array.prototype.slice.call(arguments, 1).join(','); }
     f(1, 2, 3, 4)",
    JsString("2,3,4"),
  )
}

pub fn array_like_foreach_on_arguments_test() -> Nil {
  assert_normal(
    "function f() {
       let sum = 0;
       Array.prototype.forEach.call(arguments, x => { sum += x; });
       return sum;
     }
     f(1, 2, 3, 4)",
    JsNumber(Finite(10.0)),
  )
}

pub fn array_like_every_on_string_test() -> Nil {
  assert_normal(
    "Array.prototype.every.call('aaa', c => c === 'a')",
    JsBool(True),
  )
}

pub fn array_like_some_on_string_test() -> Nil {
  assert_normal(
    "Array.prototype.some.call('abc', c => c === 'b')",
    JsBool(True),
  )
}

pub fn array_like_find_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 3, 0: 5, 1: 10, 2: 15};
     Array.prototype.find.call(o, x => x > 7)",
    JsNumber(Finite(10.0)),
  )
}

// -- Generic array-like MUTATING method tests --
// push/pop/shift/unshift/reverse/fill should all work on plain objects
// and arguments, not just real arrays.

pub fn push_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 2, 0: 'a', 1: 'b'};
     let r = Array.prototype.push.call(o, 'c', 'd');
     '' + r + ',' + o.length + ',' + o[2] + ',' + o[3]",
    JsString("4,4,c,d"),
  )
}

pub fn push_on_arguments_test() -> Nil {
  assert_normal(
    "function f() {
       Array.prototype.push.call(arguments, 'd');
       return '' + arguments[3] + ',' + arguments.length;
     }
     f('a', 'b', 'c')",
    JsString("d,4"),
  )
}

pub fn pop_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 3, 0: 'a', 1: 'b', 2: 'c'};
     let v = Array.prototype.pop.call(o);
     '' + v + ',' + o.length",
    JsString("c,2"),
  )
}

pub fn pop_on_arguments_test() -> Nil {
  assert_normal(
    "function f() {
       let v = Array.prototype.pop.call(arguments);
       return '' + v + ',' + arguments.length;
     }
     f(1, 2, 3)",
    JsString("3,2"),
  )
}

pub fn shift_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 3, 0: 'a', 1: 'b', 2: 'c'};
     let v = Array.prototype.shift.call(o);
     '' + v + ',' + o.length + ',' + o[0] + ',' + o[1]",
    JsString("a,2,b,c"),
  )
}

pub fn unshift_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 2, 0: 'b', 1: 'c'};
     let r = Array.prototype.unshift.call(o, 'a');
     '' + r + ',' + o[0] + ',' + o[1] + ',' + o[2]",
    JsString("3,a,b,c"),
  )
}

pub fn reverse_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 3, 0: 'a', 1: 'b', 2: 'c'};
     Array.prototype.reverse.call(o);
     '' + o[0] + o[1] + o[2]",
    JsString("cba"),
  )
}

pub fn fill_on_plain_object_test() -> Nil {
  assert_normal(
    "let o = {length: 4, 0: 'a', 1: 'b', 2: 'c', 3: 'd'};
     Array.prototype.fill.call(o, 'x', 1, 3);
     '' + o[0] + o[1] + o[2] + o[3]",
    JsString("axxd"),
  )
}

pub fn reverse_on_arguments_test() -> Nil {
  assert_normal(
    "function f() {
       Array.prototype.reverse.call(arguments);
       return '' + arguments[0] + arguments[1] + arguments[2];
     }
     f(1, 2, 3)",
    JsString("321"),
  )
}

pub fn push_returns_new_length_on_empty_object_test() -> Nil {
  assert_normal(
    "let o = {};
     Array.prototype.push.call(o, 'x')",
    JsNumber(Finite(1.0)),
  )
}

pub fn pop_on_empty_object_test() -> Nil {
  // {length:0} or no length → pop returns undefined, sets length to 0
  assert_normal(
    "let o = {};
     let v = Array.prototype.pop.call(o);
     '' + v + ',' + o.length",
    JsString("undefined,0"),
  )
}

pub fn shift_on_empty_object_test() -> Nil {
  assert_normal(
    "let o = {};
     let v = Array.prototype.shift.call(o);
     '' + v + ',' + o.length",
    JsString("undefined,0"),
  )
}

// -- Accessor property tests (Object.defineProperty + get/set) --

pub fn accessor_getter_via_define_property_test() -> Nil {
  assert_normal(
    "let o = {};
     Object.defineProperty(o, 'x', { get: function() { return 42; } });
     o.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn accessor_setter_via_define_property_test() -> Nil {
  assert_normal(
    "let o = { _v: 0 };
     Object.defineProperty(o, 'x', {
       get: function() { return this._v; },
       set: function(v) { this._v = v * 2; }
     });
     o.x = 5;
     o.x",
    JsNumber(Finite(10.0)),
  )
}

pub fn accessor_setter_this_binding_test() -> Nil {
  // Setter's `this` should be the receiver object
  assert_normal(
    "let o = {};
     Object.defineProperty(o, 'x', {
       set: function(v) { this.stored = v; }
     });
     o.x = 99;
     o.stored",
    JsNumber(Finite(99.0)),
  )
}

pub fn accessor_no_setter_silently_fails_test() -> Nil {
  // Writing to getter-only accessor: no-op in sloppy mode
  assert_normal(
    "let o = {};
     Object.defineProperty(o, 'x', { get: function() { return 1; } });
     o.x = 999;
     o.x",
    JsNumber(Finite(1.0)),
  )
}

pub fn accessor_no_getter_returns_undefined_test() -> Nil {
  assert_normal(
    "let o = {};
     Object.defineProperty(o, 'x', { set: function(v) {} });
     o.x",
    JsUndefined,
  )
}

pub fn accessor_on_prototype_getter_test() -> Nil {
  // Getter on proto should fire with child as `this`
  assert_normal(
    "let proto = {};
     Object.defineProperty(proto, 'x', {
       get: function() { return this.val; }
     });
     let child = Object.create(proto);
     child.val = 42;
     child.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn accessor_on_prototype_setter_test() -> Nil {
  // Setter on proto should fire with child as `this`
  assert_normal(
    "let proto = {};
     Object.defineProperty(proto, 'x', {
       get: function() { return this._x; },
       set: function(v) { this._x = v * 2; }
     });
     let child = Object.create(proto);
     child.x = 5;
     child.x",
    JsNumber(Finite(10.0)),
  )
}

pub fn non_writable_proto_blocks_set_test() -> Nil {
  // Non-writable data property on proto prevents creating own on child
  assert_normal(
    "let proto = {};
     Object.defineProperty(proto, 'x', { value: 42, writable: false });
     let child = Object.create(proto);
     child.x = 99;
     child.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn writable_proto_allows_own_property_test() -> Nil {
  // Writable data property on proto: child gets its own copy
  assert_normal(
    "let proto = {};
     Object.defineProperty(proto, 'x', { value: 42, writable: true });
     let child = Object.create(proto);
     child.x = 99;
     '' + child.x + ',' + proto.x",
    JsString("99,42"),
  )
}

// -- Object literal getter/setter syntax --

pub fn object_literal_getter_test() -> Nil {
  assert_normal(
    "let o = { get x() { return 42; } };
     o.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn object_literal_setter_test() -> Nil {
  assert_normal(
    "let o = {
       _v: 0,
       get v() { return this._v; },
       set v(x) { this._v = x * 3; }
     };
     o.v = 7;
     o.v",
    JsNumber(Finite(21.0)),
  )
}

pub fn object_literal_getter_this_binding_test() -> Nil {
  assert_normal(
    "let o = {
       name: 'hello',
       get upper() { return this.name.toUpperCase(); }
     };
     o.upper",
    JsString("HELLO"),
  )
}

pub fn get_own_property_descriptor_accessor_test() -> Nil {
  assert_normal(
    "let o = {};
     Object.defineProperty(o, 'x', {
       get: function() { return 1; },
       configurable: true
     });
     let d = Object.getOwnPropertyDescriptor(o, 'x');
     typeof d.get === 'function' && d.set === undefined && d.configurable === true",
    JsBool(True),
  )
}

pub fn function_call_undefined_this_sloppy_test() -> Nil {
  // Sloppy callee: undefined thisArg → globalThis (an object).
  assert_normal(
    "function f() { return typeof this; }
     f.call(undefined)",
    JsString("object"),
  )
}

pub fn function_call_undefined_this_strict_test() -> Nil {
  // Strict callee: undefined thisArg passes through unchanged.
  assert_normal(
    "function f() { 'use strict'; return typeof this; }
     f.call(undefined)",
    JsString("undefined"),
  )
}

pub fn function_call_null_this_sloppy_test() -> Nil {
  // Sloppy callee: null thisArg → globalThis.
  assert_normal(
    "function f() { return this === globalThis; }
     f.call(null)",
    JsBool(True),
  )
}

pub fn function_apply_basic_test() -> Nil {
  assert_normal(
    "function add(a, b) { return a + b; }
     add.apply(null, [3, 4])",
    JsNumber(Finite(7.0)),
  )
}

pub fn function_apply_with_this_test() -> Nil {
  assert_normal(
    "function greet(x) { return this.prefix + x; }
     var obj = { prefix: 'Hi ' };
     greet.apply(obj, ['there'])",
    JsString("Hi there"),
  )
}

pub fn function_apply_no_args_test() -> Nil {
  assert_normal(
    "function f() { return 42; }
     f.apply(null)",
    JsNumber(Finite(42.0)),
  )
}

pub fn function_bind_basic_test() -> Nil {
  assert_normal(
    "function greet(x) { return this.name + ': ' + x; }
     var obj = { name: 'Alice' };
     var bound = greet.bind(obj);
     bound('hello')",
    JsString("Alice: hello"),
  )
}

pub fn function_bind_with_args_test() -> Nil {
  assert_normal(
    "function add(a, b) { return a + b; }
     var add5 = add.bind(null, 5);
     add5(3)",
    JsNumber(Finite(8.0)),
  )
}

pub fn function_bind_preserves_this_test() -> Nil {
  assert_normal(
    "function getX() { return this.x; }
     var obj = { x: 99 };
     var bound = getX.bind(obj);
     bound()",
    JsNumber(Finite(99.0)),
  )
}

pub fn function_bind_name_test() -> Nil {
  assert_normal(
    "function foo() {} var b = foo.bind(null); b.name",
    JsString("bound foo"),
  )
}

pub fn function_bind_constructor_test() -> Nil {
  assert_normal(
    "function Point(x, y) { this.x = x; this.y = y; }
     var BoundPoint = Point.bind(null, 10);
     var p = new BoundPoint(20);
     p.x + p.y",
    JsNumber(Finite(30.0)),
  )
}

pub fn function_call_chained_test() -> Nil {
  // call on a method that was itself obtained via call
  assert_normal(
    "function add(a, b) { return a + b; }
     var result = add.call(null, 10, 20);
     result",
    JsNumber(Finite(30.0)),
  )
}

pub fn function_apply_empty_array_test() -> Nil {
  assert_normal(
    "function f() { return 'ok'; }
     f.apply(null, [])",
    JsString("ok"),
  )
}

pub fn function_bind_multiple_args_test() -> Nil {
  assert_normal(
    "function sum(a, b, c) { return a + b + c; }
     var bound = sum.bind(null, 1, 2);
     bound(3)",
    JsNumber(Finite(6.0)),
  )
}

// ============================================================================
// Object.getOwnPropertyDescriptor
// ============================================================================

pub fn object_gopd_basic_test() -> Nil {
  // Basic data property descriptor
  assert_normal(
    "var obj = {x: 42};
     var desc = Object.getOwnPropertyDescriptor(obj, 'x');
     desc.value",
    JsNumber(Finite(42.0)),
  )
}

pub fn object_gopd_flags_test() -> Nil {
  // Regular data property has all flags true
  assert_normal(
    "var obj = {x: 1};
     var desc = Object.getOwnPropertyDescriptor(obj, 'x');
     '' + desc.writable + ',' + desc.enumerable + ',' + desc.configurable",
    JsString("true,true,true"),
  )
}

pub fn object_gopd_missing_key_test() -> Nil {
  // Non-existent property returns undefined
  assert_normal(
    "var obj = {x: 1};
     Object.getOwnPropertyDescriptor(obj, 'y')",
    JsUndefined,
  )
}

pub fn object_gopd_after_define_test() -> Nil {
  // Property defined with defineProperty respects flags
  assert_normal(
    "var obj = {};
     Object.defineProperty(obj, 'x', {value: 10, writable: false, enumerable: false, configurable: false});
     var desc = Object.getOwnPropertyDescriptor(obj, 'x');
     '' + desc.value + ',' + desc.writable + ',' + desc.enumerable + ',' + desc.configurable",
    JsString("10,false,false,false"),
  )
}

// ============================================================================
// Object.defineProperty
// ============================================================================

pub fn object_define_property_basic_test() -> Nil {
  assert_normal(
    "var obj = {};
     Object.defineProperty(obj, 'x', {value: 42, writable: true, enumerable: true, configurable: true});
     obj.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn object_define_property_non_writable_test() -> Nil {
  // Non-writable property can't be changed in sloppy mode (silently fails)
  assert_normal(
    "var obj = {};
     Object.defineProperty(obj, 'x', {value: 42, writable: false});
     obj.x = 100;
     obj.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn object_define_property_non_enumerable_test() -> Nil {
  // Non-enumerable property doesn't show up in for-in
  assert_normal(
    "var obj = {a: 1};
     Object.defineProperty(obj, 'b', {value: 2, enumerable: false});
     var keys = '';
     for (var k in obj) { keys = keys + k; }
     keys",
    JsString("a"),
  )
}

pub fn object_define_property_returns_obj_test() -> Nil {
  // defineProperty returns the target object
  assert_normal(
    "var obj = {};
     var result = Object.defineProperty(obj, 'x', {value: 1});
     result === obj",
    JsBool(True),
  )
}

pub fn object_define_property_throws_on_undefined_test() -> Nil {
  assert_thrown("Object.defineProperty(undefined, 'x', {})")
}

pub fn object_define_property_throws_on_null_test() -> Nil {
  assert_thrown("Object.defineProperty(null, 'x', {})")
}

pub fn object_define_property_throws_on_number_test() -> Nil {
  // defineProperty is strict: NO ToObject coercion, throws on all non-objects
  assert_thrown("Object.defineProperty(5, 'x', {})")
}

pub fn object_define_property_throws_on_string_test() -> Nil {
  assert_thrown("Object.defineProperty('foo', 'x', {})")
}

pub fn object_define_property_throws_on_non_object_descriptor_test() -> Nil {
  assert_thrown("Object.defineProperty({}, 'x', 5)")
}

pub fn object_define_property_throws_type_error_test() -> Nil {
  // Verify the thrown error IS a TypeError (constructor check per assert.throws)
  assert_normal(
    "try { Object.defineProperty(undefined, 'x', {}); 'no throw' }
     catch (e) { e.constructor === TypeError }",
    JsBool(True),
  )
}

pub fn object_gopd_throws_on_undefined_test() -> Nil {
  assert_thrown("Object.getOwnPropertyDescriptor(undefined, 'x')")
}

pub fn object_gopd_throws_on_null_test() -> Nil {
  assert_thrown("Object.getOwnPropertyDescriptor(null, 'x')")
}

pub fn object_gopd_coerces_number_test() -> Nil {
  // ToObject coercion: number wrapper has no own properties → undefined
  assert_normal("Object.getOwnPropertyDescriptor(5, 'x')", JsUndefined)
}

pub fn object_keys_throws_on_undefined_test() -> Nil {
  assert_thrown("Object.keys(undefined)")
}

pub fn object_keys_throws_on_null_test() -> Nil {
  assert_thrown("Object.keys(null)")
}

pub fn object_keys_coerces_number_test() -> Nil {
  // ToObject coercion: number wrapper has no own enumerable properties → []
  assert_normal("Object.keys(5).length", JsNumber(Finite(0.0)))
}

pub fn object_gopn_throws_on_null_test() -> Nil {
  assert_thrown("Object.getOwnPropertyNames(null)")
}

pub fn object_gopn_coerces_boolean_test() -> Nil {
  assert_normal(
    "Object.getOwnPropertyNames(true).length",
    JsNumber(Finite(0.0)),
  )
}

pub fn has_own_property_throws_on_null_this_test() -> Nil {
  assert_thrown("Object.prototype.hasOwnProperty.call(null, 'x')")
}

pub fn has_own_property_throws_on_undefined_this_test() -> Nil {
  assert_thrown("Object.prototype.hasOwnProperty.call(undefined, 'x')")
}

pub fn has_own_property_coerces_number_this_test() -> Nil {
  assert_normal("Object.prototype.hasOwnProperty.call(5, 'x')", JsBool(False))
}

pub fn array_join_throws_on_null_this_test() -> Nil {
  assert_thrown("Array.prototype.join.call(null)")
}

pub fn array_join_throws_on_undefined_this_test() -> Nil {
  assert_thrown("Array.prototype.join.call(undefined)")
}

pub fn array_push_throws_on_null_this_test() -> Nil {
  assert_thrown("Array.prototype.push.call(null, 1)")
}

// ============================================================================
// Array.prototype methods — Task #3
// ============================================================================

// --- strict_equal ±0 fix (prerequisite) ---

pub fn strict_equal_pos_neg_zero_test() -> Nil {
  // JS spec: +0 === -0 is true. Was broken due to BEAM =:= distinguishing them.
  assert_normal("0 === -0", JsBool(True))
}

pub fn strict_equal_neg_zero_pos_zero_test() -> Nil {
  assert_normal("-0 === 0", JsBool(True))
}

pub fn object_is_still_distinguishes_zeros_test() -> Nil {
  // Object.is uses SameValue, NOT ===. Must still distinguish ±0.
  assert_normal("Object.is(0, -0)", JsBool(False))
}

// --- pop ---

pub fn array_pop_basic_test() -> Nil {
  assert_normal("var a = [1,2,3]; a.pop()", JsNumber(Finite(3.0)))
}

pub fn array_pop_mutates_test() -> Nil {
  assert_normal("var a = [1,2,3]; a.pop(); a.length", JsNumber(Finite(2.0)))
}

pub fn array_pop_empty_test() -> Nil {
  assert_normal("[].pop()", JsUndefined)
}

pub fn array_pop_empty_length_test() -> Nil {
  assert_normal("var a = []; a.pop(); a.length", JsNumber(Finite(0.0)))
}

pub fn array_pop_throws_on_null_test() -> Nil {
  assert_thrown("Array.prototype.pop.call(null)")
}

// --- shift ---

pub fn array_shift_basic_test() -> Nil {
  assert_normal("var a = [1,2,3]; a.shift()", JsNumber(Finite(1.0)))
}

pub fn array_shift_mutates_test() -> Nil {
  assert_normal("var a = [1,2,3]; a.shift(); a.join(',')", JsString("2,3"))
}

pub fn array_shift_empty_test() -> Nil {
  assert_normal("[].shift()", JsUndefined)
}

// --- unshift ---

pub fn array_unshift_basic_test() -> Nil {
  assert_normal(
    "var a = [3,4]; a.unshift(1,2); a.join(',')",
    JsString("1,2,3,4"),
  )
}

pub fn array_unshift_returns_length_test() -> Nil {
  assert_normal("var a = [3]; a.unshift(1,2)", JsNumber(Finite(3.0)))
}

pub fn array_unshift_empty_args_test() -> Nil {
  assert_normal("var a = [1,2]; a.unshift()", JsNumber(Finite(2.0)))
}

// --- slice ---

pub fn array_slice_basic_test() -> Nil {
  assert_normal("[1,2,3,4].slice(1,3).join(',')", JsString("2,3"))
}

pub fn array_slice_negative_start_test() -> Nil {
  assert_normal("[1,2,3,4].slice(-2).join(',')", JsString("3,4"))
}

pub fn array_slice_negative_end_test() -> Nil {
  assert_normal("[1,2,3,4].slice(0,-1).join(',')", JsString("1,2,3"))
}

pub fn array_slice_no_args_test() -> Nil {
  assert_normal("[1,2,3].slice().join(',')", JsString("1,2,3"))
}

pub fn array_slice_does_not_mutate_test() -> Nil {
  assert_normal("var a = [1,2,3]; a.slice(1); a.length", JsNumber(Finite(3.0)))
}

pub fn array_slice_out_of_bounds_test() -> Nil {
  assert_normal("[1,2,3].slice(5).length", JsNumber(Finite(0.0)))
}

// --- concat ---

pub fn array_concat_basic_test() -> Nil {
  assert_normal("[1,2].concat([3,4]).join(',')", JsString("1,2,3,4"))
}

pub fn array_concat_non_array_test() -> Nil {
  assert_normal("[1].concat(2, 3).join(',')", JsString("1,2,3"))
}

pub fn array_concat_mixed_test() -> Nil {
  assert_normal("[1].concat([2], 3, [4,5]).join(',')", JsString("1,2,3,4,5"))
}

pub fn array_concat_does_not_mutate_test() -> Nil {
  assert_normal("var a = [1,2]; a.concat([3]); a.length", JsNumber(Finite(2.0)))
}

// --- reverse ---

pub fn array_reverse_basic_test() -> Nil {
  assert_normal("var a = [1,2,3]; a.reverse().join(',')", JsString("3,2,1"))
}

pub fn array_reverse_mutates_test() -> Nil {
  assert_normal("var a = [1,2,3]; a.reverse(); a.join(',')", JsString("3,2,1"))
}

pub fn array_reverse_returns_this_test() -> Nil {
  assert_normal("var a = [1,2]; a.reverse() === a", JsBool(True))
}

pub fn array_reverse_even_length_test() -> Nil {
  assert_normal("[1,2,3,4].reverse().join(',')", JsString("4,3,2,1"))
}

// --- fill ---

pub fn array_fill_basic_test() -> Nil {
  assert_normal("[1,2,3].fill(0).join(',')", JsString("0,0,0"))
}

pub fn array_fill_range_test() -> Nil {
  assert_normal("[1,2,3,4].fill(9,1,3).join(',')", JsString("1,9,9,4"))
}

pub fn array_fill_negative_test() -> Nil {
  assert_normal("[1,2,3,4].fill(9,-2).join(',')", JsString("1,2,9,9"))
}

pub fn array_fill_returns_this_test() -> Nil {
  assert_normal("var a = [1]; a.fill(0) === a", JsBool(True))
}

// --- at ---

pub fn array_at_positive_test() -> Nil {
  assert_normal("[1,2,3].at(1)", JsNumber(Finite(2.0)))
}

pub fn array_at_negative_test() -> Nil {
  assert_normal("[1,2,3].at(-1)", JsNumber(Finite(3.0)))
}

pub fn array_at_out_of_bounds_test() -> Nil {
  assert_normal("[1,2,3].at(5)", JsUndefined)
}

pub fn array_at_negative_out_of_bounds_test() -> Nil {
  assert_normal("[1,2,3].at(-5)", JsUndefined)
}

// --- indexOf ---

pub fn array_index_of_basic_test() -> Nil {
  assert_normal("[1,2,3,2].indexOf(2)", JsNumber(Finite(1.0)))
}

pub fn array_index_of_not_found_test() -> Nil {
  assert_normal("[1,2,3].indexOf(5)", JsNumber(Finite(-1.0)))
}

pub fn array_index_of_from_index_test() -> Nil {
  assert_normal("[1,2,3,2].indexOf(2, 2)", JsNumber(Finite(3.0)))
}

pub fn array_index_of_nan_test() -> Nil {
  // indexOf uses ===, NaN !== NaN
  assert_normal("[NaN].indexOf(NaN)", JsNumber(Finite(-1.0)))
}

pub fn array_index_of_neg_zero_test() -> Nil {
  // +0 === -0 under strict equality
  assert_normal("[-0].indexOf(0)", JsNumber(Finite(0.0)))
}

pub fn array_index_of_negative_from_test() -> Nil {
  assert_normal("[1,2,3,4].indexOf(3, -2)", JsNumber(Finite(2.0)))
}

// --- lastIndexOf ---

pub fn array_last_index_of_basic_test() -> Nil {
  assert_normal("[1,2,3,2].lastIndexOf(2)", JsNumber(Finite(3.0)))
}

pub fn array_last_index_of_from_index_test() -> Nil {
  assert_normal("[1,2,3,2].lastIndexOf(2, 2)", JsNumber(Finite(1.0)))
}

pub fn array_last_index_of_undefined_from_test() -> Nil {
  // Passing undefined explicitly → ToIntegerOrInfinity(undefined) = 0
  // Different from omitting the arg! [1,2,1].lastIndexOf(1, undefined) = 0
  assert_normal("[1,2,1].lastIndexOf(1, undefined)", JsNumber(Finite(0.0)))
}

pub fn array_last_index_of_no_second_arg_test() -> Nil {
  // No 2nd arg → default to length-1. [1,2,1].lastIndexOf(1) = 2
  assert_normal("[1,2,1].lastIndexOf(1)", JsNumber(Finite(2.0)))
}

// --- includes ---

pub fn array_includes_basic_test() -> Nil {
  assert_normal("[1,2,3].includes(2)", JsBool(True))
}

pub fn array_includes_not_found_test() -> Nil {
  assert_normal("[1,2,3].includes(5)", JsBool(False))
}

pub fn array_includes_nan_test() -> Nil {
  // includes uses SameValueZero, NaN equals NaN
  assert_normal("[NaN].includes(NaN)", JsBool(True))
}

pub fn array_includes_neg_zero_test() -> Nil {
  // SameValueZero: ±0 are equal
  assert_normal("[-0].includes(0)", JsBool(True))
}

pub fn array_includes_from_index_test() -> Nil {
  assert_normal("[1,2,3].includes(1, 1)", JsBool(False))
}

// --- forEach ---

pub fn array_for_each_basic_test() -> Nil {
  assert_normal(
    "var sum = 0; [1,2,3].forEach(function(x) { sum += x }); sum",
    JsNumber(Finite(6.0)),
  )
}

pub fn array_for_each_returns_undefined_test() -> Nil {
  assert_normal("[1,2,3].forEach(function(){})", JsUndefined)
}

pub fn array_for_each_index_arg_test() -> Nil {
  assert_normal(
    "var idxs = []; [10,20,30].forEach(function(v,i) { idxs.push(i) }); idxs.join(',')",
    JsString("0,1,2"),
  )
}

pub fn array_for_each_this_arg_test() -> Nil {
  assert_normal(
    "var ctx = {n: 100}; var out = 0; "
      <> "[1].forEach(function() { out = this.n }, ctx); out",
    JsNumber(Finite(100.0)),
  )
}

pub fn array_for_each_throws_non_callable_test() -> Nil {
  assert_thrown("[1].forEach(5)")
}

pub fn array_for_each_non_callable_message_test() -> Nil {
  assert_normal(
    "try { [1].forEach(5) } catch (e) { e.message }",
    JsString("number is not a function"),
  )
}

// --- map ---

pub fn array_map_basic_test() -> Nil {
  assert_normal(
    "[1,2,3].map(function(x) { return x * 2 }).join(',')",
    JsString("2,4,6"),
  )
}

pub fn array_map_index_arg_test() -> Nil {
  assert_normal(
    "[10,20].map(function(v,i) { return i }).join(',')",
    JsString("0,1"),
  )
}

pub fn array_map_preserves_length_test() -> Nil {
  assert_normal(
    "[1,2,3].map(function(x) { return x }).length",
    JsNumber(Finite(3.0)),
  )
}

pub fn array_map_does_not_mutate_test() -> Nil {
  assert_normal(
    "var a = [1,2]; a.map(function(x) { return x*10 }); a.join(',')",
    JsString("1,2"),
  )
}

pub fn array_map_throws_on_null_this_test() -> Nil {
  assert_thrown("Array.prototype.map.call(null, function(x){return x})")
}

pub fn array_map_propagates_throw_test() -> Nil {
  assert_thrown("[1].map(function() { throw 'boom' })")
}

// --- filter ---

pub fn array_filter_basic_test() -> Nil {
  assert_normal(
    "[1,2,3,4].filter(function(x) { return x % 2 === 0 }).join(',')",
    JsString("2,4"),
  )
}

pub fn array_filter_all_false_test() -> Nil {
  assert_normal(
    "[1,2,3].filter(function() { return false }).length",
    JsNumber(Finite(0.0)),
  )
}

pub fn array_filter_truthy_coercion_test() -> Nil {
  // Filter uses ToBoolean on the callback result
  assert_normal(
    "[0,1,2,0,3].filter(function(x) { return x }).join(',')",
    JsString("1,2,3"),
  )
}

// --- every ---

pub fn array_every_all_true_test() -> Nil {
  assert_normal("[1,2,3].every(function(x) { return x > 0 })", JsBool(True))
}

pub fn array_every_one_false_test() -> Nil {
  assert_normal("[1,2,-1,3].every(function(x) { return x > 0 })", JsBool(False))
}

pub fn array_every_empty_test() -> Nil {
  // Vacuously true
  assert_normal("[].every(function() { return false })", JsBool(True))
}

pub fn array_every_short_circuit_test() -> Nil {
  // Should stop after first false
  assert_normal(
    "var n = 0; [1,0,1,1].every(function(x) { n++; return x }); n",
    JsNumber(Finite(2.0)),
  )
}

// --- some ---

pub fn array_some_one_true_test() -> Nil {
  assert_normal("[1,2,3].some(function(x) { return x === 2 })", JsBool(True))
}

pub fn array_some_all_false_test() -> Nil {
  assert_normal("[1,2,3].some(function(x) { return x > 5 })", JsBool(False))
}

pub fn array_some_empty_test() -> Nil {
  assert_normal("[].some(function() { return true })", JsBool(False))
}

pub fn array_some_short_circuit_test() -> Nil {
  assert_normal(
    "var n = 0; [0,0,1,1].some(function(x) { n++; return x }); n",
    JsNumber(Finite(3.0)),
  )
}

// --- find ---

pub fn array_find_basic_test() -> Nil {
  assert_normal(
    "[1,2,3].find(function(x) { return x > 1 })",
    JsNumber(Finite(2.0)),
  )
}

pub fn array_find_not_found_test() -> Nil {
  assert_normal("[1,2,3].find(function(x) { return x > 5 })", JsUndefined)
}

// --- findIndex ---

pub fn array_find_index_basic_test() -> Nil {
  assert_normal(
    "[1,2,3].findIndex(function(x) { return x > 1 })",
    JsNumber(Finite(1.0)),
  )
}

pub fn array_find_index_not_found_test() -> Nil {
  assert_normal(
    "[1,2,3].findIndex(function(x) { return x > 5 })",
    JsNumber(Finite(-1.0)),
  )
}

// --- reduce ---

pub fn array_reduce_basic_test() -> Nil {
  assert_normal(
    "[1,2,3,4].reduce(function(a,b) { return a + b })",
    JsNumber(Finite(10.0)),
  )
}

pub fn array_reduce_with_init_test() -> Nil {
  assert_normal(
    "[1,2,3].reduce(function(a,b) { return a + b }, 10)",
    JsNumber(Finite(16.0)),
  )
}

pub fn array_reduce_empty_with_init_test() -> Nil {
  assert_normal(
    "[].reduce(function(a,b) { return a + b }, 42)",
    JsNumber(Finite(42.0)),
  )
}

pub fn array_reduce_empty_no_init_throws_test() -> Nil {
  assert_thrown("[].reduce(function(a,b) { return a + b })")
}

pub fn array_reduce_empty_error_message_test() -> Nil {
  assert_normal(
    "try { [].reduce(function(){}) } catch (e) { e.message }",
    JsString("Reduce of empty array with no initial value"),
  )
}

pub fn array_reduce_single_no_init_test() -> Nil {
  // Single element, no init → return that element, callback never called
  assert_normal(
    "[5].reduce(function(a,b) { return a + b })",
    JsNumber(Finite(5.0)),
  )
}

pub fn array_reduce_index_arg_test() -> Nil {
  assert_normal(
    "[10,20,30].reduce(function(a,b,i) { return a + i }, 0)",
    JsNumber(Finite(3.0)),
  )
}

// --- reduceRight ---

pub fn array_reduce_right_basic_test() -> Nil {
  assert_normal(
    "[1,2,3].reduceRight(function(a,b) { return a - b })",
    JsNumber(Finite(0.0)),
  )
}

pub fn array_reduce_right_with_init_test() -> Nil {
  // With init: 10 - 3 - 2 - 1 = 4 (right to left)
  assert_normal(
    "[1,2,3].reduceRight(function(a,b) { return a - b }, 10)",
    JsNumber(Finite(4.0)),
  )
}

pub fn array_reduce_right_order_test() -> Nil {
  assert_normal(
    "['a','b','c'].reduceRight(function(a,b) { return a + b })",
    JsString("cba"),
  )
}

pub fn array_reduce_right_empty_throws_test() -> Nil {
  assert_thrown("[].reduceRight(function(a,b) { return a })")
}

// --- Callback mutation edge case ---

pub fn array_map_callback_can_read_array_test() -> Nil {
  assert_normal(
    "[1,2,3].map(function(v,i,a) { return a.length }).join(',')",
    JsString("3,3,3"),
  )
}

// --- Generic array-like support (ES §23.1.3 — methods are intentionally generic) ---

pub fn array_map_on_plain_object_test() -> Nil {
  // {0:1, 1:2, length:2} — numeric keys stored as string props, length as property.
  // Array.prototype.map should treat this exactly like [1, 2].
  assert_normal(
    "Array.prototype.map.call({0:1, 1:2, length:2}, function(x){return x*10}).join(',')",
    JsString("10,20"),
  )
}

pub fn array_for_each_on_plain_object_test() -> Nil {
  // Verify iteration order and values on a simple array-like.
  assert_normal(
    "var s = ''; Array.prototype.forEach.call({0:'a', 1:'b', 2:'c', length:3}, function(v,i){s += i + v}); s",
    JsString("0a1b2c"),
  )
}

pub fn array_join_on_plain_object_test() -> Nil {
  // join is also generic — should read .length and indices from the plain object.
  assert_normal(
    "Array.prototype.join.call({0:'x', 1:'y', length:2}, '-')",
    JsString("x-y"),
  )
}

pub fn array_like_skips_holes_test() -> Nil {
  // Sparse array-like: index 1 is missing. forEach (SkipHoles) should not visit it.
  assert_normal(
    "var r = []; Array.prototype.forEach.call({0:'a', 2:'c', length:3}, function(v,i){r.push(i)}); r.join(',')",
    JsString("0,2"),
  )
}

pub fn array_like_length_coercion_test() -> Nil {
  // length '2.9' → ToLength truncates to 2. Index 2 is out of bounds, not visited.
  assert_normal(
    "Array.prototype.map.call({0:'a', 1:'b', 2:'c', length:'2.9'}, function(v){return v.toUpperCase()}).join(',')",
    JsString("A,B"),
  )
}

pub fn array_like_negative_length_test() -> Nil {
  // ToLength clamps negative/NaN → 0. Empty iteration.
  assert_normal(
    "Array.prototype.map.call({0:'x', length:-5}, function(v){return v}).length",
    JsNumber(Finite(0.0)),
  )
}

pub fn array_like_missing_length_test() -> Nil {
  // No .length property → LengthOfArrayLike returns 0 (ToLength(undefined) = 0).
  assert_normal(
    "Array.prototype.map.call({0:'x', 1:'y'}, function(v){return v}).length",
    JsNumber(Finite(0.0)),
  )
}

pub fn array_index_of_on_string_test() -> Nil {
  // Primitive strings are array-like via their char indices.
  // ToObject("abc") → String wrapper with .length=3 and "0"→"a", "1"→"b", "2"→"c".
  assert_normal(
    "Array.prototype.indexOf.call('abc', 'b')",
    JsNumber(Finite(1.0)),
  )
}

pub fn array_map_on_string_test() -> Nil {
  // Strings passed to Array.prototype.map: each char is an element.
  assert_normal(
    "Array.prototype.map.call('abc', function(c){return c.toUpperCase()}).join('')",
    JsString("ABC"),
  )
}

pub fn array_slice_on_arguments_test() -> Nil {
  // The classic "convert arguments to a real array" pattern.
  // arguments object is array-like; slice() should copy it into a true Array.
  assert_normal(
    "(function(){ return Array.prototype.slice.call(arguments).join(',') })(10, 20, 30)",
    JsString("10,20,30"),
  )
}

pub fn array_map_on_arguments_test() -> Nil {
  // arguments object is array-like — map should iterate its indices.
  assert_normal(
    "(function(){ return Array.prototype.map.call(arguments, function(x){return x+1}).join(',') })(1, 2, 3)",
    JsString("2,3,4"),
  )
}

pub fn array_filter_on_plain_object_test() -> Nil {
  // filter should produce a true Array from the array-like, keeping only passing elements.
  assert_normal(
    "Array.prototype.filter.call({0:1, 1:2, 2:3, length:3}, function(x){return x > 1}).join(',')",
    JsString("2,3"),
  )
}

pub fn array_method_on_number_this_test() -> Nil {
  // Per spec: ToObject(5) → Number wrapper with no .length → length 0 → empty result.
  // Previously we threw TypeError here (wrong — only null/undefined throw).
  assert_normal(
    "Array.prototype.map.call(5, function(x){return x}).length",
    JsNumber(Finite(0.0)),
  )
}

pub fn array_reduce_on_plain_object_test() -> Nil {
  // reduce over an array-like: sum of values.
  assert_normal(
    "Array.prototype.reduce.call({0:1, 1:2, 2:3, length:3}, function(a,b){return a+b})",
    JsNumber(Finite(6.0)),
  )
}

pub fn array_pop_on_plain_object_reads_correctly_test() -> Nil {
  // Mutating methods on array-likes: per spec pop should delete o['1'] and set
  // o.length=1. We don't yet implement the generic Set/Delete write-back for
  // mutating methods on non-arrays (see write_array guard), but at minimum the
  // READ side works — pop should return the last element, not undefined.
  assert_normal(
    "Array.prototype.pop.call({0:'a', 1:'b', length:2})",
    JsString("b"),
  )
}

pub fn array_pop_on_plain_object_does_not_corrupt_kind_test() -> Nil {
  // Guard: calling a mutating Array method on a plain object must NOT convert
  // it into an ArrayObject. This is a regression guard for write_array's kind
  // check — before the guard, pop would rewrite the slot as ArrayObject.
  assert_normal(
    "var o = {0:'a', 1:'b', length:2}; Array.prototype.pop.call(o); Array.isArray(o)",
    JsBool(False),
  )
}

// ============================================================================
// Object statics — Task #2
// ============================================================================

// --- Object.is (SameValue) ---

pub fn object_is_nan_test() -> Nil {
  assert_normal("Object.is(NaN, NaN)", JsBool(True))
}

pub fn object_is_pos_neg_zero_test() -> Nil {
  assert_normal("Object.is(0, -0)", JsBool(False))
}

pub fn object_is_neg_neg_zero_test() -> Nil {
  assert_normal("Object.is(-0, -0)", JsBool(True))
}

pub fn object_is_pos_pos_zero_test() -> Nil {
  assert_normal("Object.is(0, 0)", JsBool(True))
}

pub fn object_is_equal_numbers_test() -> Nil {
  assert_normal("Object.is(42, 42)", JsBool(True))
}

pub fn object_is_unequal_numbers_test() -> Nil {
  assert_normal("Object.is(1, 2)", JsBool(False))
}

pub fn object_is_strings_test() -> Nil {
  assert_normal("Object.is('a', 'a')", JsBool(True))
}

pub fn object_is_null_null_test() -> Nil {
  assert_normal("Object.is(null, null)", JsBool(True))
}

pub fn object_is_undefined_undefined_test() -> Nil {
  assert_normal("Object.is(undefined, undefined)", JsBool(True))
}

pub fn object_is_no_args_test() -> Nil {
  // Both default to undefined → SameValue(undefined, undefined) → true
  assert_normal("Object.is()", JsBool(True))
}

pub fn object_is_different_types_test() -> Nil {
  assert_normal("Object.is(1, '1')", JsBool(False))
}

pub fn object_is_same_ref_test() -> Nil {
  assert_normal("var o = {}; Object.is(o, o)", JsBool(True))
}

pub fn object_is_different_ref_test() -> Nil {
  assert_normal("Object.is({}, {})", JsBool(False))
}

// --- Object.create ---

pub fn object_create_null_proto_test() -> Nil {
  assert_normal(
    "Object.getPrototypeOf(Object.create(null)) === null",
    JsBool(True),
  )
}

pub fn object_create_object_proto_test() -> Nil {
  assert_normal(
    "var p = {x: 1}; var o = Object.create(p); o.x",
    JsNumber(Finite(1.0)),
  )
}

pub fn object_create_with_props_test() -> Nil {
  assert_normal(
    "Object.create(null, {a: {value: 42, enumerable: true}}).a",
    JsNumber(Finite(42.0)),
  )
}

pub fn object_create_throws_on_number_test() -> Nil {
  assert_thrown("Object.create(5)")
}

pub fn object_create_throws_on_undefined_test() -> Nil {
  assert_thrown("Object.create(undefined)")
}

pub fn object_create_throws_on_string_test() -> Nil {
  assert_thrown("Object.create('foo')")
}

pub fn object_create_returns_type_error_test() -> Nil {
  assert_normal(
    "try { Object.create(5) } catch (e) { e instanceof TypeError }",
    JsBool(True),
  )
}

pub fn instanceof_native_constructor_test() -> Nil {
  // Regression: instanceof used to reject NativeFunction on RHS.
  assert_normal("[] instanceof Array", JsBool(True))
}

pub fn instanceof_native_constructor_error_test() -> Nil {
  assert_normal("new TypeError('x') instanceof TypeError", JsBool(True))
}

pub fn instanceof_native_constructor_error_chain_test() -> Nil {
  // TypeError.prototype inherits from Error.prototype
  assert_normal("new TypeError('x') instanceof Error", JsBool(True))
}

pub fn instanceof_native_constructor_false_test() -> Nil {
  assert_normal("[] instanceof TypeError", JsBool(False))
}

// --- Object.assign ---

pub fn object_assign_basic_test() -> Nil {
  assert_normal("Object.assign({a:1}, {b:2}).b", JsNumber(Finite(2.0)))
}

pub fn object_assign_overwrite_test() -> Nil {
  assert_normal("Object.assign({a:1}, {a:2}).a", JsNumber(Finite(2.0)))
}

pub fn object_assign_multiple_sources_test() -> Nil {
  assert_normal(
    "Object.assign({}, {a:1}, {b:2}, {c:3}).c",
    JsNumber(Finite(3.0)),
  )
}

pub fn object_assign_returns_target_test() -> Nil {
  assert_normal("var t = {}; Object.assign(t, {a:1}) === t", JsBool(True))
}

pub fn object_assign_skips_null_source_test() -> Nil {
  assert_normal("Object.assign({}, null, {a:1}).a", JsNumber(Finite(1.0)))
}

pub fn object_assign_skips_undefined_source_test() -> Nil {
  assert_normal("Object.assign({}, undefined, {a:1}).a", JsNumber(Finite(1.0)))
}

pub fn object_assign_throws_on_null_target_test() -> Nil {
  assert_thrown("Object.assign(null)")
}

pub fn object_assign_throws_on_undefined_target_test() -> Nil {
  assert_thrown("Object.assign(undefined, {})")
}

pub fn object_assign_later_overrides_earlier_test() -> Nil {
  assert_normal(
    "Object.assign({}, {a:1}, {a:2}, {a:3}).a",
    JsNumber(Finite(3.0)),
  )
}

// --- Object.values / Object.entries ---

pub fn object_values_basic_test() -> Nil {
  assert_normal("Object.values({a:1, b:2, c:3}).join(',')", JsString("1,2,3"))
}

pub fn object_values_empty_test() -> Nil {
  assert_normal("Object.values({}).length", JsNumber(Finite(0.0)))
}

pub fn object_values_throws_on_null_test() -> Nil {
  assert_thrown("Object.values(null)")
}

pub fn object_values_coerces_number_test() -> Nil {
  assert_normal("Object.values(5).length", JsNumber(Finite(0.0)))
}

pub fn object_entries_basic_test() -> Nil {
  assert_normal("Object.entries({a:1}).length", JsNumber(Finite(1.0)))
}

pub fn object_entries_pair_test() -> Nil {
  assert_normal(
    "var e = Object.entries({x:42})[0]; e[0] + '=' + e[1]",
    JsString("x=42"),
  )
}

pub fn object_entries_multiple_test() -> Nil {
  assert_normal("Object.entries({a:1, b:2}).length", JsNumber(Finite(2.0)))
}

pub fn object_entries_throws_on_undefined_test() -> Nil {
  assert_thrown("Object.entries(undefined)")
}

// --- Object.hasOwn ---

pub fn object_has_own_true_test() -> Nil {
  assert_normal("Object.hasOwn({a:1}, 'a')", JsBool(True))
}

pub fn object_has_own_false_test() -> Nil {
  assert_normal("Object.hasOwn({a:1}, 'b')", JsBool(False))
}

pub fn object_has_own_not_inherited_test() -> Nil {
  assert_normal(
    "var p = {x:1}; var o = Object.create(p); Object.hasOwn(o, 'x')",
    JsBool(False),
  )
}

pub fn object_has_own_throws_on_null_test() -> Nil {
  assert_thrown("Object.hasOwn(null, 'x')")
}

pub fn object_has_own_throws_on_undefined_test() -> Nil {
  assert_thrown("Object.hasOwn(undefined, 'x')")
}

// --- Object.getPrototypeOf / setPrototypeOf ---

pub fn object_get_prototype_of_basic_test() -> Nil {
  assert_normal(
    "var p = {}; var o = Object.create(p); Object.getPrototypeOf(o) === p",
    JsBool(True),
  )
}

pub fn object_get_prototype_of_null_proto_test() -> Nil {
  assert_normal(
    "Object.getPrototypeOf(Object.create(null)) === null",
    JsBool(True),
  )
}

pub fn object_get_prototype_of_plain_test() -> Nil {
  assert_normal("Object.getPrototypeOf({}) === Object.prototype", JsBool(True))
}

pub fn object_get_prototype_of_number_test() -> Nil {
  // ToObject coerces 5 → Number wrapper → Number.prototype
  assert_normal("Object.getPrototypeOf(5) === Number.prototype", JsBool(True))
}

pub fn object_get_prototype_of_throws_on_null_test() -> Nil {
  assert_thrown("Object.getPrototypeOf(null)")
}

pub fn object_get_prototype_of_throws_on_undefined_test() -> Nil {
  assert_thrown("Object.getPrototypeOf(undefined)")
}

pub fn object_set_prototype_of_basic_test() -> Nil {
  assert_normal(
    "var p = {x:1}; var o = {}; Object.setPrototypeOf(o, p); o.x",
    JsNumber(Finite(1.0)),
  )
}

pub fn object_set_prototype_of_null_test() -> Nil {
  assert_normal(
    "var o = {}; Object.setPrototypeOf(o, null); Object.getPrototypeOf(o) === null",
    JsBool(True),
  )
}

pub fn object_set_prototype_of_returns_target_test() -> Nil {
  assert_normal(
    "var o = {}; Object.setPrototypeOf(o, null) === o",
    JsBool(True),
  )
}

pub fn object_set_prototype_of_throws_on_null_target_test() -> Nil {
  assert_thrown("Object.setPrototypeOf(null, {})")
}

pub fn object_set_prototype_of_throws_on_undefined_target_test() -> Nil {
  assert_thrown("Object.setPrototypeOf(undefined, {})")
}

pub fn object_set_prototype_of_throws_on_invalid_proto_test() -> Nil {
  assert_thrown("Object.setPrototypeOf({}, 5)")
}

pub fn object_set_prototype_of_primitive_target_passthrough_test() -> Nil {
  // Non-null/undefined primitive target: return unchanged, no throw.
  assert_normal("Object.setPrototypeOf(5, null)", JsNumber(Finite(5.0)))
}

pub fn object_set_prototype_of_cycle_throws_test() -> Nil {
  // Direct self-cycle: Object.setPrototypeOf(a, a)
  assert_thrown("var a = {}; Object.setPrototypeOf(a, a)")
}

pub fn object_set_prototype_of_cycle_indirect_throws_test() -> Nil {
  // Indirect cycle: a -> b, then b -> a
  assert_thrown(
    "var a = {}; var b = Object.create(a); Object.setPrototypeOf(a, b)",
  )
}

pub fn object_set_prototype_of_cycle_error_message_test() -> Nil {
  assert_normal(
    "try { var a = {}; Object.setPrototypeOf(a, a) } catch (e) { e.message }",
    JsString("Cyclic __proto__ value"),
  )
}

pub fn object_set_prototype_of_cycle_is_type_error_test() -> Nil {
  assert_normal(
    "try { var a = {}; Object.setPrototypeOf(a, a) } catch (e) { e instanceof TypeError }",
    JsBool(True),
  )
}

pub fn object_set_prototype_of_no_false_positive_test() -> Nil {
  // Setting to a sibling object's proto shouldn't trip cycle detection.
  assert_normal(
    "var proto = {}; var a = Object.create(proto); var b = {}; "
      <> "Object.setPrototypeOf(b, proto); "
      <> "Object.getPrototypeOf(b) === proto",
    JsBool(True),
  )
}

// --- Object.defineProperties ---

pub fn object_define_properties_basic_test() -> Nil {
  assert_normal(
    "var o = {}; Object.defineProperties(o, {a: {value: 1, enumerable: true}}); o.a",
    JsNumber(Finite(1.0)),
  )
}

pub fn object_define_properties_multiple_test() -> Nil {
  assert_normal(
    "var o = {}; Object.defineProperties(o, {a: {value: 1}, b: {value: 2}}); o.a + o.b",
    JsNumber(Finite(3.0)),
  )
}

pub fn object_define_properties_returns_target_test() -> Nil {
  assert_normal(
    "var o = {}; Object.defineProperties(o, {}) === o",
    JsBool(True),
  )
}

pub fn object_define_properties_throws_on_non_object_target_test() -> Nil {
  assert_thrown("Object.defineProperties(5, {})")
}

pub fn object_define_properties_throws_on_null_props_test() -> Nil {
  assert_thrown("Object.defineProperties({}, null)")
}

pub fn object_define_properties_primitive_props_no_throw_test() -> Nil {
  // ToObject on second arg: primitive → wrapper with no own enumerable props → no-op
  assert_normal("var o = {}; Object.defineProperties(o, 5) === o", JsBool(True))
}

pub fn object_define_properties_throws_on_non_object_descriptor_test() -> Nil {
  // Each descriptor value must be an object
  assert_thrown("Object.defineProperties({}, {a: 5})")
}

// --- Object.freeze / isFrozen / isExtensible / preventExtensions ---

pub fn object_freeze_returns_arg_test() -> Nil {
  assert_normal("var o = {}; Object.freeze(o) === o", JsBool(True))
}

pub fn object_freeze_primitive_passthrough_test() -> Nil {
  assert_normal("Object.freeze(5)", JsNumber(Finite(5.0)))
}

pub fn object_freeze_null_passthrough_test() -> Nil {
  assert_normal("Object.freeze(null)", JsNull)
}

pub fn object_freeze_undefined_passthrough_test() -> Nil {
  assert_normal("Object.freeze(undefined)", JsUndefined)
}

pub fn object_is_frozen_primitive_test() -> Nil {
  // Primitives are always "frozen"
  assert_normal("Object.isFrozen(5)", JsBool(True))
}

pub fn object_is_frozen_null_test() -> Nil {
  assert_normal("Object.isFrozen(null)", JsBool(True))
}

pub fn object_is_frozen_undefined_test() -> Nil {
  assert_normal("Object.isFrozen(undefined)", JsBool(True))
}

pub fn object_is_frozen_object_test() -> Nil {
  // Extensible objects are never frozen
  assert_normal("Object.isFrozen({})", JsBool(False))
}

pub fn object_is_extensible_primitive_test() -> Nil {
  assert_normal("Object.isExtensible(5)", JsBool(False))
}

pub fn object_is_extensible_null_test() -> Nil {
  assert_normal("Object.isExtensible(null)", JsBool(False))
}

pub fn object_is_extensible_object_test() -> Nil {
  assert_normal("Object.isExtensible({})", JsBool(True))
}

pub fn object_prevent_extensions_returns_arg_test() -> Nil {
  assert_normal("var o = {}; Object.preventExtensions(o) === o", JsBool(True))
}

pub fn object_prevent_extensions_primitive_passthrough_test() -> Nil {
  assert_normal("Object.preventExtensions(5)", JsNumber(Finite(5.0)))
}

pub fn object_prevent_extensions_null_passthrough_test() -> Nil {
  assert_normal("Object.preventExtensions(null)", JsNull)
}

pub fn object_prevent_extensions_makes_non_extensible_test() -> Nil {
  assert_normal(
    "var o = {}; Object.preventExtensions(o); Object.isExtensible(o)",
    JsBool(False),
  )
}

pub fn object_freeze_makes_non_extensible_test() -> Nil {
  assert_normal(
    "var o = {a:1}; Object.freeze(o); Object.isExtensible(o)",
    JsBool(False),
  )
}

pub fn object_freeze_makes_frozen_test() -> Nil {
  assert_normal(
    "var o = {a:1}; Object.freeze(o); Object.isFrozen(o)",
    JsBool(True),
  )
}

pub fn object_freeze_sets_non_writable_test() -> Nil {
  assert_normal(
    "var o = {a:1}; Object.freeze(o); Object.getOwnPropertyDescriptor(o,'a').writable",
    JsBool(False),
  )
}

pub fn object_freeze_sets_non_configurable_test() -> Nil {
  assert_normal(
    "var o = {a:1}; Object.freeze(o); Object.getOwnPropertyDescriptor(o,'a').configurable",
    JsBool(False),
  )
}

pub fn object_freeze_preserves_enumerable_test() -> Nil {
  assert_normal(
    "var o = {a:1}; Object.freeze(o); Object.getOwnPropertyDescriptor(o,'a').enumerable",
    JsBool(True),
  )
}

pub fn object_prevent_extensions_empty_is_frozen_test() -> Nil {
  // Empty non-extensible object is vacuously frozen (no props to check)
  assert_normal("Object.isFrozen(Object.preventExtensions({}))", JsBool(True))
}

pub fn object_prevent_extensions_nonempty_not_frozen_test() -> Nil {
  // preventExtensions alone doesn't freeze props — still writable/configurable
  assert_normal(
    "var o = {a:1}; Object.preventExtensions(o); Object.isFrozen(o)",
    JsBool(False),
  )
}

// --- Error message is Node-style ---

pub fn object_keys_error_message_test() -> Nil {
  assert_normal(
    "try { Object.keys(null) } catch (e) { e.message }",
    JsString("Cannot convert undefined or null to object"),
  )
}

// ============================================================================
// Object.getOwnPropertyNames
// ============================================================================

pub fn object_gopn_basic_test() -> Nil {
  assert_normal(
    "var obj = {a: 1, b: 2};
     var names = Object.getOwnPropertyNames(obj);
     names.length",
    JsNumber(Finite(2.0)),
  )
}

pub fn object_gopn_includes_non_enumerable_test() -> Nil {
  // getOwnPropertyNames includes non-enumerable properties
  assert_normal(
    "var obj = {a: 1};
     Object.defineProperty(obj, 'b', {value: 2, enumerable: false});
     Object.getOwnPropertyNames(obj).length",
    JsNumber(Finite(2.0)),
  )
}

// ============================================================================
// Object.keys
// ============================================================================

pub fn object_keys_basic_test() -> Nil {
  assert_normal(
    "var obj = {a: 1, b: 2};
     Object.keys(obj).length",
    JsNumber(Finite(2.0)),
  )
}

pub fn object_keys_excludes_non_enumerable_test() -> Nil {
  // keys excludes non-enumerable properties
  assert_normal(
    "var obj = {a: 1};
     Object.defineProperty(obj, 'b', {value: 2, enumerable: false});
     Object.keys(obj).length",
    JsNumber(Finite(1.0)),
  )
}

// ============================================================================
// Object.prototype.hasOwnProperty
// ============================================================================

pub fn has_own_property_basic_test() -> Nil {
  assert_normal(
    "var obj = {x: 1};
     obj.hasOwnProperty('x')",
    JsBool(True),
  )
}

pub fn has_own_property_missing_test() -> Nil {
  assert_normal(
    "var obj = {x: 1};
     obj.hasOwnProperty('y')",
    JsBool(False),
  )
}

pub fn has_own_property_inherited_test() -> Nil {
  // hasOwnProperty should return false for inherited properties
  assert_normal(
    "function Foo() {}
     Foo.prototype.bar = 1;
     var f = new Foo();
     '' + f.hasOwnProperty('bar') + ',' + ('bar' in f)",
    JsString("false,true"),
  )
}

pub fn has_own_property_via_call_test() -> Nil {
  // The test262 harness pattern: Function.prototype.call.bind(Object.prototype.hasOwnProperty)
  assert_normal(
    "var hasOwn = Object.prototype.hasOwnProperty;
     var obj = {x: 1};
     hasOwn.call(obj, 'x')",
    JsBool(True),
  )
}

pub fn test262_property_helper_pattern_test() -> Nil {
  // Simulates the key pattern from propertyHelper.js
  assert_normal(
    "var __defineProperty = Object.defineProperty;
     var __getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
     var __getOwnPropertyNames = Object.getOwnPropertyNames;
     var __hasOwnProperty = Function.prototype.call.bind(Object.prototype.hasOwnProperty);
     var obj = {a: 1};
     __defineProperty(obj, 'b', {value: 2, enumerable: false, writable: true, configurable: true});
     var desc = __getOwnPropertyDescriptor(obj, 'b');
     var names = __getOwnPropertyNames(obj);
     '' + desc.value + ',' + desc.enumerable + ',' + __hasOwnProperty(obj, 'a') + ',' + names.length",
    JsString("2,false,true,2"),
  )
}

// ============================================================================
// Array.prototype.join tests
// ============================================================================

pub fn array_join_default_separator_test() -> Nil {
  assert_normal("[1,2,3].join()", JsString("1,2,3"))
}

pub fn array_join_custom_separator_test() -> Nil {
  assert_normal("[1,2,3].join('-')", JsString("1-2-3"))
}

pub fn array_join_empty_array_test() -> Nil {
  assert_normal("[].join()", JsString(""))
}

pub fn array_join_undefined_null_elements_test() -> Nil {
  assert_normal("[1,undefined,null,2].join(',')", JsString("1,,,2"))
}

pub fn array_join_single_element_test() -> Nil {
  assert_normal("[42].join(',')", JsString("42"))
}

pub fn array_join_empty_separator_test() -> Nil {
  assert_normal("[1,2,3].join('')", JsString("123"))
}

// ============================================================================
// Array.prototype.push tests
// ============================================================================

pub fn array_push_basic_test() -> Nil {
  assert_normal(
    "var a = [1,2]; a.push(3); '' + a[0] + ',' + a[1] + ',' + a[2] + ',' + a.length",
    JsString("1,2,3,3"),
  )
}

pub fn array_push_multiple_args_test() -> Nil {
  assert_normal("var a = []; a.push(1,2,3); a.length", JsNumber(Finite(3.0)))
}

pub fn array_push_returns_length_test() -> Nil {
  assert_normal("var a = [10]; a.push(20)", JsNumber(Finite(2.0)))
}

// ============================================================================
// Object.prototype.propertyIsEnumerable tests
// ============================================================================

pub fn property_is_enumerable_own_enumerable_test() -> Nil {
  assert_normal("var o = {a: 1}; o.propertyIsEnumerable('a')", JsBool(True))
}

pub fn property_is_enumerable_non_enumerable_test() -> Nil {
  assert_normal(
    "var o = {};
     Object.defineProperty(o, 'x', {value: 1, enumerable: false});
     o.propertyIsEnumerable('x')",
    JsBool(False),
  )
}

pub fn property_is_enumerable_inherited_test() -> Nil {
  // Inherited properties are NOT own, so should return false
  assert_normal("var o = {}; o.propertyIsEnumerable('toString')", JsBool(False))
}

pub fn property_is_enumerable_missing_test() -> Nil {
  assert_normal("var o = {}; o.propertyIsEnumerable('nope')", JsBool(False))
}

// ============================================================================
// Math.pow tests
// ============================================================================

pub fn math_pow_basic_test() -> Nil {
  assert_normal("Math.pow(2, 10)", JsNumber(Finite(1024.0)))
}

pub fn math_pow_zero_exponent_test() -> Nil {
  assert_normal("Math.pow(5, 0)", JsNumber(Finite(1.0)))
}

pub fn math_pow_fractional_test() -> Nil {
  assert_normal("Math.pow(4, 0.5)", JsNumber(Finite(2.0)))
}

pub fn math_pow_two_32_test() -> Nil {
  assert_normal("Math.pow(2, 32)", JsNumber(Finite(4_294_967_296.0)))
}

// ============================================================================
// String.length and string indexing
// ============================================================================

pub fn string_length_test() -> Nil {
  assert_normal("'hello'.length", JsNumber(Finite(5.0)))
}

pub fn string_length_empty_test() -> Nil {
  assert_normal("''.length", JsNumber(Finite(0.0)))
}

pub fn string_index_test() -> Nil {
  assert_normal("'hello'[0]", JsString("h"))
}

pub fn string_index_last_test() -> Nil {
  assert_normal("'hello'[4]", JsString("o"))
}

pub fn string_index_out_of_bounds_test() -> Nil {
  assert_normal("'hello'[10]", JsUndefined)
}

pub fn string_index_negative_test() -> Nil {
  assert_normal("'hello'[-1]", JsUndefined)
}

pub fn string_length_via_var_test() -> Nil {
  assert_normal("var s = 'abc'; s.length", JsNumber(Finite(3.0)))
}

// ============================================================================
// String.prototype methods
// ============================================================================

pub fn string_char_at_test() -> Nil {
  assert_normal("'hello'.charAt(1)", JsString("e"))
}

pub fn string_char_at_oob_test() -> Nil {
  assert_normal("'hello'.charAt(10)", JsString(""))
}

pub fn string_char_code_at_test() -> Nil {
  assert_normal("'A'.charCodeAt(0)", JsNumber(Finite(65.0)))
}

pub fn string_char_code_at_oob_test() -> Nil {
  assert_normal("'A'.charCodeAt(5)", JsNumber(NaN))
}

pub fn string_index_of_test() -> Nil {
  assert_normal("'hello world'.indexOf('world')", JsNumber(Finite(6.0)))
}

pub fn string_index_of_not_found_test() -> Nil {
  assert_normal("'hello'.indexOf('xyz')", JsNumber(Finite(-1.0)))
}

pub fn string_index_of_from_test() -> Nil {
  assert_normal("'abcabc'.indexOf('abc', 1)", JsNumber(Finite(3.0)))
}

pub fn string_last_index_of_test() -> Nil {
  assert_normal("'abcabc'.lastIndexOf('abc')", JsNumber(Finite(3.0)))
}

pub fn string_includes_test() -> Nil {
  assert_normal("'hello world'.includes('world')", JsBool(True))
}

pub fn string_includes_false_test() -> Nil {
  assert_normal("'hello'.includes('xyz')", JsBool(False))
}

pub fn string_starts_with_test() -> Nil {
  assert_normal("'hello'.startsWith('hel')", JsBool(True))
}

pub fn string_starts_with_false_test() -> Nil {
  assert_normal("'hello'.startsWith('ell')", JsBool(False))
}

pub fn string_ends_with_test() -> Nil {
  assert_normal("'hello'.endsWith('llo')", JsBool(True))
}

pub fn string_ends_with_false_test() -> Nil {
  assert_normal("'hello'.endsWith('hel')", JsBool(False))
}

pub fn string_slice_test() -> Nil {
  assert_normal("'hello'.slice(1, 3)", JsString("el"))
}

pub fn string_slice_negative_test() -> Nil {
  assert_normal("'hello'.slice(-3)", JsString("llo"))
}

pub fn string_slice_no_end_test() -> Nil {
  assert_normal("'hello'.slice(2)", JsString("llo"))
}

pub fn string_substring_test() -> Nil {
  assert_normal("'hello'.substring(1, 3)", JsString("el"))
}

pub fn string_substring_swap_test() -> Nil {
  // substring swaps args if start > end
  assert_normal("'hello'.substring(3, 1)", JsString("el"))
}

pub fn string_to_lower_case_test() -> Nil {
  assert_normal("'HELLO'.toLowerCase()", JsString("hello"))
}

pub fn string_to_upper_case_test() -> Nil {
  assert_normal("'hello'.toUpperCase()", JsString("HELLO"))
}

pub fn string_trim_test() -> Nil {
  assert_normal("'  hello  '.trim()", JsString("hello"))
}

pub fn string_trim_start_test() -> Nil {
  assert_normal("'  hello  '.trimStart()", JsString("hello  "))
}

pub fn string_trim_end_test() -> Nil {
  assert_normal("'  hello  '.trimEnd()", JsString("  hello"))
}

pub fn string_prototype_concat_test() -> Nil {
  assert_normal("'hello'.concat(' ', 'world')", JsString("hello world"))
}

pub fn string_repeat_test() -> Nil {
  assert_normal("'ab'.repeat(3)", JsString("ababab"))
}

pub fn string_pad_start_test() -> Nil {
  assert_normal("'5'.padStart(3, '0')", JsString("005"))
}

pub fn string_pad_end_test() -> Nil {
  assert_normal("'5'.padEnd(3, '0')", JsString("500"))
}

pub fn string_at_test() -> Nil {
  assert_normal("'hello'.at(0)", JsString("h"))
}

pub fn string_at_negative_test() -> Nil {
  assert_normal("'hello'.at(-1)", JsString("o"))
}

pub fn string_at_oob_test() -> Nil {
  assert_normal("'hello'.at(10)", JsUndefined)
}

pub fn string_to_string_test() -> Nil {
  assert_normal("'hello'.toString()", JsString("hello"))
}

pub fn string_value_of_test() -> Nil {
  assert_normal("'hello'.valueOf()", JsString("hello"))
}

// ============================================================================
// Math methods (abs, floor, ceil, round, trunc, sqrt, max, min, log, sin, cos)
// ============================================================================

pub fn math_abs_positive_test() -> Nil {
  assert_normal("Math.abs(5)", JsNumber(Finite(5.0)))
}

pub fn math_abs_negative_test() -> Nil {
  assert_normal("Math.abs(-5)", JsNumber(Finite(5.0)))
}

pub fn math_abs_zero_test() -> Nil {
  assert_normal("Math.abs(0)", JsNumber(Finite(0.0)))
}

pub fn math_floor_test() -> Nil {
  assert_normal("Math.floor(4.7)", JsNumber(Finite(4.0)))
}

pub fn math_floor_negative_test() -> Nil {
  assert_normal("Math.floor(-4.1)", JsNumber(Finite(-5.0)))
}

pub fn math_ceil_test() -> Nil {
  assert_normal("Math.ceil(4.1)", JsNumber(Finite(5.0)))
}

pub fn math_ceil_negative_test() -> Nil {
  assert_normal("Math.ceil(-4.7)", JsNumber(Finite(-4.0)))
}

pub fn math_round_test() -> Nil {
  assert_normal("Math.round(4.5)", JsNumber(Finite(5.0)))
}

pub fn math_round_down_test() -> Nil {
  assert_normal("Math.round(4.4)", JsNumber(Finite(4.0)))
}

pub fn math_round_negative_half_test() -> Nil {
  // JS: Math.round(-0.5) → 0 (rounds toward +Infinity)
  assert_normal("Math.round(-0.5)", JsNumber(Finite(0.0)))
}

pub fn math_trunc_positive_test() -> Nil {
  assert_normal("Math.trunc(4.9)", JsNumber(Finite(4.0)))
}

pub fn math_trunc_negative_test() -> Nil {
  assert_normal("Math.trunc(-4.9)", JsNumber(Finite(-4.0)))
}

pub fn math_sqrt_test() -> Nil {
  assert_normal("Math.sqrt(9)", JsNumber(Finite(3.0)))
}

pub fn math_sqrt_negative_test() -> Nil {
  assert_normal("Math.sqrt(-1)", JsNumber(NaN))
}

pub fn math_max_test() -> Nil {
  assert_normal("Math.max(1, 3, 2)", JsNumber(Finite(3.0)))
}

pub fn math_min_test() -> Nil {
  assert_normal("Math.min(1, 3, 2)", JsNumber(Finite(1.0)))
}

pub fn math_max_no_args_test() -> Nil {
  assert_normal("Math.max()", JsNumber(value.NegInfinity))
}

pub fn math_min_no_args_test() -> Nil {
  assert_normal("Math.min()", JsNumber(value.Infinity))
}

// ============================================================================
// Math constants
// ============================================================================

pub fn math_pi_test() -> Nil {
  assert_normal("Math.PI", JsNumber(Finite(3.141592653589793)))
}

pub fn math_e_test() -> Nil {
  assert_normal("Math.E", JsNumber(Finite(2.718281828459045)))
}

pub fn math_pi_computation_test() -> Nil {
  // Use PI in a computation
  assert_normal("Math.floor(Math.PI)", JsNumber(Finite(3.0)))
}

// ============================================================================
// String.prototype.split (returns array, test via .join or .length)
// ============================================================================

pub fn string_split_length_test() -> Nil {
  assert_normal("'a,b,c'.split(',').length", JsNumber(Finite(3.0)))
}

pub fn string_split_rejoin_test() -> Nil {
  assert_normal("'a,b,c'.split(',').join('-')", JsString("a-b-c"))
}

pub fn string_split_empty_sep_test() -> Nil {
  assert_normal("'abc'.split('').length", JsNumber(Finite(3.0)))
}

pub fn string_split_no_match_test() -> Nil {
  assert_normal("'abc'.split('x').length", JsNumber(Finite(1.0)))
}

// ============================================================================
// Compound dot-member assignment
// ============================================================================

pub fn compound_dot_member_add_test() -> Nil {
  assert_normal("var o = {x: 1}; o.x += 2; o.x", JsNumber(Finite(3.0)))
}

pub fn compound_dot_member_sub_test() -> Nil {
  assert_normal("var o = {x: 10}; o.x -= 3; o.x", JsNumber(Finite(7.0)))
}

pub fn compound_dot_member_mul_test() -> Nil {
  assert_normal("var o = {x: 5}; o.x *= 4; o.x", JsNumber(Finite(20.0)))
}

// ============================================================================
// String/Number/Boolean constructors (type coercion)
// ============================================================================

pub fn string_constructor_coerce_number_test() -> Nil {
  assert_normal("String(42)", JsString("42"))
}

pub fn string_constructor_coerce_bool_test() -> Nil {
  assert_normal("String(true)", JsString("true"))
}

pub fn string_constructor_no_args_test() -> Nil {
  assert_normal("String()", JsString(""))
}

pub fn string_constructor_coerce_undefined_test() -> Nil {
  assert_normal("String(undefined)", JsString("undefined"))
}

pub fn number_constructor_coerce_string_test() -> Nil {
  assert_normal("Number('42')", JsNumber(Finite(42.0)))
}

pub fn number_constructor_coerce_bool_test() -> Nil {
  assert_normal("Number(true)", JsNumber(Finite(1.0)))
}

pub fn number_constructor_no_args_test() -> Nil {
  assert_normal("Number()", JsNumber(Finite(0.0)))
}

pub fn number_constructor_nan_test() -> Nil {
  assert_normal("Number('abc')", JsNumber(NaN))
}

pub fn boolean_constructor_truthy_test() -> Nil {
  assert_normal("Boolean(1)", JsBool(True))
}

pub fn boolean_constructor_falsy_test() -> Nil {
  assert_normal("Boolean(0)", JsBool(False))
}

pub fn boolean_constructor_empty_string_test() -> Nil {
  assert_normal("Boolean('')", JsBool(False))
}

pub fn boolean_constructor_no_args_test() -> Nil {
  assert_normal("Boolean()", JsBool(False))
}

pub fn boolean_constructor_object_truthy_test() -> Nil {
  assert_normal("Boolean({})", JsBool(True))
}

// ============================================================================
// Global utility functions
// ============================================================================

pub fn parse_int_basic_test() -> Nil {
  assert_normal("parseInt('42')", JsNumber(Finite(42.0)))
}

pub fn parse_int_hex_test() -> Nil {
  assert_normal("parseInt('0xff', 16)", JsNumber(Finite(255.0)))
}

pub fn parse_int_leading_chars_test() -> Nil {
  assert_normal("parseInt('123abc')", JsNumber(Finite(123.0)))
}

pub fn parse_int_nan_test() -> Nil {
  assert_normal("parseInt('abc')", JsNumber(NaN))
}

pub fn parse_float_basic_test() -> Nil {
  assert_normal("parseFloat('3.14')", JsNumber(Finite(3.14)))
}

pub fn parse_float_nan_test() -> Nil {
  assert_normal("parseFloat('abc')", JsNumber(NaN))
}

pub fn is_nan_true_test() -> Nil {
  assert_normal("isNaN(NaN)", JsBool(True))
}

pub fn is_nan_false_test() -> Nil {
  assert_normal("isNaN(42)", JsBool(False))
}

pub fn is_nan_string_coerce_test() -> Nil {
  assert_normal("isNaN('abc')", JsBool(True))
}

pub fn is_finite_true_test() -> Nil {
  assert_normal("isFinite(42)", JsBool(True))
}

pub fn is_finite_infinity_test() -> Nil {
  assert_normal("isFinite(Infinity)", JsBool(False))
}

pub fn is_finite_nan_test() -> Nil {
  assert_normal("isFinite(NaN)", JsBool(False))
}

// ============================================================================
// Number.isNaN / Number.isFinite / Number.isInteger (strict — no coercion)
// ============================================================================

pub fn number_is_nan_true_test() -> Nil {
  assert_normal("Number.isNaN(NaN)", JsBool(True))
}

pub fn number_is_nan_string_no_coerce_test() -> Nil {
  // Unlike global isNaN('abc'), Number.isNaN does NOT coerce
  assert_normal("Number.isNaN('abc')", JsBool(False))
}

pub fn number_is_nan_undefined_no_coerce_test() -> Nil {
  assert_normal("Number.isNaN(undefined)", JsBool(False))
}

pub fn number_is_nan_number_false_test() -> Nil {
  assert_normal("Number.isNaN(42)", JsBool(False))
}

pub fn number_is_finite_true_test() -> Nil {
  assert_normal("Number.isFinite(42)", JsBool(True))
}

pub fn number_is_finite_infinity_test() -> Nil {
  assert_normal("Number.isFinite(Infinity)", JsBool(False))
}

pub fn number_is_finite_string_no_coerce_test() -> Nil {
  // Unlike global isFinite('42'), Number.isFinite does NOT coerce
  assert_normal("Number.isFinite('42')", JsBool(False))
}

pub fn number_is_integer_true_test() -> Nil {
  assert_normal("Number.isInteger(42)", JsBool(True))
}

pub fn number_is_integer_float_test() -> Nil {
  assert_normal("Number.isInteger(42.5)", JsBool(False))
}

pub fn number_is_integer_zero_test() -> Nil {
  assert_normal("Number.isInteger(0)", JsBool(True))
}

pub fn number_is_integer_string_no_coerce_test() -> Nil {
  assert_normal("Number.isInteger('42')", JsBool(False))
}

pub fn number_is_integer_nan_test() -> Nil {
  assert_normal("Number.isInteger(NaN)", JsBool(False))
}

pub fn number_is_integer_infinity_test() -> Nil {
  assert_normal("Number.isInteger(Infinity)", JsBool(False))
}

// ============================================================================
// Class Inheritance Tests
// ============================================================================

pub fn class_extends_basic_test() -> Nil {
  assert_normal(
    "class Animal { constructor(name) { this.name = name; } }
     class Dog extends Animal { constructor(name) { super(name); this.type = 'dog'; } }
     var d = new Dog('Rex');
     d.name",
    JsString("Rex"),
  )
}

pub fn class_extends_type_field_test() -> Nil {
  assert_normal(
    "class Animal { constructor(name) { this.name = name; } }
     class Dog extends Animal { constructor(name) { super(name); this.type = 'dog'; } }
     var d = new Dog('Rex');
     d.type",
    JsString("dog"),
  )
}

pub fn class_extends_method_inheritance_test() -> Nil {
  assert_normal(
    "class Animal {
       constructor(name) { this.name = name; }
       speak() { return this.name + ' makes a noise'; }
     }
     class Dog extends Animal {
       constructor(name) { super(name); }
     }
     var d = new Dog('Rex');
     d.speak()",
    JsString("Rex makes a noise"),
  )
}

pub fn class_extends_method_override_test() -> Nil {
  assert_normal(
    "class Animal {
       constructor(name) { this.name = name; }
       speak() { return 'generic noise'; }
     }
     class Dog extends Animal {
       constructor(name) { super(name); }
       speak() { return this.name + ' barks'; }
     }
     var d = new Dog('Rex');
     d.speak()",
    JsString("Rex barks"),
  )
}

pub fn class_extends_instanceof_child_test() -> Nil {
  assert_normal(
    "class Animal { constructor() {} }
     class Dog extends Animal { constructor() { super(); } }
     var d = new Dog();
     d instanceof Dog",
    JsBool(True),
  )
}

pub fn class_extends_instanceof_parent_test() -> Nil {
  assert_normal(
    "class Animal { constructor() {} }
     class Dog extends Animal { constructor() { super(); } }
     var d = new Dog();
     d instanceof Animal",
    JsBool(True),
  )
}

pub fn class_extends_super_with_args_test() -> Nil {
  assert_normal(
    "class Base { constructor(x, y) { this.sum = x + y; } }
     class Child extends Base { constructor(x, y) { super(x, y); } }
     var c = new Child(3, 4);
     c.sum",
    JsNumber(Finite(7.0)),
  )
}

pub fn class_extends_default_constructor_test() -> Nil {
  // When no constructor is provided, a default one that calls super() is synthesized
  assert_normal(
    "class Base { constructor() { this.x = 42; } }
     class Child extends Base {}
     var c = new Child();
     c.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn class_extends_static_method_test() -> Nil {
  assert_normal(
    "class Base {
       static greet() { return 'hello'; }
     }
     class Child extends Base {
       constructor() { super(); }
     }
     Child.greet()",
    JsString("hello"),
  )
}

pub fn class_extends_this_tdz_test() -> Nil {
  // Accessing this before super() should throw ReferenceError
  assert_thrown(
    "class Base { constructor() {} }
     class Child extends Base {
       constructor() { this.x = 1; super(); }
     }
     new Child()",
  )
}

pub fn class_extends_no_super_return_test() -> Nil {
  // Returning from derived constructor without calling super() should throw
  assert_thrown(
    "class Base { constructor() {} }
     class Child extends Base {
       constructor() { }
     }
     new Child()",
  )
}

pub fn class_extends_multi_level_test() -> Nil {
  assert_normal(
    "class A { constructor() { this.a = 1; } }
     class B extends A { constructor() { super(); this.b = 2; } }
     class C extends B { constructor() { super(); this.c = 3; } }
     var obj = new C();
     obj.a + obj.b + obj.c",
    JsNumber(Finite(6.0)),
  )
}

pub fn class_extends_multi_level_method_test() -> Nil {
  assert_normal(
    "class A {
       constructor() {}
       foo() { return 'A'; }
     }
     class B extends A {
       constructor() { super(); }
     }
     class C extends B {
       constructor() { super(); }
     }
     var c = new C();
     c.foo()",
    JsString("A"),
  )
}

pub fn class_extends_expression_test() -> Nil {
  // class expression with extends
  assert_normal(
    "class Base { constructor() { this.x = 10; } }
     var Child = class extends Base { constructor() { super(); } };
     var c = new Child();
     c.x",
    JsNumber(Finite(10.0)),
  )
}

// ============================================================================
// Promise tests
// ============================================================================

pub fn promise_typeof_test() -> Nil {
  assert_normal("typeof Promise", JsString("function"))
}

pub fn promise_resolve_typeof_test() -> Nil {
  assert_normal("typeof Promise.resolve", JsString("function"))
}

pub fn promise_reject_typeof_test() -> Nil {
  assert_normal("typeof Promise.reject", JsString("function"))
}

pub fn promise_resolve_basic_test() -> Nil {
  // Promise.resolve returns a fulfilled promise
  assert_promise_resolves("Promise.resolve(42)", JsNumber(Finite(42.0)))
}

pub fn promise_reject_basic_test() -> Nil {
  assert_promise_rejects("Promise.reject('err')", JsString("err"))
}

pub fn promise_resolve_then_test() -> Nil {
  // .then transforms the value; returns a new promise with the result
  assert_promise_resolves(
    "Promise.resolve(1).then(function(x) { return x + 1; })",
    JsNumber(Finite(2.0)),
  )
}

pub fn promise_chaining_test() -> Nil {
  assert_promise_resolves(
    "Promise.resolve(1)
       .then(function(x) { return x + 1; })
       .then(function(x) { return x + 1; })",
    JsNumber(Finite(3.0)),
  )
}

pub fn promise_reject_catch_test() -> Nil {
  assert_promise_resolves(
    "Promise.reject('e').catch(function(e) { return e; })",
    JsString("e"),
  )
}

pub fn promise_then_throw_catch_test() -> Nil {
  assert_promise_resolves(
    "Promise.resolve(1)
       .then(function() { throw 'fail'; })
       .catch(function(e) { return e; })",
    JsString("fail"),
  )
}

pub fn promise_constructor_resolve_test() -> Nil {
  assert_promise_resolves(
    "new Promise(function(resolve) { resolve(99); })",
    JsNumber(Finite(99.0)),
  )
}

pub fn promise_constructor_then_test() -> Nil {
  assert_promise_resolves(
    "new Promise(function(resolve) { resolve(99); })
       .then(function(x) { return x + 1; })",
    JsNumber(Finite(100.0)),
  )
}

pub fn promise_constructor_reject_test() -> Nil {
  assert_promise_rejects(
    "new Promise(function(_, reject) { reject('no'); })",
    JsString("no"),
  )
}

pub fn promise_constructor_reject_catch_test() -> Nil {
  assert_promise_resolves(
    "new Promise(function(_, reject) { reject('no'); })
       .catch(function(e) { return e; })",
    JsString("no"),
  )
}

pub fn promise_constructor_throws_test() -> Nil {
  assert_promise_rejects(
    "new Promise(function() { throw 'boom'; })",
    JsString("boom"),
  )
}

pub fn promise_constructor_throws_catch_test() -> Nil {
  assert_promise_resolves(
    "new Promise(function() { throw 'boom'; })
       .catch(function(e) { return e; })",
    JsString("boom"),
  )
}

pub fn promise_multiple_resolve_test() -> Nil {
  assert_promise_resolves(
    "new Promise(function(resolve, reject) {
       resolve(1); resolve(2); reject(3);
     })",
    JsNumber(Finite(1.0)),
  )
}

pub fn promise_thenable_resolution_test() -> Nil {
  assert_promise_resolves(
    "Promise.resolve(Promise.resolve(42))",
    JsNumber(Finite(42.0)),
  )
}

pub fn promise_constructor_not_function_test() -> Nil {
  assert_thrown("new Promise(123)")
}

pub fn promise_reject_propagation_test() -> Nil {
  // Rejection propagates through .then with no reject handler, caught by .catch
  assert_promise_resolves(
    "Promise.reject('err')
       .then(function(x) { return 'wrong'; })
       .catch(function(e) { return e; })",
    JsString("err"),
  )
}

pub fn promise_resolve_identity_test() -> Nil {
  // Promise.resolve on a non-thenable returns a fulfilled promise
  assert_promise_resolves("Promise.resolve('hello')", JsString("hello"))
}

pub fn promise_then_identity_test() -> Nil {
  // .then with no handlers passes through the value
  assert_promise_resolves("Promise.resolve(42).then()", JsNumber(Finite(42.0)))
}

pub fn promise_finally_passthrough_test() -> Nil {
  // .finally preserves the resolved value
  assert_promise_resolves(
    "Promise.resolve(42).finally(function() {}).then(function(x) { return x })",
    JsNumber(Finite(42.0)),
  )
}

pub fn promise_finally_reject_passthrough_test() -> Nil {
  // .finally preserves the rejection reason
  assert_promise_rejects(
    "Promise.reject('err').finally(function() {})",
    JsString("err"),
  )
}

pub fn promise_finally_return_ignored_test() -> Nil {
  // .finally callback's return value is ignored (original value preserved)
  assert_promise_resolves(
    "Promise.resolve(42).finally(function() { return 99 })",
    JsNumber(Finite(42.0)),
  )
}

pub fn promise_finally_non_callable_test() -> Nil {
  // .finally with non-callable passes through
  assert_promise_resolves(
    "Promise.resolve(42).finally(undefined)",
    JsNumber(Finite(42.0)),
  )
}

// ============================================================================
// Arc global
// ============================================================================

pub fn arc_global_exists_test() -> Nil {
  assert_normal("typeof Arc", JsString("object"))
}

pub fn arc_peek_exists_test() -> Nil {
  assert_normal("typeof Arc.peek", JsString("function"))
}

pub fn arc_peek_resolved_type_test() -> Nil {
  assert_normal("Arc.peek(Promise.resolve(42)).type", JsString("resolved"))
}

pub fn arc_peek_resolved_value_test() -> Nil {
  assert_normal("Arc.peek(Promise.resolve(42)).value", JsNumber(Finite(42.0)))
}

pub fn arc_peek_rejected_type_test() -> Nil {
  assert_normal(
    "var r = Arc.peek(Promise.reject('oops')); r.type",
    JsString("rejected"),
  )
}

pub fn arc_peek_rejected_reason_test() -> Nil {
  assert_normal(
    "var r = Arc.peek(Promise.reject('oops')); r.reason",
    JsString("oops"),
  )
}

pub fn arc_peek_pending_type_test() -> Nil {
  assert_normal(
    "Arc.peek(new Promise(function() {})).type",
    JsString("pending"),
  )
}

pub fn arc_peek_pending_no_value_test() -> Nil {
  // pending result has no value field
  assert_normal("Arc.peek(new Promise(function() {})).value", JsUndefined)
}

pub fn arc_peek_not_promise_throws_test() -> Nil {
  assert_thrown("Arc.peek(42)")
}

pub fn arc_peek_no_arg_throws_test() -> Nil {
  assert_thrown("Arc.peek()")
}

pub fn arc_peek_object_throws_test() -> Nil {
  // plain object is not a promise
  assert_thrown("Arc.peek({})")
}

pub fn arc_peek_after_microtask_test() -> Nil {
  // A promise that resolves via .then is pending until microtasks drain.
  // Since Arc.peek is synchronous, peeking the outer .then() promise
  // immediately should show pending (microtasks haven't run yet).
  assert_normal(
    "var p = Promise.resolve(1).then(function(x) { return x + 1; });
     Arc.peek(p).type",
    JsString("pending"),
  )
}

// ============================================================================
// Generators
// ============================================================================

pub fn generator_basic_test() -> Nil {
  // Basic generator: yield values, then done
  assert_normal_number(
    "function* g() { yield 1; yield 2; yield 3; }
     var it = g();
     var a = it.next();
     a.value",
    1.0,
  )
}

pub fn generator_next_value_test() -> Nil {
  // .next() returns {value, done} objects
  assert_normal(
    "function* g() { yield 1; yield 2; }
     var it = g();
     it.next().done",
    JsBool(False),
  )
}

pub fn generator_done_test() -> Nil {
  // After all yields, done is true
  assert_normal(
    "function* g() { yield 1; }
     var it = g();
     it.next();
     it.next().done",
    JsBool(True),
  )
}

pub fn generator_return_value_test() -> Nil {
  // Return value appears as final {value, done: true}
  assert_normal_number(
    "function* g() { yield 1; return 42; }
     var it = g();
     it.next();
     it.next().value",
    42.0,
  )
}

pub fn generator_next_sends_value_test() -> Nil {
  // .next(val) sends val as the result of yield
  assert_normal_number(
    "function* g() { var x = yield 1; yield x + 10; }
     var it = g();
     it.next();
     it.next(5).value",
    15.0,
  )
}

pub fn generator_multiple_yields_test() -> Nil {
  // Multiple yields produce sequential values
  assert_normal_number(
    "function* count() { yield 1; yield 2; yield 3; }
     var it = count();
     var sum = 0;
     sum += it.next().value;
     sum += it.next().value;
     sum += it.next().value;
     sum",
    6.0,
  )
}

pub fn generator_completed_next_test() -> Nil {
  // Calling .next() on completed generator returns {value: undefined, done: true}
  assert_normal(
    "function* g() { yield 1; }
     var it = g();
     it.next();
     it.next();
     it.next().value",
    JsUndefined,
  )
}

pub fn generator_return_method_test() -> Nil {
  // .return(val) completes the generator with {value: val, done: true}
  assert_normal_number(
    "function* g() { yield 1; yield 2; yield 3; }
     var it = g();
     it.next();
     it.return(42).value",
    42.0,
  )
}

pub fn generator_return_method_done_test() -> Nil {
  // .return() marks generator as done
  assert_normal(
    "function* g() { yield 1; yield 2; }
     var it = g();
     it.next();
     it.return(42).done",
    JsBool(True),
  )
}

pub fn generator_return_then_next_test() -> Nil {
  // After .return(), subsequent .next() returns {value: undefined, done: true}
  assert_normal(
    "function* g() { yield 1; yield 2; }
     var it = g();
     it.next();
     it.return(42);
     it.next().done",
    JsBool(True),
  )
}

pub fn generator_throw_method_test() -> Nil {
  // .throw() can be caught inside the generator
  assert_normal_number(
    "function* g() {
       try { yield 1; } catch(e) { yield e + 10; }
     }
     var it = g();
     it.next();
     it.throw(5).value",
    15.0,
  )
}

pub fn generator_throw_uncaught_test() -> Nil {
  // .throw() with no catch propagates
  assert_thrown(
    "function* g() { yield 1; yield 2; }
     var it = g();
     it.next();
     it.throw(42);",
  )
}

pub fn generator_return_with_finally_test() -> Nil {
  // .return() runs finally blocks
  assert_normal_number(
    "var cleanup = 0;
     function* g() {
       try { yield 1; yield 2; }
       finally { cleanup = 99; }
     }
     var it = g();
     it.next();
     it.return(42);
     cleanup",
    99.0,
  )
}

pub fn generator_expression_test() -> Nil {
  // Generator expressions work too
  assert_normal_number(
    "var g = function*() { yield 10; yield 20; };
     var it = g();
     it.next().value + it.next().value",
    30.0,
  )
}

pub fn generator_with_params_test() -> Nil {
  // Generator functions accept parameters
  assert_normal_number(
    "function* range(start, end) {
       for (var i = start; i < end; i++) { yield i; }
     }
     var it = range(3, 6);
     var sum = 0;
     var r = it.next();
     while (!r.done) { sum += r.value; r = it.next(); }
     sum",
    12.0,
  )
}

pub fn generator_closure_test() -> Nil {
  // Generators capture closure variables
  assert_normal_number(
    "function makeGen(x) {
       return function*() { yield x; yield x * 2; };
     }
     var it = makeGen(5)();
     it.next().value + it.next().value",
    15.0,
  )
}

pub fn generator_for_of_test() -> Nil {
  // for-of loop over a generator
  assert_normal_number(
    "function* g() { yield 1; yield 2; yield 3; }
     var sum = 0;
     for (var x of g()) sum += x;
     sum",
    6.0,
  )
}

pub fn generator_for_of_break_test() -> Nil {
  // for-of with break exits early
  assert_normal_number(
    "function* g() { yield 1; yield 2; yield 3; yield 4; }
     var sum = 0;
     for (var x of g()) { if (x > 2) break; sum += x; }
     sum",
    3.0,
  )
}

pub fn generator_fibonacci_test() -> Nil {
  // Classic fibonacci generator
  assert_normal_number(
    "function* fib() {
       var a = 0, b = 1;
       while (true) { yield a; var t = a + b; a = b; b = t; }
     }
     var it = fib();
     var result;
     for (var i = 0; i < 10; i++) result = it.next().value;
     result",
    34.0,
  )
}

// ============================================================================
// Async/Await tests
// ============================================================================

pub fn async_return_value_test() -> Nil {
  // Async function that returns a value resolves the promise
  assert_promise_resolves(
    "async function f() { return 42; } f()",
    JsNumber(Finite(42.0)),
  )
}

pub fn async_return_undefined_test() -> Nil {
  // Async function with no return resolves to undefined
  assert_promise_resolves("async function f() {} f()", JsUndefined)
}

pub fn async_await_resolved_promise_test() -> Nil {
  // Await a resolved promise
  assert_promise_resolves(
    "async function f() { return await Promise.resolve(10); } f()",
    JsNumber(Finite(10.0)),
  )
}

pub fn async_await_plain_value_test() -> Nil {
  // Await a non-promise value wraps it in Promise.resolve
  assert_promise_resolves(
    "async function f() { return await 5; } f()",
    JsNumber(Finite(5.0)),
  )
}

pub fn async_multiple_awaits_test() -> Nil {
  // Multiple sequential awaits
  assert_promise_resolves(
    "async function f() {
       var a = await Promise.resolve(1);
       var b = await Promise.resolve(2);
       return a + b;
     }
     f()",
    JsNumber(Finite(3.0)),
  )
}

pub fn async_await_chain_test() -> Nil {
  // Await inside expression
  assert_promise_resolves(
    "async function f() {
       return await Promise.resolve(3) + await Promise.resolve(4);
     }
     f()",
    JsNumber(Finite(7.0)),
  )
}

pub fn async_throw_rejects_test() -> Nil {
  // Async function that throws rejects the promise
  assert_promise_rejects(
    "async function f() { throw 'error'; } f()",
    JsString("error"),
  )
}

pub fn async_await_rejected_test() -> Nil {
  // Await a rejected promise without try/catch rejects the outer promise
  assert_promise_rejects(
    "async function f() { return await Promise.reject('fail'); } f()",
    JsString("fail"),
  )
}

pub fn async_try_catch_test() -> Nil {
  // Try/catch inside async function catches rejected await
  assert_promise_resolves(
    "async function f() {
       try {
         await Promise.reject('oops');
       } catch (e) {
         return 'caught: ' + e;
       }
     }
     f()",
    JsString("caught: oops"),
  )
}

pub fn async_expression_test() -> Nil {
  // Async function expression
  assert_promise_resolves(
    "var f = async function() { return 99; }; f()",
    JsNumber(Finite(99.0)),
  )
}

pub fn async_arrow_test() -> Nil {
  // Async arrow function
  assert_promise_resolves("var f = async () => 77; f()", JsNumber(Finite(77.0)))
}

pub fn async_arrow_await_test() -> Nil {
  // Async arrow with await
  assert_promise_resolves(
    "var f = async (x) => await Promise.resolve(x * 2);
     f(21)",
    JsNumber(Finite(42.0)),
  )
}

pub fn async_sequential_test() -> Nil {
  // Sequential async operations
  assert_promise_resolves(
    "async function f() {
       var x = await 1;
       var y = await 2;
       var z = await 3;
       return x + y + z;
     }
     f()",
    JsNumber(Finite(6.0)),
  )
}

pub fn async_nested_call_test() -> Nil {
  // Async function calling another async function
  assert_promise_resolves(
    "async function double(x) { return x * 2; }
     async function main() {
       var a = await double(5);
       var b = await double(a);
       return b;
     }
     main()",
    JsNumber(Finite(20.0)),
  )
}

pub fn async_try_finally_test() -> Nil {
  // try/finally in async function
  assert_promise_resolves(
    "async function f() {
       var x = 0;
       try {
         x = await Promise.resolve(1);
       } finally {
         x = x + 10;
       }
       return x;
     }
     f()",
    JsNumber(Finite(11.0)),
  )
}

pub fn async_promise_chain_test() -> Nil {
  // Awaiting a .then() chain
  assert_promise_resolves(
    "async function f() {
       return await Promise.resolve(2).then(function(x) { return x * 3; });
     }
     f()",
    JsNumber(Finite(6.0)),
  )
}

// ============================================================================
// Promise ordering / microtask timing tests
// ============================================================================

pub fn promise_ordering_sync_before_microtask_test() -> Nil {
  // Synchronous code runs before .then() callbacks
  assert_promise_resolves(
    "var log = '';
     async function test() {
       log += '1';
       Promise.resolve().then(function() { log += '3'; });
       log += '2';
       await Promise.resolve();
       return log;
     }
     test()",
    JsString("123"),
  )
}

pub fn promise_ordering_then_chain_test() -> Nil {
  // A .then() chain and the test function's awaits interleave across drain rounds.
  // Each await advances one round; the chain also advances one step per round.
  assert_promise_resolves(
    "var log = '';
     async function test() {
       Promise.resolve()
         .then(function() { log += 'a'; })
         .then(function() { log += 'b'; })
         .then(function() { log += 'c'; });
       await Promise.resolve();
       log += '|';
       await Promise.resolve();
       log += '|';
       await Promise.resolve();
       return log;
     }
     test()",
    JsString("a|b|c"),
  )
}

pub fn promise_ordering_multiple_resolves_test() -> Nil {
  // Multiple .then() on the same promise run in registration order
  assert_promise_resolves(
    "var log = '';
     async function test() {
       var p = Promise.resolve();
       p.then(function() { log += '1'; });
       p.then(function() { log += '2'; });
       p.then(function() { log += '3'; });
       await Promise.resolve();
       return log;
     }
     test()",
    JsString("123"),
  )
}

pub fn promise_ordering_nested_then_test() -> Nil {
  // .then() inside a .then() runs on the next tick
  assert_promise_resolves(
    "var log = '';
     async function test() {
       Promise.resolve().then(function() {
         log += '1';
         Promise.resolve().then(function() { log += '3'; });
         log += '2';
       });
       await Promise.resolve();
       await Promise.resolve();
       return log;
     }
     test()",
    JsString("123"),
  )
}

pub fn promise_ordering_await_interleave_test() -> Nil {
  // Two async functions interleave via await
  assert_promise_resolves(
    "var log = '';
     async function a() {
       log += 'a1';
       await Promise.resolve();
       log += 'a2';
       await Promise.resolve();
       log += 'a3';
     }
     async function b() {
       log += 'b1';
       await Promise.resolve();
       log += 'b2';
       await Promise.resolve();
       log += 'b3';
     }
     async function test() {
       var pa = a();
       var pb = b();
       await pa;
       await pb;
       return log;
     }
     test()",
    JsString("a1b1a2b2a3b3"),
  )
}

pub fn promise_ordering_resolve_vs_then_test() -> Nil {
  // Promise.resolve(val).then(fn) — fn runs asynchronously, not synchronously
  assert_promise_resolves(
    "var log = '';
     async function test() {
       log += 'before';
       Promise.resolve('x').then(function(v) { log += v; });
       log += 'after';
       await Promise.resolve();
       return log;
     }
     test()",
    JsString("beforeafterx"),
  )
}

pub fn promise_ordering_reject_catch_test() -> Nil {
  // Rejection .catch() callback runs as microtask
  assert_promise_resolves(
    "var log = '';
     async function test() {
       log += '1';
       Promise.reject('e').catch(function() { log += '3'; });
       log += '2';
       await Promise.resolve();
       return log;
     }
     test()",
    JsString("123"),
  )
}

pub fn promise_ordering_finally_timing_test() -> Nil {
  // .finally() runs as microtask, value passes through
  assert_promise_resolves(
    "var log = '';
     async function test() {
       log += '1';
       var p = Promise.resolve('val').finally(function() { log += '2'; });
       log += '3';
       var result = await p;
       log += '4';
       return log + ':' + result;
     }
     test()",
    JsString("1324:val"),
  )
}

// ============================================================================
// REPL mode tests
// ============================================================================

/// Evaluate multiple REPL lines in sequence, returning the result of the last line.
fn run_repl_lines(
  lines: List(String),
) -> Result(#(value.JsValue, heap.Heap), String) {
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, globals) = builtins.globals(b, h)
  let env =
    vm.ReplEnv(
      globals:,
      const_globals: set.new(),
      symbol_descriptions: dict.new(),
    )
  run_repl_lines_loop(lines, h, b, env)
}

fn run_repl_lines_loop(
  lines: List(String),
  h: heap.Heap,
  b: common.Builtins,
  env: vm.ReplEnv,
) -> Result(#(value.JsValue, heap.Heap), String) {
  case lines {
    [] -> Error("no lines to evaluate")
    [line] -> {
      // Last line — return its result
      case eval_repl_line(line, h, b, env) {
        Ok(#(val, new_h, _new_env)) -> Ok(#(val, new_h))
        Error(err) -> Error(err)
      }
    }
    [line, ..rest] -> {
      case eval_repl_line(line, h, b, env) {
        Ok(#(_val, new_h, new_env)) ->
          run_repl_lines_loop(rest, new_h, b, new_env)
        Error(err) -> Error(err)
      }
    }
  }
}

fn eval_repl_line(
  source: String,
  h: heap.Heap,
  b: common.Builtins,
  env: vm.ReplEnv,
) -> Result(#(value.JsValue, heap.Heap, vm.ReplEnv), String) {
  case parser.parse(source, parser.Script) {
    Error(err) -> Error("parse error: " <> parser.parse_error_to_string(err))
    Ok(program) ->
      case compiler.compile_repl(program) {
        Error(compiler.Unsupported(desc)) ->
          Error("compile error: unsupported " <> desc)
        Error(compiler.BreakOutsideLoop) ->
          Error("compile error: break outside loop")
        Error(compiler.ContinueOutsideLoop) ->
          Error("compile error: continue outside loop")
        Ok(template) ->
          case vm.run_and_drain_repl(template, h, b, env) {
            Ok(#(vm.NormalCompletion(val, new_h), new_env)) ->
              Ok(#(val, new_h, new_env))
            Ok(#(vm.ThrowCompletion(val, _), _)) ->
              Error("throw: " <> string.inspect(val))
            Ok(#(vm.YieldCompletion(_, _), _)) -> Error("unexpected yield")
            Error(vm_err) -> Error("vm error: " <> string.inspect(vm_err))
          }
      }
  }
}

/// Evaluate REPL lines where the last line is expected to throw.
fn run_repl_lines_expect_throw(lines: List(String)) -> Result(Nil, String) {
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, globals) = builtins.globals(b, h)
  let env =
    vm.ReplEnv(
      globals:,
      const_globals: set.new(),
      symbol_descriptions: dict.new(),
    )
  run_repl_throw_loop(lines, h, b, env)
}

fn run_repl_throw_loop(
  lines: List(String),
  h: heap.Heap,
  b: common.Builtins,
  env: vm.ReplEnv,
) -> Result(Nil, String) {
  case lines {
    [] -> Error("no lines to evaluate")
    [line] -> {
      // Last line — expect it to throw
      case parser.parse(line, parser.Script) {
        Error(err) ->
          Error("parse error: " <> parser.parse_error_to_string(err))
        Ok(program) ->
          case compiler.compile_repl(program) {
            Error(_) -> Error("compile error on last line")
            Ok(template) ->
              case vm.run_and_drain_repl(template, h, b, env) {
                Ok(#(vm.ThrowCompletion(_, _), _)) -> Ok(Nil)
                Ok(#(vm.NormalCompletion(val, _), _)) ->
                  Error("expected throw, got normal: " <> string.inspect(val))
                _ -> Error("unexpected result")
              }
          }
      }
    }
    [line, ..rest] -> {
      case eval_repl_line(line, h, b, env) {
        Ok(#(_val, new_h, new_env)) ->
          run_repl_throw_loop(rest, new_h, b, new_env)
        Error(err) -> Error(err)
      }
    }
  }
}

fn assert_repl(lines: List(String), expected: value.JsValue) -> Nil {
  case run_repl_lines(lines) {
    Ok(#(val, _h)) -> {
      assert val == expected
    }
    Error(err) ->
      panic as {
        "REPL error: " <> err <> " for lines: " <> string.inspect(lines)
      }
  }
}

pub fn repl_let_persistence_test() -> Nil {
  assert_repl(["let x = 10", "x"], JsNumber(Finite(10.0)))
}

pub fn repl_const_persistence_test() -> Nil {
  assert_repl(["const x = 42", "x"], JsNumber(Finite(42.0)))
}

pub fn repl_var_persistence_test() -> Nil {
  assert_repl(["var x = 5", "x"], JsNumber(Finite(5.0)))
}

pub fn repl_function_persistence_test() -> Nil {
  assert_repl(["function f() { return 1; }", "f()"], JsNumber(Finite(1.0)))
}

pub fn repl_redeclaration_let_test() -> Nil {
  assert_repl(["let x = 1", "let x = 2", "x"], JsNumber(Finite(2.0)))
}

pub fn repl_redeclaration_const_test() -> Nil {
  assert_repl(["const x = 1", "const x = 2", "x"], JsNumber(Finite(2.0)))
}

pub fn repl_const_assignment_throws_test() -> Result(Nil, String) {
  // const x = 1; x = 2 on next line should throw TypeError
  let assert Ok(Nil) = run_repl_lines_expect_throw(["const x = 1", "x = 2"])
}

pub fn repl_const_assignment_same_line_throws_test() -> Result(Nil, String) {
  // const x = 1; x = 2 on same line should also throw
  let assert Ok(Nil) = run_repl_lines_expect_throw(["const x = 1; x = 2"])
}

pub fn repl_let_reassignment_test() -> Nil {
  // let allows reassignment across lines
  assert_repl(["let y = 1", "y = 2", "y"], JsNumber(Finite(2.0)))
}

pub fn globalthis_typeof_test() -> Nil {
  assert_normal("typeof globalThis", JsString("object"))
}

pub fn globalthis_has_builtins_test() -> Nil {
  assert_normal("globalThis.Object === Object", JsBool(True))
}

// ============================================================================
// Symbol tests
// ============================================================================

pub fn typeof_symbol_test() -> Nil {
  assert_normal("typeof Symbol.toStringTag", JsString("symbol"))
}

pub fn symbol_equality_test() -> Nil {
  assert_normal("Symbol('x') === Symbol('x')", JsBool(False))
}

pub fn symbol_equality_same_test() -> Nil {
  assert_normal("var s = Symbol(); s === s", JsBool(True))
}

pub fn symbol_property_test() -> Nil {
  assert_normal(
    "var s = Symbol(); var o = {}; o[s] = 42; o[s]",
    JsNumber(Finite(42.0)),
  )
}

pub fn symbol_to_string_tag_math_test() -> Nil {
  assert_normal("Math[Symbol.toStringTag]", JsString("Math"))
}

pub fn symbol_to_string_tag_custom_test() -> Nil {
  assert_normal(
    "var o = {}; o[Symbol.toStringTag] = 'Foo'; typeof o",
    JsString("object"),
  )
}

pub fn symbol_well_known_iterator_test() -> Nil {
  assert_normal("typeof Symbol.iterator", JsString("symbol"))
}

pub fn symbol_constructor_no_args_test() -> Nil {
  assert_normal("typeof Symbol()", JsString("symbol"))
}

// ============================================================================
// ToPrimitive + Object.prototype.toString/valueOf
// ============================================================================

pub fn object_to_string_plain_test() -> Nil {
  assert_normal("var o = {}; o.toString()", JsString("[object Object]"))
}

pub fn object_to_string_array_test() -> Nil {
  assert_normal(
    "Object.prototype.toString.call([1,2,3])",
    JsString("[object Array]"),
  )
}

pub fn object_to_string_function_test() -> Nil {
  assert_normal(
    "Object.prototype.toString.call(function(){})",
    JsString("[object Function]"),
  )
}

pub fn object_to_string_null_test() -> Nil {
  assert_normal(
    "Object.prototype.toString.call(null)",
    JsString("[object Null]"),
  )
}

pub fn object_to_string_undefined_test() -> Nil {
  assert_normal(
    "Object.prototype.toString.call(undefined)",
    JsString("[object Undefined]"),
  )
}

pub fn object_value_of_test() -> Nil {
  assert_normal("var o = {}; o.valueOf() === o", JsBool(True))
}

pub fn to_primitive_custom_to_string_test() -> Nil {
  assert_normal(
    "var o = { toString: function() { return 'hello'; } }; '' + o",
    JsString("hello"),
  )
}

pub fn to_primitive_custom_value_of_test() -> Nil {
  assert_normal_number(
    "var o = { valueOf: function() { return 42; } }; o + 1",
    43.0,
  )
}

pub fn to_primitive_string_function_test() -> Nil {
  assert_normal(
    "var o = { toString: function() { return 'custom'; } }; String(o)",
    JsString("custom"),
  )
}

pub fn to_primitive_add_both_objects_test() -> Nil {
  assert_normal(
    "var a = { valueOf: function() { return 1; } };
     var b = { valueOf: function() { return 2; } };
     a + b",
    JsNumber(Finite(3.0)),
  )
}

pub fn to_primitive_add_string_concat_test() -> Nil {
  assert_normal(
    "var o = { toString: function() { return 'world'; } };
     'hello ' + o",
    JsString("hello world"),
  )
}

pub fn to_primitive_default_to_string_test() -> Nil {
  // Default Object.prototype.toString returns "[object Object]"
  assert_normal("'' + {}", JsString("[object Object]"))
}

pub fn object_to_string_tag_test() -> Nil {
  assert_normal(
    "var o = {}; o[Symbol.toStringTag] = 'MyTag'; Object.prototype.toString.call(o)",
    JsString("[object MyTag]"),
  )
}

pub fn arguments_is_not_array_test() -> Nil {
  // Per spec, arguments is an ordinary object with Object.prototype, not an array
  assert_normal(
    "function f() { return Array.isArray(arguments); } f(1, 2)",
    JsBool(False),
  )
}

pub fn arguments_proto_is_object_proto_test() -> Nil {
  assert_normal(
    "function f() { return Object.getPrototypeOf(arguments) === Object.prototype; } f()",
    JsBool(True),
  )
}

pub fn arguments_unmapped_index_write_no_alias_test() -> Nil {
  // Unmapped: writing arguments[0] does NOT change the param
  assert_normal_number(
    "function f(a) { arguments[0] = 99; return a; } f(1)",
    1.0,
  )
}

pub fn arguments_unmapped_param_write_no_alias_test() -> Nil {
  // Unmapped: writing the param does NOT change arguments[0]
  assert_normal_number(
    "function f(a) { a = 99; return arguments[0]; } f(1)",
    1.0,
  )
}

pub fn arguments_spread_test() -> Nil {
  // arguments is iterable — [...arguments] works
  assert_normal(
    "function f() { return [...arguments].join(','); } f(1, 2, 3)",
    JsString("1,2,3"),
  )
}

pub fn arguments_for_of_test() -> Nil {
  assert_normal_number(
    "function f() { let s = 0; for (const x of arguments) s += x; return s; } f(1, 2, 3, 4)",
    10.0,
  )
}

pub fn arguments_apply_test() -> Nil {
  // Classic ES5 pattern: forward arguments via apply
  assert_normal_number(
    "function g(a, b, c) { return a + b + c; } function f() { return g.apply(null, arguments); } f(1, 2, 3)",
    6.0,
  )
}

pub fn arguments_arrow_ignores_own_args_test() -> Nil {
  // Arrow's own invocation args don't shadow the inherited arguments object
  assert_normal_number(
    "function f() { const g = (x, y) => arguments[0]; return g(100, 200); } f(7)",
    7.0,
  )
}

pub fn arguments_typeof_test() -> Nil {
  assert_normal(
    "function f() { return typeof arguments; } f()",
    JsString("object"),
  )
}

pub fn arguments_in_operator_test() -> Nil {
  assert_normal("function f() { return 0 in arguments; } f('a')", JsBool(True))
}

pub fn arguments_in_operator_missing_test() -> Nil {
  assert_normal("function f() { return 5 in arguments; } f('a')", JsBool(False))
}

pub fn arguments_length_in_test() -> Nil {
  assert_normal(
    "function f() { return 'length' in arguments; } f()",
    JsBool(True),
  )
}

pub fn arguments_delete_index_test() -> Nil {
  assert_normal(
    "function f() { delete arguments[1]; return arguments[1]; } f(1, 2, 3)",
    value.JsUndefined,
  )
}

pub fn arguments_excess_indexed_test() -> Nil {
  // Indices past declared arity still work
  assert_normal_number("function f(a) { return arguments[2]; } f(1, 2, 3)", 3.0)
}

// ============================================================================
// strict mode runtime enforcement
// ============================================================================

pub fn strict_undeclared_assign_throws_test() -> Nil {
  // In strict mode, assigning to an undeclared variable throws ReferenceError
  assert_thrown("function f() { 'use strict'; undeclared = 1; } f()")
}

pub fn strict_undeclared_assign_top_level_test() -> Nil {
  // Top-level "use strict" directive also enforces
  assert_thrown("'use strict'; undeclared = 1;")
}

pub fn sloppy_undeclared_assign_creates_global_test() -> Nil {
  // In sloppy mode, assigning to an undeclared variable creates a global
  assert_normal_number(
    "function f() { undeclared = 42; } f(); undeclared",
    42.0,
  )
}

pub fn strict_existing_global_write_ok_test() -> Nil {
  // Writing to an EXISTING global in strict mode is fine
  assert_normal_number(
    "var x = 1; function f() { 'use strict'; x = 2; } f(); x",
    2.0,
  )
}

pub fn strict_inherited_from_parent_test() -> Nil {
  // Strictness is inherited — nested function in strict parent is strict
  assert_thrown(
    "function outer() {
       'use strict';
       function inner() { undeclared = 1; }
       inner();
     }
     outer()",
  )
}

pub fn strict_not_inherited_upward_test() -> Nil {
  // A strict nested function does NOT make the parent strict
  assert_normal_number(
    "function outer() {
       function inner() { 'use strict'; }
       undeclared = 99;
       return undeclared;
     }
     outer()",
    99.0,
  )
}

pub fn strict_arrow_inherits_test() -> Nil {
  // Arrow functions inherit strictness from enclosing scope
  assert_thrown(
    "function f() {
       'use strict';
       let g = () => { undeclared = 1; };
       g();
     }
     f()",
  )
}

pub fn strict_class_method_test() -> Nil {
  // Class bodies are always strict (ES §15.7.1)
  assert_thrown(
    "class C { m() { undeclared = 1; } }
     new C().m()",
  )
}

pub fn strict_class_constructor_test() -> Nil {
  // Class constructors are strict too
  assert_thrown(
    "class C { constructor() { undeclared = 1; } }
     new C()",
  )
}

pub fn strict_class_in_sloppy_no_leak_test() -> Nil {
  // Class strictness doesn't leak into surrounding sloppy scope
  assert_normal_number(
    "class C { m() { return 1; } }
     undeclaredFromSloppy = 42;
     undeclaredFromSloppy",
    42.0,
  )
}

pub fn strict_directive_not_first_test() -> Nil {
  // "use strict" after non-directive statement has no effect (spec: directive
  // prologue ends at first non-string-literal expression statement)
  assert_normal_number(
    "function f() {
       var x = 0;
       'use strict';
       undeclared = 99;
       return undeclared;
     }
     f()",
    99.0,
  )
}

pub fn strict_directive_after_other_directive_test() -> Nil {
  // Multiple directives in prologue are fine — "use strict" doesn't have to be first
  assert_thrown(
    "function f() {
       'some other directive';
       'use strict';
       undeclared = 1;
     }
     f()",
  )
}

pub fn strict_reference_error_message_test() -> Nil {
  // Verify the error message
  assert_normal(
    "function f() { 'use strict'; try { undeclared = 1; } catch (e) { return e.message; } }
     f()",
    JsString("undeclared is not defined"),
  )
}

pub fn strict_reference_error_type_test() -> Nil {
  // Verify it's a ReferenceError (not TypeError or generic Error)
  assert_normal(
    "function f() { 'use strict'; try { undeclared = 1; } catch (e) { return e instanceof ReferenceError; } }
     f()",
    JsBool(True),
  )
}

// ============================================================================
// Module compilation
// ============================================================================

/// Parse + compile + run JS module source via the module system.
fn run_module(source: String) -> Result(vm.Completion, String) {
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, globals) = builtins.globals(b, h)
  let store = module.new_store(h, b)
  let specifier = "<test>"

  case module.load_module(store, specifier, source) {
    Error(err) -> Error("module error: " <> string.inspect(err))
    Ok(#(store, _record)) ->
      case
        module.resolve_dependencies(store, specifier, fn(_dep, _parent) {
          Error("no module loader in tests")
        })
      {
        Error(err) -> Error("module error: " <> string.inspect(err))
        Ok(store) -> {
          let #(_store, result) =
            module.evaluate_module(store, specifier, h, b, globals)
          case result {
            Ok(#(val, new_heap)) -> Ok(vm.NormalCompletion(val, new_heap))
            Error(module.EvaluationError(val)) -> Ok(vm.ThrowCompletion(val, h))
            Error(err) -> Error("module error: " <> string.inspect(err))
          }
        }
      }
  }
}

fn assert_module_normal(source: String, expected: value.JsValue) -> Nil {
  case run_module(source) {
    Ok(vm.NormalCompletion(value, _)) -> {
      let assert True = value == expected
      Nil
    }
    Ok(vm.ThrowCompletion(thrown, heap)) ->
      panic as {
        "Expected normal completion but got throw: "
        <> string.inspect(thrown)
        <> " heap="
        <> string.inspect(heap)
      }
    Ok(vm.YieldCompletion(_, _)) -> panic as "unexpected YieldCompletion"
    Error(err) -> panic as { "run_module failed: " <> err }
  }
}

pub fn module_basic_strict_mode_test() -> Nil {
  // Modules are always strict — `this` at the top level is undefined
  assert_module_normal("typeof this", JsString("undefined"))
}

pub fn module_variable_declaration_test() -> Nil {
  assert_module_normal("let x = 42; x", JsNumber(Finite(42.0)))
}

pub fn module_function_declaration_test() -> Nil {
  assert_module_normal(
    "function add(a, b) { return a + b; } add(1, 2)",
    JsNumber(Finite(3.0)),
  )
}

pub fn module_export_named_declaration_test() -> Nil {
  // export let x = 42 should still work (the declaration runs)
  assert_module_normal("export let x = 42; x", JsNumber(Finite(42.0)))
}

pub fn module_export_function_test() -> Nil {
  assert_module_normal(
    "export function greet() { return 'hello'; } greet()",
    JsString("hello"),
  )
}

pub fn module_import_arc_peek_test() -> Nil {
  // import { peek } from 'arc' should resolve peek to Arc.peek
  assert_module_normal(
    "import { peek } from 'arc';
     var p = Promise.resolve(42);
     peek(p).type",
    JsString("resolved"),
  )
}

pub fn module_import_arc_namespace_test() -> Nil {
  // import * as arc from 'arc' should give the Arc object
  assert_module_normal(
    "import * as arc from 'arc';
     typeof arc.peek",
    JsString("function"),
  )
}

pub fn module_repl_harness_globals_test() -> Nil {
  // Test the REPL→module globals flow:
  // 1. Evaluate a REPL script that defines a function
  // 2. Run a module that accesses that function via GetGlobal
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, globals) = builtins.globals(b, h)

  // Step 1: Compile and run harness script in REPL mode
  let harness_source =
    "function greetFromHarness() { return 'hello from harness'; }"
  let assert Ok(harness_program) = parser.parse(harness_source, parser.Script)
  let assert Ok(harness_template) = compiler.compile_repl(harness_program)

  let env =
    vm.ReplEnv(
      globals:,
      const_globals: set.new(),
      symbol_descriptions: dict.new(),
    )
  let assert Ok(#(harness_completion, env)) =
    vm.run_and_drain_repl(harness_template, h, b, env)
  let assert vm.NormalCompletion(_, h) = harness_completion

  // Verify greetFromHarness is in globals
  let assert Ok(_) = dict.get(env.globals, "greetFromHarness")

  // Step 2: Compile and run a module that uses the harness function
  let module_source = "greetFromHarness()"
  let store = module.new_store(h, b)
  let specifier = "<test-module>"
  let assert Ok(#(store, _record)) =
    module.load_module(store, specifier, module_source)
  let assert Ok(store) =
    module.resolve_dependencies(store, specifier, fn(_dep, _parent) {
      Error("no module loader")
    })

  // Evaluate the module, passing in REPL globals
  let #(_store, result) =
    module.evaluate_module(store, specifier, h, b, env.globals)
  case result {
    Ok(#(val, _heap)) -> {
      let assert True = val == JsString("hello from harness")
      Nil
    }
    Error(err) -> panic as { "module failed: " <> string.inspect(err) }
  }
}
