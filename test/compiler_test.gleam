import gleam/dict
import gleam/int
import gleam/string
import lumen/compiler
import lumen/parser
import lumen/vm/builtins
import lumen/vm/heap
import lumen/vm/value.{
  Finite, JsBool, JsNull, JsNumber, JsObject, JsString, JsUndefined,
}
import lumen/vm/vm

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
          let globals =
            dict.from_list([
              #("NaN", value.JsNumber(value.NaN)),
              #("Infinity", value.JsNumber(value.Infinity)),
              #("undefined", JsUndefined),
              #("Object", JsObject(b.object.constructor)),
              #("Function", JsObject(b.function.constructor)),
              #("Array", JsObject(b.array.constructor)),
              #("Error", JsObject(b.error.constructor)),
              #("TypeError", JsObject(b.type_error.constructor)),
              #("ReferenceError", JsObject(b.reference_error.constructor)),
              #("RangeError", JsObject(b.range_error.constructor)),
              #("SyntaxError", JsObject(b.syntax_error.constructor)),
              #("Math", JsObject(b.math)),
            ])
          case vm.run_with_globals(template, h, b, globals) {
            Ok(completion) -> Ok(completion)
            Error(vm_err) -> Error("vm error: " <> inspect_vm_error(vm_err))
          }
        }
      }
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

fn assert_normal(source: String, expected: value.JsValue) {
  case run_js(source) {
    Ok(vm.NormalCompletion(val, _)) -> {
      assert val == expected
    }
    Ok(vm.ThrowCompletion(_, _)) ->
      panic as {
        "expected NormalCompletion, got ThrowCompletion for: " <> source
      }
    Error(err) -> panic as { "error for: " <> source <> " — " <> err }
  }
}

fn assert_normal_number(source: String, expected: Float) {
  assert_normal(source, JsNumber(Finite(expected)))
}

fn assert_thrown(source: String) {
  case run_js(source) {
    Ok(vm.ThrowCompletion(_, _)) -> Nil
    Ok(vm.NormalCompletion(val, _)) ->
      panic as {
        "expected ThrowCompletion, got NormalCompletion("
        <> string.inspect(val)
        <> ") for: "
        <> source
      }
    Error(err) -> panic as { "error for: " <> source <> " — " <> err }
  }
}

// ============================================================================
// Literal tests
// ============================================================================

pub fn number_literal_test() {
  assert_normal_number("42", 42.0)
}

pub fn string_literal_test() {
  assert_normal("\"hello\"", JsString("hello"))
}

pub fn boolean_true_test() {
  assert_normal("true", JsBool(True))
}

pub fn boolean_false_test() {
  assert_normal("false", JsBool(False))
}

pub fn null_literal_test() {
  assert_normal("null", JsNull)
}

pub fn undefined_literal_test() {
  assert_normal("undefined", JsUndefined)
}

// ============================================================================
// Binary operator tests
// ============================================================================

pub fn addition_test() {
  assert_normal_number("1 + 2", 3.0)
}

pub fn subtraction_test() {
  assert_normal_number("10 - 3", 7.0)
}

pub fn multiplication_test() {
  assert_normal_number("4 * 5", 20.0)
}

pub fn division_test() {
  assert_normal_number("15 / 3", 5.0)
}

pub fn modulo_test() {
  assert_normal_number("10 % 3", 1.0)
}

pub fn comparison_less_than_test() {
  assert_normal("1 < 2", JsBool(True))
}

pub fn comparison_greater_than_test() {
  assert_normal("2 > 1", JsBool(True))
}

pub fn strict_equal_test() {
  assert_normal("1 === 1", JsBool(True))
}

pub fn strict_not_equal_test() {
  assert_normal("1 !== 2", JsBool(True))
}

pub fn string_concat_test() {
  assert_normal("\"hello\" + \" world\"", JsString("hello world"))
}

pub fn nested_arithmetic_test() {
  assert_normal_number("(1 + 2) * (3 + 4)", 21.0)
}

// ============================================================================
// Unary operator tests
// ============================================================================

pub fn negate_test() {
  assert_normal_number("-5", -5.0)
}

pub fn logical_not_test() {
  assert_normal("!true", JsBool(False))
}

pub fn unary_plus_test() {
  assert_normal_number("+42", 42.0)
}

pub fn void_test() {
  assert_normal("void 0", JsUndefined)
}

// ============================================================================
// Variable tests
// ============================================================================

pub fn var_declaration_test() {
  assert_normal_number("var x = 10; x", 10.0)
}

pub fn var_reassignment_test() {
  assert_normal_number("var x = 1; x = 5; x", 5.0)
}

pub fn multiple_vars_test() {
  assert_normal_number("var x = 1; var y = 2; x + y", 3.0)
}

pub fn let_declaration_test() {
  assert_normal_number("let x = 42; x", 42.0)
}

pub fn const_declaration_test() {
  assert_normal_number("const x = 99; x", 99.0)
}

pub fn compound_assignment_test() {
  assert_normal_number("var x = 10; x += 5; x", 15.0)
}

pub fn var_no_init_test() {
  assert_normal("var x; x", JsUndefined)
}

// ============================================================================
// Control flow tests
// ============================================================================

pub fn if_true_test() {
  assert_normal_number("var x = 0; if (true) { x = 1; } x", 1.0)
}

pub fn if_false_test() {
  assert_normal_number("var x = 0; if (false) { x = 1; } x", 0.0)
}

pub fn if_else_test() {
  assert_normal_number("var x; if (true) { x = 1; } else { x = 2; } x", 1.0)
}

pub fn if_else_false_test() {
  assert_normal_number("var x; if (false) { x = 1; } else { x = 2; } x", 2.0)
}

pub fn while_loop_test() {
  assert_normal_number("var i = 0; while (i < 5) { i = i + 1; } i", 5.0)
}

pub fn for_loop_test() {
  assert_normal_number(
    "var sum = 0; for (var i = 0; i < 5; i = i + 1) { sum = sum + i; } sum",
    10.0,
  )
}

pub fn do_while_test() {
  assert_normal_number("var i = 0; do { i = i + 1; } while (i < 3); i", 3.0)
}

pub fn break_test() {
  assert_normal_number(
    "var i = 0; while (true) { if (i === 5) { break; } i = i + 1; } i",
    5.0,
  )
}

pub fn continue_test() {
  assert_normal_number(
    "var sum = 0; for (var i = 0; i < 5; i = i + 1) { if (i === 2) { continue; } sum = sum + i; } sum",
    8.0,
  )
}

// ============================================================================
// Block scoping tests
// ============================================================================

pub fn block_scope_let_test() {
  // let inside block should not be visible outside
  assert_normal_number("var x = 1; { let y = 2; x = x + y; } x", 3.0)
}

// ============================================================================
// Logical operator tests
// ============================================================================

pub fn logical_and_short_circuit_test() {
  assert_normal("false && true", JsBool(False))
}

pub fn logical_and_evaluates_both_test() {
  assert_normal_number("1 && 2", 2.0)
}

pub fn logical_or_short_circuit_test() {
  assert_normal("true || false", JsBool(True))
}

pub fn logical_or_evaluates_second_test() {
  assert_normal_number("0 || 42", 42.0)
}

pub fn nullish_coalescing_null_test() {
  assert_normal_number("null ?? 42", 42.0)
}

pub fn nullish_coalescing_value_test() {
  assert_normal_number("5 ?? 42", 5.0)
}

// ============================================================================
// Conditional (ternary) tests
// ============================================================================

pub fn ternary_true_test() {
  assert_normal_number("true ? 1 : 2", 1.0)
}

pub fn ternary_false_test() {
  assert_normal_number("false ? 1 : 2", 2.0)
}

// ============================================================================
// Object tests
// ============================================================================

pub fn empty_object_test() {
  // Just test that it doesn't crash — the result is an object ref
  case run_js("({})") {
    Ok(vm.NormalCompletion(value.JsObject(_), _)) -> Nil
    _other -> panic as { "expected object, got something else" }
  }
}

pub fn object_property_access_test() {
  assert_normal_number("var obj = {x: 42}; obj.x", 42.0)
}

pub fn object_multiple_properties_test() {
  assert_normal_number("var obj = {x: 1, y: 2}; obj.x + obj.y", 3.0)
}

pub fn object_property_undefined_test() {
  assert_normal("var obj = {}; obj.x", JsUndefined)
}

// ============================================================================
// Expression statement result tests
// ============================================================================

pub fn expression_result_test() {
  // The last expression statement's value should be the program result
  assert_normal_number("1; 2; 3", 3.0)
}

pub fn empty_program_test() {
  assert_normal("", JsUndefined)
}

// ============================================================================
// Update expression tests
// ============================================================================

pub fn prefix_increment_test() {
  assert_normal_number("var x = 5; ++x", 6.0)
}

pub fn postfix_increment_test() {
  assert_normal_number("var x = 5; x++", 5.0)
}

pub fn postfix_increment_side_effect_test() {
  assert_normal_number("var x = 5; x++; x", 6.0)
}

// ============================================================================
// Try/catch tests
// ============================================================================

pub fn try_catch_basic_test() {
  assert_normal_number("try { throw 42; } catch(e) { e }", 42.0)
}

pub fn try_no_throw_test() {
  assert_normal_number("var x = 0; try { x = 1; } catch(e) { x = 2; } x", 1.0)
}

// ============================================================================
// Throw as ThrowCompletion test
// ============================================================================

pub fn uncaught_throw_test() {
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

pub fn sequence_expression_test() {
  assert_normal_number("(1, 2, 3)", 3.0)
}

// ============================================================================
// Function tests
// ============================================================================

pub fn function_declaration_basic_test() {
  assert_normal_number("function add(a, b) { return a + b; } add(3, 4)", 7.0)
}

pub fn function_no_args_test() {
  assert_normal_number("function f() { return 42; } f()", 42.0)
}

pub fn function_implicit_return_test() {
  assert_normal("function f() {} f()", JsUndefined)
}

pub fn function_expression_test() {
  assert_normal_number("var f = function(x) { return x * 2; }; f(5)", 10.0)
}

pub fn function_multiple_calls_test() {
  assert_normal_number(
    "function inc(x) { return x + 1; } inc(inc(inc(0)))",
    3.0,
  )
}

pub fn function_extra_args_ignored_test() {
  assert_normal_number("function f(a) { return a; } f(1, 2, 3)", 1.0)
}

pub fn function_missing_args_undefined_test() {
  assert_normal("function f(a, b) { return b; } f(1)", JsUndefined)
}

pub fn function_with_locals_test() {
  assert_normal_number(
    "function f(x) { var y = x + 1; return y * 2; } f(4)",
    10.0,
  )
}

pub fn function_hoisting_test() {
  assert_normal_number("var x = f(); function f() { return 99; } x", 99.0)
}

pub fn function_recursion_test() {
  assert_normal_number(
    "function fact(n) { if (n <= 1) return 1; return n * fact(n - 1); } fact(5)",
    120.0,
  )
}

// ============================================================================
// Arrow function tests
// ============================================================================

pub fn arrow_expression_body_test() {
  assert_normal_number("var f = (x) => x + 1; f(5)", 6.0)
}

pub fn arrow_two_params_test() {
  assert_normal_number("var f = (a, b) => a + b; f(3, 4)", 7.0)
}

pub fn arrow_block_body_test() {
  assert_normal_number("var f = (x) => { return x * 2; }; f(5)", 10.0)
}

pub fn arrow_no_params_test() {
  assert_normal_number("var f = () => 42; f()", 42.0)
}

pub fn arrow_nested_expression_test() {
  assert_normal_number("var f = (x) => x * x + 1; f(3)", 10.0)
}

// ============================================================================
// Closure tests
// ============================================================================

pub fn closure_basic_test() {
  // Use function expression (not declaration) so the closure is created
  // AFTER x=10 is assigned (function declarations are hoisted above vars)
  assert_normal_number(
    "function make() { var x = 10; var inner = function() { return x; }; return inner; } make()()",
    10.0,
  )
}

pub fn closure_hoisted_declaration_test() {
  assert_normal_number(
    "function make() { var x = 10; function inner() { return x; } return inner; } make()()",
    10.0,
  )
}

pub fn closure_param_capture_test() {
  assert_normal_number(
    "function adder(n) { return function(x) { return x + n; }; } adder(5)(3)",
    8.0,
  )
}

pub fn closure_two_params_test() {
  assert_normal_number(
    "function f(a, b) { return function() { return a + b; }; } f(3, 4)()",
    7.0,
  )
}

pub fn closure_arrow_test() {
  assert_normal_number("function f(x) { return () => x; } f(42)()", 42.0)
}

pub fn closure_let_test() {
  assert_normal_number(
    "function f() { let x = 99; return function() { return x; }; } f()()",
    99.0,
  )
}

pub fn closure_multiple_captures_test() {
  assert_normal_number(
    "function f(a, b, c) { return function() { return a + b + c; }; } f(1, 2, 3)()",
    6.0,
  )
}

pub fn closure_factory_test() {
  assert_normal_number(
    "function multiplier(factor) { return function(x) { return x * factor; }; } var double = multiplier(2); double(7)",
    14.0,
  )
}

pub fn closure_arrow_expression_capture_test() {
  assert_normal_number("function f(x) { return (y) => x + y; } f(10)(20)", 30.0)
}

pub fn closure_var_capture_test() {
  // Closures capture by reference via BoxSlot — mutations are visible.
  assert_normal_number(
    "function f() { var x = 5; var g = function() { return x; }; x = 10; return g; } f()()",
    10.0,
  )
}

pub fn closure_independent_copies_test() {
  // Two closures from same factory get independent copies
  assert_normal_number(
    "function make(n) { return function() { return n; }; } var a = make(1); var b = make(2); a() + b()",
    3.0,
  )
}

pub fn closure_mutation_after_creation_test() {
  // Mutation after closure creation is visible through the closure
  assert_normal_number(
    "function f() { var x = 5; var g = function() { return x; }; x = 10; return g(); } f()",
    10.0,
  )
}

pub fn closure_mutation_through_closure_test() {
  // Mutation through the closure is visible to the parent
  assert_normal_number(
    "function f() { var x = 0; var inc = function() { x = x + 1; }; inc(); inc(); inc(); return x; } f()",
    3.0,
  )
}

pub fn closure_shared_between_siblings_test() {
  // Two closures share the same variable
  assert_normal_number(
    "function f() { var x = 0; var inc = function() { x = x + 1; return x; }; var get = function() { return x; }; inc(); inc(); return get(); } f()",
    2.0,
  )
}

pub fn closure_counter_pattern_test() {
  // Classic counter closure pattern
  assert_normal_number(
    "function counter() { var n = 0; return function() { n = n + 1; return n; }; } var c = counter(); c(); c(); c()",
    3.0,
  )
}

pub fn closure_hoisted_fn_mutation_test() {
  // Hoisted function declaration + mutation in parent
  assert_normal_number(
    "function f() { function get() { return x; } var x = 42; return get(); } f()",
    42.0,
  )
}

pub fn closure_param_mutation_test() {
  // Closure captures a param, parent mutates it
  assert_normal_number(
    "function f(x) { var g = function() { return x; }; x = 99; return g(); } f(1)",
    99.0,
  )
}

// ============================================================================
// Array tests
// ============================================================================

pub fn array_literal_test() {
  // Array literal produces an object
  case run_js("[1, 2, 3]") {
    Ok(vm.NormalCompletion(value.JsObject(_), _)) -> Nil
    _other -> panic as "expected array object"
  }
}

pub fn array_index_access_test() {
  assert_normal_number("[10, 20, 30][1]", 20.0)
}

pub fn array_index_zero_test() {
  assert_normal_number("[42, 99][0]", 42.0)
}

pub fn array_length_test() {
  assert_normal_number("[1, 2, 3].length", 3.0)
}

pub fn array_empty_length_test() {
  assert_normal_number("[].length", 0.0)
}

pub fn array_out_of_bounds_test() {
  assert_normal("[1, 2][5]", JsUndefined)
}

pub fn array_index_assignment_test() {
  assert_normal_number("var a = [1, 2, 3]; a[0] = 42; a[0]", 42.0)
}

pub fn array_index_assignment_grows_length_test() {
  assert_normal_number("var a = [1]; a[5] = 99; a.length", 6.0)
}

pub fn array_sparse_hole_test() {
  // Holes in array literals become undefined
  assert_normal_number("[1, , 3].length", 3.0)
}

pub fn array_sparse_hole_value_test() {
  assert_normal("[1, , 3][1]", JsUndefined)
}

pub fn array_string_elements_test() {
  assert_normal("['a', 'b', 'c'][2]", JsString("c"))
}

pub fn array_nested_access_test() {
  assert_normal_number("var a = [10, 20]; var b = [a[0] + a[1]]; b[0]", 30.0)
}

pub fn array_compound_assignment_test() {
  assert_normal_number("var a = [10]; a[0] += 5; a[0]", 15.0)
}

pub fn array_in_variable_test() {
  assert_normal_number("var a = [5, 10, 15]; a[2]", 15.0)
}

pub fn array_length_after_assignment_test() {
  assert_normal_number("var a = []; a[0] = 1; a[1] = 2; a.length", 2.0)
}

// ============================================================================
// this / new / methods
// ============================================================================

pub fn new_basic_constructor_test() {
  assert_normal_number(
    "function Foo(x) { this.x = x; }
     var o = new Foo(42);
     o.x",
    42.0,
  )
}

pub fn function_prototype_exists_test() {
  assert_normal(
    "function Foo() {}
     typeof Foo.prototype",
    JsString("object"),
  )
}

pub fn prototype_method_with_this_test() {
  assert_normal_number(
    "function Foo(x) { this.x = x; }
     Foo.prototype.getX = function() { return this.x; };
     var o = new Foo(5);
     o.getX()",
    5.0,
  )
}

pub fn constructor_implicit_return_test() {
  // Constructor returns undefined → new object used
  assert_normal_number(
    "function Foo(x) { this.x = x; }
     var o = new Foo(7);
     o.x",
    7.0,
  )
}

pub fn constructor_explicit_object_return_test() {
  // Constructor returns an object → that object is used
  assert_normal_number(
    "function Foo() { this.a = 1; return { b: 99 }; }
     var o = new Foo();
     o.b",
    99.0,
  )
}

pub fn static_property_on_function_test() {
  assert_normal_number(
    "function Foo() {}
     Foo.bar = 42;
     Foo.bar",
    42.0,
  )
}

pub fn sta_js_pattern_test() {
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

pub fn prototype_chain_property_lookup_test() {
  assert_normal_number(
    "function Foo() {}
     Foo.prototype.x = 10;
     var o = new Foo();
     o.x",
    10.0,
  )
}

pub fn this_undefined_in_plain_call_test() {
  assert_normal(
    "function f() { return this; }
     f()",
    JsUndefined,
  )
}

pub fn method_call_binds_this_test() {
  assert_normal_number(
    "var obj = { x: 99, getX: function() { return this.x; } };
     obj.getX()",
    99.0,
  )
}

// ============================================================================
// Template literals
// ============================================================================

pub fn template_literal_no_expressions_test() {
  assert_normal("`hello world`", JsString("hello world"))
}

pub fn template_literal_with_expression_test() {
  assert_normal("var x = 42; `value is ${x}`", JsString("value is 42"))
}

pub fn template_literal_multiple_expressions_test() {
  assert_normal(
    "var a = 1; var b = 2; `${a} + ${b} = ${a + b}`",
    JsString("1 + 2 = 3"),
  )
}

pub fn template_literal_empty_test() {
  assert_normal("``", JsString(""))
}

// ============================================================================
// Switch statements
// ============================================================================

pub fn switch_basic_match_test() {
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

pub fn switch_default_test() {
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

pub fn switch_fallthrough_test() {
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

pub fn switch_no_match_no_default_test() {
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

pub fn assert_js_harness_basic_test() {
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

pub fn full_assert_js_compiles_test() {
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

pub fn switch_string_cases_test() {
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

pub fn function_declaration_name_test() {
  assert_normal("function foo() {} foo.name", JsString("foo"))
}

pub fn function_expression_named_name_test() {
  assert_normal("var f = function bar() {}; f.name", JsString("bar"))
}

pub fn function_expression_anonymous_name_test() {
  assert_normal("var f = function() {}; f.name", JsString(""))
}

pub fn arrow_function_name_test() {
  assert_normal("var f = () => 1; f.name", JsString(""))
}

// ============================================================================
// .prototype.constructor backlink
// ============================================================================

pub fn prototype_constructor_backlink_test() {
  assert_normal(
    "function Foo() {}
     Foo.prototype.constructor === Foo",
    JsBool(True),
  )
}

pub fn prototype_is_object_test() {
  assert_normal(
    "function Foo() {}
     typeof Foo.prototype",
    JsString("object"),
  )
}

pub fn constructor_via_new_test() {
  assert_normal(
    "function Foo() {}
     var o = new Foo();
     o.constructor === Foo",
    JsBool(True),
  )
}

pub fn arrow_has_no_prototype_test() {
  assert_normal(
    "var f = () => 1;
     typeof f.prototype",
    JsString("undefined"),
  )
}

pub fn constructor_return_object_overrides_test() {
  // If constructor explicitly returns an object, that object is used
  assert_normal(
    "function Foo() { return {x: 99}; }
     var o = new Foo();
     o.x",
    JsNumber(Finite(99.0)),
  )
}

pub fn constructor_return_primitive_ignored_test() {
  // If constructor returns a primitive, the constructed object is used
  assert_normal(
    "function Foo() { this.x = 42; return 5; }
     var o = new Foo();
     o.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn prototype_chain_inheritance_test() {
  // Properties set on prototype are accessible via the chain
  assert_normal(
    "function Foo() {}
     Foo.prototype.hello = 'world';
     var o = new Foo();
     o.hello",
    JsString("world"),
  )
}

pub fn constructor_non_object_prototype_fallback_test() {
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

pub fn object_destructuring_basic_test() {
  assert_normal_number("let {a, b} = {a: 1, b: 2}; a + b", 3.0)
}

pub fn array_destructuring_basic_test() {
  assert_normal_number("let [x, y] = [10, 20]; x + y", 30.0)
}

pub fn object_destructuring_default_used_test() {
  assert_normal_number("let {a = 5} = {}; a", 5.0)
}

pub fn object_destructuring_default_not_used_test() {
  assert_normal_number("let {a = 5} = {a: 42}; a", 42.0)
}

pub fn object_destructuring_rename_test() {
  assert_normal_number("let {a: x} = {a: 99}; x", 99.0)
}

pub fn object_destructuring_nested_test() {
  assert_normal_number("let {a: {b}} = {a: {b: 7}}; b", 7.0)
}

pub fn array_destructuring_nested_test() {
  assert_normal_number("let [a, [b, c]] = [1, [2, 3]]; b", 2.0)
}

pub fn function_param_destructuring_test() {
  assert_normal_number(
    "function f({x, y}) { return x + y; } f({x: 3, y: 4})",
    7.0,
  )
}

pub fn var_destructuring_hoisting_test() {
  assert_normal_number("var {a} = {a: 1}; a", 1.0)
}

pub fn array_destructuring_hole_test() {
  assert_normal_number("let [, b] = [10, 20]; b", 20.0)
}

pub fn array_destructuring_default_test() {
  assert_normal_number("let [a = 99] = []; a", 99.0)
}

pub fn const_destructuring_test() {
  assert_normal_number("const {x, y} = {x: 3, y: 7}; x + y", 10.0)
}

pub fn nested_default_test() {
  assert_normal_number("let {a: {b} = {b: 5}} = {}; b", 5.0)
}

pub fn arrow_param_destructuring_test() {
  assert_normal_number("var f = ({a, b}) => a * b; f({a: 3, b: 4})", 12.0)
}

// ============================================================================
// For-in loop tests
// ============================================================================

pub fn for_in_object_basic_test() {
  // Collect keys from a plain object
  assert_normal_number(
    "var sum = 0; var obj = {a: 1, b: 2, c: 3}; for (var k in obj) { sum += obj[k]; } sum",
    6.0,
  )
}

pub fn for_in_let_binding_test() {
  assert_normal(
    "var result = ''; var obj = {x: 1, y: 2}; for (let k in obj) { result += k; } result",
    JsString("xy"),
  )
}

pub fn for_in_null_test() {
  // for-in on null should not iterate
  assert_normal_number("var x = 0; for (var k in null) { x = 1; } x", 0.0)
}

pub fn for_in_undefined_test() {
  // for-in on undefined should not iterate
  assert_normal_number("var x = 0; for (var k in undefined) { x = 1; } x", 0.0)
}

pub fn for_in_array_test() {
  // for-in on array iterates indices as strings
  assert_normal(
    "var result = ''; for (var k in [10, 20, 30]) { result += k; } result",
    JsString("012"),
  )
}

pub fn for_in_break_test() {
  assert_normal_number(
    "var count = 0; for (var k in {a: 1, b: 2, c: 3}) { count++; if (count === 2) break; } count",
    2.0,
  )
}

pub fn for_in_continue_test() {
  assert_normal_number(
    "var sum = 0; var obj = {a: 1, b: 2, c: 3}; for (var k in obj) { if (k === 'b') continue; sum += obj[k]; } sum",
    4.0,
  )
}

pub fn for_in_existing_var_test() {
  // for-in with existing variable (no declaration)
  assert_normal(
    "var k; for (k in {hello: 1, world: 2}) {} k",
    JsString("world"),
  )
}

// ============================================================================
// For-of loop tests
// ============================================================================

pub fn for_of_array_basic_test() {
  assert_normal_number(
    "var sum = 0; for (var x of [1, 2, 3]) { sum += x; } sum",
    6.0,
  )
}

pub fn for_of_let_binding_test() {
  assert_normal_number(
    "var sum = 0; for (let x of [10, 20, 30]) { sum += x; } sum",
    60.0,
  )
}

pub fn for_of_const_binding_test() {
  assert_normal_number(
    "var sum = 0; for (const x of [5, 10, 15]) { sum += x; } sum",
    30.0,
  )
}

pub fn for_of_break_test() {
  assert_normal_number(
    "var sum = 0; for (var x of [1, 2, 3, 4, 5]) { if (x > 3) break; sum += x; } sum",
    6.0,
  )
}

pub fn for_of_continue_test() {
  assert_normal_number(
    "var sum = 0; for (var x of [1, 2, 3, 4]) { if (x === 2) continue; sum += x; } sum",
    8.0,
  )
}

pub fn for_of_empty_array_test() {
  assert_normal_number("var sum = 0; for (var x of []) { sum += x; } sum", 0.0)
}

pub fn for_of_destructuring_test() {
  // for-of with array destructuring
  assert_normal_number(
    "var sum = 0; var arr = [[1, 2], [3, 4]]; for (var [a, b] of arr) { sum += a + b; } sum",
    10.0,
  )
}

pub fn for_of_string_values_test() {
  assert_normal(
    "var result = ''; for (var x of ['a', 'b', 'c']) { result += x; } result",
    JsString("abc"),
  )
}

// ============================================================================
// delete operator
// ============================================================================

pub fn delete_property_test() {
  assert_normal("var obj = {x: 1, y: 2}; delete obj.x; obj.x", JsUndefined)
}

pub fn delete_returns_true_test() {
  assert_normal("var obj = {x: 1}; delete obj.x", JsBool(True))
}

pub fn delete_nonexistent_test() {
  assert_normal("var obj = {}; delete obj.x", JsBool(True))
}

pub fn delete_computed_test() {
  assert_normal(
    "var obj = {a: 10}; var k = 'a'; delete obj[k]; obj.a",
    JsUndefined,
  )
}

pub fn delete_variable_test() {
  // delete of a plain variable returns true in sloppy mode
  assert_normal("var x = 1; delete x", JsBool(True))
}

pub fn delete_non_object_test() {
  assert_normal("delete 42", JsBool(True))
}

// ============================================================================
// in operator
// ============================================================================

pub fn in_own_property_test() {
  assert_normal("'x' in {x: 1}", JsBool(True))
}

pub fn in_missing_property_test() {
  assert_normal("'y' in {x: 1}", JsBool(False))
}

pub fn in_prototype_chain_test() {
  // "constructor" exists on the prototype (inherited)
  assert_normal("'constructor' in {}", JsBool(True))
}

pub fn in_array_index_test() {
  assert_normal("0 in [10, 20]", JsBool(True))
}

pub fn in_array_length_test() {
  assert_normal("'length' in []", JsBool(True))
}

pub fn in_throws_for_non_object_test() {
  assert_thrown("'x' in 42")
}

// ============================================================================
// Property descriptor behavior
// ============================================================================

pub fn for_in_skips_non_enumerable_test() {
  // for-in on new Foo() should NOT include "constructor" (non-enumerable)
  assert_normal(
    "function Foo() {} var result = ''; for (var k in new Foo()) { result += k; } result",
    JsString(""),
  )
}

pub fn for_in_includes_own_enumerable_test() {
  // Own properties set via assignment ARE enumerable
  assert_normal(
    "function Foo() { this.x = 1; this.y = 2; } var result = ''; for (var k in new Foo()) { result += k; } result",
    JsString("xy"),
  )
}

pub fn for_in_prototype_enumerable_test() {
  // User-set prototype properties are enumerable and show in for-in
  assert_normal(
    "function Foo() {} Foo.prototype.bar = 42; var result = ''; for (var k in new Foo()) { result += k; } result",
    JsString("bar"),
  )
}

pub fn function_name_not_enumerable_test() {
  // function.name is not enumerable
  assert_normal(
    "function foo() {} var result = ''; for (var k in foo) { result += k; } result",
    JsString(""),
  )
}

pub fn function_prototype_not_enumerable_test() {
  // function.prototype is not enumerable
  assert_normal(
    "function foo() {} var keys = []; for (var k in foo) { keys.push(k); } keys.length",
    JsNumber(Finite(0.0)),
  )
}

pub fn delete_then_in_test() {
  assert_normal("var obj = {x: 1}; delete obj.x; 'x' in obj", JsBool(False))
}

// ============================================================================
// Class tests
// ============================================================================

pub fn class_basic_constructor_test() {
  assert_normal(
    "class Foo { constructor(x) { this.x = x; } } var f = new Foo(42); f.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn class_instance_method_test() {
  assert_normal(
    "class Foo { greet() { return 'hi'; } } var f = new Foo(); f.greet()",
    JsString("hi"),
  )
}

pub fn class_method_accesses_this_test() {
  assert_normal(
    "class Foo { constructor(x) { this.x = x; } getX() { return this.x; } } var f = new Foo(10); f.getX()",
    JsNumber(Finite(10.0)),
  )
}

pub fn class_static_method_test() {
  assert_normal(
    "class Foo { static create() { return 99; } } Foo.create()",
    JsNumber(Finite(99.0)),
  )
}

pub fn class_typeof_test() {
  assert_normal("class Foo {} typeof Foo", JsString("function"))
}

pub fn class_instanceof_test() {
  assert_normal(
    "class Foo {} var f = new Foo(); f instanceof Foo",
    JsBool(True),
  )
}

pub fn class_expression_test() {
  assert_normal(
    "var Foo = class { constructor(x) { this.x = x; } }; var f = new Foo(5); f.x",
    JsNumber(Finite(5.0)),
  )
}

pub fn class_field_initializer_test() {
  assert_normal(
    "class Foo { x = 42; } var f = new Foo(); f.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn class_field_with_constructor_test() {
  assert_normal(
    "class Foo { x = 1; constructor(y) { this.y = y; } } var f = new Foo(2); f.x + f.y",
    JsNumber(Finite(3.0)),
  )
}

pub fn class_multiple_methods_test() {
  assert_normal(
    "class Calc { add(a, b) { return a + b; } mul(a, b) { return a * b; } } var c = new Calc(); c.add(2, 3) + c.mul(4, 5)",
    JsNumber(Finite(25.0)),
  )
}

pub fn class_method_not_enumerable_test() {
  assert_normal(
    "class Foo { bar() {} } var f = new Foo(); var keys = []; for (var k in f) { keys.push(k); } keys.length",
    JsNumber(Finite(0.0)),
  )
}

pub fn class_no_constructor_test() {
  assert_normal(
    "class Empty {} var e = new Empty(); typeof e",
    JsString("object"),
  )
}

// ============================================================================
// Function.prototype.call / apply / bind
// ============================================================================

pub fn function_call_basic_test() {
  assert_normal(
    "function greet(x) { return this.prefix + x; }
     var obj = { prefix: 'Hello ' };
     greet.call(obj, 'world')",
    JsString("Hello world"),
  )
}

pub fn function_call_no_args_test() {
  assert_normal(
    "function getThis() { return this; }
     getThis.call(42)",
    JsNumber(Finite(42.0)),
  )
}

pub fn function_call_undefined_this_test() {
  assert_normal(
    "function f() { return typeof this; }
     f.call(undefined)",
    JsString("undefined"),
  )
}

pub fn function_apply_basic_test() {
  assert_normal(
    "function add(a, b) { return a + b; }
     add.apply(null, [3, 4])",
    JsNumber(Finite(7.0)),
  )
}

pub fn function_apply_with_this_test() {
  assert_normal(
    "function greet(x) { return this.prefix + x; }
     var obj = { prefix: 'Hi ' };
     greet.apply(obj, ['there'])",
    JsString("Hi there"),
  )
}

pub fn function_apply_no_args_test() {
  assert_normal(
    "function f() { return 42; }
     f.apply(null)",
    JsNumber(Finite(42.0)),
  )
}

pub fn function_bind_basic_test() {
  assert_normal(
    "function greet(x) { return this.name + ': ' + x; }
     var obj = { name: 'Alice' };
     var bound = greet.bind(obj);
     bound('hello')",
    JsString("Alice: hello"),
  )
}

pub fn function_bind_with_args_test() {
  assert_normal(
    "function add(a, b) { return a + b; }
     var add5 = add.bind(null, 5);
     add5(3)",
    JsNumber(Finite(8.0)),
  )
}

pub fn function_bind_preserves_this_test() {
  assert_normal(
    "function getX() { return this.x; }
     var obj = { x: 99 };
     var bound = getX.bind(obj);
     bound()",
    JsNumber(Finite(99.0)),
  )
}

pub fn function_bind_name_test() {
  assert_normal(
    "function foo() {} var b = foo.bind(null); b.name",
    JsString("bound foo"),
  )
}

pub fn function_bind_constructor_test() {
  assert_normal(
    "function Point(x, y) { this.x = x; this.y = y; }
     var BoundPoint = Point.bind(null, 10);
     var p = new BoundPoint(20);
     p.x + p.y",
    JsNumber(Finite(30.0)),
  )
}

pub fn function_call_chained_test() {
  // call on a method that was itself obtained via call
  assert_normal(
    "function add(a, b) { return a + b; }
     var result = add.call(null, 10, 20);
     result",
    JsNumber(Finite(30.0)),
  )
}

pub fn function_apply_empty_array_test() {
  assert_normal(
    "function f() { return 'ok'; }
     f.apply(null, [])",
    JsString("ok"),
  )
}

pub fn function_bind_multiple_args_test() {
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

pub fn object_gopd_basic_test() {
  // Basic data property descriptor
  assert_normal(
    "var obj = {x: 42};
     var desc = Object.getOwnPropertyDescriptor(obj, 'x');
     desc.value",
    JsNumber(Finite(42.0)),
  )
}

pub fn object_gopd_flags_test() {
  // Regular data property has all flags true
  assert_normal(
    "var obj = {x: 1};
     var desc = Object.getOwnPropertyDescriptor(obj, 'x');
     '' + desc.writable + ',' + desc.enumerable + ',' + desc.configurable",
    JsString("true,true,true"),
  )
}

pub fn object_gopd_missing_key_test() {
  // Non-existent property returns undefined
  assert_normal(
    "var obj = {x: 1};
     Object.getOwnPropertyDescriptor(obj, 'y')",
    JsUndefined,
  )
}

pub fn object_gopd_after_define_test() {
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

pub fn object_define_property_basic_test() {
  assert_normal(
    "var obj = {};
     Object.defineProperty(obj, 'x', {value: 42, writable: true, enumerable: true, configurable: true});
     obj.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn object_define_property_non_writable_test() {
  // Non-writable property can't be changed in sloppy mode (silently fails)
  assert_normal(
    "var obj = {};
     Object.defineProperty(obj, 'x', {value: 42, writable: false});
     obj.x = 100;
     obj.x",
    JsNumber(Finite(42.0)),
  )
}

pub fn object_define_property_non_enumerable_test() {
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

pub fn object_define_property_returns_obj_test() {
  // defineProperty returns the target object
  assert_normal(
    "var obj = {};
     var result = Object.defineProperty(obj, 'x', {value: 1});
     result === obj",
    JsBool(True),
  )
}

// ============================================================================
// Object.getOwnPropertyNames
// ============================================================================

pub fn object_gopn_basic_test() {
  assert_normal(
    "var obj = {a: 1, b: 2};
     var names = Object.getOwnPropertyNames(obj);
     names.length",
    JsNumber(Finite(2.0)),
  )
}

pub fn object_gopn_includes_non_enumerable_test() {
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

pub fn object_keys_basic_test() {
  assert_normal(
    "var obj = {a: 1, b: 2};
     Object.keys(obj).length",
    JsNumber(Finite(2.0)),
  )
}

pub fn object_keys_excludes_non_enumerable_test() {
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

pub fn has_own_property_basic_test() {
  assert_normal(
    "var obj = {x: 1};
     obj.hasOwnProperty('x')",
    JsBool(True),
  )
}

pub fn has_own_property_missing_test() {
  assert_normal(
    "var obj = {x: 1};
     obj.hasOwnProperty('y')",
    JsBool(False),
  )
}

pub fn has_own_property_inherited_test() {
  // hasOwnProperty should return false for inherited properties
  assert_normal(
    "function Foo() {}
     Foo.prototype.bar = 1;
     var f = new Foo();
     '' + f.hasOwnProperty('bar') + ',' + ('bar' in f)",
    JsString("false,true"),
  )
}

pub fn has_own_property_via_call_test() {
  // The test262 harness pattern: Function.prototype.call.bind(Object.prototype.hasOwnProperty)
  assert_normal(
    "var hasOwn = Object.prototype.hasOwnProperty;
     var obj = {x: 1};
     hasOwn.call(obj, 'x')",
    JsBool(True),
  )
}

pub fn test262_property_helper_pattern_test() {
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

pub fn array_join_default_separator_test() {
  assert_normal("[1,2,3].join()", JsString("1,2,3"))
}

pub fn array_join_custom_separator_test() {
  assert_normal("[1,2,3].join('-')", JsString("1-2-3"))
}

pub fn array_join_empty_array_test() {
  assert_normal("[].join()", JsString(""))
}

pub fn array_join_undefined_null_elements_test() {
  assert_normal("[1,undefined,null,2].join(',')", JsString("1,,,2"))
}

pub fn array_join_single_element_test() {
  assert_normal("[42].join(',')", JsString("42"))
}

pub fn array_join_empty_separator_test() {
  assert_normal("[1,2,3].join('')", JsString("123"))
}

// ============================================================================
// Array.prototype.push tests
// ============================================================================

pub fn array_push_basic_test() {
  assert_normal(
    "var a = [1,2]; a.push(3); '' + a[0] + ',' + a[1] + ',' + a[2] + ',' + a.length",
    JsString("1,2,3,3"),
  )
}

pub fn array_push_multiple_args_test() {
  assert_normal("var a = []; a.push(1,2,3); a.length", JsNumber(Finite(3.0)))
}

pub fn array_push_returns_length_test() {
  assert_normal("var a = [10]; a.push(20)", JsNumber(Finite(2.0)))
}

// ============================================================================
// Object.prototype.propertyIsEnumerable tests
// ============================================================================

pub fn property_is_enumerable_own_enumerable_test() {
  assert_normal("var o = {a: 1}; o.propertyIsEnumerable('a')", JsBool(True))
}

pub fn property_is_enumerable_non_enumerable_test() {
  assert_normal(
    "var o = {};
     Object.defineProperty(o, 'x', {value: 1, enumerable: false});
     o.propertyIsEnumerable('x')",
    JsBool(False),
  )
}

pub fn property_is_enumerable_inherited_test() {
  // Inherited properties are NOT own, so should return false
  assert_normal("var o = {}; o.propertyIsEnumerable('toString')", JsBool(False))
}

pub fn property_is_enumerable_missing_test() {
  assert_normal("var o = {}; o.propertyIsEnumerable('nope')", JsBool(False))
}

// ============================================================================
// Math.pow tests
// ============================================================================

pub fn math_pow_basic_test() {
  assert_normal("Math.pow(2, 10)", JsNumber(Finite(1024.0)))
}

pub fn math_pow_zero_exponent_test() {
  assert_normal("Math.pow(5, 0)", JsNumber(Finite(1.0)))
}

pub fn math_pow_fractional_test() {
  assert_normal("Math.pow(4, 0.5)", JsNumber(Finite(2.0)))
}

pub fn math_pow_two_32_test() {
  assert_normal("Math.pow(2, 32)", JsNumber(Finite(4_294_967_296.0)))
}
