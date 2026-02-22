import gleam/string
import lumen/compiler
import lumen/parser
import lumen/vm/builtins
import lumen/vm/heap
import lumen/vm/value.{Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined}
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
          case vm.run(template, h, b) {
            Ok(completion) -> Ok(completion)
            Error(vm_err) -> Error("vm error: " <> inspect_vm_error(vm_err))
          }
        }
      }
  }
}

fn inspect_vm_error(err: vm.VmError) -> String {
  case err {
    vm.PcOutOfBounds(pc) -> "PcOutOfBounds(" <> int_to_string(pc) <> ")"
    vm.StackUnderflow(op) -> "StackUnderflow(" <> op <> ")"
    vm.LocalIndexOutOfBounds(i) ->
      "LocalIndexOutOfBounds(" <> int_to_string(i) <> ")"
    vm.Unimplemented(op) -> "Unimplemented(" <> op <> ")"
  }
}

@external(erlang, "erlang", "integer_to_list")
fn int_to_string(n: Int) -> String

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
