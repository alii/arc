import arc/vm/array
import arc/vm/builtins
import arc/vm/builtins/common
import arc/vm/heap
import arc/vm/object
import arc/vm/opcode.{
  type FuncTemplate, type Op, Add, BinOp, BitAnd, BitNot, BitOr, BitXor,
  DefineField, Div, Dup, Eq, Exp, FuncTemplate, GetField, GetLocal, Gt, GtEq,
  Jump, JumpIfFalse, JumpIfTrue, LogicalNot, Lt, LtEq, Mod, Mul, Neg, NewObject,
  NotEq, Pop, Pos, PushConst, PushTry, PutField, PutLocal, Return, ShiftLeft,
  ShiftRight, StrictEq, StrictNotEq, Sub, Swap, UShiftRight, UnaryOp, Void,
}
import arc/vm/value.{
  AccessorProperty, DataProperty, Finite, JsBool, JsNull, JsNumber, JsObject,
  JsString, JsUndefined,
}
import arc/vm/vm
import gleam/option.{None, Some}
import gleam/result

/// Test helper: read a data property walking the prototype chain.
fn get_data(
  h: heap.Heap,
  ref: value.Ref,
  key: String,
) -> Result(value.JsValue, Nil) {
  case object.get_own_property(h, ref, key) {
    Some(DataProperty(value: val, ..)) -> Ok(val)
    Some(AccessorProperty(..)) -> Error(Nil)
    None ->
      case heap.read(h, ref) {
        Some(value.ObjectSlot(prototype: Some(proto_ref), ..)) ->
          get_data(h, proto_ref, key)
        _ -> Error(Nil)
      }
  }
}

fn make_func(
  bytecode: List(Op),
  constants: List(value.JsValue),
  local_count: Int,
) -> FuncTemplate {
  FuncTemplate(
    name: None,
    arity: 0,
    local_count:,
    bytecode: array.from_list(bytecode),
    constants: array.from_list(constants),
    functions: array.from_list([]),
    env_descriptors: [],
    is_strict: False,
    is_arrow: False,
    is_derived_constructor: False,
    is_generator: False,
    is_async: False,
  )
}

/// Helper: run bytecode with builtins, return just the value for normal completion.
fn run_simple(
  bytecode: List(Op),
  constants: List(value.JsValue),
) -> Result(value.JsValue, vm.VmError) {
  let func = make_func(bytecode, constants, 0)
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  case vm.run(func, h, b) {
    Ok(#(vm.NormalCompletion(val, _heap), _state)) -> Ok(val)
    Ok(#(vm.ThrowCompletion(_, _), _)) -> panic as "unexpected ThrowCompletion"
    Ok(#(vm.YieldCompletion(_, _), _)) -> panic as "unexpected YieldCompletion"
    Error(e) -> Error(e)
  }
}

/// Helper: run bytecode expecting a ThrowCompletion, return the thrown value.
fn run_throwing(
  bytecode: List(Op),
  constants: List(value.JsValue),
) -> Result(value.JsValue, vm.VmError) {
  let func = make_func(bytecode, constants, 0)
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  case vm.run(func, h, b) {
    Ok(#(vm.ThrowCompletion(val, _heap), _state)) -> Ok(val)
    Ok(#(vm.NormalCompletion(_, _), _)) ->
      panic as "expected ThrowCompletion, got NormalCompletion"
    Ok(#(vm.YieldCompletion(_, _), _)) -> panic as "unexpected YieldCompletion"
    Error(e) -> Error(e)
  }
}

/// Helper: run func with locals + builtins, return Completion.
fn run_func(func: FuncTemplate) -> Result(vm.Completion, vm.VmError) {
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  use #(completion, _state) <- result.map(vm.run(func, h, b))
  completion
}

// ============================================================================
// Basic stack ops
// ============================================================================

pub fn push_const_test() {
  let assert Ok(JsNumber(Finite(42.0))) =
    run_simple([PushConst(0)], [JsNumber(Finite(42.0))])
}

pub fn push_const_string_test() {
  let assert Ok(JsString("hello")) =
    run_simple([PushConst(0)], [JsString("hello")])
}

pub fn empty_program_returns_undefined_test() {
  let assert Ok(JsUndefined) = run_simple([], [])
}

pub fn pop_test() {
  // Push two, pop one, top should be the first
  let assert Ok(JsNumber(Finite(1.0))) =
    run_simple([PushConst(0), PushConst(1), Pop], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn dup_test() {
  // Push 5, dup, pop — should still have 5
  let assert Ok(JsNumber(Finite(5.0))) =
    run_simple([PushConst(0), Dup, Pop], [JsNumber(Finite(5.0))])
}

pub fn swap_test() {
  // Push 1 then 2, swap => top is 1
  let assert Ok(JsNumber(Finite(1.0))) =
    run_simple([PushConst(0), PushConst(1), Swap], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn return_test() {
  // Return stops execution and returns top of stack
  let assert Ok(JsNumber(Finite(10.0))) =
    run_simple([PushConst(0), Return, PushConst(1)], [
      JsNumber(Finite(10.0)),
      JsNumber(Finite(99.0)),
    ])
}

// ============================================================================
// Arithmetic
// ============================================================================

pub fn add_numbers_test() {
  // 1 + 2 = 3
  let assert Ok(JsNumber(Finite(3.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(Add)], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn sub_test() {
  // 10 - 3 = 7
  let assert Ok(JsNumber(Finite(7.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(Sub)], [
      JsNumber(Finite(10.0)),
      JsNumber(Finite(3.0)),
    ])
}

pub fn mul_test() {
  // 4 * 5 = 20
  let assert Ok(JsNumber(Finite(20.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(Mul)], [
      JsNumber(Finite(4.0)),
      JsNumber(Finite(5.0)),
    ])
}

pub fn div_test() {
  // 10 / 4 = 2.5
  let assert Ok(JsNumber(Finite(2.5))) =
    run_simple([PushConst(0), PushConst(1), BinOp(Div)], [
      JsNumber(Finite(10.0)),
      JsNumber(Finite(4.0)),
    ])
}

pub fn mod_test() {
  // 7 % 3 = 1
  let assert Ok(JsNumber(Finite(1.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(Mod)], [
      JsNumber(Finite(7.0)),
      JsNumber(Finite(3.0)),
    ])
}

pub fn exp_test() {
  // 2 ** 10 = 1024
  let assert Ok(JsNumber(Finite(1024.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(Exp)], [
      JsNumber(Finite(2.0)),
      JsNumber(Finite(10.0)),
    ])
}

// ============================================================================
// String concat via +
// ============================================================================

pub fn add_strings_test() {
  // "hello" + " world"
  let assert Ok(JsString("hello world")) =
    run_simple([PushConst(0), PushConst(1), BinOp(Add)], [
      JsString("hello"),
      JsString(" world"),
    ])
}

pub fn add_string_number_test() {
  // "x=" + 42 => "x=42.0"
  let assert Ok(JsString(_)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Add)], [
      JsString("x="),
      JsNumber(Finite(42.0)),
    ])
}

pub fn add_number_string_test() {
  // 5 + "px" => "5.0px"
  let assert Ok(JsString(_)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Add)], [
      JsNumber(Finite(5.0)),
      JsString("px"),
    ])
}

// ============================================================================
// Comparison
// ============================================================================

pub fn strict_eq_same_test() {
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(StrictEq)], [
      JsNumber(Finite(42.0)),
      JsNumber(Finite(42.0)),
    ])
}

pub fn strict_eq_diff_test() {
  let assert Ok(JsBool(False)) =
    run_simple([PushConst(0), PushConst(1), BinOp(StrictEq)], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn strict_neq_test() {
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(StrictNotEq)], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn abstract_eq_null_undefined_test() {
  // null == undefined => true
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Eq)], [JsNull, JsUndefined])
}

pub fn abstract_eq_number_string_test() {
  // 42 == "42" => true
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Eq)], [
      JsNumber(Finite(42.0)),
      JsString("42"),
    ])
}

pub fn abstract_neq_test() {
  // 1 != 2 => true
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(NotEq)], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn lt_test() {
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Lt)], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn lt_false_test() {
  let assert Ok(JsBool(False)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Lt)], [
      JsNumber(Finite(5.0)),
      JsNumber(Finite(3.0)),
    ])
}

pub fn lteq_test() {
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(LtEq)], [
      JsNumber(Finite(3.0)),
      JsNumber(Finite(3.0)),
    ])
}

pub fn gt_test() {
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Gt)], [
      JsNumber(Finite(5.0)),
      JsNumber(Finite(3.0)),
    ])
}

pub fn gteq_test() {
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(GtEq)], [
      JsNumber(Finite(3.0)),
      JsNumber(Finite(3.0)),
    ])
}

pub fn string_compare_test() {
  // "apple" < "banana" => true
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Lt)], [
      JsString("apple"),
      JsString("banana"),
    ])
}

// ============================================================================
// Bitwise
// ============================================================================

pub fn bit_and_test() {
  // 0b1100 & 0b1010 = 0b1000 = 8
  let assert Ok(JsNumber(Finite(8.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(BitAnd)], [
      JsNumber(Finite(12.0)),
      JsNumber(Finite(10.0)),
    ])
}

pub fn bit_or_test() {
  // 0b1100 | 0b1010 = 0b1110 = 14
  let assert Ok(JsNumber(Finite(14.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(BitOr)], [
      JsNumber(Finite(12.0)),
      JsNumber(Finite(10.0)),
    ])
}

pub fn bit_xor_test() {
  // 0b1100 ^ 0b1010 = 0b0110 = 6
  let assert Ok(JsNumber(Finite(6.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(BitXor)], [
      JsNumber(Finite(12.0)),
      JsNumber(Finite(10.0)),
    ])
}

pub fn shift_left_test() {
  // 1 << 4 = 16
  let assert Ok(JsNumber(Finite(16.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(ShiftLeft)], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(4.0)),
    ])
}

pub fn shift_right_test() {
  // 16 >> 2 = 4
  let assert Ok(JsNumber(Finite(4.0))) =
    run_simple([PushConst(0), PushConst(1), BinOp(ShiftRight)], [
      JsNumber(Finite(16.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn unsigned_shift_right_test() {
  // -1 >>> 0 = 4294967295 (0xFFFFFFFF)
  let assert Ok(JsNumber(Finite(n))) =
    run_simple([PushConst(0), PushConst(1), BinOp(UShiftRight)], [
      JsNumber(Finite(-1.0)),
      JsNumber(Finite(0.0)),
    ])
  // 4294967295.0
  assert n >. 4_294_967_294.0
}

// ============================================================================
// Unary ops
// ============================================================================

pub fn neg_test() {
  let assert Ok(JsNumber(Finite(-5.0))) =
    run_simple([PushConst(0), UnaryOp(Neg)], [JsNumber(Finite(5.0))])
}

pub fn pos_test() {
  // +"42" => 42.0 (coerce string to number)
  let assert Ok(JsNumber(Finite(42.0))) =
    run_simple([PushConst(0), UnaryOp(Pos)], [JsString("42")])
}

pub fn bitnot_test() {
  // ~5 = -6
  let assert Ok(JsNumber(Finite(-6.0))) =
    run_simple([PushConst(0), UnaryOp(BitNot)], [JsNumber(Finite(5.0))])
}

pub fn logical_not_test() {
  let assert Ok(JsBool(False)) =
    run_simple([PushConst(0), UnaryOp(LogicalNot)], [JsBool(True)])
}

pub fn logical_not_falsy_test() {
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), UnaryOp(LogicalNot)], [JsNumber(Finite(0.0))])
}

pub fn void_test() {
  let assert Ok(JsUndefined) =
    run_simple([PushConst(0), UnaryOp(Void)], [JsNumber(Finite(42.0))])
}

// ============================================================================
// Local variables
// ============================================================================

pub fn local_store_load_test() {
  // var x = 42; x
  let func =
    make_func(
      [PushConst(0), PutLocal(0), GetLocal(0)],
      [JsNumber(Finite(42.0))],
      1,
    )
  let assert Ok(vm.NormalCompletion(JsNumber(Finite(42.0)), _)) = run_func(func)
}

pub fn var_x_eq_1_plus_2_test() {
  // var x = 1 + 2; x
  let func =
    make_func(
      [PushConst(0), PushConst(1), BinOp(Add), PutLocal(0), GetLocal(0)],
      [JsNumber(Finite(1.0)), JsNumber(Finite(2.0))],
      1,
    )
  let assert Ok(vm.NormalCompletion(JsNumber(Finite(3.0)), _)) = run_func(func)
}

pub fn multiple_locals_test() {
  // var a = 10; var b = 20; a + b
  let func =
    make_func(
      [
        PushConst(0),
        PutLocal(0),
        // a = 10
        PushConst(1),
        PutLocal(1),
        // b = 20
        GetLocal(0),
        GetLocal(1),
        BinOp(Add),
        // a + b
      ],
      [JsNumber(Finite(10.0)), JsNumber(Finite(20.0))],
      2,
    )
  let assert Ok(vm.NormalCompletion(JsNumber(Finite(30.0)), _)) = run_func(func)
}

// ============================================================================
// Control flow — jumps
// ============================================================================

pub fn jump_test() {
  // Jump over one instruction
  let assert Ok(JsNumber(Finite(2.0))) =
    run_simple([PushConst(0), Jump(3), PushConst(1), PushConst(2)], [
      JsNumber(Finite(1.0)),
      JsNumber(Finite(99.0)),
      JsNumber(Finite(2.0)),
    ])
}

pub fn jump_if_false_taken_test() {
  let assert Ok(JsNumber(Finite(42.0))) =
    run_simple([PushConst(0), JumpIfFalse(3), PushConst(1), PushConst(2)], [
      JsBool(False),
      JsNumber(Finite(99.0)),
      JsNumber(Finite(42.0)),
    ])
}

pub fn jump_if_false_not_taken_test() {
  let assert Ok(JsNumber(Finite(99.0))) =
    run_simple([PushConst(0), JumpIfFalse(3), PushConst(1), Return], [
      JsBool(True),
      JsNumber(Finite(99.0)),
    ])
}

pub fn jump_if_true_test() {
  let assert Ok(JsString("yes")) =
    run_simple([PushConst(0), JumpIfTrue(3), PushConst(1), PushConst(2)], [
      JsBool(True),
      JsString("nope"),
      JsString("yes"),
    ])
}

// ============================================================================
// Truthiness
// ============================================================================

pub fn truthy_zero_is_false_test() {
  let assert Ok(JsString("falsy")) =
    run_simple([PushConst(0), JumpIfFalse(3), PushConst(1), PushConst(2)], [
      JsNumber(Finite(0.0)),
      JsString("truthy"),
      JsString("falsy"),
    ])
}

pub fn truthy_empty_string_is_false_test() {
  let assert Ok(JsString("falsy")) =
    run_simple([PushConst(0), JumpIfFalse(3), PushConst(1), PushConst(2)], [
      JsString(""),
      JsString("truthy"),
      JsString("falsy"),
    ])
}

pub fn truthy_null_is_false_test() {
  let assert Ok(JsString("falsy")) =
    run_simple([PushConst(0), JumpIfFalse(3), PushConst(1), PushConst(2)], [
      JsNull,
      JsString("truthy"),
      JsString("falsy"),
    ])
}

pub fn truthy_nonempty_string_is_true_test() {
  let assert Ok(JsString("truthy")) =
    run_simple(
      [PushConst(0), JumpIfFalse(3), PushConst(1), Return, PushConst(2)],
      [JsString("hello"), JsString("truthy"), JsString("falsy")],
    )
}

// ============================================================================
// Type coercion in ==
// ============================================================================

pub fn abstract_eq_bool_number_test() {
  // true == 1 => true
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Eq)], [
      JsBool(True),
      JsNumber(Finite(1.0)),
    ])
}

pub fn abstract_eq_false_zero_test() {
  // false == 0 => true
  let assert Ok(JsBool(True)) =
    run_simple([PushConst(0), PushConst(1), BinOp(Eq)], [
      JsBool(False),
      JsNumber(Finite(0.0)),
    ])
}

pub fn strict_eq_bool_number_false_test() {
  // true === 1 => false (different types)
  let assert Ok(JsBool(False)) =
    run_simple([PushConst(0), PushConst(1), BinOp(StrictEq)], [
      JsBool(True),
      JsNumber(Finite(1.0)),
    ])
}

// ============================================================================
// Error cases — internal VM errors (not JS exceptions)
// ============================================================================

pub fn stack_underflow_test() {
  let assert Error(vm.StackUnderflow("Pop")) = run_simple([Pop], [])
}

pub fn local_out_of_bounds_test() {
  let func = make_func([GetLocal(5)], [], 1)
  let assert Error(vm.LocalIndexOutOfBounds(5)) = run_func(func)
}

// ============================================================================
// JS-level thrown errors (ThrowCompletion, not VmError)
// ============================================================================

pub fn const_out_of_bounds_throws_range_error_test() {
  // Constant index out of bounds now throws a RangeError (JS object)
  let assert Ok(JsObject(_)) = run_throwing([PushConst(99)], [])
}

pub fn throw_without_catch_test() {
  // throw "boom" => ThrowCompletion with "boom" string
  let assert Ok(JsString("boom")) =
    run_throwing([PushConst(0), opcode.Throw], [JsString("boom")])
}

pub fn try_catch_basic_test() {
  // try { throw "caught!" } catch(e) { e }
  //  0: PushTry(3)        -- catch at pc=3
  //  1: PushConst(0)      -- push "caught!"
  //  2: Throw             -- throw it
  //  3: Return            -- catch: stack has thrown value, return it
  let assert Ok(JsString("caught!")) =
    run_simple([PushTry(3), PushConst(0), opcode.Throw, Return], [
      JsString("caught!"),
    ])
}

pub fn try_no_throw_test() {
  // try { 42 } catch(e) { e }
  // No throw happens — PopTry clears the handler, normal return
  //  0: PushTry(4)        -- catch at pc=4
  //  1: PushConst(0)      -- push 42
  //  2: opcode.PopTry     -- clear handler (normal path)
  //  3: Return            -- return 42
  //  4: Return            -- catch (never reached)
  let assert Ok(JsNumber(Finite(42.0))) =
    run_simple([PushTry(4), PushConst(0), opcode.PopTry, Return, Return], [
      JsNumber(Finite(42.0)),
    ])
}

pub fn tdz_throws_reference_error_test() {
  // Accessing an uninitialized local throws a ReferenceError
  // We need a func with JsUninitialized in locals — use PutLocal with JsUninitialized
  // Actually, the default local is JsUndefined not JsUninitialized.
  // We need to explicitly set a local to JsUninitialized via constant.
  let func =
    make_func(
      [PushConst(0), PutLocal(0), GetLocal(0)],
      [value.JsUninitialized],
      1,
    )
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let assert Ok(#(vm.ThrowCompletion(JsObject(ref), heap), _state)) =
    vm.run(func, h, b)
  // Check it's a ReferenceError via prototype chain
  let assert Ok(JsString("ReferenceError")) = get_data(heap, ref, "name")
}

pub fn type_error_thrown_for_symbol_conversion_test() {
  // +Symbol() should throw TypeError
  let func =
    make_func(
      [PushConst(0), UnaryOp(Pos)],
      [value.JsSymbol(value.SymbolId(1))],
      0,
    )
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let assert Ok(#(vm.ThrowCompletion(JsObject(ref), heap), _state)) =
    vm.run(func, h, b)
  let assert Ok(JsString("TypeError")) = get_data(heap, ref, "name")
}

// ============================================================================
// Property access — NewObject, DefineField, GetField, PutField
// ============================================================================

pub fn new_object_test() {
  // NewObject pushes a JsObject ref
  let assert Ok(JsObject(_)) = run_simple([NewObject], [])
}

pub fn define_and_get_field_test() {
  // var obj = {x: 42}; obj.x
  //  0: NewObject         -- push {}
  //  1: PushConst(0)      -- push 42
  //  2: DefineField("x")  -- obj.x = 42, keep obj on stack
  //  3: GetField("x")     -- pop obj, push obj.x
  let assert Ok(JsNumber(Finite(42.0))) =
    run_simple([NewObject, PushConst(0), DefineField("x"), GetField("x")], [
      JsNumber(Finite(42.0)),
    ])
}

pub fn put_and_get_field_test() {
  // var obj = {}; obj.y = "hello"; obj.y
  //  0: NewObject         -- push {}
  //  1: Dup               -- [obj, obj]
  //  2: PushConst(0)      -- [obj, obj, "hello"]
  //  3: PutField("y")     -- set obj.y = "hello", leaves "hello" on stack
  //  4: Pop               -- discard the "hello" value
  //  5: GetField("y")     -- pop obj (the Dup'd copy), push obj.y
  let assert Ok(JsString("hello")) =
    run_simple(
      [NewObject, Dup, PushConst(0), PutField("y"), Pop, GetField("y")],
      [JsString("hello")],
    )
}

pub fn get_field_nonexistent_returns_undefined_test() {
  // {}.doesNotExist => undefined
  let assert Ok(JsUndefined) =
    run_simple([NewObject, GetField("doesNotExist")], [])
}

pub fn get_field_on_null_throws_type_error_test() {
  // null.x => TypeError
  let func = make_func([PushConst(0), GetField("x")], [JsNull], 0)
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let assert Ok(#(vm.ThrowCompletion(JsObject(ref), heap), _state)) =
    vm.run(func, h, b)
  let assert Ok(JsString("TypeError")) = get_data(heap, ref, "name")
}

pub fn get_field_on_undefined_throws_type_error_test() {
  // undefined.x => TypeError
  let func = make_func([PushConst(0), GetField("x")], [JsUndefined], 0)
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let assert Ok(#(vm.ThrowCompletion(JsObject(ref), heap), _state)) =
    vm.run(func, h, b)
  let assert Ok(JsString("TypeError")) = get_data(heap, ref, "name")
}

// ============================================================================
// Prototype chain property lookup
// ============================================================================

pub fn prototype_chain_inherited_property_test() {
  // NewObject gets Object.prototype. Error prototypes have "name" on them.
  // Let's verify prototype chain works by making an error and reading "name"
  // through the chain.
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, err_val) = common.make_type_error(h, b, "test error")
  let assert JsObject(ref) = err_val
  // "message" is own property
  let assert Ok(JsString("test error")) = get_data(h, ref, "message")
  // "name" is inherited from TypeError.prototype
  let assert Ok(JsString("TypeError")) = get_data(h, ref, "name")
  // Property not in chain returns Error(Nil)
  let assert Error(_) = get_data(h, ref, "nonexistent")
}

// ============================================================================
// Compound programs
// ============================================================================

pub fn fibonacci_like_loop_test() {
  let func =
    make_func(
      [
        PushConst(0),
        PutLocal(0),
        // a = 0
        PushConst(1),
        PutLocal(1),
        // b = 1
        GetLocal(0),
        GetLocal(1),
        BinOp(Add),
        PutLocal(0),
        // a = a + b
        GetLocal(0),
        GetLocal(1),
        BinOp(Add),
        PutLocal(1),
        // b = a + b
        GetLocal(1),
        Return,
      ],
      [JsNumber(Finite(0.0)), JsNumber(Finite(1.0))],
      2,
    )
  let assert Ok(vm.NormalCompletion(JsNumber(Finite(2.0)), _)) = run_func(func)
}

pub fn simple_loop_with_jump_test() {
  // i = 0; while (i < 3) { i = i + 1 }; return i
  let func =
    make_func(
      [
        PushConst(0),
        PutLocal(0),
        // 0,1: i = 0
        GetLocal(0),
        PushConst(1),
        // 2,3: push i, push 3
        BinOp(Lt),
        JumpIfFalse(11),
        // 4,5: i < 3 ? continue : exit
        GetLocal(0),
        PushConst(2),
        // 6,7: push i, push 1
        BinOp(Add),
        PutLocal(0),
        // 8,9: i = i + 1
        Jump(2),
        // 10: back to loop start
        GetLocal(0),
        Return,
        // 11,12: return i
      ],
      [JsNumber(Finite(0.0)), JsNumber(Finite(3.0)), JsNumber(Finite(1.0))],
      1,
    )
  let assert Ok(vm.NormalCompletion(JsNumber(Finite(3.0)), _)) = run_func(func)
}

pub fn try_catch_with_computation_test() {
  // try {
  //   let x = 1 + 2;  // 3
  //   throw x;
  // } catch(e) {
  //   return e * 10;  // 30
  // }
  //  0: PushTry(6)
  //  1: PushConst(0)      -- 1
  //  2: PushConst(1)      -- 2
  //  3: BinOp(Add)        -- 3
  //  4: PutLocal(0)       -- x = 3
  //  5: GetLocal(0); Throw -- throw x (combined into two ops at 5,6)
  // Wait, let me re-index:
  //  0: PushTry(7)
  //  1: PushConst(0)      -- 1
  //  2: PushConst(1)      -- 2
  //  3: BinOp(Add)        -- 3
  //  4: PutLocal(0)       -- x = 3
  //  5: GetLocal(0)       -- push x
  //  6: Throw             -- throw x
  //  7: PutLocal(0)       -- catch: e in local 0 (caught value)
  //  8: GetLocal(0)       -- push e
  //  9: PushConst(2)      -- push 10
  // 10: BinOp(Mul)        -- e * 10
  // 11: Return
  let func =
    make_func(
      [
        PushTry(7),
        PushConst(0),
        PushConst(1),
        BinOp(Add),
        PutLocal(0),
        GetLocal(0),
        opcode.Throw,
        // catch:
        PutLocal(0),
        GetLocal(0),
        PushConst(2),
        BinOp(Mul),
        Return,
      ],
      [JsNumber(Finite(1.0)), JsNumber(Finite(2.0)), JsNumber(Finite(10.0))],
      1,
    )
  let assert Ok(vm.NormalCompletion(JsNumber(Finite(30.0)), _)) = run_func(func)
}
