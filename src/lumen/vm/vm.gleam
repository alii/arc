import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/order
import gleam/string
import lumen/vm/builtins.{type Builtins}
import lumen/vm/frame.{type FinallyCompletion, type TryFrame, TryFrame}
import lumen/vm/heap.{type Heap}
import lumen/vm/object
import lumen/vm/opcode.{
  type BinOpKind, type FuncTemplate, type Op, type UnaryOpKind, Add, BinOp,
  BitAnd, BitNot, BitOr, BitXor, DefineField, Div, Dup, Eq, Exp, GetField,
  GetLocal, Gt, GtEq, Jump, JumpIfFalse, JumpIfTrue, LogicalNot, Lt, LtEq, Mod,
  Mul, Neg, NewObject, NotEq, Pop, Pos, PushConst, PushTry, PutField, PutLocal,
  Return, ShiftLeft, ShiftRight, StrictEq, StrictNotEq, Sub, Swap, UShiftRight,
  UnaryOp, Void,
}
import lumen/vm/value.{
  type JsNum, type JsValue, Finite, Infinity, JsBigInt, JsBool, JsFunction,
  JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined, JsUninitialized,
  NaN, NegInfinity, ObjectSlot,
}

// ============================================================================
// Public types
// ============================================================================

/// JS-level completion — either normal return or uncaught exception.
pub type Completion {
  NormalCompletion(value: JsValue, heap: Heap)
  ThrowCompletion(value: JsValue, heap: Heap)
}

/// Internal VM error — these are bugs in the VM, not JS-level errors.
pub type VmError {
  /// Tried to read past end of bytecode
  PcOutOfBounds(pc: Int)
  /// Stack underflow
  StackUnderflow(op: String)
  /// Local variable index out of bounds
  LocalIndexOutOfBounds(index: Int)
  /// Unimplemented opcode
  Unimplemented(op: String)
}

// ============================================================================
// Internal state
// ============================================================================

/// Internal execution state for the VM loop.
type State {
  State(
    stack: List(JsValue),
    locals: List(JsValue),
    constants: List(JsValue),
    code: List(Op),
    heap: Heap,
    pc: Int,
    try_stack: List(TryFrame),
    finally_stack: List(FinallyCompletion),
    builtins: Builtins,
  )
}

/// Signals from step() — either continue with new state, or stop.
type StepResult {
  Done
  VmErr(VmError)
  Thrown
}

// ============================================================================
// Public API
// ============================================================================

/// Run a function template and return its completion + updated heap.
pub fn run(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
) -> Result(Completion, VmError) {
  let locals = list.repeat(JsUndefined, func.local_count)
  let state =
    State(
      stack: [],
      locals:,
      constants: func.constants,
      code: func.bytecode,
      heap:,
      pc: 0,
      try_stack: [],
      finally_stack: [],
      builtins:,
    )
  execute(state)
}

// ============================================================================
// Execution loop
// ============================================================================

/// Main execution loop. Tail-recursive.
fn execute(state: State) -> Result(Completion, VmError) {
  case list_get(state.code, state.pc) {
    Error(_) -> {
      // Reached end of bytecode — return top of stack or undefined
      case state.stack {
        [top, ..] -> Ok(NormalCompletion(top, state.heap))
        [] -> Ok(NormalCompletion(JsUndefined, state.heap))
      }
    }
    Ok(op) -> {
      case step(state, op) {
        Ok(new_state) -> execute(new_state)
        Error(#(Done, result, heap)) -> Ok(NormalCompletion(result, heap))
        Error(#(VmErr(err), _, _)) -> Error(err)
        Error(#(Thrown, thrown_value, heap)) -> {
          // Try to unwind to a catch handler
          let updated_state = State(..state, heap:)
          case unwind_to_catch(updated_state, thrown_value) {
            Ok(caught_state) -> execute(caught_state)
            Error(_) -> Ok(ThrowCompletion(thrown_value, heap))
          }
        }
      }
    }
  }
}

/// Try to find a catch handler on the try_stack.
/// If found: restore stack to saved depth, push thrown value, jump to catch_target.
/// If not found: return Error(Nil) → uncaught exception.
fn unwind_to_catch(state: State, thrown_value: JsValue) -> Result(State, Nil) {
  case state.try_stack {
    [] -> Error(Nil)
    [TryFrame(catch_target:, stack_depth:), ..rest_try] -> {
      let restored_stack = truncate_stack(state.stack, stack_depth)
      Ok(
        State(
          ..state,
          stack: [thrown_value, ..restored_stack],
          try_stack: rest_try,
          pc: catch_target,
        ),
      )
    }
  }
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  case list.length(stack) > depth {
    True -> truncate_stack(list.drop(stack, 1), depth)
    False -> stack
  }
}

// ============================================================================
// Step — single instruction dispatch
// ============================================================================

/// Execute a single instruction. Returns Ok(new_state) to continue,
/// or Error(#(signal, value, heap)) to stop.
fn step(state: State, op: Op) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    PushConst(index) -> {
      case list_get(state.constants, index) {
        Ok(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        Error(_) -> {
          let #(heap, err) =
            object.make_range_error(
              state.heap,
              state.builtins,
              "constant index out of bounds: " <> int.to_string(index),
            )
          Error(#(Thrown, err, heap))
        }
      }
    }

    Pop -> {
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> Error(#(VmErr(StackUnderflow("Pop")), JsUndefined, state.heap))
      }
    }

    Dup -> {
      case state.stack {
        [top, ..] ->
          Ok(State(..state, stack: [top, ..state.stack], pc: state.pc + 1))
        [] -> Error(#(VmErr(StackUnderflow("Dup")), JsUndefined, state.heap))
      }
    }

    Swap -> {
      case state.stack {
        [a, b, ..rest] ->
          Ok(State(..state, stack: [b, a, ..rest], pc: state.pc + 1))
        _ -> Error(#(VmErr(StackUnderflow("Swap")), JsUndefined, state.heap))
      }
    }

    GetLocal(index) -> {
      case list_get(state.locals, index) {
        Ok(JsUninitialized) -> {
          let #(heap, err) =
            object.make_reference_error(
              state.heap,
              state.builtins,
              "Cannot access variable before initialization (TDZ)",
            )
          Error(#(Thrown, err, heap))
        }
        Ok(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        Error(_) ->
          Error(#(VmErr(LocalIndexOutOfBounds(index)), JsUndefined, state.heap))
      }
    }

    PutLocal(index) -> {
      case state.stack {
        [value, ..rest] -> {
          case list_set(state.locals, index, value) {
            Ok(new_locals) ->
              Ok(
                State(
                  ..state,
                  stack: rest,
                  locals: new_locals,
                  pc: state.pc + 1,
                ),
              )
            Error(_) ->
              Error(#(
                VmErr(LocalIndexOutOfBounds(index)),
                JsUndefined,
                state.heap,
              ))
          }
        }
        [] ->
          Error(#(VmErr(StackUnderflow("PutLocal")), JsUndefined, state.heap))
      }
    }

    BinOp(kind) -> {
      case state.stack {
        [right, left, ..rest] -> {
          case exec_binop(kind, left, right) {
            Ok(result) ->
              Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
            Error(msg) -> {
              let #(heap, err) =
                object.make_type_error(state.heap, state.builtins, msg)
              Error(#(Thrown, err, heap))
            }
          }
        }
        _ -> Error(#(VmErr(StackUnderflow("BinOp")), JsUndefined, state.heap))
      }
    }

    UnaryOp(kind) -> {
      case state.stack {
        [operand, ..rest] -> {
          case exec_unaryop(kind, operand) {
            Ok(result) ->
              Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
            Error(msg) -> {
              let #(heap, err) =
                object.make_type_error(state.heap, state.builtins, msg)
              Error(#(Thrown, err, heap))
            }
          }
        }
        [] ->
          Error(#(VmErr(StackUnderflow("UnaryOp")), JsUndefined, state.heap))
      }
    }

    Return -> {
      case state.stack {
        [value, ..] -> Error(#(Done, value, state.heap))
        [] -> Error(#(Done, JsUndefined, state.heap))
      }
    }

    Jump(target) -> Ok(State(..state, pc: target))

    JumpIfFalse(target) -> {
      case state.stack {
        [value, ..rest] -> {
          case is_truthy(value) {
            True -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            False -> Ok(State(..state, stack: rest, pc: target))
          }
        }
        [] ->
          Error(#(VmErr(StackUnderflow("JumpIfFalse")), JsUndefined, state.heap))
      }
    }

    JumpIfTrue(target) -> {
      case state.stack {
        [value, ..rest] -> {
          case is_truthy(value) {
            True -> Ok(State(..state, stack: rest, pc: target))
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
          }
        }
        [] ->
          Error(#(VmErr(StackUnderflow("JumpIfTrue")), JsUndefined, state.heap))
      }
    }

    // -- Exception handling --
    PushTry(catch_target) -> {
      let frame = TryFrame(catch_target:, stack_depth: list.length(state.stack))
      Ok(
        State(..state, try_stack: [frame, ..state.try_stack], pc: state.pc + 1),
      )
    }

    opcode.PopTry -> {
      case state.try_stack {
        [_, ..rest] -> Ok(State(..state, try_stack: rest, pc: state.pc + 1))
        [] ->
          Error(#(
            VmErr(StackUnderflow("PopTry: empty try_stack")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    opcode.Throw -> {
      case state.stack {
        [value, ..] -> Error(#(Thrown, value, state.heap))
        [] -> Error(#(VmErr(StackUnderflow("Throw")), JsUndefined, state.heap))
      }
    }

    opcode.EnterFinally -> {
      Ok(
        State(
          ..state,
          finally_stack: [frame.NormalCompletion, ..state.finally_stack],
          pc: state.pc + 1,
        ),
      )
    }

    opcode.LeaveFinally -> {
      case state.finally_stack {
        [frame.NormalCompletion, ..rest] ->
          Ok(State(..state, finally_stack: rest, pc: state.pc + 1))
        [frame.ThrowCompletion(value:), ..] ->
          Error(#(Thrown, value, state.heap))
        [frame.ReturnCompletion(value:), ..] ->
          Error(#(Done, value, state.heap))
        [] ->
          Error(#(
            VmErr(StackUnderflow("LeaveFinally: empty finally_stack")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Property access --
    NewObject -> {
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            properties: dict.new(),
            prototype: Some(state.builtins.object_prototype),
          ),
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    GetField(name) -> {
      case state.stack {
        [JsNull, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of null (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsUndefined, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of undefined (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsObject(ref), ..rest] -> {
          let val = case object.get_property(state.heap, ref, name) {
            Ok(v) -> v
            Error(_) -> JsUndefined
          }
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        }
        [_, ..rest] -> {
          // Non-object, non-null/undefined: return undefined (simplified)
          Ok(State(..state, stack: [JsUndefined, ..rest], pc: state.pc + 1))
        }
        [] ->
          Error(#(VmErr(StackUnderflow("GetField")), JsUndefined, state.heap))
      }
    }

    PutField(name) -> {
      case state.stack {
        [value, JsObject(ref), ..rest] -> {
          let heap = object.set_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: rest, pc: state.pc + 1))
        }
        [_, _, ..rest] -> {
          // PutField on non-object: silently ignore (JS sloppy mode behavior)
          Ok(State(..state, stack: rest, pc: state.pc + 1))
        }
        _ ->
          Error(#(VmErr(StackUnderflow("PutField")), JsUndefined, state.heap))
      }
    }

    DefineField(name) -> {
      // Like PutField but keeps the object on the stack (for object literal construction)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let heap = object.set_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> {
          // DefineField on non-object: no-op, keep object on stack
          Ok(State(..state, pc: state.pc + 1))
        }
        _ ->
          Error(#(VmErr(StackUnderflow("DefineField")), JsUndefined, state.heap))
      }
    }

    _ ->
      Error(#(
        VmErr(Unimplemented("opcode: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

// ============================================================================
// JS type coercion and operators
// ============================================================================

/// JS ToBoolean: https://tc39.es/ecma262/#sec-toboolean
fn is_truthy(value: JsValue) -> Bool {
  case value {
    JsUndefined | JsNull | JsUninitialized -> False
    JsBool(b) -> b
    JsNumber(NaN) -> False
    JsNumber(Finite(n)) -> n != 0.0
    JsNumber(Infinity) | JsNumber(NegInfinity) -> True
    JsString(s) -> s != ""
    JsBigInt(value.BigInt(n)) -> n != 0
    // Objects, functions, symbols are always truthy
    JsObject(_) | JsFunction(_) | JsSymbol(_) -> True
  }
}

/// Execute a binary operation on two JsValues.
fn exec_binop(
  kind: BinOpKind,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, String) {
  case kind {
    // Arithmetic — JS + is overloaded for string concat
    Add -> js_add(left, right)
    Sub -> num_binop(left, right, num_sub)
    Mul -> num_binop(left, right, num_mul)
    Div -> num_binop(left, right, num_div)
    Mod -> num_binop(left, right, num_mod)
    Exp -> num_binop(left, right, num_exp)

    // Bitwise — convert to i32, operate, convert back
    BitAnd -> bitwise_binop(left, right, int.bitwise_and)
    BitOr -> bitwise_binop(left, right, int.bitwise_or)
    BitXor -> bitwise_binop(left, right, int.bitwise_exclusive_or)
    ShiftLeft ->
      bitwise_binop(left, right, fn(a, b) {
        int.bitwise_shift_left(a, int.bitwise_and(b, 31))
      })
    ShiftRight ->
      bitwise_binop(left, right, fn(a, b) {
        int.bitwise_shift_right(a, int.bitwise_and(b, 31))
      })
    UShiftRight ->
      bitwise_binop(left, right, fn(a, b) {
        int.bitwise_shift_right(
          int.bitwise_and(a, 0xFFFFFFFF),
          int.bitwise_and(b, 31),
        )
      })

    // Comparison
    StrictEq -> Ok(JsBool(strict_equal(left, right)))
    StrictNotEq -> Ok(JsBool(!strict_equal(left, right)))
    Eq -> Ok(JsBool(abstract_equal(left, right)))
    NotEq -> Ok(JsBool(!abstract_equal(left, right)))

    Lt -> compare_values(left, right, fn(ord) { ord == LtOrd })
    LtEq ->
      compare_values(left, right, fn(ord) { ord == LtOrd || ord == EqOrd })
    Gt -> compare_values(left, right, fn(ord) { ord == GtOrd })
    GtEq ->
      compare_values(left, right, fn(ord) { ord == GtOrd || ord == EqOrd })

    // These need runtime object support — defer
    opcode.In | opcode.InstanceOf -> Error("in/instanceof not yet implemented")
  }
}

/// JS + operator: string concat if either side is string, else numeric add.
fn js_add(left: JsValue, right: JsValue) -> Result(JsValue, String) {
  case left, right {
    JsString(a), JsString(b) -> Ok(JsString(a <> b))
    JsString(a), _ -> Ok(JsString(a <> to_js_string(right)))
    _, JsString(b) -> Ok(JsString(to_js_string(left) <> b))
    _, _ -> num_binop(left, right, num_add)
  }
}

/// Execute a unary operation.
fn exec_unaryop(kind: UnaryOpKind, operand: JsValue) -> Result(JsValue, String) {
  case kind {
    Neg ->
      case to_number(operand) {
        Ok(n) -> Ok(JsNumber(num_negate(n)))
        Error(e) -> Error(e)
      }
    Pos ->
      case to_number(operand) {
        Ok(n) -> Ok(JsNumber(n))
        Error(e) -> Error(e)
      }
    BitNot ->
      case to_number(operand) {
        Ok(n) ->
          Ok(JsNumber(Finite(int.to_float(int.bitwise_not(num_to_int32(n))))))
        Error(e) -> Error(e)
      }
    LogicalNot -> Ok(JsBool(!is_truthy(operand)))
    Void -> Ok(JsUndefined)
  }
}

// ============================================================================
// JsNum arithmetic — IEEE 754 semantics without BEAM floats for special values
// ============================================================================

fn num_add(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, NegInfinity | NegInfinity, Infinity -> NaN
    Infinity, _ | _, Infinity -> Infinity
    NegInfinity, _ | _, NegInfinity -> NegInfinity
    Finite(x), Finite(y) -> Finite(x +. y)
  }
}

fn num_sub(a: JsNum, b: JsNum) -> JsNum {
  num_add(a, num_negate(b))
}

fn num_mul(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, Finite(0.0) | Finite(0.0), Infinity -> NaN
    NegInfinity, Finite(0.0) | Finite(0.0), NegInfinity -> NaN
    Infinity, Finite(x) | Finite(x), Infinity ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    NegInfinity, Finite(x) | Finite(x), NegInfinity ->
      case x >. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Infinity, Infinity | NegInfinity, NegInfinity -> Infinity
    Infinity, NegInfinity | NegInfinity, Infinity -> NegInfinity
    Finite(x), Finite(y) -> Finite(x *. y)
  }
}

fn num_div(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, Infinity
    | Infinity, NegInfinity
    | NegInfinity, Infinity
    | NegInfinity, NegInfinity
    -> NaN
    Infinity, Finite(x) ->
      case x >=. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    NegInfinity, Finite(x) ->
      case x >=. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Finite(_), Infinity | Finite(_), NegInfinity -> Finite(0.0)
    Finite(0.0), Finite(0.0) -> NaN
    Finite(x), Finite(0.0) ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    Finite(x), Finite(y) -> Finite(x /. y)
  }
}

fn num_mod(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, _ | NegInfinity, _ -> NaN
    _, Infinity | _, NegInfinity -> a
    Finite(_), Finite(0.0) -> NaN
    Finite(0.0), Finite(_) -> Finite(0.0)
    Finite(x), Finite(y) ->
      Finite(x -. int.to_float(float.truncate(x /. y)) *. y)
  }
}

fn num_exp(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    _, Finite(0.0) -> Finite(1.0)
    _, NaN -> NaN
    NaN, _ -> NaN
    Finite(x), Finite(y) -> Finite(float_power(x, y))
    Infinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    NegInfinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    _, Infinity -> NaN
    _, NegInfinity -> NaN
  }
}

fn num_negate(n: JsNum) -> JsNum {
  case n {
    Finite(x) -> Finite(float.negate(x))
    NaN -> NaN
    Infinity -> NegInfinity
    NegInfinity -> Infinity
  }
}

/// Apply a JsNum binary operation after coercing both operands to numbers.
fn num_binop(
  left: JsValue,
  right: JsValue,
  op: fn(JsNum, JsNum) -> JsNum,
) -> Result(JsValue, String) {
  case to_number(left), to_number(right) {
    Ok(a), Ok(b) -> Ok(JsNumber(op(a, b)))
    Error(e), _ | _, Error(e) -> Error(e)
  }
}

/// Apply a bitwise binary operation (convert to i32, operate, convert back).
fn bitwise_binop(
  left: JsValue,
  right: JsValue,
  op: fn(Int, Int) -> Int,
) -> Result(JsValue, String) {
  case to_number(left), to_number(right) {
    Ok(a), Ok(b) ->
      Ok(JsNumber(Finite(int.to_float(op(num_to_int32(a), num_to_int32(b))))))
    Error(e), _ | _, Error(e) -> Error(e)
  }
}

/// JS ToNumber: https://tc39.es/ecma262/#sec-tonumber
fn to_number(value: JsValue) -> Result(JsNum, String) {
  case value {
    JsNumber(n) -> Ok(n)
    JsUndefined -> Ok(NaN)
    JsNull -> Ok(Finite(0.0))
    JsBool(True) -> Ok(Finite(1.0))
    JsBool(False) -> Ok(Finite(0.0))
    JsString("") -> Ok(Finite(0.0))
    JsString(s) ->
      case float.parse(s) {
        Ok(n) -> Ok(Finite(n))
        Error(_) ->
          case int.parse(s) {
            Ok(n) -> Ok(Finite(int.to_float(n)))
            Error(_) -> Ok(NaN)
          }
      }
    JsBigInt(_) -> Error("Cannot convert BigInt to number")
    JsSymbol(_) -> Error("Cannot convert Symbol to number")
    JsObject(_) | JsFunction(_) -> Ok(NaN)
    JsUninitialized -> Error("Cannot access before initialization")
  }
}

/// JS ToString (simplified): https://tc39.es/ecma262/#sec-tostring
fn to_js_string(value: JsValue) -> String {
  case value {
    JsUndefined -> "undefined"
    JsNull -> "null"
    JsBool(True) -> "true"
    JsBool(False) -> "false"
    JsNumber(Finite(n)) -> float.to_string(n)
    JsNumber(NaN) -> "NaN"
    JsNumber(Infinity) -> "Infinity"
    JsNumber(NegInfinity) -> "-Infinity"
    JsString(s) -> s
    JsBigInt(value.BigInt(n)) -> int.to_string(n)
    JsSymbol(_) -> "Symbol()"
    JsObject(_) -> "[object Object]"
    JsFunction(_) -> "function() {}"
    JsUninitialized -> "undefined"
  }
}

/// JS === (strict equality)
fn strict_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    JsUndefined, JsUndefined -> True
    JsNull, JsNull -> True
    JsBool(a), JsBool(b) -> a == b
    // NaN !== NaN
    JsNumber(NaN), _ | _, JsNumber(NaN) -> False
    JsNumber(a), JsNumber(b) -> a == b
    JsString(a), JsString(b) -> a == b
    JsBigInt(a), JsBigInt(b) -> a == b
    // Object identity (same Ref)
    JsObject(a), JsObject(b) -> a == b
    JsFunction(a), JsFunction(b) -> a == b
    JsSymbol(a), JsSymbol(b) -> a == b
    _, _ -> False
  }
}

/// JS == (abstract equality, simplified)
fn abstract_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // Same type — use strict equality
    JsNull, JsNull
    | JsUndefined, JsUndefined
    | JsNull, JsUndefined
    | JsUndefined, JsNull
    -> True
    JsNumber(_), JsNumber(_)
    | JsBool(_), JsBool(_)
    | JsString(_), JsString(_)
    | JsObject(_), JsObject(_)
    | JsFunction(_), JsFunction(_)
    | JsSymbol(_), JsSymbol(_)
    | JsBigInt(_), JsBigInt(_)
    -> strict_equal(left, right)
    // Number vs String — coerce string to number
    JsNumber(_), JsString(s) ->
      case to_number(JsString(s)) {
        Ok(n) -> strict_equal(left, JsNumber(n))
        Error(_) -> False
      }
    JsString(_), JsNumber(_) -> abstract_equal(right, left)
    // Bool vs anything — coerce bool to number
    JsBool(_), _ ->
      case to_number(left) {
        Ok(n) -> abstract_equal(JsNumber(n), right)
        Error(_) -> False
      }
    _, JsBool(_) -> abstract_equal(right, left)
    _, _ -> False
  }
}

/// Comparison order for relational ops.
type CompareOrd {
  LtOrd
  EqOrd
  GtOrd
}

/// Compare two values for relational operators (<, <=, >, >=).
fn compare_values(
  left: JsValue,
  right: JsValue,
  pred: fn(CompareOrd) -> Bool,
) -> Result(JsValue, String) {
  case left, right {
    JsString(a), JsString(b) -> {
      let ord = case string.compare(a, b) {
        order.Lt -> LtOrd
        order.Eq -> EqOrd
        order.Gt -> GtOrd
      }
      Ok(JsBool(pred(ord)))
    }
    _, _ -> {
      case to_number(left), to_number(right) {
        Ok(NaN), _ | _, Ok(NaN) -> Ok(JsBool(False))
        Ok(a), Ok(b) -> Ok(JsBool(pred(compare_nums(a, b))))
        Error(e), _ | _, Error(e) -> Error(e)
      }
    }
  }
}

/// Compare two JsNums (neither is NaN).
fn compare_nums(a: JsNum, b: JsNum) -> CompareOrd {
  case a, b {
    Infinity, Infinity | NegInfinity, NegInfinity -> EqOrd
    Infinity, _ -> GtOrd
    _, Infinity -> LtOrd
    NegInfinity, _ -> LtOrd
    _, NegInfinity -> GtOrd
    Finite(x), Finite(y) ->
      case x == y {
        True -> EqOrd
        False ->
          case x <. y {
            True -> LtOrd
            False -> GtOrd
          }
      }
    // NaN cases handled by caller
    NaN, _ | _, NaN -> EqOrd
  }
}

// ============================================================================
// Helpers
// ============================================================================

/// Get element at index from a list. O(n) — will replace with Array later.
fn list_get(items: List(a), index: Int) -> Result(a, Nil) {
  case index < 0 {
    True -> Error(Nil)
    False ->
      case items {
        [] -> Error(Nil)
        [first, ..] if index == 0 -> Ok(first)
        [_, ..rest] -> list_get(rest, index - 1)
      }
  }
}

/// Set element at index in a list. O(n).
fn list_set(items: List(a), index: Int, value: a) -> Result(List(a), Nil) {
  case items, index {
    [], _ -> Error(Nil)
    [_, ..rest], 0 -> Ok([value, ..rest])
    [first, ..rest], n ->
      case list_set(rest, n - 1, value) {
        Ok(new_rest) -> Ok([first, ..new_rest])
        Error(e) -> Error(e)
      }
  }
}

/// Convert JsNum to int32 (JS ToInt32).
fn num_to_int32(n: JsNum) -> Int {
  case n {
    NaN | Infinity | NegInfinity -> 0
    Finite(f) -> {
      let i = float.truncate(f)
      // Wrap to 32 bits
      let wrapped = int.bitwise_and(i, 0xFFFFFFFF)
      // Sign extend if needed
      case wrapped > 0x7FFFFFFF {
        True -> wrapped - 0x100000000
        False -> wrapped
      }
    }
  }
}

// ============================================================================
// Float helpers — only power needs FFI now
// ============================================================================

@external(erlang, "lumen_vm_ffi", "float_power")
fn float_power(base: Float, exp: Float) -> Float
