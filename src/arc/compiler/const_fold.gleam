import arc/bytecode/binop.{
  type ClassifiedBinOp, AddOp, InOp, InstanceOfOp, PureOp,
}
import arc/bytecode/opcode
import arc/parser/ast
import arc/rt/types.{
  type JsNum, type JsVal, JFloat, JInt, JNegInf, JPosInf, mk_bigint, mk_bool,
  mk_int, mk_number, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}

// same int/float rule as the aot emitter: ints in [0, 2^31) except -0
pub fn number_const(n: ast.LiteralNumber) -> JsVal {
  mk_number(literal_num(n))
}

pub fn literal_num(n: ast.LiteralNumber) -> JsNum {
  case n {
    ast.InfiniteNumber -> JPosInf
    ast.FiniteNumber(f) -> {
      // range-check first, truncating 1e308 needs bignums
      let in_int_range = f >=. 0.0 && f <. 2_147_483_648.0
      case in_int_range {
        True -> {
          let i = float.truncate(f)
          case int.to_float(i) == f && !rt_val.is_neg_zero(f) {
            True -> JInt(i)
            False -> JFloat(f)
          }
        }
        False -> JFloat(f)
      }
    }
  }
}

pub fn literal_truthy(expr: ast.Expression) -> Option(Bool) {
  case expr {
    ast.BooleanLiteral(_, b) -> Some(b)
    ast.NumberLiteral(_, ast.FiniteNumber(f)) -> Some(f != 0.0)
    ast.NumberLiteral(_, ast.InfiniteNumber) -> Some(True)
    ast.BigIntLiteral(_, n) -> Some(n != 0)
    ast.StringLiteral(_, s) -> Some(s != "")
    ast.NullLiteral(_) | ast.UndefinedExpression(_) -> Some(False)
    _ -> None
  }
}

pub fn fold_unary(op: ast.UnaryOp, arg: ast.Expression) -> Option(JsVal) {
  case op, arg {
    ast.Negate, ast.NumberLiteral(_, n) ->
      Some(
        mk_number(case literal_num(n) {
          JInt(0) -> JFloat(-0.0)
          JInt(x) -> JInt(-x)
          JFloat(x) -> JFloat(float.negate(x))
          _ -> JNegInf
        }),
      )
    ast.Negate, ast.BigIntLiteral(_, n) -> Some(mk_bigint(-n))
    ast.UnaryPlus, ast.NumberLiteral(_, n) -> Some(number_const(n))
    // already an int32
    ast.BitwiseNot, ast.NumberLiteral(_, n) ->
      case literal_num(n) {
        JInt(x) -> Some(mk_int(-x - 1))
        _ -> None
      }
    ast.LogicalNot, _ ->
      literal_truthy(arg) |> option.map(fn(t) { mk_bool(!t) })
    ast.Void, _ -> literal_truthy(arg) |> option.map(fn(_) { mk_undefined() })
    _, _ -> None
  }
}

pub fn translate_binop(op: ast.BinaryOp) -> ClassifiedBinOp {
  case op {
    ast.Add -> AddOp
    ast.Subtract -> PureOp(binop.Arith(binop.Sub))
    ast.Multiply -> PureOp(binop.Arith(binop.Mul))
    ast.Divide -> PureOp(binop.Arith(binop.Div))
    ast.Modulo -> PureOp(binop.Arith(binop.Mod))
    ast.Exponentiation -> PureOp(binop.Arith(binop.Exp))
    ast.StrictEqual -> PureOp(binop.Equality(binop.StrictEq))
    ast.StrictNotEqual -> PureOp(binop.Equality(binop.StrictNotEq))
    ast.Equal -> PureOp(binop.Equality(binop.LooseEq))
    ast.NotEqual -> PureOp(binop.Equality(binop.LooseNotEq))
    ast.LessThan -> PureOp(binop.Compare(binop.Less))
    ast.GreaterThan -> PureOp(binop.Compare(binop.Greater))
    ast.LessThanEqual -> PureOp(binop.Compare(binop.LessEq))
    ast.GreaterThanEqual -> PureOp(binop.Compare(binop.GreaterEq))
    ast.LeftShift -> PureOp(binop.Bitwise(binop.ShiftLeft))
    ast.RightShift -> PureOp(binop.Bitwise(binop.ShiftRight))
    ast.UnsignedRightShift -> PureOp(binop.Bitwise(binop.ShiftRightUnsigned))
    ast.BitwiseAnd -> PureOp(binop.Bitwise(binop.BitAnd))
    ast.BitwiseOr -> PureOp(binop.Bitwise(binop.BitOr))
    ast.BitwiseXor -> PureOp(binop.Bitwise(binop.BitXor))
    ast.In -> InOp
    ast.InstanceOf -> InstanceOfOp
  }
}

pub fn update_binop(op: ast.UpdateOp) -> ClassifiedBinOp {
  case op {
    ast.Increment -> AddOp
    ast.Decrement -> PureOp(binop.Arith(binop.Sub))
  }
}

// typeof and delete map to None: they have dedicated arms
pub fn translate_unaryop(op: ast.UnaryOp) -> Option(opcode.UnaryOpKind) {
  case op {
    ast.Negate -> Some(opcode.Neg)
    ast.UnaryPlus -> Some(opcode.Pos)
    ast.LogicalNot -> Some(opcode.LogicalNot)
    ast.BitwiseNot -> Some(opcode.BitNot)
    ast.Void -> Some(opcode.Void)
    ast.TypeOf | ast.Delete -> None
  }
}

pub fn compound_to_binop(op: ast.AssignmentOp) -> Result(ClassifiedBinOp, Nil) {
  case op {
    ast.AddAssign -> Ok(AddOp)
    ast.SubtractAssign -> Ok(PureOp(binop.Arith(binop.Sub)))
    ast.MultiplyAssign -> Ok(PureOp(binop.Arith(binop.Mul)))
    ast.DivideAssign -> Ok(PureOp(binop.Arith(binop.Div)))
    ast.ModuloAssign -> Ok(PureOp(binop.Arith(binop.Mod)))
    ast.ExponentiationAssign -> Ok(PureOp(binop.Arith(binop.Exp)))
    ast.LeftShiftAssign -> Ok(PureOp(binop.Bitwise(binop.ShiftLeft)))
    ast.RightShiftAssign -> Ok(PureOp(binop.Bitwise(binop.ShiftRight)))
    ast.UnsignedRightShiftAssign ->
      Ok(PureOp(binop.Bitwise(binop.ShiftRightUnsigned)))
    ast.BitwiseAndAssign -> Ok(PureOp(binop.Bitwise(binop.BitAnd)))
    ast.BitwiseOrAssign -> Ok(PureOp(binop.Bitwise(binop.BitOr)))
    ast.BitwiseXorAssign -> Ok(PureOp(binop.Bitwise(binop.BitXor)))
    ast.Assign -> Error(Nil)
    ast.LogicalAndAssign | ast.LogicalOrAssign | ast.NullishCoalesceAssign ->
      Error(Nil)
  }
}
