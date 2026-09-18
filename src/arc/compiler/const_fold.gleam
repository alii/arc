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
    ast.StringExpression(_, s) -> Some(s != "")
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
