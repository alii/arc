// flat, so the loop tells them apart with one test
pub type BinOp {
  Add
  Sub
  Mul
  Div
  Mod
  Exp
  BitAnd
  BitOr
  BitXor
  ShiftLeft
  ShiftRight
  ShiftRightUnsigned
  Less
  LessEq
  Greater
  GreaterEq
  LooseEq
  LooseNotEq
  StrictEq
  StrictNotEq
  In
  InstanceOf
}

pub fn is_comparison(op: BinOp) -> Bool {
  case op {
    Less
    | LessEq
    | Greater
    | GreaterEq
    | LooseEq
    | LooseNotEq
    | StrictEq
    | StrictNotEq -> True
    _ -> False
  }
}
