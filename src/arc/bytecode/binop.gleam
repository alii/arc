// pure ops only; add, in, instanceof need the heap
pub type PureBinOp {
  Arith(ArithOp)
  Bitwise(BitwiseOp)
  Compare(CompareOp)
  Equality(EqualityOp)
}

pub type ArithOp {
  Sub
  Mul
  Div
  Mod
  Exp
}

pub type BitwiseOp {
  BitAnd
  BitOr
  BitXor
  ShiftLeft
  ShiftRight
  ShiftRightUnsigned
}

pub type CompareOp {
  Less
  LessEq
  Greater
  GreaterEq
}

pub type EqualityOp {
  LooseEq
  LooseNotEq
  StrictEq
  StrictNotEq
}

// classified once at emit time, not per execution
pub type ClassifiedBinOp {
  PureOp(op: PureBinOp)
  AddOp
  InOp
  InstanceOfOp
}
