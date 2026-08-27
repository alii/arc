// pure ops only; add, in, instanceof need the heap
pub type PureBinOp {
  Arith(ArithOp)
  Bitwise(BitwiseOp)
  Compare(CompareOp)
  Equality(EqualityOp)
}

pub type ArithOp {
  ArithSub
  ArithMul
  ArithDiv
  ArithMod
  ArithExp
}

pub type BitwiseOp {
  AndOp
  OrOp
  XorOp
  ShlOp
  ShrOp
  UShrOp
}

pub type CompareOp {
  LtCmp
  LtEqCmp
  GtCmp
  GtEqCmp
}

pub type EqualityOp {
  EqOp
  NotEqOp
  StrictEqOp
  StrictNotEqOp
}
