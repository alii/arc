/// The binary operators that are PURE — total functions of two already-
/// primitive JsValues, needing no heap, no user code, and no `State`.
///
/// This is deliberately NOT `opcode.BinOpKind`. Three of that type's variants
/// (`Add`, `In`, `InstanceOf`) are handled by the interpreter itself: `Add`
/// needs ToPrimitive-then-string-or-numeric dispatch, `In`/`InstanceOf` need
/// heap access and can run user code. `ops/operators.exec_binop` structurally
/// cannot evaluate them, and used to say so with `panic`s guarded only by
/// `_ ->` catch-alls at each call site.
///
/// Taking `PureBinOp` instead makes `exec_binop(Add, ..)` a compile error, and
/// `opcode.classify` the single total place where a `BinOpKind` narrows to one.
///
/// It also lets the fused compare-and-branch superinstructions
/// (`opcode.CmpLocalLocalJump` / `CmpLocalConstJump`) declare that they can
/// only ever carry a pure operator, which is what the resolver's peephole
/// already guarantees.
///
/// The four constructors are the four evaluation FAMILIES: `exec_binop`
/// dispatches on the constructor and hands the payload straight to the family
/// evaluator, so "which family runs this op" is answered by the type — there
/// is no second enum to keep in sync, and no re-derivation to get wrong.
///
/// Lives in its own module purely because Gleam requires constructor names to
/// be unique per module and `opcode.BinOpKind` already owns most of these.
pub type PureBinOp {
  Arith(ArithOp)
  Bitwise(BitwiseOp)
  Compare(CompareOp)
  Equality(EqualityOp)
}

/// The five arithmetic operators (Add is NOT here — see the module doc).
pub type ArithOp {
  ArithSub
  ArithMul
  ArithDiv
  ArithMod
  ArithExp
}

/// The six bitwise/shift operators.
pub type BitwiseOp {
  AndOp
  OrOp
  XorOp
  ShlOp
  ShrOp
  UShrOp
}

/// The four relational operators, all of which reduce to a `<`/`=`/`>`
/// ordering (or "unordered", for NaN).
pub type CompareOp {
  LtCmp
  LtEqCmp
  GtCmp
  GtEqCmp
}

/// The four equality operators.
pub type EqualityOp {
  EqOp
  NotEqOp
  StrictEqOp
  StrictNotEqOp
}
