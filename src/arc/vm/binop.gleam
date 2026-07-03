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
/// Lives in its own module purely because Gleam requires constructor names to
/// be unique per module and `opcode.BinOpKind` already owns most of these.
pub type PureBinOp {
  // Arithmetic (Add is NOT here — see the module doc)
  Sub
  Mul
  Div
  Mod
  Exp
  // Bitwise
  BitAnd
  BitOr
  BitXor
  Shl
  Shr
  UShr
  // Comparison (== with coercion)
  Eq
  NotEq
  // Comparison (=== strict)
  StrictEq
  StrictNotEq
  // Relational (In / InstanceOf are NOT here — see the module doc)
  Lt
  LtEq
  Gt
  GtEq
}

/// The five arithmetic operators, as a type of their own so `bigint_arith`
/// (which handles exactly these) cannot be handed a `Shl`.
pub type ArithOp {
  ArithSub
  ArithMul
  ArithDiv
  ArithMod
  ArithExp
}

/// The six bitwise/shift operators — see `ArithOp`.
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

// `ops/operators.exec_binop` narrows a `PureBinOp` straight into one of the
// four families above with a single exhaustive `case` — adding a variant here
// is a compile error there, and the per-family helpers cannot be handed an
// operator they don't evaluate (`bigint_arith` cannot see a `Shl`).
