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

/// A `PureBinOp` split by WHICH evaluator handles it. The split is exactly the
/// one `ops/operators.exec_binop` used to draw by hand with `Sub | Mul | ...`
/// group patterns and then re-dispatch on inside each group — the group arms
/// forced the per-group helpers to accept the whole 20-variant `PureBinOp` and
/// carry an unreachable `_ ->` arm that fabricated a user-visible error.
pub type BinOpClass {
  Arith(op: ArithOp)
  Bitwise(op: BitwiseOp)
  Compare(op: CompareOp)
  Equality(op: EqualityOp)
}

/// Total classification of a pure binary operator. Adding a `PureBinOp`
/// variant is a compile error here, and only here.
pub fn classify(op: PureBinOp) -> BinOpClass {
  case op {
    Sub -> Arith(ArithSub)
    Mul -> Arith(ArithMul)
    Div -> Arith(ArithDiv)
    Mod -> Arith(ArithMod)
    Exp -> Arith(ArithExp)
    BitAnd -> Bitwise(AndOp)
    BitOr -> Bitwise(OrOp)
    BitXor -> Bitwise(XorOp)
    Shl -> Bitwise(ShlOp)
    Shr -> Bitwise(ShrOp)
    UShr -> Bitwise(UShrOp)
    Eq -> Equality(EqOp)
    NotEq -> Equality(NotEqOp)
    StrictEq -> Equality(StrictEqOp)
    StrictNotEq -> Equality(StrictNotEqOp)
    Lt -> Compare(LtCmp)
    LtEq -> Compare(LtEqCmp)
    Gt -> Compare(GtCmp)
    GtEq -> Compare(GtEqCmp)
  }
}
