import gleam/option.{type Option}
import lumen/vm/value.{type JsValue}

// ============================================================================
// Final Bytecode — resolved, ready for VM execution
// ============================================================================

/// Resolved bytecode instruction. All variable references are numeric indices,
/// all jump targets are absolute PC addresses. The VM only sees these.
pub type Op {
  // -- Literals + Stack --
  PushConst(index: Int)
  Pop
  Dup
  Swap
  Rot3

  // -- Variable Access (resolved) --
  GetLocal(index: Int)
  PutLocal(index: Int)
  GetEnvVar(index: Int)
  PutEnvVar(index: Int)
  GetGlobal(name: String)
  PutGlobal(name: String)
  GetThis

  // -- Property Access --
  GetField(name: String)
  GetField2(name: String)
  PutField(name: String)
  GetElem
  GetElem2
  PutElem
  DeleteField(name: String)
  DeleteElem

  // -- Object/Array Construction --
  NewObject
  DefineField(name: String)
  DefineFieldComputed
  DefineAccessor(name: String, kind: AccessorKind)
  ObjectSpread
  ArrayFrom(count: Int)

  // -- Calls --
  Call(arity: Int)
  CallMethod(name: String, arity: Int)
  CallConstructor(arity: Int)
  CallApply
  Return

  // -- Control Flow (absolute PC targets) --
  Jump(target: Int)
  JumpIfFalse(target: Int)
  JumpIfTrue(target: Int)
  JumpIfNullish(target: Int)

  // -- Exception Handling --
  Throw
  PushTry(catch_target: Int)
  PopTry
  EnterFinally
  LeaveFinally

  // -- Closures --
  MakeClosure(func_index: Int)
  CloseVar(index: Int)

  // -- Operators --
  BinOp(kind: BinOpKind)
  UnaryOp(kind: UnaryOpKind)
  TypeOf
  TypeofGlobal(name: String)

  // -- Iteration --
  ForInStart
  ForInNext
  GetIterator
  IteratorNext
  IteratorClose
}

pub type AccessorKind {
  Getter
  Setter
}

// ============================================================================
// Operator Kinds
// ============================================================================

pub type BinOpKind {
  // Arithmetic
  Add
  Sub
  Mul
  Div
  Mod
  Exp
  // Bitwise
  BitAnd
  BitOr
  BitXor
  ShiftLeft
  ShiftRight
  UShiftRight
  // Comparison (== with coercion)
  Eq
  NotEq
  // Comparison (=== strict)
  StrictEq
  StrictNotEq
  // Relational
  Lt
  LtEq
  Gt
  GtEq
  // Relational keywords
  In
  InstanceOf
}

pub type UnaryOpKind {
  Neg
  Pos
  BitNot
  LogicalNot
  Void
}

// ============================================================================
// IR Opcodes — symbolic, emitted by compiler Phase 1
// ============================================================================

/// Symbolic IR instruction. Variable references use names (resolved in Phase 2),
/// jump targets use label IDs (resolved in Phase 3).
pub type IrOp {
  // -- Scope-aware variable access (resolved in Phase 2) --
  IrScopeGetVar(name: String)
  IrScopePutVar(name: String)
  IrScopeTypeofVar(name: String)

  // -- Labels and jumps (resolved in Phase 3) --
  IrLabel(id: Int)
  IrJump(label: Int)
  IrJumpIfFalse(label: Int)
  IrJumpIfTrue(label: Int)
  IrJumpIfNullish(label: Int)
  IrPushTry(catch_label: Int, finally_label: Int)

  // -- Everything else is the same as final Op --
  IrPushConst(index: Int)
  IrPop
  IrDup
  IrSwap
  IrRot3
  IrGetThis
  IrGetField(name: String)
  IrGetField2(name: String)
  IrPutField(name: String)
  IrGetElem
  IrGetElem2
  IrPutElem
  IrDeleteField(name: String)
  IrDeleteElem
  IrNewObject
  IrDefineField(name: String)
  IrDefineFieldComputed
  IrDefineAccessor(name: String, kind: AccessorKind)
  IrObjectSpread
  IrArrayFrom(count: Int)
  IrCall(arity: Int)
  IrCallMethod(name: String, arity: Int)
  IrCallConstructor(arity: Int)
  IrCallApply
  IrReturn
  IrThrow
  IrPopTry
  IrEnterFinally
  IrLeaveFinally
  IrMakeClosure(func_index: Int)
  IrCloseVar(index: Int)
  IrBinOp(kind: BinOpKind)
  IrUnaryOp(kind: UnaryOpKind)
  IrTypeOf
  IrForInStart
  IrForInNext
  IrGetIterator
  IrIteratorNext
  IrIteratorClose
}

// ============================================================================
// Function Template — metadata per compiled function
// ============================================================================

/// Compiled function definition. Stored in the module's function table.
/// Referenced by MakeClosure(func_index).
pub type FuncTemplate {
  FuncTemplate(
    name: Option(String),
    arity: Int,
    local_count: Int,
    bytecode: List(Op),
    constants: List(JsValue),
    functions: List(FuncTemplate),
    env_descriptors: List(EnvCapture),
    is_strict: Bool,
    is_arrow: Bool,
  )
}

/// Describes how to capture one variable from the enclosing scope
/// when creating a closure.
pub type EnvCapture {
  /// Capture from parent's local frame at the given index.
  CaptureLocal(parent_index: Int)
  /// Capture from parent's EnvSlot at the given index (transitive).
  CaptureEnv(parent_env_index: Int)
}
