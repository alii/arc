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
  DefineMethod(name: String)
  DefineAccessor(name: String, kind: AccessorKind)
  DefineAccessorComputed(kind: AccessorKind)
  ObjectSpread
  ArrayFrom(count: Int)
  /// Pop `count - length(holes)` values, build a sparse array of length `count`
  /// with the hole indices left empty. Used for array literals with elisions
  /// (e.g. `[1,,3]` → ArrayFromWithHoles(3, [1])). Hole indices must be sorted
  /// ascending. Falls back to ArrayFrom when holes is empty — emitter decides.
  ArrayFromWithHoles(count: Int, holes: List(Int))
  /// [val, arr] → [arr]; sets arr[arr.length] = val, increments length.
  /// Used for non-spread elements after the first spread in an array literal.
  ArrayPush
  /// [arr] → [arr]; increments length WITHOUT setting any element (creates a
  /// hole at the previous length). Used for elisions after the first spread
  /// in an array literal (e.g. the hole in `[1, ...x, , 3]`).
  ArrayPushHole
  /// [iterable, arr] → [arr]; iterates iterable, appends each to arr.
  /// Used for spread elements in array literals and argument lists.
  /// Throws TypeError if iterable is not iterable (unlike ObjectSpread).
  ArraySpread

  // -- Calls --
  Call(arity: Int)
  CallMethod(name: String, arity: Int)
  CallConstructor(arity: Int)
  /// [args_array, callee] → [result]; this=undefined. Spread-call path.
  CallApply
  /// [args_array, callee, receiver] → [result]; this=receiver. Spread-method-call.
  CallMethodApply
  /// [args_array, ctor] → [new instance]. Spread-new path.
  CallConstructorApply
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
  EnterFinallyThrow
  LeaveFinally

  // -- Closures --
  MakeClosure(func_index: Int)
  CloseVar(index: Int)
  /// Wrap locals[index] value into a BoxSlot on the heap, replace local with ref.
  BoxLocal(index: Int)
  /// Read locals[index] (a box ref), dereference BoxSlot, push value on stack.
  GetBoxed(index: Int)
  /// Pop value from stack, read locals[index] (a box ref), write value into BoxSlot.
  PutBoxed(index: Int)

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

  // -- Class Inheritance --
  /// Wire prototype chain for derived class: [parent, ctor] → [ctor]
  SetupDerivedClass
  /// Call super constructor: [arg_n, ..., arg_1] → [new_obj]
  CallSuper(arity: Int)

  // -- Generator --
  /// Emitted at start of generator body. Suspends immediately (SuspendedStart).
  InitialYield
  /// Pop value from stack, suspend generator. On resume, .next(arg) pushed.
  Yield

  // -- Async --
  /// Pop value from stack, wrap in Promise.resolve, suspend async function.
  /// On resume, resolved value is pushed onto stack.
  Await

  // -- Arguments object --
  /// Create an arguments object from the current call's original args.
  /// Reads state.call_args, allocates ArgumentsObject, pushes ref onto stack.
  CreateArguments

  // -- RegExp --
  /// Pop flags string, pop pattern string -> push new RegExp object.
  NewRegExp

  // -- Global Environment Record --
  /// §9.1.1.4.17: Create writable/enumerable/configurable property on globalThis (if not already there).
  DeclareGlobalVar(name: String)
  /// Create entry in lexical_globals (with JsUninitialized for TDZ).
  DeclareGlobalLex(name: String, is_const: Bool)
  /// Pop value from stack, initialize lexical binding (TDZ → value).
  InitGlobalLex(name: String)
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

  // -- Resolved variable access (emitted by Phase 2) --
  IrGetLocal(index: Int)
  IrPutLocal(index: Int)
  IrGetGlobal(name: String)
  IrPutGlobal(name: String)
  IrTypeofGlobal(name: String)

  // -- Everything else is the same as final Op --
  IrPushConst(index: Int)
  IrPop
  IrDup
  IrSwap
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
  IrDefineMethod(name: String)
  IrDefineAccessor(name: String, kind: AccessorKind)
  IrDefineAccessorComputed(kind: AccessorKind)
  IrObjectSpread
  IrArrayFrom(count: Int)
  IrArrayFromWithHoles(count: Int, holes: List(Int))
  IrArrayPush
  IrArrayPushHole
  IrArraySpread
  IrCall(arity: Int)
  IrCallMethod(name: String, arity: Int)
  IrCallConstructor(arity: Int)
  IrCallApply
  IrCallMethodApply
  IrCallConstructorApply
  IrReturn
  IrThrow
  IrPopTry
  IrEnterFinally
  IrEnterFinallyThrow
  IrLeaveFinally
  IrMakeClosure(func_index: Int)
  IrCloseVar(index: Int)
  IrBoxLocal(index: Int)
  IrGetBoxed(index: Int)
  IrPutBoxed(index: Int)
  IrBinOp(kind: BinOpKind)
  IrUnaryOp(kind: UnaryOpKind)
  IrTypeOf
  IrForInStart
  IrForInNext
  IrGetIterator
  IrIteratorNext
  IrIteratorClose
  IrSetupDerivedClass
  IrCallSuper(arity: Int)
  IrInitialYield
  IrYield
  IrAwait
  IrCreateArguments
  IrNewRegExp

  // -- Global Environment Record --
  IrDeclareGlobalVar(name: String)
  IrDeclareGlobalLex(name: String, is_const: Bool)
  IrInitGlobalLex(name: String)
}
