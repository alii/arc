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
  GetGlobal(name: String)
  PutGlobal(name: String)
  /// Check state.eval_env dict for `name`; if present push its value, else
  /// fall through to GetGlobal semantics. Emitted in sloppy functions that
  /// contain a direct eval call — lets eval-created vars be read by name.
  GetEvalVar(name: String)
  /// Write to state.eval_env dict if `name` already exists there, else fall
  /// through to PutGlobal semantics. Emitted in sloppy direct-eval contexts.
  PutEvalVar(name: String)
  /// Seed `name` = undefined into state.eval_env dict (create key if absent).
  /// Emitted for `var` declarations at the top level of sloppy direct-eval
  /// code, in place of DeclareGlobalVar.
  DeclareEvalVar(name: String)
  /// `typeof name` — check eval_env first, fall through to TypeofGlobal.
  TypeofEvalVar(name: String)
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
  DefineMethodComputed
  DefineAccessor(name: String, kind: AccessorKind)
  DefineAccessorComputed(kind: AccessorKind)
  ObjectSpread
  /// Stack: [src, key_n, ..., key_1, ...] → [rest_obj, ...]
  /// Allocates a fresh object and CopyDataProperties(rest_obj, src, excluded)
  /// where excluded is the `excluded_count` keys popped beneath src. Throws
  /// TypeError if src is null/undefined (RequireObjectCoercible). Used for
  /// destructuring rest pattern `{a, b, ...rest}`.
  ObjectRestCopy(excluded_count: Int)
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
  /// Like Call but emitted for a syntactic `eval(...)` (identifier callee
  /// named "eval"). At runtime, if the callee resolves to the intrinsic eval
  /// function, performs a DIRECT eval (sees caller's local scope via boxed
  /// locals + FuncTemplate.local_names). Otherwise behaves identically to Call.
  CallEval(arity: Int)
  CallMethod(name: String, arity: Int)
  CallConstructor(arity: Int)
  /// [args_array, callee] → [result]; this=undefined. Spread-call path.
  CallApply
  /// [args_array, callee, receiver] → [result]; this=receiver. Spread-method-call.
  CallMethodApply
  /// [args_array, ctor] → [new instance]. Spread-new path.
  CallConstructorApply
  /// [args_array] → [this]. Spread-super path; same semantics as CallSuper
  /// but reads its arg list from a runtime array (built by IrArraySpread).
  CallSuperApply
  Return

  // -- Control Flow (absolute PC targets) --
  Jump(target: Int)
  JumpIfFalse(target: Int)
  JumpIfTrue(target: Int)
  JumpIfNullish(target: Int)
  /// Push (pc+1) onto operand stack as return address, jump to target.
  /// QuickJS OP_gosub — used to enter the finally block as a subroutine.
  Gosub(target: Int)
  /// Pop return address from operand stack, jump to it. QuickJS OP_ret —
  /// used to return from the finally block to the caller's continuation.
  Ret

  // -- Exception Handling --
  Throw
  PushTry(catch_target: Int)
  PopTry

  // -- Closures --
  MakeClosure(func_index: Int)
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
  GetAsyncIterator
  IteratorNext
  /// §7.4.11 normal-completion close. Stack: [iter, ..] → [..]. Get .return;
  /// undef/null → no-op; call it; throw → propagate; non-object → TypeError.
  IteratorClose
  /// §7.4.11 throw-completion close. Stack: [thrown, iter, ..] → rethrows
  /// thrown. Get .return; call it swallowing any error; rethrow original.
  /// Reached as a PushTry catch_target when a for-of/destructuring body throws.
  IteratorCloseThrow
  /// Stack: [v, ..] → [v, ..]. Throws TypeError if v is not a JsObject.
  /// Used after Await in AsyncIteratorClose normal path (§7.4.12 step 6) and
  /// after Await(next()) in for-await-of (§14.7.5.6 step 6.c).
  IteratorCheckObject
  /// §13.15.5.3 / §14.3.3 rest element. Stack: [iter, ..] → [arr, ..]. Drains
  /// iter via .next() into a fresh Array — does NOT re-GetIterator. Emitter
  /// pops the close-guard try frame first so .next() throw skips close.
  IteratorRest

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
  /// Self-looping delegate yield. Stack: [arg, iter, ..]. Calls iter.next(arg).
  /// If done → pops both, pushes result.value, pc+1. If !done → yields
  /// result.value, leaves [iter] on stack, pc stays (re-executes on resume
  /// with [resume_val, iter]).
  YieldStar
  /// Async-generator yield* — phase 1/2. Stack: [arg, iter, ..]. Calls
  /// iter.next(arg), pops arg, pushes the (possibly thenable) result →
  /// [result, iter, ..], pc+1. Always followed by Await then
  /// AsyncYieldStarResume; Await suspends and resume pushes the settled
  /// {value,done} object back → [result_obj, iter, ..].
  /// Ok-returning (not Awaited) so job_queue mutations from the inner
  /// .next() call (e.g. AsyncFromSyncIterator's unwrap microtask) thread
  /// through to the Await step. ES §15.5.5 step 8.a.i-ii.
  AsyncYieldStarNext
  /// Async-generator yield* — phase 2/2. Stack: [result_obj, iter, ..].
  /// IteratorComplete(result_obj): if done → pop both, push value, pc+1.
  /// If !done → Yielded(value); execute_inner's Yielded arm pops result_obj,
  /// keeps iter, sets pc to `next_pc` (the AsyncYieldStarNext op) so
  /// .next(v) resumes with [v, iter, ..]. ES §15.5.5 step 8.a.iv-vii.
  AsyncYieldStarResume(next_pc: Int)

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
  IrScopeReboxVar(name: String)

  // -- Labels and jumps (resolved in Phase 3) --
  IrLabel(id: Int)
  IrJump(label: Int)
  IrJumpIfFalse(label: Int)
  IrJumpIfTrue(label: Int)
  IrJumpIfNullish(label: Int)
  IrPushTry(catch_label: Int)
  IrGosub(label: Int)
  IrRet

  // -- Resolved variable access (emitted by Phase 2) --
  IrGetLocal(index: Int)
  IrPutLocal(index: Int)
  IrGetGlobal(name: String)
  IrPutGlobal(name: String)
  IrTypeofGlobal(name: String)
  IrGetEvalVar(name: String)
  IrPutEvalVar(name: String)
  IrDeclareEvalVar(name: String)
  IrTypeofEvalVar(name: String)

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
  IrDefineMethodComputed
  IrDefineAccessor(name: String, kind: AccessorKind)
  IrDefineAccessorComputed(kind: AccessorKind)
  IrObjectSpread
  IrObjectRestCopy(excluded_count: Int)
  IrArrayFrom(count: Int)
  IrArrayFromWithHoles(count: Int, holes: List(Int))
  IrArrayPush
  IrArrayPushHole
  IrArraySpread
  IrCall(arity: Int)
  IrCallEval(arity: Int)
  IrCallMethod(name: String, arity: Int)
  IrCallConstructor(arity: Int)
  IrCallApply
  IrCallMethodApply
  IrCallConstructorApply
  IrReturn
  IrThrow
  IrPopTry
  IrMakeClosure(func_index: Int)
  IrBoxLocal(index: Int)
  IrGetBoxed(index: Int)
  IrPutBoxed(index: Int)
  IrBinOp(kind: BinOpKind)
  IrUnaryOp(kind: UnaryOpKind)
  IrTypeOf
  IrForInStart
  IrForInNext
  IrGetIterator
  IrGetAsyncIterator
  IrIteratorNext
  IrIteratorClose
  IrIteratorCloseThrow
  IrIteratorCheckObject
  IrIteratorRest
  IrSetupDerivedClass
  IrCallSuper(arity: Int)
  IrCallSuperApply
  IrInitialYield
  IrYield
  IrYieldStar
  IrAsyncYieldStarNext
  IrAsyncYieldStarResume(next_label: Int)
  IrAwait
  IrCreateArguments
  IrNewRegExp

  // -- Global Environment Record --
  IrDeclareGlobalVar(name: String)
  IrDeclareGlobalLex(name: String, is_const: Bool)
  IrInitGlobalLex(name: String)
}
