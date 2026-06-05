import gleam/int
import gleam/option.{type Option, None}

// ============================================================================
// Lexical bindings — `this` / active-function (super) / new.target
// ============================================================================
//
// ES2024 §9.1.1.3: a non-arrow FunctionEnvironmentRecord holds four value
// slots that GetThisEnvironment() / GetSuperBase() expose: [[ThisValue]],
// [[FunctionObject]], [[HomeObject]], [[NewTarget]]. Arrows inherit all four
// from the nearest enclosing non-arrow. Arc models them as a 4-keyed family
// of pseudo-locals captured through the ordinary closure path — exactly
// QuickJS's resolve_pseudo_var quartet. [[HomeObject]] is sourced from the
// function object at call-setup (FunctionObject.home_object) and cached here
// per-frame so super-reference reads are pure local/capture loads.

/// Which lexical pseudo-binding an IrGetLexical/DeclareLexical refers to.
pub type LexicalRef {
  RefThis
  RefActiveFunc
  RefHomeObject
  RefNewTarget
}

/// Canonical iteration order. Compiler, resolver and runtime MUST agree on
/// this so slot indices line up — see call.setup_locals.
pub const all_lexical_refs = [
  RefThis,
  RefActiveFunc,
  RefHomeObject,
  RefNewTarget,
]

/// Per-binding local-slot index for a body. None when the binding is neither
/// owned (non-arrow, allocated by DeclareLexical) nor inherited as a capture.
pub type LexicalSlots {
  LexicalSlots(
    this: Option(Int),
    active_func: Option(Int),
    home_object: Option(Int),
    new_target: Option(Int),
  )
}

pub const no_lexical_slots = LexicalSlots(None, None, None, None)

pub fn lexical_slot(slots: LexicalSlots, ref: LexicalRef) -> Option(Int) {
  case ref {
    RefThis -> slots.this
    RefActiveFunc -> slots.active_func
    RefHomeObject -> slots.home_object
    RefNewTarget -> slots.new_target
  }
}

/// Per-binding "is referenced in this body or a nested arrow" flags.
/// Mirrors JSC's InnerArrowFunctionCodeFeatures bitmask.
pub type LexicalRefs {
  LexicalRefs(
    this: Bool,
    active_func: Bool,
    home_object: Bool,
    new_target: Bool,
  )
}

pub const no_lexical_refs = LexicalRefs(False, False, False, False)

pub fn lexical_refs_or(a: LexicalRefs, b: LexicalRefs) -> LexicalRefs {
  LexicalRefs(
    this: a.this || b.this,
    active_func: a.active_func || b.active_func,
    home_object: a.home_object || b.home_object,
    new_target: a.new_target || b.new_target,
  )
}

pub fn lexical_refs_get(refs: LexicalRefs, ref: LexicalRef) -> Bool {
  case ref {
    RefThis -> refs.this
    RefActiveFunc -> refs.active_func
    RefHomeObject -> refs.home_object
    RefNewTarget -> refs.new_target
  }
}

/// Syntax-legality flags inherited by arrows and direct eval. Stored on
/// FuncTemplate so PerformEval (§19.2.1.1 steps 6-12) can SyntaxError on
/// `new.target` / `super` / `super()` / `arguments` correctly. Mirrors
/// QuickJS's new_target_allowed/super_allowed/super_call_allowed/
/// arguments_allowed bits on JSFunctionBytecode.
pub type SyntaxPerms {
  SyntaxPerms(
    new_target_allowed: Bool,
    super_prop_allowed: Bool,
    super_call_allowed: Bool,
    arguments_allowed: Bool,
  )
}

/// Top-level scripts/modules/indirect-eval: none of the function-only
/// syntax forms are legal. `arguments` resolves as an ordinary (likely
/// undefined) global, so it's "allowed" in the sense of not being a
/// SyntaxError.
pub const script_perms = SyntaxPerms(False, False, False, True)

/// Ordinary non-arrow function body: new.target and arguments allowed,
/// super forms not (no [[HomeObject]]).
pub const fn_perms = SyntaxPerms(True, False, False, True)

/// Class/object-literal method body: super.prop allowed (has [[HomeObject]]).
pub const method_perms = SyntaxPerms(True, True, False, True)

/// Derived-class constructor body: super.prop and super() allowed.
pub const derived_ctor_perms = SyntaxPerms(True, True, True, True)

/// Class instance-field initializer body (§15.7.14): runs as a synthetic
/// method-like function. new.target is allowed (evaluates to undefined since
/// the init fn is [[Call]]ed, not constructed); super.prop is allowed (init
/// fn has [[HomeObject]] = class prototype); super() and arguments are not.
pub const field_init_perms = SyntaxPerms(True, True, False, False)

// ============================================================================
// Precomputed property keys
// ============================================================================

/// Precomputed canonical property key, baked into bytecode at resolve time so
/// the interpreter never re-parses a compile-time-constant string per dispatch.
/// Mirrors value.PropertyKey — separate type only to avoid the value↔opcode
/// import cycle (value imports opcode for FuncTemplate). value.from_op_key
/// converts in O(1).
pub type OpKey {
  /// Canonical array index — `s` parsed to non-negative int and round-trips.
  OpIndex(Int)
  /// Any other string key.
  OpNamed(String)
}

/// Canonicalize a string key. Same algorithm as value.canonical_key —
/// CanonicalNumericIndexString (§7.1.21) + array-index range check.
/// Called once per opcode in resolve.gleam, never at runtime.
pub fn make_key(s: String) -> OpKey {
  case int.parse(s) {
    Ok(n) if n >= 0 ->
      case int.to_string(n) == s {
        True -> OpIndex(n)
        False -> OpNamed(s)
      }
    _ -> OpNamed(s)
  }
}

/// Render an OpKey back to its string form for error messages. Only called on
/// the cold throw path.
pub fn key_name(k: OpKey) -> String {
  case k {
    OpIndex(n) -> int.to_string(n)
    OpNamed(s) -> s
  }
}

// ============================================================================
// Final Bytecode — resolved, ready for VM execution
// ============================================================================

/// Native error constructor selector for `ThrowError` — mirrors the kind
/// argument to QuickJS OP_throw_error. Extend as needed.
pub type ErrorKind {
  ReferenceErrorKind
  TypeErrorKind
}

/// Resolved bytecode instruction. All variable references are numeric indices,
/// all jump targets are absolute PC addresses. The VM only sees these.
pub type Op {
  // -- Source mapping --
  /// Record the source line of the following instructions into
  /// `state.current_line`. Emitted by the compiler at the start of each
  /// statement whose line differs from the previous one. Used to build the
  /// line numbers in `Error.prototype.stack`. No stack effect; pc+1.
  SetLine(line: Int)

  // -- Literals + Stack --
  PushConst(index: Int)
  Pop
  Dup
  Swap

  // -- Variable Access (resolved) --
  GetLocal(index: Int)
  PutLocal(index: Int)
  /// §9.1.1.3.1 BindThisValue — write locals[index] only if currently
  /// JsUninitialized, else throw ReferenceError. Used for `super()` result.
  PutLocalCheckInit(index: Int)
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

  // -- Property Access --
  GetField(key: OpKey)
  GetField2(key: OpKey)
  PutField(key: OpKey)
  GetElem
  GetElem2
  PutElem
  DeleteField(key: OpKey)
  DeleteElem
  /// §7.3.31 PrivateGet. Stack: [obj, ..] → [val, ..]. Throws TypeError if
  /// obj is not a JsObject or if obj lacks the private brand (has_property
  /// returns False). Brand check uses proto-chain walk so private methods
  /// stored on the prototype pass — pragmatic, not spec-pure PrivateName.
  GetPrivateField(key: OpKey)
  /// Like GetPrivateField but keeps obj on stack: [obj, ..] → [val, obj, ..].
  /// Used for `obj.#m(args)` method calls (mirrors GetField2).
  GetPrivateField2(key: OpKey)
  /// §7.3.32 PrivateSet. Stack: [val, obj, ..] → [val, ..]. Throws TypeError
  /// if obj is not a JsObject or lacks the brand.
  PutPrivateField(key: OpKey)
  /// §13.10.1 `#x in obj`. Stack: [obj, ..] → [JsBool, ..]. Throws TypeError
  /// if obj is not a JsObject (step 2). Name is encoded in the opcode, not
  /// on the stack — unlike BinOp(In) which pops two operands.
  PrivateIn(key: OpKey)

  // -- Object/Array Construction --
  NewObject
  DefineField(key: OpKey)
  DefineFieldComputed
  DefineMethod(key: OpKey)
  DefineMethodComputed
  DefineAccessor(key: OpKey, kind: AccessorKind)
  DefineAccessorComputed(kind: AccessorKind)
  /// §15.4.4 MakeMethod — peek [fn, obj, ...], set fn.[[HomeObject]] = obj.
  /// Stack-neutral. Emitted for object-literal shorthand methods so super.x
  /// works; not for `{m: fn}` (no HomeObject per spec).
  MakeMethod
  /// Annex B §B.3.1 — `{__proto__: v}`. Stack: [val, obj, ...] → [obj, ...].
  /// If val is an Object or null, sets obj.[[Prototype]] = val; else no-op.
  SetProto
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
  /// `new ctor(args)` and `super(args)`. Stack:
  /// [arg_n, ..., arg_1, new_target, ctor, ..] → [instance, ..].
  /// §10.1.13 [[Construct]] takes (args, newTarget); for plain `new X()` the
  /// emitter Dups X so newTarget == ctor, for `super()` it pushes the lexical
  /// new.target slot. Mirrors QuickJS OP_call_constructor.
  CallConstructor(arity: Int)
  /// [args_array, callee] → [result]; this=undefined. Spread-call path.
  CallApply
  /// [args_array, callee, receiver] → [result]; this=receiver. Spread-method-call.
  CallMethodApply
  /// [args_array, new_target, ctor] → [new instance]. Spread-new path.
  CallConstructorApply
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
  /// §9.1.1.1.5 step 6 — assignment to an immutable (const) local binding.
  /// Always throws TypeError (const bindings are strict per §14.3.1.3
  /// CreateImmutableBinding(dn, true)). Emitted by scope.gleam in place of
  /// PutLocal/PutBoxed when IrScopePutVar resolves to a ConstBinding. Mirrors
  /// QuickJS OP_throw_error/JS_THROW_VAR_RO.
  ThrowConstAssign(name: String)
  /// Compile-time-determined runtime error — throw the given native error
  /// kind with a static message. Used for forms the spec defines as
  /// unconditional runtime throws (e.g. §13.5.1.2 `delete super.x`). Mirrors
  /// QuickJS OP_throw_error.
  ThrowError(kind: ErrorKind, msg: String)
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
  /// BindThisValue for boxed `this` (captured by arrow inside ctor).
  PutBoxedCheckInit(index: Int)

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

  // -- Class Inheritance / Super --
  /// Wire prototype chain for derived class: [parent, ctor] → [ctor]
  SetupDerivedClass
  /// Generic [[GetPrototypeOf]]. Stack: [obj, ..] → [proto|null, ..].
  /// Reads the object's [[Prototype]] slot — JsNull if none/null. Emitted as
  /// the second hop for `super.x` (home_object → proto) and for `super()`
  /// (active_func → parent ctor). Mirrors QuickJS OP_get_super.
  GetPrototypeOf
  /// `super.x` / `super[k]`. Stack: [key, base, this, ..] → [val, ..].
  /// OrdinaryGet on base with receiver = this. Mirrors QuickJS
  /// OP_get_super_value.
  GetSuperValue
  /// Read-under variant for `super.x op= v` / `++super[k]`. Stack:
  /// [key, base, this, ..] → [val, pk, base, this, ..]. ToPropertyKey is
  /// applied ONCE; the coerced key is left on stack as a primitive so the
  /// trailing PutSuperValue re-coerces side-effect-free. Mirrors QuickJS
  /// `to_propkey; dup3; get_super_value` but as one op (arc has no Dup3).
  GetSuperValue2
  /// `super.x = v` / `super[k] = v`. Stack: [val, key, base, this, ..] →
  /// [val, ..]. OrdinarySet on base with receiver = this. Mirrors QuickJS
  /// OP_put_super_value.
  PutSuperValue

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

  /// Create a rest-parameter array from the current call's args, taking those
  /// at index `from_index` and beyond. Reads state.call_args, allocates a plain
  /// Array, pushes ref onto stack.
  CreateRestArray(from_index: Int)

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
  /// Initialization of a let/const binding (not reassignment). Lowers like
  /// IrScopePutVar but is never const-checked. Mirrors QuickJS
  /// `OP_scope_put_var_init` vs `OP_scope_put_var`.
  IrScopeInitVar(name: String)
  IrScopeTypeofVar(name: String)
  IrScopeReboxVar(name: String)
  /// Read a lexical pseudo-binding (this/active_func/new.target). Phase 2
  /// lowers to GetLocal/GetBoxed against the slot allocated by
  /// `DeclareLexical(ref)` (owned) or the capture slot threaded in for
  /// arrows/direct-eval.
  IrGetLexical(ref: LexicalRef)
  /// Write the lexical-`this` slot (super() result). Only `this` is mutable.
  IrSetThis

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
  IrPutLocalCheckInit(index: Int)
  IrGetGlobal(name: String)
  IrPutGlobal(name: String)
  IrTypeofGlobal(name: String)
  IrGetEvalVar(name: String)
  IrPutEvalVar(name: String)
  IrDeclareEvalVar(name: String)
  IrTypeofEvalVar(name: String)

  // -- Everything else is the same as final Op --
  /// Lowers 1:1 to SetLine. See Op.SetLine.
  IrSetLine(line: Int)
  IrPushConst(index: Int)
  IrPop
  IrDup
  IrSwap
  IrGetField(name: String)
  IrGetField2(name: String)
  IrPutField(name: String)
  IrGetElem
  IrGetElem2
  IrPutElem
  IrDeleteField(name: String)
  IrDeleteElem
  IrGetPrivateField(name: String)
  IrGetPrivateField2(name: String)
  IrPutPrivateField(name: String)
  IrPrivateIn(name: String)
  IrNewObject
  IrDefineField(name: String)
  IrDefineFieldComputed
  IrDefineMethod(name: String)
  IrDefineMethodComputed
  IrDefineAccessor(name: String, kind: AccessorKind)
  IrDefineAccessorComputed(kind: AccessorKind)
  IrMakeMethod
  IrSetProto
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
  IrThrowConstAssign(name: String)
  IrThrowError(kind: ErrorKind, msg: String)
  IrPopTry
  IrMakeClosure(func_index: Int)
  IrBoxLocal(index: Int)
  IrGetBoxed(index: Int)
  IrPutBoxed(index: Int)
  IrPutBoxedCheckInit(index: Int)
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
  IrGetPrototypeOf
  IrGetSuperValue
  IrGetSuperValue2
  IrPutSuperValue
  IrInitialYield
  IrYield
  IrYieldStar
  IrAsyncYieldStarNext
  IrAsyncYieldStarResume(next_label: Int)
  IrAwait
  IrCreateArguments
  IrCreateRestArray(from_index: Int)
  IrNewRegExp

  // -- Global Environment Record --
  IrDeclareGlobalVar(name: String)
  IrDeclareGlobalLex(name: String, is_const: Bool)
  IrInitGlobalLex(name: String)
}
