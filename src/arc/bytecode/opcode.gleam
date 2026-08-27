import arc/bytecode/binop.{type PureBinOp}
import arc/bytecode/key.{type PropertyKey}
import gleam/option.{type Option}

pub type TemplateQuasi {
  // cooked is none for an invalid escape
  TemplateQuasi(cooked: Option(String), raw: String)
}

pub type ErrorKind {
  ReferenceErrorKind
  TypeErrorKind
}

pub type LabelId {
  LabelId(id: Int)
}

pub type Pc {
  Pc(pc: Int)
}

pub fn pc_int(pc: Pc) -> Int {
  let Pc(n) = pc
  n
}

pub type TryKind(target) {
  CatchOnly
  Finally(fin_label: target)
  // return completion closes the iterator at stack_depth; sync only
  IterCloseGuard
}

pub type Op {
  PushConst(index: Int)
  Pop
  Dup
  Swap
  // [a, b, c] -> [c, a, b]
  Rot3
  // [a, b, c, d] -> [b, c, d, a]
  Unrot4

  GetLocal(index: Int)
  PutLocal(index: Int)
  // throws unless slot is uninitialized (super() result)
  PutLocalCheckInit(index: Int)
  GetGlobal(name: String)
  PutGlobal(name: String)
  // §9.1.1.4.7 sloppy delete identifier at global scope
  DeleteGlobalVar(name: String)
  // eval_env first, else global; sloppy direct-eval contexts
  GetEvalVar(name: String)
  PutEvalVar(name: String)
  DeclareEvalVar(name: String)
  TypeofEvalVar(name: String)

  ToObject
  // string-hint tostring, for template substitutions
  ToStringVal
  // §13.2.8.4 cached per site and parse id
  GetTemplateObject(site: Int, quasis: List(TemplateQuasi))
  // [obj] -> [val] and jump on hit, else pop obj
  WithGetVar(name: String, target: Pc)
  // like withgetvar but keeps obj: [obj] -> [val, obj]
  WithGetVarThis(name: String, target: Pc)
  // [obj, val] -> [] and jump on hit, else pop obj
  WithPutVar(name: String, target: Pc)
  // [obj] -> [bool] and jump on hit, else pop obj
  WithDeleteVar(name: String, target: Pc)
  // keeps obj and jumps if bound, else pop obj
  WithMakeRef(name: String, target: Pc)
  // [base] -> [val] and jump if object, else pop sentinel
  WithGetRefValue(name: String, target: Pc)
  // [base, val] -> [] and jump if object, else pop sentinel
  WithPutRefValue(name: String, target: Pc)

  GetField(key: PropertyKey)
  GetField2(key: PropertyKey)
  PutField(key: PropertyKey)
  GetElem
  GetElem2
  PutElem
  DeleteField(key: PropertyKey)
  DeleteElem
  // mints a fresh unique private key string per class evaluation
  NewPrivateName(name: String)
  // [key, obj] -> [val], own-only
  GetPrivateFieldDyn
  // [key, obj] -> [val, obj]
  GetPrivateFieldDyn2
  // [key, val, obj] -> [val]
  PutPrivateFieldDyn
  // [key, obj] -> [bool]
  PrivateInDyn
  // [val, key, obj] -> [obj]
  DefinePrivateField
  // [fn, key, obj] -> [obj]
  DefinePrivateMethod
  // [fn, key, obj] -> [obj]
  DefinePrivateAccessor(kind: AccessorKind)

  NewObject
  // keys stored last key first, the order values pop
  NewObjectWith(keys: List(PropertyKey), count: Int)
  DefineField(key: PropertyKey)
  DefineFieldComputed
  ToPropertyKey
  DefineMethod(key: PropertyKey)
  DefineMethodComputed
  DefineAccessor(key: PropertyKey, kind: AccessorKind, enumerable: Bool)
  DefineAccessorComputed(kind: AccessorKind, enumerable: Bool)
  // peek [fn, obj], sets fn home object; stack-neutral
  MakeMethod
  // annex b __proto__ literal: [val, obj] -> [obj]
  SetProto
  ObjectSpread
  // [src, key_n .. key_1] -> [rest_obj]
  ObjectRestCopy(excluded_count: Int)
  ArrayFrom(count: Int)
  // holes sorted ascending
  ArrayFromWithHoles(count: Int, holes: List(Int))
  // [val, arr] -> [arr]
  ArrayPush
  // [arr] -> [arr], length++ leaving a hole
  ArrayPushHole
  // [iterable, arr] -> [arr]
  ArraySpread

  Call(arity: Int)
  // direct eval if callee is intrinsic eval, else call
  CallEval(
    arity: Int,
    param_scope_names: List(String),
    with_names: List(String),
    private_names: List(String),
  )
  // [arg_n .. arg_1, receiver, fn] -> [result]
  CallMethod(arity: Int)
  // [arg_n .. arg_1, new_target, ctor] -> [instance]
  CallConstructor(arity: Int)
  // [arg_n .. arg_1, ctor] -> [instance]
  CallNew(arity: Int)
  // [args_array, callee] -> [result]
  CallApply
  // [args_array, callee, receiver] -> [result]
  CallMethodApply
  // lazy f.apply(t, arguments): [t, apply_fn, f] -> [result]
  ApplyArguments(slot: Int, simple_params: Bool)
  // [args_array, new_target, ctor] -> [instance]
  CallConstructorApply
  Return

  Jump(target: Pc)
  JumpIfFalse(target: Pc)
  JumpIfTrue(target: Pc)
  JumpIfNullish(target: Pc)
  JumpIfNotNullish(target: Pc)
  // push pc+1 as return address, jump (finally entry)
  Gosub(target: Pc)
  Ret

  Throw
  // assignment to const, always typeerror
  ThrowConstAssign(name: String)
  ThrowError(kind: ErrorKind, msg: String)
  PushTry(catch_target: Pc, kind: TryKind(Pc))
  PopTry

  MakeClosure(func_index: Int)
  BoxLocal(index: Int)
  GetBoxed(index: Int)
  PutBoxed(index: Int)
  // bindthisvalue for boxed this
  PutBoxedCheckInit(index: Int)

  // build with bin_op, never classify at execution time
  BinOp(kind: Classified)
  UnaryOp(kind: UnaryOpKind)
  TypeOf
  TypeofGlobal(name: String)

  // statement-position i++ on a plain local
  IncLocal(index: Int)
  DecLocal(index: Int)
  // jumps when local truthiness equals when
  JumpIfLocal(index: Int, target: Pc, when: Bool)
  IncLocalJump(index: Int, target: Pc)
  IncLocalCmpConstJump(
    index: Int,
    const_index: Int,
    kind: PureBinOp,
    target: Pc,
    when: Bool,
  )
  IncLocalCmpLocalJump(
    index: Int,
    right: Int,
    kind: PureBinOp,
    target: Pc,
    when: Bool,
  )
  CmpLocalLocalJump(
    left: Int,
    right: Int,
    kind: PureBinOp,
    target: Pc,
    when: Bool,
  )
  CmpLocalConstJump(
    left: Int,
    const_index: Int,
    kind: PureBinOp,
    target: Pc,
    when: Bool,
  )
  // [right, left] -> []
  CmpJump(kind: PureBinOp, target: Pc, when: Bool)
  // [left] -> []
  CmpConstJump(const_index: Int, kind: PureBinOp, target: Pc, when: Bool)
  GetLocalField(index: Int, key: PropertyKey)
  // [] -> [val, obj]
  GetLocalField2(index: Int, key: PropertyKey)
  // [obj] -> [result]
  GetFieldCall(key: PropertyKey)
  // method read before the arg tdz check
  GetFieldCall1(key: PropertyKey, arg: Int)
  GetLocalFieldCall(index: Int, key: PropertyKey)
  // [val, obj] -> []
  PutFieldPop(key: PropertyKey)
  PutLocalLocalField(obj: Int, value: Int, key: PropertyKey)
  PutLocalConstField(obj: Int, const_index: Int, key: PropertyKey)
  BinOpConst(kind: Classified, const_index: Int)
  BinOpLocal(kind: Classified, index: Int)
  BinOpLocalLocal(kind: Classified, left: Int, right: Int)
  BinOpLocalConst(kind: Classified, left: Int, const_index: Int)
  // [] -> [tonumber(old)], local becomes old + 1
  PostIncLocal(index: Int)
  PostDecLocal(index: Int)
  // [val, key, obj] -> []
  PutElemPop
  GetElemLocals(obj: Int, key: Int)
  BinOpLocalField(kind: Classified, index: Int, key: PropertyKey)
  // [right, left] -> []
  BinOpPut(kind: Classified, dst: Int)
  BinOpConstPut(kind: Classified, const_index: Int, dst: Int)
  BinOpLocalPut(kind: Classified, index: Int, dst: Int)
  BinOpLocalLocalPut(kind: Classified, left: Int, right: Int, dst: Int)

  ForInStart
  ForInNext
  GetIterator
  GetAsyncIterator
  // caches next method for async yield*
  IteratorRecord
  IteratorNext
  // §7.4.11 normal close: [iter] -> []
  IteratorClose
  // §7.4.11 throw close: [thrown, iter], rethrows thrown
  IteratorCloseThrow
  // typeerror if top is not an object, stack unchanged
  IteratorCheckObject
  // [iter] -> [arr], drains without re-getting iterator
  IteratorRest

  // [parent, ctor] -> [ctor]
  SetupDerivedClass
  GetPrototypeOf
  // [key, base, this] -> [val]
  GetSuperValue
  // [key, base, this] -> [val, pk, base, this], key coerced once
  GetSuperValue2
  // [val, key, base, this] -> [val]
  PutSuperValue

  InitialYield
  Yield
  // [arg, iter]; loops on itself until inner is done
  YieldStar
  // after_pc is the op after the whole yield* sequence
  AsyncYieldStarNext(after_pc: Pc)
  // next_pc points back at the asyncyieldstarnext op
  AsyncYieldStarResume(next_pc: Pc)

  Await

  // simple_params false gives an unmapped arguments object
  CreateArguments(simple_params: Bool)

  CreateRestArray(from_index: Int)

  NewRegExp

  DynamicImport
  DynamicImportSource
  // todo: deferral not implemented, behaves as import()
  DynamicImportDefer

  // §9.1.1.4.17, deletable becomes configurable
  DeclareGlobalVar(name: String, deletable: Bool)
  // typeerror if candeclareglobalfunction rejects
  DeclareGlobalFn(name: String, deletable: Bool)
  DeclareGlobalLex(name: String, is_const: Bool)
  InitGlobalLex(name: String)

  // [resource] -> [disposer fn or undefined]
  GetDisposer(is_async: Bool)
  // [suppressed, error] -> [suppressederror]
  MakeSuppressed
}

pub type AccessorKind {
  Getter
  Setter
}

pub type BinOpKind {
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
  UShiftRight
  Eq
  NotEq
  StrictEq
  StrictNotEq
  Lt
  LtEq
  Gt
  GtEq
  In
  InstanceOf
}

// classified once at resolve time, not per execution
pub type Classified {
  PureOp(op: PureBinOp)
  AddOp
  InOp
  InstanceOfOp
}

pub fn classify(kind: BinOpKind) -> Classified {
  case kind {
    Add -> AddOp
    In -> InOp
    InstanceOf -> InstanceOfOp
    Sub -> PureOp(binop.Arith(binop.ArithSub))
    Mul -> PureOp(binop.Arith(binop.ArithMul))
    Div -> PureOp(binop.Arith(binop.ArithDiv))
    Mod -> PureOp(binop.Arith(binop.ArithMod))
    Exp -> PureOp(binop.Arith(binop.ArithExp))
    BitAnd -> PureOp(binop.Bitwise(binop.AndOp))
    BitOr -> PureOp(binop.Bitwise(binop.OrOp))
    BitXor -> PureOp(binop.Bitwise(binop.XorOp))
    ShiftLeft -> PureOp(binop.Bitwise(binop.ShlOp))
    ShiftRight -> PureOp(binop.Bitwise(binop.ShrOp))
    UShiftRight -> PureOp(binop.Bitwise(binop.UShrOp))
    Eq -> PureOp(binop.Equality(binop.EqOp))
    NotEq -> PureOp(binop.Equality(binop.NotEqOp))
    StrictEq -> PureOp(binop.Equality(binop.StrictEqOp))
    StrictNotEq -> PureOp(binop.Equality(binop.StrictNotEqOp))
    Lt -> PureOp(binop.Compare(binop.LtCmp))
    LtEq -> PureOp(binop.Compare(binop.LtEqCmp))
    Gt -> PureOp(binop.Compare(binop.GtCmp))
    GtEq -> PureOp(binop.Compare(binop.GtEqCmp))
  }
}

pub fn bin_op(kind: BinOpKind) -> Op {
  BinOp(classify(kind))
}

pub type UnaryOpKind {
  Neg
  Pos
  BitNot
  LogicalNot
  Void
}

// final ops wrapped in irfinal; only label/key/binop stay symbolic
pub type IrOp {
  IrFinal(op: Op)

  // no pc slot; resolve records it in functemplate.lines
  IrLine(line: Int)

  // no pc slot
  IrLabel(id: LabelId)
  IrJump(label: LabelId)
  IrJumpIfFalse(label: LabelId)
  IrJumpIfTrue(label: LabelId)
  IrJumpIfNullish(label: LabelId)
  IrJumpIfNotNullish(label: LabelId)
  IrPushTry(catch_label: LabelId, kind: TryKind(LabelId))
  IrGosub(label: LabelId)
  IrAsyncYieldStarNext(after_label: LabelId)
  IrAsyncYieldStarResume(next_label: LabelId)

  IrWithGetVar(name: String, label: LabelId)
  IrWithGetVarThis(name: String, label: LabelId)
  IrWithPutVar(name: String, label: LabelId)
  IrWithDeleteVar(name: String, label: LabelId)
  IrWithMakeRef(name: String, label: LabelId)
  IrWithGetRefValue(name: String, label: LabelId)
  IrWithPutRefValue(name: String, label: LabelId)

  IrGetField(name: String)
  IrGetField2(name: String)
  IrPutField(name: String)
  IrDeleteField(name: String)
  IrDefineField(name: String)
  IrDefineMethod(name: String)
  IrDefineAccessor(name: String, kind: AccessorKind, enumerable: Bool)

  IrBinOp(kind: BinOpKind)

  IrCmpLocalLocalJump(
    left: Int,
    right: Int,
    kind: PureBinOp,
    label: LabelId,
    when: Bool,
  )
  IrCmpLocalConstJump(
    left: Int,
    const_index: Int,
    kind: PureBinOp,
    label: LabelId,
    when: Bool,
  )
  IrCmpJump(kind: PureBinOp, label: LabelId, when: Bool)
  IrCmpConstJump(const_index: Int, kind: PureBinOp, label: LabelId, when: Bool)
  IrIncLocalJump(index: Int, label: LabelId)
  IrJumpIfLocal(index: Int, label: LabelId, when: Bool)
  IrIncLocalCmpConstJump(
    index: Int,
    const_index: Int,
    kind: PureBinOp,
    label: LabelId,
    when: Bool,
  )
  IrIncLocalCmpLocalJump(
    index: Int,
    right: Int,
    kind: PureBinOp,
    label: LabelId,
    when: Bool,
  )
}
