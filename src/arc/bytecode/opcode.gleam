import arc/bytecode/binop.{type PureBinOp}
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
  GetGlobal(key: Int)
  PutGlobal(key: Int)
  // §9.1.1.4.7 sloppy delete identifier at global scope
  DeleteGlobalVar(key: Int)
  // eval_env first, else global; sloppy direct-eval contexts
  GetEvalVar(key: Int)
  PutEvalVar(key: Int)
  DeclareEvalVar(key: Int)
  TypeofEvalVar(key: Int)

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

  // key operands are slots in functemplate.keys
  GetField(key: Int)
  GetField2(key: Int)
  PutField(key: Int)
  GetElem
  GetElem2
  PutElem
  DeleteField(key: Int)
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
  // key slots stored last key first, the order values pop
  NewObjectWith(keys: List(Int), count: Int)
  DefineField(key: Int)
  DefineFieldComputed
  ToPropertyKey
  DefineMethod(key: Int)
  DefineMethodComputed
  DefineAccessor(key: Int, kind: AccessorKind, enumerable: Bool)
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
  TypeofGlobal(key: Int)

  // statement-position i++ on a plain local
  IncLocal(index: Int)
  DecLocal(index: Int)
  // jumps when local truthiness equals when
  JumpIfLocal(index: Int, target: Pc, when: Bool)
  IncLocalJump(index: Int, target: Pc)
  // by is the step, 1 or -1
  IncLocalCmpConstJump(
    index: Int,
    by: Int,
    const_index: Int,
    kind: PureBinOp,
    target: Pc,
    when: Bool,
  )
  IncLocalCmpLocalJump(
    index: Int,
    by: Int,
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
  GetLocalField(index: Int, key: Int)
  // [] -> [val, obj]
  GetLocalField2(index: Int, key: Int)
  // [obj] -> [result]
  GetFieldCall(key: Int)
  // method read before the arg tdz check
  GetFieldCall1(key: Int, arg: Int)
  GetLocalFieldCall(index: Int, key: Int)
  // [val, obj] -> []
  PutFieldPop(key: Int)
  PutLocalLocalField(obj: Int, value: Int, key: Int)
  PutLocalConstField(obj: Int, const_index: Int, key: Int)
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
  // obj[key++] with both plain locals
  GetElemPostInc(obj: Int, key: Int)
  BinOpLocalField(kind: Classified, index: Int, key: Int)
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
  DeclareGlobalVar(key: Int, deletable: Bool)
  // typeerror if candeclareglobalfunction rejects
  DeclareGlobalFn(key: Int, deletable: Bool)
  DeclareGlobalLex(key: Int, is_const: Bool)
  InitGlobalLex(key: Int)

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

// final ops wrapped in irfinal; only label/binop stay symbolic
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
    by: Int,
    const_index: Int,
    kind: PureBinOp,
    label: LabelId,
    when: Bool,
  )
  IrIncLocalCmpLocalJump(
    index: Int,
    by: Int,
    right: Int,
    kind: PureBinOp,
    label: LabelId,
    when: Bool,
  )
}

// local slots an op touches, paired with whether it writes them
pub fn slot_uses(op: Op) -> List(#(Int, Bool)) {
  case op {
    GetLocal(i)
    | JumpIfLocal(i, _, _)
    | GetLocalField(i, _)
    | GetLocalField2(i, _)
    | GetFieldCall1(_, i)
    | GetLocalFieldCall(i, _)
    | PutLocalConstField(i, _, _)
    | BinOpLocal(_, i)
    | BinOpLocalConst(_, i, _)
    | BinOpLocalField(_, i, _)
    | CmpLocalConstJump(i, _, _, _, _) -> [#(i, False)]
    PutLocal(i) -> [#(i, True)]
    IncLocal(i)
    | DecLocal(i)
    | IncLocalJump(i, _)
    | PostIncLocal(i)
    | PostDecLocal(i)
    | IncLocalCmpConstJump(i, _, _, _, _, _) -> [#(i, False), #(i, True)]
    IncLocalCmpLocalJump(i, _, r, _, _, _) -> [
      #(i, False),
      #(i, True),
      #(r, False),
    ]
    CmpLocalLocalJump(a, b, _, _, _)
    | PutLocalLocalField(a, b, _)
    | BinOpLocalLocal(_, a, b)
    | GetElemLocals(a, b) -> [#(a, False), #(b, False)]
    GetElemPostInc(a, b) -> [#(a, False), #(b, False), #(b, True)]
    BinOpPut(_, d) | BinOpConstPut(_, _, d) -> [#(d, True)]
    BinOpLocalPut(_, i, d) -> [#(i, False), #(d, True)]
    BinOpLocalLocalPut(_, a, b, d) -> [#(a, False), #(b, False), #(d, True)]
    _ -> []
  }
}

// slots these ops name can never live outside the tuple
pub fn pinned_slots(op: Op) -> List(Int) {
  case op {
    PutLocalCheckInit(i)
    | ApplyArguments(slot: i, ..)
    | BoxLocal(i)
    | GetBoxed(i)
    | PutBoxed(i)
    | PutBoxedCheckInit(i) -> [i]
    _ -> []
  }
}

pub fn map_slots(op: Op, f: fn(Int) -> Int) -> Op {
  case op {
    GetLocal(i) -> GetLocal(f(i))
    PutLocal(i) -> PutLocal(f(i))
    IncLocal(i) -> IncLocal(f(i))
    DecLocal(i) -> DecLocal(f(i))
    JumpIfLocal(i, t, w) -> JumpIfLocal(f(i), t, w)
    IncLocalJump(i, t) -> IncLocalJump(f(i), t)
    IncLocalCmpConstJump(i, by, c, k, t, w) ->
      IncLocalCmpConstJump(f(i), by, c, k, t, w)
    IncLocalCmpLocalJump(i, by, r, k, t, w) ->
      IncLocalCmpLocalJump(f(i), by, f(r), k, t, w)
    CmpLocalLocalJump(a, b, k, t, w) -> CmpLocalLocalJump(f(a), f(b), k, t, w)
    CmpLocalConstJump(a, c, k, t, w) -> CmpLocalConstJump(f(a), c, k, t, w)
    GetLocalField(i, k) -> GetLocalField(f(i), k)
    GetLocalField2(i, k) -> GetLocalField2(f(i), k)
    GetFieldCall1(k, i) -> GetFieldCall1(k, f(i))
    GetLocalFieldCall(i, k) -> GetLocalFieldCall(f(i), k)
    PutLocalLocalField(a, b, k) -> PutLocalLocalField(f(a), f(b), k)
    PutLocalConstField(a, c, k) -> PutLocalConstField(f(a), c, k)
    BinOpLocal(k, i) -> BinOpLocal(k, f(i))
    BinOpLocalLocal(k, a, b) -> BinOpLocalLocal(k, f(a), f(b))
    BinOpLocalConst(k, a, c) -> BinOpLocalConst(k, f(a), c)
    PostIncLocal(i) -> PostIncLocal(f(i))
    PostDecLocal(i) -> PostDecLocal(f(i))
    GetElemLocals(a, b) -> GetElemLocals(f(a), f(b))
    GetElemPostInc(a, b) -> GetElemPostInc(f(a), f(b))
    BinOpLocalField(k, i, key) -> BinOpLocalField(k, f(i), key)
    BinOpPut(k, d) -> BinOpPut(k, f(d))
    BinOpConstPut(k, c, d) -> BinOpConstPut(k, c, f(d))
    BinOpLocalPut(k, i, d) -> BinOpLocalPut(k, f(i), f(d))
    BinOpLocalLocalPut(k, a, b, d) -> BinOpLocalLocalPut(k, f(a), f(b), f(d))
    other -> other
  }
}

// backward edges only, enough to find loops
pub fn jump_target(op: Op) -> Int {
  case op {
    Jump(Pc(t))
    | JumpIfFalse(Pc(t))
    | JumpIfTrue(Pc(t))
    | JumpIfNullish(Pc(t))
    | JumpIfNotNullish(Pc(t))
    | JumpIfLocal(_, Pc(t), _)
    | IncLocalJump(_, Pc(t))
    | IncLocalCmpConstJump(_, _, _, _, Pc(t), _)
    | IncLocalCmpLocalJump(_, _, _, _, Pc(t), _)
    | CmpLocalLocalJump(_, _, _, Pc(t), _)
    | CmpLocalConstJump(_, _, _, Pc(t), _)
    | CmpJump(_, Pc(t), _)
    | CmpConstJump(_, _, Pc(t), _) -> t
    _ -> -1
  }
}
