/// Phase 3: Label Resolution
///
/// Converts IrOp (with label IDs for jumps) into final Op (with absolute PC addresses).
/// Two-pass algorithm:
///   Pass 1: Walk IR, skip IrLabel markers, build Dict(label_id → PC)
///   Pass 2: Walk IR, replace IrJump(label) → Jump(pc), drop IrLabel, translate all Ir* → Op
import arc/vm/internal/tuple_array
import arc/vm/opcode.{
  type IrOp, type Op, IrArrayFrom, IrArrayFromWithHoles, IrArrayPush,
  IrArrayPushHole, IrArraySpread, IrAsyncYieldStarNext, IrAsyncYieldStarResume,
  IrAwait, IrBinOp, IrBoxLocal, IrCall, IrCallApply, IrCallConstructor,
  IrCallConstructorApply, IrCallEval, IrCallMethod, IrCallMethodApply,
  IrCallSuper, IrCallSuperApply, IrCreateArguments, IrDeclareEvalVar,
  IrDeclareGlobalLex, IrDeclareGlobalVar, IrDefineAccessor,
  IrDefineAccessorComputed, IrDefineField, IrDefineFieldComputed, IrDefineMethod,
  IrDefineMethodComputed, IrDeleteElem, IrDeleteField, IrDup, IrForInNext,
  IrForInStart, IrGetAsyncIterator, IrGetBoxed, IrGetElem, IrGetElem2,
  IrGetEvalVar, IrGetField, IrGetField2, IrGetGlobal, IrGetIterator, IrGetLocal,
  IrGetPrivateField, IrGetPrivateField2, IrGetThis, IrGosub, IrInitGlobalLex,
  IrInitialYield, IrIteratorCheckObject, IrIteratorClose, IrIteratorCloseThrow,
  IrIteratorNext, IrIteratorRest, IrJump, IrJumpIfFalse, IrJumpIfNullish,
  IrJumpIfTrue, IrLabel, IrMakeClosure, IrNewObject, IrNewRegExp,
  IrObjectRestCopy, IrObjectSpread, IrPop, IrPopTry, IrPrivateIn, IrPushConst,
  IrPushTry, IrPutBoxed, IrPutElem, IrPutEvalVar, IrPutField, IrPutGlobal,
  IrPutLocal, IrPutPrivateField, IrRet, IrReturn, IrScopeGetVar, IrScopePutVar,
  IrScopeReboxVar, IrScopeTypeofVar, IrSetupDerivedClass, IrSwap, IrThrow,
  IrTypeOf, IrTypeofEvalVar, IrTypeofGlobal, IrUnaryOp, IrYield, IrYieldStar,
}
import arc/vm/value.{
  type EnvCapture, type FuncTemplate, type JsValue, FuncTemplate,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}

/// Resolve a list of IrOps into a FuncTemplate.
/// The IrOps must have all scope markers already consumed (by Phase 2).
/// Only IrLabel/IrJump/IrJumpIfFalse/IrJumpIfTrue/IrJumpIfNullish/IrPushTry
/// still need label→PC resolution.
pub fn resolve(
  code: List(IrOp),
  constants: List(JsValue),
  local_count: Int,
  functions: List(FuncTemplate),
  name: Option(String),
  arity: Int,
  env_descriptors: List(EnvCapture),
  is_strict: Bool,
  is_arrow: Bool,
  is_derived_constructor: Bool,
  is_generator: Bool,
  is_async: Bool,
  local_names: Option(List(#(String, Int))),
) -> FuncTemplate {
  let label_map = build_label_map(code, 0, dict.new())
  let ops = resolve_ops(code, label_map, [])
  FuncTemplate(
    name:,
    arity:,
    local_count:,
    bytecode: tuple_array.from_list(ops),
    constants: tuple_array.from_list(constants),
    functions: tuple_array.from_list(functions),
    env_descriptors:,
    is_strict:,
    is_arrow:,
    is_derived_constructor:,
    is_generator:,
    is_async:,
    local_names:,
  )
}

/// Pass 1: Walk the IR, counting real ops and recording label positions.
fn build_label_map(
  code: List(IrOp),
  pc: Int,
  map: Dict(Int, Int),
) -> Dict(Int, Int) {
  case code {
    [] -> map
    [IrLabel(id), ..rest] ->
      // Labels don't occupy a PC slot
      build_label_map(rest, pc, dict.insert(map, id, pc))
    [_, ..rest] ->
      // All other ops occupy one PC slot
      build_label_map(rest, pc + 1, map)
  }
}

/// Pass 2: Walk the IR, resolve labels to PCs, translate IrOp → Op.
/// Appends a sentinel Return at the end so the interpreter's fetch loop
/// can use unchecked element/2 — termination happens via normal Return
/// dispatch instead of Option/None detection on every instruction.
fn resolve_ops(
  code: List(IrOp),
  labels: Dict(Int, Int),
  acc: List(Op),
) -> List(Op) {
  case code {
    [] -> list.reverse([opcode.Return, ..acc])

    // Labels are dropped (they were just markers)
    [IrLabel(_), ..rest] -> resolve_ops(rest, labels, acc)

    // Jump ops: resolve label → PC
    [IrJump(label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.Jump(pc), ..acc])
    }
    [IrJumpIfFalse(label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.JumpIfFalse(pc), ..acc])
    }
    [IrJumpIfTrue(label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.JumpIfTrue(pc), ..acc])
    }
    [IrJumpIfNullish(label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.JumpIfNullish(pc), ..acc])
    }
    [IrPushTry(catch_label), ..rest] -> {
      let assert Ok(catch_pc) = dict.get(labels, catch_label)
      resolve_ops(rest, labels, [opcode.PushTry(catch_pc), ..acc])
    }
    [IrGosub(label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.Gosub(pc), ..acc])
    }

    // Scope-aware ops should NOT appear here (consumed by Phase 2)
    [IrScopeGetVar(_), ..]
    | [IrScopePutVar(_), ..]
    | [IrScopeTypeofVar(_), ..]
    | [IrScopeReboxVar(_), ..] ->
      panic as "resolve: scope ops should be consumed by Phase 2"

    // Resolved variable access (emitted by Phase 2)
    [IrGetLocal(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetLocal(index), ..acc])
    [IrPutLocal(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutLocal(index), ..acc])
    [IrGetGlobal(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetGlobal(name), ..acc])
    [IrPutGlobal(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutGlobal(name), ..acc])
    [IrTypeofGlobal(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.TypeofGlobal(name), ..acc])
    [IrGetEvalVar(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetEvalVar(name), ..acc])
    [IrPutEvalVar(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutEvalVar(name), ..acc])
    [IrDeclareEvalVar(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.DeclareEvalVar(name), ..acc])
    [IrTypeofEvalVar(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.TypeofEvalVar(name), ..acc])

    // 1:1 translations
    [IrPushConst(i), ..rest] ->
      resolve_ops(rest, labels, [opcode.PushConst(i), ..acc])
    [IrPop, ..rest] -> resolve_ops(rest, labels, [opcode.Pop, ..acc])
    [IrDup, ..rest] -> resolve_ops(rest, labels, [opcode.Dup, ..acc])
    [IrSwap, ..rest] -> resolve_ops(rest, labels, [opcode.Swap, ..acc])
    [IrGetThis, ..rest] -> resolve_ops(rest, labels, [opcode.GetThis, ..acc])

    // Property access
    [IrGetField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetField(name), ..acc])
    [IrGetField2(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetField2(name), ..acc])
    [IrPutField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutField(name), ..acc])
    [IrGetElem, ..rest] -> resolve_ops(rest, labels, [opcode.GetElem, ..acc])
    [IrGetElem2, ..rest] -> resolve_ops(rest, labels, [opcode.GetElem2, ..acc])
    [IrPutElem, ..rest] -> resolve_ops(rest, labels, [opcode.PutElem, ..acc])
    [IrDeleteField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.DeleteField(name), ..acc])
    [IrDeleteElem, ..rest] ->
      resolve_ops(rest, labels, [opcode.DeleteElem, ..acc])
    [IrGetPrivateField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetPrivateField(name), ..acc])
    [IrGetPrivateField2(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetPrivateField2(name), ..acc])
    [IrPutPrivateField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutPrivateField(name), ..acc])
    [IrPrivateIn(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.PrivateIn(name), ..acc])

    // Object/array construction
    [IrNewObject, ..rest] ->
      resolve_ops(rest, labels, [opcode.NewObject, ..acc])
    [IrDefineField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineField(name), ..acc])
    [IrDefineFieldComputed, ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineFieldComputed, ..acc])
    [IrDefineMethod(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineMethod(name), ..acc])
    [IrDefineMethodComputed, ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineMethodComputed, ..acc])
    [IrDefineAccessor(name, kind), ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineAccessor(name, kind), ..acc])
    [IrDefineAccessorComputed(kind), ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineAccessorComputed(kind), ..acc])
    [IrObjectSpread, ..rest] ->
      resolve_ops(rest, labels, [opcode.ObjectSpread, ..acc])
    [IrObjectRestCopy(n), ..rest] ->
      resolve_ops(rest, labels, [opcode.ObjectRestCopy(n), ..acc])
    [IrArrayFrom(count), ..rest] ->
      resolve_ops(rest, labels, [opcode.ArrayFrom(count), ..acc])
    [IrArrayFromWithHoles(count, holes), ..rest] ->
      resolve_ops(rest, labels, [opcode.ArrayFromWithHoles(count, holes), ..acc])
    [IrArrayPush, ..rest] ->
      resolve_ops(rest, labels, [opcode.ArrayPush, ..acc])
    [IrArrayPushHole, ..rest] ->
      resolve_ops(rest, labels, [opcode.ArrayPushHole, ..acc])
    [IrArraySpread, ..rest] ->
      resolve_ops(rest, labels, [opcode.ArraySpread, ..acc])

    // Calls
    [IrCall(arity), ..rest] ->
      resolve_ops(rest, labels, [opcode.Call(arity), ..acc])
    [IrCallEval(arity), ..rest] ->
      resolve_ops(rest, labels, [opcode.CallEval(arity), ..acc])
    [IrCallMethod(name, arity), ..rest] ->
      resolve_ops(rest, labels, [opcode.CallMethod(name, arity), ..acc])
    [IrCallConstructor(arity), ..rest] ->
      resolve_ops(rest, labels, [opcode.CallConstructor(arity), ..acc])
    [IrCallApply, ..rest] ->
      resolve_ops(rest, labels, [opcode.CallApply, ..acc])
    [IrCallMethodApply, ..rest] ->
      resolve_ops(rest, labels, [opcode.CallMethodApply, ..acc])
    [IrCallConstructorApply, ..rest] ->
      resolve_ops(rest, labels, [opcode.CallConstructorApply, ..acc])
    [IrReturn, ..rest] -> resolve_ops(rest, labels, [opcode.Return, ..acc])

    // Exception handling
    [IrThrow, ..rest] -> resolve_ops(rest, labels, [opcode.Throw, ..acc])
    [IrPopTry, ..rest] -> resolve_ops(rest, labels, [opcode.PopTry, ..acc])
    [IrRet, ..rest] -> resolve_ops(rest, labels, [opcode.Ret, ..acc])

    // Closures
    [IrMakeClosure(func_index), ..rest] ->
      resolve_ops(rest, labels, [opcode.MakeClosure(func_index), ..acc])
    [IrBoxLocal(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.BoxLocal(index), ..acc])
    [IrGetBoxed(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetBoxed(index), ..acc])
    [IrPutBoxed(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutBoxed(index), ..acc])

    // Operators
    [IrBinOp(kind), ..rest] ->
      resolve_ops(rest, labels, [opcode.BinOp(kind), ..acc])
    [IrUnaryOp(kind), ..rest] ->
      resolve_ops(rest, labels, [opcode.UnaryOp(kind), ..acc])
    [IrTypeOf, ..rest] -> resolve_ops(rest, labels, [opcode.TypeOf, ..acc])

    // Iteration
    [IrForInStart, ..rest] ->
      resolve_ops(rest, labels, [opcode.ForInStart, ..acc])
    [IrForInNext, ..rest] ->
      resolve_ops(rest, labels, [opcode.ForInNext, ..acc])
    [IrGetIterator, ..rest] ->
      resolve_ops(rest, labels, [opcode.GetIterator, ..acc])
    [IrGetAsyncIterator, ..rest] ->
      resolve_ops(rest, labels, [opcode.GetAsyncIterator, ..acc])
    [IrIteratorNext, ..rest] ->
      resolve_ops(rest, labels, [opcode.IteratorNext, ..acc])
    [IrIteratorClose, ..rest] ->
      resolve_ops(rest, labels, [opcode.IteratorClose, ..acc])
    [IrIteratorCloseThrow, ..rest] ->
      resolve_ops(rest, labels, [opcode.IteratorCloseThrow, ..acc])
    [IrIteratorCheckObject, ..rest] ->
      resolve_ops(rest, labels, [opcode.IteratorCheckObject, ..acc])
    [IrIteratorRest, ..rest] ->
      resolve_ops(rest, labels, [opcode.IteratorRest, ..acc])

    // Class inheritance
    [IrSetupDerivedClass, ..rest] ->
      resolve_ops(rest, labels, [opcode.SetupDerivedClass, ..acc])
    [IrCallSuper(arity), ..rest] ->
      resolve_ops(rest, labels, [opcode.CallSuper(arity), ..acc])
    [IrCallSuperApply, ..rest] ->
      resolve_ops(rest, labels, [opcode.CallSuperApply, ..acc])

    // Generator
    [IrInitialYield, ..rest] ->
      resolve_ops(rest, labels, [opcode.InitialYield, ..acc])
    [IrYield, ..rest] -> resolve_ops(rest, labels, [opcode.Yield, ..acc])
    [IrYieldStar, ..rest] ->
      resolve_ops(rest, labels, [opcode.YieldStar, ..acc])
    [IrAsyncYieldStarNext, ..rest] ->
      resolve_ops(rest, labels, [opcode.AsyncYieldStarNext, ..acc])
    [IrAsyncYieldStarResume(next_label), ..rest] -> {
      let assert Ok(next_pc) = dict.get(labels, next_label)
      resolve_ops(rest, labels, [opcode.AsyncYieldStarResume(next_pc), ..acc])
    }

    // Async
    [IrAwait, ..rest] -> resolve_ops(rest, labels, [opcode.Await, ..acc])

    // Arguments object
    [IrCreateArguments, ..rest] ->
      resolve_ops(rest, labels, [opcode.CreateArguments, ..acc])

    // RegExp
    [IrNewRegExp, ..rest] ->
      resolve_ops(rest, labels, [opcode.NewRegExp, ..acc])

    // Global Environment Record
    [IrDeclareGlobalVar(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.DeclareGlobalVar(name), ..acc])
    [IrDeclareGlobalLex(name, is_const), ..rest] ->
      resolve_ops(rest, labels, [opcode.DeclareGlobalLex(name, is_const), ..acc])
    [IrInitGlobalLex(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.InitGlobalLex(name), ..acc])
  }
}
