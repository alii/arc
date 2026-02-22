/// Phase 3: Label Resolution
///
/// Converts IrOp (with label IDs for jumps) into final Op (with absolute PC addresses).
/// Two-pass algorithm:
///   Pass 1: Walk IR, skip IrLabel markers, build Dict(label_id → PC)
///   Pass 2: Walk IR, replace IrJump(label) → Jump(pc), drop IrLabel, translate all Ir* → Op
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import lumen/vm/opcode.{
  type FuncTemplate, type IrOp, type Op, FuncTemplate, IrArrayFrom, IrBinOp,
  IrBoxLocal, IrCall, IrCallApply, IrCallConstructor, IrCallMethod, IrCloseVar,
  IrDefineAccessor, IrDefineField, IrDefineFieldComputed, IrDeleteElem,
  IrDeleteField, IrDup, IrEnterFinally, IrForInNext, IrForInStart, IrGetBoxed,
  IrGetElem, IrGetElem2, IrGetField, IrGetField2, IrGetGlobal, IrGetIterator,
  IrGetLocal, IrGetThis, IrIteratorClose, IrIteratorNext, IrJump, IrJumpIfFalse,
  IrJumpIfNullish, IrJumpIfTrue, IrLabel, IrLeaveFinally, IrMakeClosure,
  IrNewObject, IrObjectSpread, IrPop, IrPopTry, IrPushConst, IrPushTry,
  IrPutBoxed, IrPutElem, IrPutField, IrPutGlobal, IrPutLocal, IrReturn, IrRot3,
  IrScopeGetVar, IrScopePutVar, IrScopeTypeofVar, IrSwap, IrThrow, IrTypeOf,
  IrTypeofGlobal, IrUnaryOp,
}
import lumen/vm/value.{type JsValue}

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
  env_descriptors: List(opcode.EnvCapture),
  is_strict: Bool,
  is_arrow: Bool,
) -> FuncTemplate {
  let label_map = build_label_map(code, 0, dict.new())
  let ops = resolve_ops(code, label_map, [])
  FuncTemplate(
    name:,
    arity:,
    local_count:,
    bytecode: ops,
    constants:,
    functions:,
    env_descriptors:,
    is_strict:,
    is_arrow:,
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
fn resolve_ops(
  code: List(IrOp),
  labels: Dict(Int, Int),
  acc: List(Op),
) -> List(Op) {
  case code {
    [] -> list.reverse(acc)

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
    [IrPushTry(catch_label, _finally_label), ..rest] -> {
      let assert Ok(catch_pc) = dict.get(labels, catch_label)
      resolve_ops(rest, labels, [opcode.PushTry(catch_pc), ..acc])
    }

    // Scope-aware ops should NOT appear here (consumed by Phase 2)
    [IrScopeGetVar(_), ..] | [IrScopePutVar(_), ..] | [IrScopeTypeofVar(_), ..] ->
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

    // 1:1 translations
    [IrPushConst(i), ..rest] ->
      resolve_ops(rest, labels, [opcode.PushConst(i), ..acc])
    [IrPop, ..rest] -> resolve_ops(rest, labels, [opcode.Pop, ..acc])
    [IrDup, ..rest] -> resolve_ops(rest, labels, [opcode.Dup, ..acc])
    [IrSwap, ..rest] -> resolve_ops(rest, labels, [opcode.Swap, ..acc])
    [IrRot3, ..rest] -> resolve_ops(rest, labels, [opcode.Rot3, ..acc])
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

    // Object/array construction
    [IrNewObject, ..rest] ->
      resolve_ops(rest, labels, [opcode.NewObject, ..acc])
    [IrDefineField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineField(name), ..acc])
    [IrDefineFieldComputed, ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineFieldComputed, ..acc])
    [IrDefineAccessor(name, kind), ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineAccessor(name, kind), ..acc])
    [IrObjectSpread, ..rest] ->
      resolve_ops(rest, labels, [opcode.ObjectSpread, ..acc])
    [IrArrayFrom(count), ..rest] ->
      resolve_ops(rest, labels, [opcode.ArrayFrom(count), ..acc])

    // Calls
    [IrCall(arity), ..rest] ->
      resolve_ops(rest, labels, [opcode.Call(arity), ..acc])
    [IrCallMethod(name, arity), ..rest] ->
      resolve_ops(rest, labels, [opcode.CallMethod(name, arity), ..acc])
    [IrCallConstructor(arity), ..rest] ->
      resolve_ops(rest, labels, [opcode.CallConstructor(arity), ..acc])
    [IrCallApply, ..rest] ->
      resolve_ops(rest, labels, [opcode.CallApply, ..acc])
    [IrReturn, ..rest] -> resolve_ops(rest, labels, [opcode.Return, ..acc])

    // Exception handling
    [IrThrow, ..rest] -> resolve_ops(rest, labels, [opcode.Throw, ..acc])
    [IrPopTry, ..rest] -> resolve_ops(rest, labels, [opcode.PopTry, ..acc])
    [IrEnterFinally, ..rest] ->
      resolve_ops(rest, labels, [opcode.EnterFinally, ..acc])
    [IrLeaveFinally, ..rest] ->
      resolve_ops(rest, labels, [opcode.LeaveFinally, ..acc])

    // Closures
    [IrMakeClosure(func_index), ..rest] ->
      resolve_ops(rest, labels, [opcode.MakeClosure(func_index), ..acc])
    [IrCloseVar(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.CloseVar(index), ..acc])
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
    [IrIteratorNext, ..rest] ->
      resolve_ops(rest, labels, [opcode.IteratorNext, ..acc])
    [IrIteratorClose, ..rest] ->
      resolve_ops(rest, labels, [opcode.IteratorClose, ..acc])
  }
}
