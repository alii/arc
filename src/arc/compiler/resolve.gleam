/// Phase 3: Label Resolution
///
/// Converts IrOp (with label IDs for jumps) into final Op (with absolute PC addresses).
/// Two-pass algorithm:
///   Pass 1: Walk IR, skip IrLabel markers, build Dict(label_id → PC)
///   Pass 2: Walk IR, replace IrJump(label) → Jump(pc), drop IrLabel, translate all Ir* → Op
import arc/vm/internal/tuple_array
import arc/vm/opcode.{
  type IrOp, type LexicalSlots, type Op, type SyntaxPerms, IrArrayFrom,
  IrArrayFromWithHoles, IrArrayPush, IrArrayPushHole, IrArraySpread,
  IrAsyncYieldStarNext, IrAsyncYieldStarResume, IrAwait, IrBinOp, IrBoxLocal,
  IrCall, IrCallApply, IrCallConstructor, IrCallConstructorApply, IrCallEval,
  IrCallMethod, IrCallMethodApply, IrCmpLocalConstJump, IrCmpLocalLocalJump,
  IrCreateArguments, IrCreateRestArray, IrDecLocal, IrDeclareEvalVar,
  IrDeclareGlobalLex, IrDeclareGlobalVar, IrDefineAccessor,
  IrDefineAccessorComputed, IrDefineField, IrDefineFieldComputed, IrDefineMethod,
  IrDefineMethodComputed, IrDefinePrivateAccessor, IrDefinePrivateField,
  IrDefinePrivateMethod, IrDeleteElem, IrDeleteField, IrDup, IrForInNext,
  IrForInStart, IrGetAsyncIterator, IrGetBoxed, IrGetElem, IrGetElem2,
  IrGetEvalVar, IrGetField, IrGetField2, IrGetGlobal, IrGetIterator,
  IrGetLexical, IrGetLocal, IrGetPrivateField, IrGetPrivateField2,
  IrGetPrivateFieldDyn, IrGetPrivateFieldDyn2, IrGetPrototypeOf, IrGetSuperValue,
  IrGetSuperValue2, IrGetTemplateObject, IrGosub, IrIncLocal, IrInitGlobalLex,
  IrInitialYield, IrIteratorCheckObject, IrIteratorClose, IrIteratorCloseThrow,
  IrIteratorNext, IrIteratorRecord, IrIteratorRest, IrJump, IrJumpIfFalse,
  IrJumpIfNullish, IrJumpIfTrue, IrLabel, IrMakeClosure, IrMakeMethod,
  IrNewObject, IrNewPrivateName, IrNewRegExp, IrObjectRestCopy, IrObjectSpread,
  IrPop, IrPopTry, IrPrivateIn, IrPrivateInDyn, IrPushConst, IrPushTry,
  IrPutBoxed, IrPutBoxedCheckInit, IrPutElem, IrPutEvalVar, IrPutField,
  IrPutGlobal, IrPutLocal, IrPutLocalCheckInit, IrPutPrivateField,
  IrPutPrivateFieldDyn, IrPutSuperValue, IrRet, IrReturn, IrRot3,
  IrScopeDeleteVar, IrScopeGetRef, IrScopeGetVar, IrScopeGetVarThis,
  IrScopeInitVar, IrScopeMakeRef, IrScopePutRef, IrScopePutVar, IrScopeReboxVar,
  IrScopeTypeofVar, IrSetLine, IrSetProto, IrSetThis, IrSetupDerivedClass,
  IrSwap, IrThrow, IrThrowConstAssign, IrThrowError, IrToObject, IrToPropertyKey,
  IrToStringVal, IrTypeOf, IrTypeofEvalVar, IrTypeofGlobal, IrUnaryOp, IrUnrot4,
  IrWithDeleteVar, IrWithGetRefValue, IrWithGetVar, IrWithGetVarThis,
  IrWithMakeRef, IrWithPutRefValue, IrWithPutVar, IrYield, IrYieldStar,
}
import arc/vm/value.{
  type EnvCapture, type FuncTemplate, type JsValue, FuncTemplate,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

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
  length: Int,
  env_descriptors: List(EnvCapture),
  is_strict: Bool,
  is_arrow: Bool,
  is_derived_constructor: Bool,
  is_generator: Bool,
  is_async: Bool,
  is_constructor: Bool,
  is_class_constructor: Bool,
  local_names: Option(List(#(String, Int))),
  lexical: LexicalSlots,
  syntax_perms: SyntaxPerms,
) -> FuncTemplate {
  let const_arr = tuple_array.from_list(constants)
  let code = peephole(code, const_arr, [])
  let label_map = build_label_map(code, 0, dict.new())
  let ops = resolve_ops(code, label_map, [])
  FuncTemplate(
    name:,
    arity:,
    length:,
    local_count:,
    bytecode: tuple_array.from_list(ops),
    constants: const_arr,
    functions: tuple_array.from_list(functions),
    env_descriptors:,
    is_strict:,
    is_arrow:,
    is_derived_constructor:,
    is_generator:,
    is_async:,
    is_constructor:,
    is_class_constructor:,
    local_names:,
    lexical:,
    syntax_perms:,
  )
}

/// Peephole pass over the IR, run BEFORE label resolution so removing or
/// fusing ops cannot invalidate jump targets (labels are still symbolic
/// IrLabel markers; a label inside a candidate window simply prevents the
/// pattern from matching, so no jump can land mid-fusion).
///
/// Rewrites (all semantics-preserving op-for-op):
///   1. Statement-position postfix update on a plain local
///      (GetLocal i; UnaryOp Pos; Dup; PushConst 1; BinOp Add|Sub;
///       PutLocal i; Pop) → IncLocal/DecLocal — the Dup'd old value is
///      immediately discarded by the trailing Pop.
///   2. The same shape on a boxed local (or with a non-1 constant): the
///      dead Dup/Pop pair is dropped, keeping the explicit ops.
///   3. Dup; PutLocal/PutBoxed; Pop → PutLocal/PutBoxed (prefix updates and
///      any other store whose expression value is discarded).
///   4. Loop-condition compare-and-branch (GetLocal; GetLocal|PushConst;
///      BinOp Lt|LtEq|Gt|GtEq; JumpIfFalse) → CmpLocal*Jump.
fn peephole(
  code: List(IrOp),
  consts: tuple_array.TupleArray(JsValue),
  acc: List(IrOp),
) -> List(IrOp) {
  case code {
    [] -> list.reverse(acc)

    // -- Pattern 1/2: postfix update statement on a plain local ----------
    [
      IrGetLocal(i),
      IrUnaryOp(opcode.Pos),
      IrDup,
      IrPushConst(c),
      IrBinOp(kind),
      IrPutLocal(j),
      IrPop,
      ..rest
    ]
      if i == j
    -> {
      let fused = case is_const_one(consts, c), kind {
        True, opcode.Add -> Some(IrIncLocal(i))
        True, opcode.Sub -> Some(IrDecLocal(i))
        _, _ -> None
      }
      case fused {
        Some(op) -> peephole(rest, consts, [op, ..acc])
        // Non-±1 update: still drop the dead Dup/Pop pair.
        None ->
          peephole(rest, consts, [
            IrPutLocal(j),
            IrBinOp(kind),
            IrPushConst(c),
            IrUnaryOp(opcode.Pos),
            IrGetLocal(i),
            ..acc
          ])
      }
    }

    // -- Pattern 2: postfix update statement on a boxed local ------------
    // Same shape via GetBoxed/PutBoxed: drop the dead Dup/Pop pair.
    [
      IrGetBoxed(i),
      IrUnaryOp(opcode.Pos),
      IrDup,
      IrPushConst(c),
      IrBinOp(kind),
      IrPutBoxed(j),
      IrPop,
      ..rest
    ]
      if i == j
    ->
      peephole(rest, consts, [
        IrPutBoxed(j),
        IrBinOp(kind),
        IrPushConst(c),
        IrUnaryOp(opcode.Pos),
        IrGetBoxed(i),
        ..acc
      ])

    // -- Pattern 3: dead Dup under a store whose value is discarded ------
    // Dup; PutLocal(i); Pop ≡ PutLocal(i) (likewise PutBoxed).
    [IrDup, IrPutLocal(i), IrPop, ..rest] ->
      peephole(rest, consts, [IrPutLocal(i), ..acc])
    [IrDup, IrPutBoxed(i), IrPop, ..rest] ->
      peephole(rest, consts, [IrPutBoxed(i), ..acc])

    // -- Pattern 4: fused compare-and-branch loop conditions -------------
    [IrGetLocal(a), IrGetLocal(b), IrBinOp(kind), IrJumpIfFalse(l), ..rest] ->
      case is_fusable_cmp(kind) {
        True ->
          peephole(rest, consts, [IrCmpLocalLocalJump(a, b, kind, l), ..acc])
        False ->
          peephole(
            [IrGetLocal(b), IrBinOp(kind), IrJumpIfFalse(l), ..rest],
            consts,
            [IrGetLocal(a), ..acc],
          )
      }
    [IrGetLocal(a), IrPushConst(c), IrBinOp(kind), IrJumpIfFalse(l), ..rest] ->
      case is_fusable_cmp(kind) {
        True ->
          peephole(rest, consts, [IrCmpLocalConstJump(a, c, kind, l), ..acc])
        False ->
          peephole(
            [IrPushConst(c), IrBinOp(kind), IrJumpIfFalse(l), ..rest],
            consts,
            [IrGetLocal(a), ..acc],
          )
      }

    [op, ..rest] -> peephole(rest, consts, [op, ..acc])
  }
}

/// Only the pure relational kinds are fused — their step semantics are
/// exactly binop_direct / binop_with_to_primitive (no In/InstanceOf heap
/// access, no Add string-concat split, no loose-eq coercion table).
fn is_fusable_cmp(kind: opcode.BinOpKind) -> Bool {
  case kind {
    opcode.Lt | opcode.LtEq | opcode.Gt | opcode.GtEq -> True
    _ -> False
  }
}

fn is_const_one(consts: tuple_array.TupleArray(JsValue), index: Int) -> Bool {
  tuple_array.unsafe_get(index, consts) == value.JsNumber(value.Finite(1.0))
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

    // `with`-object access: label-carrying, resolved like jumps
    [IrWithGetVar(name, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.WithGetVar(name, pc), ..acc])
    }
    [IrWithGetVarThis(name, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.WithGetVarThis(name, pc), ..acc])
    }
    [IrWithPutVar(name, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.WithPutVar(name, pc), ..acc])
    }
    [IrWithDeleteVar(name, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.WithDeleteVar(name, pc), ..acc])
    }
    [IrWithMakeRef(name, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.WithMakeRef(name, pc), ..acc])
    }
    [IrWithGetRefValue(name, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.WithGetRefValue(name, pc), ..acc])
    }
    [IrWithPutRefValue(name, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [opcode.WithPutRefValue(name, pc), ..acc])
    }
    [IrToObject, ..rest] -> resolve_ops(rest, labels, [opcode.ToObject, ..acc])
    [IrToStringVal, ..rest] ->
      resolve_ops(rest, labels, [opcode.ToStringVal, ..acc])
    [IrGetTemplateObject(site, cooked, raw), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.GetTemplateObject(site, cooked, raw),
        ..acc
      ])

    // Scope-aware ops should NOT appear here (consumed by Phase 2)
    [IrScopeGetVar(_), ..]
    | [IrScopeGetVarThis(_), ..]
    | [IrScopePutVar(_), ..]
    | [IrScopeInitVar(_), ..]
    | [IrScopeTypeofVar(_), ..]
    | [IrScopeReboxVar(_), ..]
    | [IrScopeDeleteVar(_), ..]
    | [IrScopeMakeRef(_), ..]
    | [IrScopeGetRef(_), ..]
    | [IrScopePutRef(_), ..]
    | [IrGetLexical(_), ..]
    | [IrSetThis, ..] ->
      panic as "resolve: scope ops should be consumed by Phase 2"

    // Resolved variable access (emitted by Phase 2)
    [IrGetLocal(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetLocal(index), ..acc])
    [IrPutLocal(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutLocal(index), ..acc])
    [IrPutLocalCheckInit(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutLocalCheckInit(index), ..acc])
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
    [IrSetLine(line), ..rest] ->
      resolve_ops(rest, labels, [opcode.SetLine(line), ..acc])
    [IrPushConst(i), ..rest] ->
      resolve_ops(rest, labels, [opcode.PushConst(i), ..acc])
    [IrPop, ..rest] -> resolve_ops(rest, labels, [opcode.Pop, ..acc])
    [IrDup, ..rest] -> resolve_ops(rest, labels, [opcode.Dup, ..acc])
    [IrSwap, ..rest] -> resolve_ops(rest, labels, [opcode.Swap, ..acc])
    [IrRot3, ..rest] -> resolve_ops(rest, labels, [opcode.Rot3, ..acc])
    [IrUnrot4, ..rest] -> resolve_ops(rest, labels, [opcode.Unrot4, ..acc])

    // Property access — precompute canonical key once so the interpreter
    // never re-parses the constant string per dispatch.
    [IrGetField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetField(opcode.make_key(name)), ..acc])
    [IrGetField2(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetField2(opcode.make_key(name)), ..acc])
    [IrPutField(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutField(opcode.make_key(name)), ..acc])
    [IrGetElem, ..rest] -> resolve_ops(rest, labels, [opcode.GetElem, ..acc])
    [IrGetElem2, ..rest] -> resolve_ops(rest, labels, [opcode.GetElem2, ..acc])
    [IrPutElem, ..rest] -> resolve_ops(rest, labels, [opcode.PutElem, ..acc])
    [IrDeleteField(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DeleteField(opcode.make_key(name)),
        ..acc
      ])
    [IrDeleteElem, ..rest] ->
      resolve_ops(rest, labels, [opcode.DeleteElem, ..acc])
    [IrGetPrivateField(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.GetPrivateField(opcode.make_key(name)),
        ..acc
      ])
    [IrGetPrivateField2(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.GetPrivateField2(opcode.make_key(name)),
        ..acc
      ])
    [IrPutPrivateField(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.PutPrivateField(opcode.make_key(name)),
        ..acc
      ])
    [IrPrivateIn(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.PrivateIn(opcode.make_key(name)), ..acc])
    [IrNewPrivateName(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.NewPrivateName(name), ..acc])
    [IrGetPrivateFieldDyn, ..rest] ->
      resolve_ops(rest, labels, [opcode.GetPrivateFieldDyn, ..acc])
    [IrGetPrivateFieldDyn2, ..rest] ->
      resolve_ops(rest, labels, [opcode.GetPrivateFieldDyn2, ..acc])
    [IrPutPrivateFieldDyn, ..rest] ->
      resolve_ops(rest, labels, [opcode.PutPrivateFieldDyn, ..acc])
    [IrPrivateInDyn, ..rest] ->
      resolve_ops(rest, labels, [opcode.PrivateInDyn, ..acc])
    [IrDefinePrivateField, ..rest] ->
      resolve_ops(rest, labels, [opcode.DefinePrivateField, ..acc])
    [IrDefinePrivateMethod, ..rest] ->
      resolve_ops(rest, labels, [opcode.DefinePrivateMethod, ..acc])
    [IrDefinePrivateAccessor(kind), ..rest] ->
      resolve_ops(rest, labels, [opcode.DefinePrivateAccessor(kind), ..acc])

    // Object/array construction
    [IrNewObject, ..rest] ->
      resolve_ops(rest, labels, [opcode.NewObject, ..acc])
    [IrDefineField(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DefineField(opcode.make_key(name)),
        ..acc
      ])
    [IrDefineFieldComputed, ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineFieldComputed, ..acc])
    [IrToPropertyKey, ..rest] ->
      resolve_ops(rest, labels, [opcode.ToPropertyKey, ..acc])
    [IrDefineMethod(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DefineMethod(opcode.make_key(name)),
        ..acc
      ])
    [IrDefineMethodComputed, ..rest] ->
      resolve_ops(rest, labels, [opcode.DefineMethodComputed, ..acc])
    [IrDefineAccessor(name, kind, enumerable), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DefineAccessor(opcode.make_key(name), kind, enumerable),
        ..acc
      ])
    [IrDefineAccessorComputed(kind, enumerable), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DefineAccessorComputed(kind, enumerable),
        ..acc
      ])
    [IrMakeMethod, ..rest] ->
      resolve_ops(rest, labels, [opcode.MakeMethod, ..acc])
    [IrSetProto, ..rest] -> resolve_ops(rest, labels, [opcode.SetProto, ..acc])
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
    [IrCallEval(arity, param_scope_names, with_names, private_names), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.CallEval(arity, param_scope_names, with_names, private_names),
        ..acc
      ])
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
    [IrThrowConstAssign(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.ThrowConstAssign(name), ..acc])
    [IrThrowError(kind, msg), ..rest] ->
      resolve_ops(rest, labels, [opcode.ThrowError(kind, msg), ..acc])
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
    [IrPutBoxedCheckInit(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.PutBoxedCheckInit(index), ..acc])

    // Fused superinstructions (created by the peephole pass above)
    [IrIncLocal(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.IncLocal(index), ..acc])
    [IrDecLocal(index), ..rest] ->
      resolve_ops(rest, labels, [opcode.DecLocal(index), ..acc])
    [IrCmpLocalLocalJump(left, right, kind, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [
        opcode.CmpLocalLocalJump(left, right, kind, pc),
        ..acc
      ])
    }
    [IrCmpLocalConstJump(left, const_index, kind, label), ..rest] -> {
      let assert Ok(pc) = dict.get(labels, label)
      resolve_ops(rest, labels, [
        opcode.CmpLocalConstJump(left, const_index, kind, pc),
        ..acc
      ])
    }

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
    [IrIteratorRecord, ..rest] ->
      resolve_ops(rest, labels, [opcode.IteratorRecord, ..acc])
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

    // Class inheritance / super
    [IrSetupDerivedClass, ..rest] ->
      resolve_ops(rest, labels, [opcode.SetupDerivedClass, ..acc])
    [IrGetPrototypeOf, ..rest] ->
      resolve_ops(rest, labels, [opcode.GetPrototypeOf, ..acc])
    [IrGetSuperValue, ..rest] ->
      resolve_ops(rest, labels, [opcode.GetSuperValue, ..acc])
    [IrGetSuperValue2, ..rest] ->
      resolve_ops(rest, labels, [opcode.GetSuperValue2, ..acc])
    [IrPutSuperValue, ..rest] ->
      resolve_ops(rest, labels, [opcode.PutSuperValue, ..acc])

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
    [IrCreateArguments(simple_params:), ..rest] ->
      resolve_ops(rest, labels, [opcode.CreateArguments(simple_params:), ..acc])

    [IrCreateRestArray(from_index), ..rest] ->
      resolve_ops(rest, labels, [opcode.CreateRestArray(from_index), ..acc])

    // RegExp
    [IrNewRegExp, ..rest] ->
      resolve_ops(rest, labels, [opcode.NewRegExp, ..acc])

    // Dynamic import
    [opcode.IrDynamicImport, ..rest] ->
      resolve_ops(rest, labels, [opcode.DynamicImport, ..acc])
    [opcode.IrDynamicImportSource, ..rest] ->
      resolve_ops(rest, labels, [opcode.DynamicImportSource, ..acc])
    [opcode.IrDynamicImportDefer, ..rest] ->
      resolve_ops(rest, labels, [opcode.DynamicImportDefer, ..acc])

    // Global Environment Record
    [IrDeclareGlobalVar(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.DeclareGlobalVar(name), ..acc])
    [IrDeclareGlobalLex(name, is_const), ..rest] ->
      resolve_ops(rest, labels, [opcode.DeclareGlobalLex(name, is_const), ..acc])
    [IrInitGlobalLex(name), ..rest] ->
      resolve_ops(rest, labels, [opcode.InitGlobalLex(name), ..acc])

    // Explicit Resource Management (using / await using desugar)
    [opcode.IrGetDisposer(is_async), ..rest] ->
      resolve_ops(rest, labels, [opcode.GetDisposer(is_async), ..acc])
    [opcode.IrMakeSuppressed, ..rest] ->
      resolve_ops(rest, labels, [opcode.MakeSuppressed, ..acc])
  }
}
