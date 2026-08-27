/// Phase 3: Label Resolution
///
/// Converts IrOp (with label IDs for jumps) into final Op (with absolute PC addresses).
/// Two-pass algorithm:
///   Pass 1: Walk IR, skip IrLabel / IrLine markers, build Dict(label_id → PC)
///   Pass 2: Walk IR, replace IrJump(label) → Jump(pc), drop the markers, fold
///           IrLine into the per-pc line table, translate all Ir* → Op
import arc/bytecode/binop
import arc/bytecode/key
import arc/bytecode/opcode.{
  type IrOp, type LabelId, type Op, type Pc, IrAsyncYieldStarNext,
  IrAsyncYieldStarResume, IrBinOp, IrCmpConstJump, IrCmpJump,
  IrCmpLocalConstJump, IrCmpLocalLocalJump, IrDefineAccessor, IrDefineField,
  IrDefineMethod, IrDeleteField, IrFinal, IrGetField, IrGetField2, IrGosub,
  IrIncLocalCmpConstJump, IrIncLocalCmpLocalJump, IrIncLocalJump, IrJump,
  IrJumpIfFalse, IrJumpIfLocal, IrJumpIfNotNullish, IrJumpIfNullish,
  IrJumpIfTrue, IrLabel, IrLine, IrPushTry, IrPutField, IrWithDeleteVar,
  IrWithGetRefValue, IrWithGetVar, IrWithGetVarThis, IrWithMakeRef,
  IrWithPutRefValue, IrWithPutVar, Pc,
}
import arc/internal/tuple_array
import arc/rt/types.{type JsVal, JInt}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

/// Run Phase 3 over one function body's IR: peephole-fuse, then resolve label
/// IDs to absolute PCs. Returns the runnable bytecode array, the constant
/// pool it indexes into (peephole reads constants, so the two are produced
/// together) and the per-PC source line table.
///
/// Variable access is already concrete (the emitter consults the scope tree
/// and emits GetLocal/GetBoxed/GetGlobal/IrWith* directly); only
/// IrLabel/IrJump/IrJumpIfFalse/IrJumpIfTrue/IrJumpIfNullish/IrPushTry (both
/// its catch target and a `Finally` kind's subroutine entry) still need
/// label→PC resolution.
///
/// Assembling the surrounding `FuncTemplate` is the CALLER's job
/// (`compiler.resolve_top_level` / `compiler.compile_child`): they own the
/// scope-analysis metadata every other template field comes from, so this
/// module never has to thread sixteen values through untouched.
pub fn resolve(code: List(IrOp), constants: List(JsVal)) -> Resolved {
  let const_arr = tuple_array.from_list(constants)
  let code = thread_jumps(code, label_suffixes(code, dict.new()), [])
  let code = drop_dead_labels(code, referenced_labels(code, set.new()), [])
  let code = peephole(code, const_arr, [])
  let label_map = build_label_map(code, 0, dict.new())
  let #(ops, lines) = resolve_ops(code, label_map, 0, [], [])
  Resolved(
    bytecode: tuple_array.from_list(ops),
    constants: const_arr,
    lines: tuple_array.from_list(lines),
  )
}

/// One function body after Phase 3: the runnable bytecode, the constant pool
/// it indexes, and the source line of each instruction (index = PC, 0 for
/// instructions ahead of the body's first statement marker).
pub type Resolved {
  Resolved(
    bytecode: tuple_array.TupleArray(Op),
    constants: tuple_array.TupleArray(JsVal),
    lines: tuple_array.TupleArray(Int),
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
///   1b. Prefix update on a plain local (GetLocal i; UnaryOp Pos;
///      PushConst 1; BinOp Add|Sub; Dup; PutLocal i) → IncLocal/DecLocal;
///      GetLocal i (or just IncLocal/DecLocal when a Pop follows).
///   3b. PushConst; PutLocal i; PutLocal i → PutLocal i (dead seed).
///   4. Compare-and-branch (GetLocal; GetLocal|PushConst;
///      BinOp Lt|LtEq|Gt|GtEq|==|!=|===|!==; JumpIfFalse|JumpIfTrue) →
///      CmpLocal*Jump.
///   5. GetLocal i; GetField k → GetLocalField, GetLocal i; GetField2 k →
///      GetLocalField2, PutField k; Pop → PutFieldPop, and
///      GetLocal o; GetLocal v | PushConst c; PutFieldPop k →
///      PutLocalLocalField / PutLocalConstField for a Named k, and
///      [GetLocal i;] GetField2 k; CallMethod 0 → GetFieldCall /
///      GetLocalFieldCall.
///   1c. Pattern 1's shape without the trailing Pop (the old value is used)
///      → PostIncLocal/PostDecLocal.
///   6. GetLocal/PushConst operand loads feeding a BinOp fold into
///      BinOpLocalLocal / BinOpLocalConst / BinOpLocal / BinOpConst, and a
///      PutLocal of the result into BinOpPut / BinOpConstPut /
///      BinOpLocalPut; GetLocal; GetField; BinOp → BinOpLocalField;
///      GetLocal; GetLocal; GetElem → GetElemLocals;
///      PutElem; Pop → PutElemPop; IncLocal; Jump → IncLocalJump;
///      IncLocal i; CmpLocal*Jump on i → IncLocalCmp*Jump (a counted
///      loop's bottom); a
///      comparison feeding JumpIfFalse|JumpIfTrue → CmpJump / CmpConstJump.
fn peephole(
  code: List(IrOp),
  consts: tuple_array.TupleArray(JsVal),
  acc: List(IrOp),
) -> List(IrOp) {
  case code {
    [] -> list.reverse(acc)

    // -- Pattern 1/2: postfix update statement on a plain local ----------
    [
      IrFinal(opcode.GetLocal(i)),
      IrFinal(opcode.UnaryOp(opcode.Pos)),
      IrFinal(opcode.Dup),
      IrFinal(opcode.PushConst(c)),
      IrBinOp(kind),
      IrFinal(opcode.PutLocal(j)),
      IrFinal(opcode.Pop),
      ..rest
    ]
      if i == j
    -> {
      let fused = case is_const_one(consts, c), kind {
        True, opcode.Add -> Some(IrFinal(opcode.IncLocal(i)))
        True, opcode.Sub -> Some(IrFinal(opcode.DecLocal(i)))
        _, _ -> None
      }
      case fused {
        Some(op) -> peephole(rest, consts, [op, ..acc])
        // Non-±1 update: still drop the dead Dup/Pop pair.
        None ->
          peephole(rest, consts, [
            IrFinal(opcode.PutLocal(j)),
            IrBinOp(kind),
            IrFinal(opcode.PushConst(c)),
            IrFinal(opcode.UnaryOp(opcode.Pos)),
            IrFinal(opcode.GetLocal(i)),
            ..acc
          ])
      }
    }

    // -- Pattern 1b: prefix update on a plain local ------------------------
    // GetLocal i; Pos; PushConst 1; Add|Sub; Dup; PutLocal i leaves the new
    // value on the stack: IncLocal i; GetLocal i is the same store and the
    // same value (nothing can observe slot i in between). The GetLocal is
    // fed back through the window so a following compare still fuses, and a
    // trailing Pop (statement position) drops with it.
    [
      IrFinal(opcode.GetLocal(i)) as get,
      IrFinal(opcode.UnaryOp(opcode.Pos)),
      IrFinal(opcode.PushConst(c)),
      IrBinOp(kind),
      IrFinal(opcode.Dup),
      IrFinal(opcode.PutLocal(j)),
      ..rest
    ]
      if i == j
    -> {
      let fused = case is_const_one(consts, c), kind {
        True, opcode.Add -> Some(IrFinal(opcode.IncLocal(i)))
        True, opcode.Sub -> Some(IrFinal(opcode.DecLocal(i)))
        _, _ -> None
      }
      case fused, rest {
        Some(op), [IrFinal(opcode.Pop), ..rest] ->
          peephole(rest, consts, [op, ..acc])
        Some(op), _ -> peephole([get, ..rest], consts, [op, ..acc])
        None, _ -> peephole(list.drop(code, 1), consts, [get, ..acc])
      }
    }

    // -- Pattern 2: postfix update statement on a boxed local ------------
    // Same shape via GetBoxed/PutBoxed: drop the dead Dup/Pop pair.
    [
      IrFinal(opcode.GetBoxed(i)),
      IrFinal(opcode.UnaryOp(opcode.Pos)),
      IrFinal(opcode.Dup),
      IrFinal(opcode.PushConst(c)),
      IrBinOp(kind),
      IrFinal(opcode.PutBoxed(j)),
      IrFinal(opcode.Pop),
      ..rest
    ]
      if i == j
    ->
      peephole(rest, consts, [
        IrFinal(opcode.PutBoxed(j)),
        IrBinOp(kind),
        IrFinal(opcode.PushConst(c)),
        IrFinal(opcode.UnaryOp(opcode.Pos)),
        IrFinal(opcode.GetBoxed(i)),
        ..acc
      ])

    // -- Pattern 3: dead Dup under a store whose value is discarded ------
    // Dup; PutLocal(i); Pop ≡ PutLocal(i) (likewise PutBoxed).
    [
      IrFinal(opcode.Dup),
      IrFinal(opcode.PutLocal(i)),
      IrFinal(opcode.Pop),
      ..rest
    ] -> peephole(rest, consts, put_local(acc, i))
    [
      IrFinal(opcode.Dup),
      IrFinal(opcode.PutBoxed(i)),
      IrFinal(opcode.Pop),
      ..rest
    ] -> peephole(rest, consts, [IrFinal(opcode.PutBoxed(i)), ..acc])

    // -- Pattern 3b: seed immediately overwritten -------------------------
    // PushConst c; PutLocal i; PutLocal i (a per-iteration TDZ seed followed
    // straight away by the binding's init) ≡ PutLocal i.
    [
      IrFinal(opcode.PushConst(_)),
      IrFinal(opcode.PutLocal(i)),
      IrFinal(opcode.PutLocal(j)),
      ..rest
    ]
      if i == j
    -> peephole(rest, consts, put_local(acc, i))

    // -- Pattern 4: fused compare-and-branch ------------------------------
    // A relational/equality BinOp feeding a conditional jump becomes one
    // Cmp*Jump (operands from two locals, a local and a constant, or the
    // stack); any other operator with in-place operands is pattern 6.
    [
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.GetLocal(b)),
      IrBinOp(kind),
      ..rest
    ] ->
      case fusable_cmp(kind), rest {
        Some(pure), [IrJumpIfFalse(l), ..rest] ->
          peephole(rest, consts, [
            IrCmpLocalLocalJump(a, b, pure, l, False),
            ..acc
          ])
        Some(pure), [IrJumpIfTrue(l), ..rest] ->
          case acc {
            [IrFinal(opcode.IncLocal(i)), ..acc] if i == a && i != b ->
              peephole(rest, consts, [
                IrIncLocalCmpLocalJump(a, b, pure, l, True),
                ..acc
              ])
            _ ->
              peephole(rest, consts, [
                IrCmpLocalLocalJump(a, b, pure, l, True),
                ..acc
              ])
          }
        _, _ ->
          peephole(rest, consts, [
            IrFinal(opcode.BinOpLocalLocal(opcode.classify(kind), a, b)),
            ..acc
          ])
      }
    [
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.PushConst(c)),
      IrBinOp(kind),
      ..rest
    ] ->
      case fusable_cmp(kind), rest {
        Some(pure), [IrJumpIfFalse(l), ..rest] ->
          peephole(rest, consts, [
            IrCmpLocalConstJump(a, c, pure, l, False),
            ..acc
          ])
        Some(pure), [IrJumpIfTrue(l), ..rest] ->
          case acc {
            [IrFinal(opcode.IncLocal(i)), ..acc] if i == a ->
              peephole(rest, consts, [
                IrIncLocalCmpConstJump(a, c, pure, l, True),
                ..acc
              ])
            _ ->
              peephole(rest, consts, [
                IrCmpLocalConstJump(a, c, pure, l, True),
                ..acc
              ])
          }
        _, _ ->
          peephole(rest, consts, [
            IrFinal(opcode.BinOpLocalConst(opcode.classify(kind), a, c)),
            ..acc
          ])
      }

    // -- Pattern 1c: postfix update on a plain local in value position ----
    // GetLocal i; Pos; Dup; PushConst 1; Add|Sub; PutLocal i (no Pop: the
    // old numeric value is used) → PostIncLocal/PostDecLocal.
    [
      IrFinal(opcode.GetLocal(i)),
      IrFinal(opcode.UnaryOp(opcode.Pos)),
      IrFinal(opcode.Dup),
      IrFinal(opcode.PushConst(c)),
      IrBinOp(kind),
      IrFinal(opcode.PutLocal(j)),
      ..rest
    ]
      if i == j
    -> {
      let fused = case is_const_one(consts, c), kind {
        True, opcode.Add -> Some(IrFinal(opcode.PostIncLocal(i)))
        True, opcode.Sub -> Some(IrFinal(opcode.PostDecLocal(i)))
        _, _ -> None
      }
      case fused {
        Some(op) -> peephole(rest, consts, [op, ..acc])
        None ->
          peephole(list.drop(code, 1), consts, [
            IrFinal(opcode.GetLocal(i)),
            ..acc
          ])
      }
    }

    // A plain local tested in place.
    [IrFinal(opcode.GetLocal(i)), IrJumpIfFalse(l), ..rest] ->
      peephole(rest, consts, [IrJumpIfLocal(i, l, False), ..acc])
    [IrFinal(opcode.GetLocal(i)), IrJumpIfTrue(l), ..rest] ->
      peephole(rest, consts, [IrJumpIfLocal(i, l, True), ..acc])

    // -- Pattern 6: binary operators with folded operand loads -----------
    [IrFinal(opcode.GetLocal(i)), IrBinOp(kind), ..rest] ->
      peephole(
        [IrFinal(opcode.BinOpLocal(opcode.classify(kind), i)), ..rest],
        consts,
        acc,
      )
    [IrFinal(opcode.PushConst(c)), IrBinOp(kind), ..rest] ->
      peephole(
        [IrFinal(opcode.BinOpConst(opcode.classify(kind), c)), ..rest],
        consts,
        acc,
      )
    // ... and with the result's store folded in.
    [IrFinal(opcode.PutLocal(i)), ..rest] ->
      peephole(rest, consts, put_local(acc, i))
    // A jump to the very next instruction is no jump; a loop's `i++`
    // update flowing into its back edge is one op.
    [IrJump(l), ..rest] ->
      case lands_here(rest, l), acc {
        True, _ -> peephole(rest, consts, acc)
        False, [IrFinal(opcode.IncLocal(i)), ..acc] ->
          peephole(rest, consts, [IrIncLocalJump(i, l), ..acc])
        False, _ -> peephole(rest, consts, [IrJump(l), ..acc])
      }
    // Return reads only the top of the stack: a Pop right before the
    // returned value's load is dead.
    [
      IrFinal(opcode.Pop),
      IrFinal(opcode.PushConst(_)) as value,
      IrFinal(opcode.Return) as ret,
      ..rest
    ]
    | [
        IrFinal(opcode.Pop),
        IrFinal(opcode.GetLocal(_)) as value,
        IrFinal(opcode.Return) as ret,
        ..rest
      ] -> peephole([value, ret, ..rest], consts, acc)
    // ... and with a conditional branch on the result folded in.
    [IrBinOp(kind) as op, IrJumpIfFalse(l) as jump, ..rest]
    | [IrBinOp(kind) as op, IrJumpIfTrue(l) as jump, ..rest] ->
      case fusable_cmp(kind) {
        Some(pure) ->
          peephole(rest, consts, [
            IrCmpJump(pure, l, jump == IrJumpIfTrue(l)),
            ..acc
          ])
        None -> peephole(rest, consts, [jump, op, ..acc])
      }
    [
      IrFinal(opcode.BinOpConst(kind, c)) as op,
      IrJumpIfFalse(l) as jump,
      ..rest
    ]
    | [
        IrFinal(opcode.BinOpConst(kind, c)) as op,
        IrJumpIfTrue(l) as jump,
        ..rest
      ] ->
      case kind {
        opcode.PureOp(binop.Compare(_) as pure)
        | opcode.PureOp(binop.Equality(_) as pure) ->
          peephole(rest, consts, [
            IrCmpConstJump(c, pure, l, jump == IrJumpIfTrue(l)),
            ..acc
          ])
        _ -> peephole(rest, consts, [jump, op, ..acc])
      }
    [
      IrFinal(opcode.GetLocal(obj)),
      IrFinal(opcode.GetLocal(k)),
      IrFinal(opcode.GetElem),
      ..rest
    ] -> peephole(rest, consts, [IrFinal(opcode.GetElemLocals(obj, k)), ..acc])
    [IrFinal(opcode.PutElem), IrFinal(opcode.Pop), ..rest] ->
      peephole(rest, consts, [IrFinal(opcode.PutElemPop), ..acc])
    [IrFinal(opcode.GetLocal(i)), IrGetField(name), IrBinOp(kind), ..rest] ->
      peephole(rest, consts, [
        IrFinal(opcode.BinOpLocalField(
          opcode.classify(kind),
          i,
          key.canonical_key(name),
        )),
        ..acc
      ])

    // -- Pattern 5: field access superinstructions -----------------------
    [
      IrGetField2(name),
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.CallMethod(1)),
      ..rest
    ] ->
      case key.canonical_key(name) {
        key.Named(_) as k ->
          peephole(rest, consts, [IrFinal(opcode.GetFieldCall1(k, a)), ..acc])
        k ->
          peephole(rest, consts, [
            IrFinal(opcode.CallMethod(1)),
            IrFinal(opcode.GetLocal(a)),
            IrFinal(opcode.GetField2(k)),
            ..acc
          ])
      }
    [
      IrFinal(opcode.GetLocal(_)) as recv,
      IrGetField2(_) as get,
      IrFinal(opcode.GetLocal(_)) as arg,
      IrFinal(opcode.CallMethod(1)) as call,
      ..rest
    ] -> peephole([get, arg, call, ..rest], consts, [recv, ..acc])
    [
      IrFinal(opcode.GetLocal(i)),
      IrGetField2(name),
      IrFinal(opcode.CallMethod(0)),
      ..rest
    ] ->
      case key.canonical_key(name) {
        key.Named(_) as k ->
          peephole(rest, consts, [
            IrFinal(opcode.GetLocalFieldCall(i, k)),
            ..acc
          ])
        k ->
          peephole(rest, consts, [
            IrFinal(opcode.CallMethod(0)),
            IrFinal(opcode.GetLocalField2(i, k)),
            ..acc
          ])
      }
    [IrGetField2(name), IrFinal(opcode.CallMethod(0)), ..rest] ->
      case key.canonical_key(name) {
        key.Named(_) as k ->
          peephole(rest, consts, [IrFinal(opcode.GetFieldCall(k)), ..acc])
        k ->
          peephole(rest, consts, [
            IrFinal(opcode.CallMethod(0)),
            IrFinal(opcode.GetField2(k)),
            ..acc
          ])
      }
    [IrFinal(opcode.GetLocal(i)), IrGetField(name), ..rest] ->
      peephole(rest, consts, [
        IrFinal(opcode.GetLocalField(i, key.canonical_key(name))),
        ..acc
      ])
    [IrFinal(opcode.GetLocal(i)), IrGetField2(name), ..rest] ->
      peephole(rest, consts, [
        IrFinal(opcode.GetLocalField2(i, key.canonical_key(name))),
        ..acc
      ])
    [
      IrFinal(opcode.GetLocal(o)),
      IrFinal(opcode.GetLocal(v)),
      IrPutField(name),
      IrFinal(opcode.Pop),
      ..rest
    ] ->
      case key.canonical_key(name) {
        key.Named(_) as k ->
          peephole(rest, consts, [
            IrFinal(opcode.PutLocalLocalField(o, v, k)),
            ..acc
          ])
        k ->
          peephole(rest, consts, [
            IrFinal(opcode.PutFieldPop(k)),
            IrFinal(opcode.GetLocal(v)),
            IrFinal(opcode.GetLocal(o)),
            ..acc
          ])
      }
    [
      IrFinal(opcode.GetLocal(o)),
      IrFinal(opcode.PushConst(c)),
      IrPutField(name),
      IrFinal(opcode.Pop),
      ..rest
    ] ->
      case key.canonical_key(name) {
        key.Named(_) as k ->
          peephole(rest, consts, [
            IrFinal(opcode.PutLocalConstField(o, c, k)),
            ..acc
          ])
        k ->
          peephole(rest, consts, [
            IrFinal(opcode.PutFieldPop(k)),
            IrFinal(opcode.PushConst(c)),
            IrFinal(opcode.GetLocal(o)),
            ..acc
          ])
      }
    [IrPutField(name), IrFinal(opcode.Pop), ..rest] ->
      peephole(rest, consts, [
        IrFinal(opcode.PutFieldPop(key.canonical_key(name))),
        ..acc
      ])

    [op, ..rest] -> peephole(rest, consts, [op, ..acc])
  }
}

/// `acc` (the ops emitted so far, last first) with a PutLocal(dst) appended,
/// folded into a preceding BinOp / BinOpConst / BinOpLocal /
/// BinOpLocalLocal as its store.
fn put_local(acc: List(IrOp), dst: Int) -> List(IrOp) {
  case acc {
    [IrBinOp(kind), ..acc] -> [
      IrFinal(opcode.BinOpPut(opcode.classify(kind), dst)),
      ..acc
    ]
    [IrFinal(opcode.BinOpConst(kind, c)), ..acc] -> [
      IrFinal(opcode.BinOpConstPut(kind, c, dst)),
      ..acc
    ]
    [IrFinal(opcode.BinOpLocal(kind, i)), ..acc] -> [
      IrFinal(opcode.BinOpLocalPut(kind, i, dst)),
      ..acc
    ]
    [IrFinal(opcode.BinOpLocalLocal(kind, a, b)), ..acc] -> [
      IrFinal(opcode.BinOpLocalLocalPut(kind, a, b, dst)),
      ..acc
    ]
    _ -> [IrFinal(opcode.PutLocal(dst)), ..acc]
  }
}

/// Only the relational and equality kinds are fused: their step semantics
/// are exactly the guarded pure-binop path (no In/InstanceOf heap access, no
/// Add string-concat split). Returns the narrowed `PureBinOp` the fused
/// opcode carries, so the fusion cannot smuggle an operator the fused step
/// handler can't run.
pub fn fusable_cmp(kind: opcode.BinOpKind) -> Option(binop.PureBinOp) {
  case kind {
    opcode.Lt -> Some(binop.Compare(binop.LtCmp))
    opcode.LtEq -> Some(binop.Compare(binop.LtEqCmp))
    opcode.Gt -> Some(binop.Compare(binop.GtCmp))
    opcode.GtEq -> Some(binop.Compare(binop.GtEqCmp))
    opcode.StrictEq -> Some(binop.Equality(binop.StrictEqOp))
    opcode.StrictNotEq -> Some(binop.Equality(binop.StrictNotEqOp))
    opcode.Eq -> Some(binop.Equality(binop.EqOp))
    opcode.NotEq -> Some(binop.Equality(binop.NotEqOp))
    _ -> None
  }
}

fn is_const_one(consts: tuple_array.TupleArray(JsVal), index: Int) -> Bool {
  tuple_array.get_unchecked(index, consts) == types.mk_number(JInt(1))
}

/// Pass 1: Walk the IR, counting real ops and recording label positions.
/// `rest` begins (past markers) at label `l`.
fn lands_here(rest: List(IrOp), l: LabelId) -> Bool {
  case rest {
    [IrLabel(id), ..] if id == l -> True
    [IrLabel(_), ..more] | [IrLine(_), ..more] -> lands_here(more, l)
    _ -> False
  }
}

/// Jump threading, done ahead of the peephole pass so the labels such jumps
/// kept alive can go and the ops around them fuse: `Jump L` where L (through
/// a chain of unconditional jumps) lands on a frame exit becomes that exit,
/// and otherwise targets the end of the chain.
fn thread_jumps(
  code: List(IrOp),
  suffixes: Dict(LabelId, List(IrOp)),
  acc: List(IrOp),
) -> List(IrOp) {
  case code {
    [] -> list.reverse(acc)
    [IrJump(l), ..rest] -> {
      let target = follow(suffixes, l, 0)
      let acc = case dict.get(suffixes, target) {
        Ok([IrFinal(opcode.Return) as ret, ..]) -> [ret, ..acc]
        Ok([
          IrFinal(opcode.PushConst(_)) as c,
          IrFinal(opcode.Return) as ret,
          ..
        ]) -> [ret, c, ..acc]
        _ -> [IrJump(target), ..acc]
      }
      thread_jumps(rest, suffixes, acc)
    }
    [op, ..rest] -> thread_jumps(rest, suffixes, [op, ..acc])
  }
}

/// The label a chain of unconditional jumps starting at `label` ends on.
fn follow(
  suffixes: Dict(LabelId, List(IrOp)),
  label: LabelId,
  hops: Int,
) -> LabelId {
  case dict.get(suffixes, label) {
    Ok([IrJump(next), ..]) if hops < 8 -> follow(suffixes, next, hops + 1)
    _ -> label
  }
}

/// Map each label to the first real instruction after it (markers occupy no
/// slot, so a run of them all share one suffix).
fn label_suffixes(
  code: List(IrOp),
  map: Dict(LabelId, List(IrOp)),
) -> Dict(LabelId, List(IrOp)) {
  case code {
    [] -> map
    [IrLabel(id), ..rest] ->
      label_suffixes(rest, dict.insert(map, id, skip_markers(rest)))
    [_, ..rest] -> label_suffixes(rest, map)
  }
}

fn skip_markers(code: List(IrOp)) -> List(IrOp) {
  case code {
    [IrLabel(_), ..rest] | [IrLine(_), ..rest] -> skip_markers(rest)
    _ -> code
  }
}

/// Every label some instruction still targets.
fn referenced_labels(code: List(IrOp), acc: Set(LabelId)) -> Set(LabelId) {
  case code {
    [] -> acc
    [op, ..rest] ->
      referenced_labels(rest, list.fold(label_refs(op), acc, set.insert))
  }
}

fn label_refs(op: IrOp) -> List(LabelId) {
  case op {
    IrJump(l)
    | IrJumpIfFalse(l)
    | IrJumpIfTrue(l)
    | IrJumpIfNullish(l)
    | IrJumpIfNotNullish(l)
    | IrGosub(l)
    | IrAsyncYieldStarNext(l)
    | IrAsyncYieldStarResume(l)
    | IrWithGetVar(_, l)
    | IrWithGetVarThis(_, l)
    | IrWithPutVar(_, l)
    | IrWithDeleteVar(_, l)
    | IrWithMakeRef(_, l)
    | IrWithGetRefValue(_, l)
    | IrWithPutRefValue(_, l)
    | IrCmpLocalLocalJump(label: l, ..)
    | IrCmpLocalConstJump(label: l, ..)
    | IrCmpJump(label: l, ..)
    | IrCmpConstJump(label: l, ..)
    | IrIncLocalJump(label: l, ..)
    | IrIncLocalCmpConstJump(label: l, ..)
    | IrIncLocalCmpLocalJump(label: l, ..)
    | IrJumpIfLocal(label: l, ..)
    | IrPushTry(l, opcode.CatchOnly)
    | IrPushTry(l, opcode.IterCloseGuard) -> [l]
    IrPushTry(l, opcode.Finally(fin)) -> [l, fin]
    IrFinal(_)
    | IrLabel(_)
    | IrLine(_)
    | IrGetField(_)
    | IrGetField2(_)
    | IrPutField(_)
    | IrDeleteField(_)
    | IrDefineField(_)
    | IrDefineMethod(_)
    | IrDefineAccessor(..)
    | IrBinOp(_) -> []
  }
}

/// A label nothing targets only gets in the way of the peephole window.
fn drop_dead_labels(
  code: List(IrOp),
  live: Set(LabelId),
  acc: List(IrOp),
) -> List(IrOp) {
  case code {
    [] -> list.reverse(acc)
    [IrLabel(l) as op, ..rest] ->
      case set.contains(live, l) {
        True -> drop_dead_labels(rest, live, [op, ..acc])
        False -> drop_dead_labels(rest, live, acc)
      }
    [op, ..rest] -> drop_dead_labels(rest, live, [op, ..acc])
  }
}

fn build_label_map(
  code: List(IrOp),
  pc: Int,
  map: Dict(LabelId, Pc),
) -> Dict(LabelId, Pc) {
  case code {
    [] -> map
    [IrLabel(id), ..rest] ->
      // Labels don't occupy a PC slot
      build_label_map(rest, pc, dict.insert(map, id, Pc(pc)))
    // Nor do line markers
    [IrLine(_), ..rest] -> build_label_map(rest, pc, map)
    [_, ..rest] ->
      // All other ops occupy one PC slot
      build_label_map(rest, pc + 1, map)
  }
}

/// Resolve a label id to its PC; crashes if the emitter forgot to place it.
fn label_pc(labels: Dict(LabelId, Pc), label: LabelId) -> Pc {
  let assert Ok(pc) = dict.get(labels, label) as "unbound label"
  pc
}

/// The ONLY bridge from an IR `TryKind(LabelId)` to a bytecode `TryKind(Pc)`.
/// The only label a `TryKind` can carry is `Finally`'s finally-subroutine
/// entry, resolved here like any other target; skipping this function is a
/// type error, since `opcode.PushTry` accepts nothing but a `TryKind(Pc)`.
fn resolve_try_kind(
  labels: Dict(LabelId, Pc),
  kind: opcode.TryKind(LabelId),
) -> opcode.TryKind(Pc) {
  case kind {
    opcode.Finally(fin_label:) -> opcode.Finally(label_pc(labels, fin_label))
    opcode.CatchOnly -> opcode.CatchOnly
    opcode.IterCloseGuard -> opcode.IterCloseGuard
  }
}

/// Pass 2: Walk the IR, resolve labels to PCs, translate IrOp → Op, and
/// fold the `IrLine` markers into the per-pc line table.
/// Appends a sentinel Return at the end so the interpreter's fetch loop
/// can use unchecked element/2 — termination happens via normal Return
/// dispatch instead of Option/None detection on every instruction.
fn resolve_ops(
  code: List(IrOp),
  labels: Dict(LabelId, Pc),
  line: Int,
  acc: List(Op),
  lines: List(Int),
) -> #(List(Op), List(Int)) {
  case code {
    [] -> #(list.reverse([opcode.Return, ..acc]), list.reverse([line, ..lines]))
    // Labels are dropped (they were just markers)
    [IrLabel(_), ..rest] -> resolve_ops(rest, labels, line, acc, lines)
    // A line marker sets the line of the ops after it
    [IrLine(l), ..rest] -> resolve_ops(rest, labels, l, acc, lines)
    [op, ..rest] ->
      resolve_ops(rest, labels, line, [resolve_op(op, labels), ..acc], [
        line,
        ..lines
      ])
  }
}

/// Translate one pc-occupying IrOp → Op. Every opcode that needs no
/// resolution rides through as `IrFinal(op)` and is simply unwrapped, so
/// this only ever has to know about the handful of IR-only variants below.
fn resolve_op(op: IrOp, labels: Dict(LabelId, Pc)) -> Op {
  case op {
    // Already-final: nothing to resolve. A PC-carrying op (Jump, PushTry, …)
    // is unrepresentable here — every such Op field is a `Pc`, and the emitter
    // only ever holds `LabelId`s (via `emit.fresh_label`), so the type checker
    // rejects an `IrFinal(Jump(..))` at the emit site.
    IrFinal(op) -> op

    // Jump ops: resolve label → PC
    IrJump(l) -> opcode.Jump(label_pc(labels, l))
    IrJumpIfFalse(l) -> opcode.JumpIfFalse(label_pc(labels, l))
    IrJumpIfTrue(l) -> opcode.JumpIfTrue(label_pc(labels, l))
    IrJumpIfNullish(l) -> opcode.JumpIfNullish(label_pc(labels, l))
    IrJumpIfNotNullish(l) -> opcode.JumpIfNotNullish(label_pc(labels, l))
    IrPushTry(l, kind) ->
      opcode.PushTry(label_pc(labels, l), resolve_try_kind(labels, kind))
    IrGosub(l) -> opcode.Gosub(label_pc(labels, l))
    IrAsyncYieldStarNext(l) -> opcode.AsyncYieldStarNext(label_pc(labels, l))
    IrAsyncYieldStarResume(l) ->
      opcode.AsyncYieldStarResume(label_pc(labels, l))

    // `with`-object access: label-carrying, resolved like jumps
    IrWithGetVar(name, l) -> opcode.WithGetVar(name, label_pc(labels, l))
    IrWithGetVarThis(name, l) ->
      opcode.WithGetVarThis(name, label_pc(labels, l))
    IrWithPutVar(name, l) -> opcode.WithPutVar(name, label_pc(labels, l))
    IrWithDeleteVar(name, l) -> opcode.WithDeleteVar(name, label_pc(labels, l))
    IrWithMakeRef(name, l) -> opcode.WithMakeRef(name, label_pc(labels, l))
    IrWithGetRefValue(name, l) ->
      opcode.WithGetRefValue(name, label_pc(labels, l))
    IrWithPutRefValue(name, l) ->
      opcode.WithPutRefValue(name, label_pc(labels, l))

    // Static property access — precompute the canonical key once
    // (key.canonical_key, THE canonicalizer shared with the runtime) so the
    // interpreter never re-parses the constant string per dispatch.
    IrGetField(name) -> opcode.GetField(key.canonical_key(name))
    IrGetField2(name) -> opcode.GetField2(key.canonical_key(name))
    IrPutField(name) -> opcode.PutField(key.canonical_key(name))
    IrDeleteField(name) -> opcode.DeleteField(key.canonical_key(name))
    IrDefineField(name) -> opcode.DefineField(key.canonical_key(name))
    IrDefineMethod(name) -> opcode.DefineMethod(key.canonical_key(name))
    IrDefineAccessor(name, kind, enumerable) ->
      opcode.DefineAccessor(key.canonical_key(name), kind, enumerable)

    // Narrow the operator to its handler HERE, once per instruction resolved,
    // rather than once per instruction executed.
    IrBinOp(kind) -> opcode.bin_op(kind)

    // Fused superinstructions (created by the peephole pass above)
    IrCmpLocalLocalJump(a, b, kind, l, when) ->
      opcode.CmpLocalLocalJump(a, b, kind, label_pc(labels, l), when)
    IrCmpLocalConstJump(a, c, kind, l, when) ->
      opcode.CmpLocalConstJump(a, c, kind, label_pc(labels, l), when)
    IrIncLocalJump(i, l) -> opcode.IncLocalJump(i, label_pc(labels, l))
    IrJumpIfLocal(i, l, when) ->
      opcode.JumpIfLocal(i, label_pc(labels, l), when)
    IrIncLocalCmpConstJump(i, c, kind, l, when) ->
      opcode.IncLocalCmpConstJump(i, c, kind, label_pc(labels, l), when)
    IrIncLocalCmpLocalJump(i, b, kind, l, when) ->
      opcode.IncLocalCmpLocalJump(i, b, kind, label_pc(labels, l), when)
    IrCmpJump(kind, l, when) -> opcode.CmpJump(kind, label_pc(labels, l), when)
    IrCmpConstJump(c, kind, l, when) ->
      opcode.CmpConstJump(c, kind, label_pc(labels, l), when)
    IrLabel(_) | IrLine(_) -> panic as "marker occupies no pc"
  }
}
