import arc/bytecode/binop
import arc/bytecode/key
import arc/bytecode/opcode.{
  type IrOp, type LabelId, IrBinOp, IrCmpConstJump, IrCmpJump,
  IrCmpLocalConstJump, IrCmpLocalLocalJump, IrFinal, IrGetField, IrGetFieldKeep,
  IrIncLocalCmpConstJump, IrIncLocalCmpLocalJump, IrIncLocalJump, IrJump,
  IrJumpIfFalse, IrJumpIfLocal, IrJumpIfTrue, IrLabel, IrLine, IrPutField,
}
import arc/rt/types
import gleam/list
import gleam/option.{type Option, None, Some}

// runs before labels become pcs so fusion can't break jumps
pub fn run(code: List(IrOp)) -> List(IrOp) {
  peephole(drop_dead_tdz_stores(code, []), [])
}

// a let or const slot need not be marked uninitialised when its first value
// is stored before anything could read it or jump
fn drop_dead_tdz_stores(code: List(IrOp), acc: List(IrOp)) -> List(IrOp) {
  case code {
    [] -> list.reverse(acc)
    [
      IrFinal(opcode.PushConst(c)) as push,
      IrFinal(opcode.PutLocal(slot)),
      ..rest
    ] ->
      case c == types.mk_tdz() && stored_before_use(rest, slot) {
        True -> drop_dead_tdz_stores(rest, acc)
        False ->
          drop_dead_tdz_stores(rest, [
            IrFinal(opcode.PutLocal(slot)),
            push,
            ..acc
          ])
      }
    [op, ..rest] -> drop_dead_tdz_stores(rest, [op, ..acc])
  }
}

// looks only past ops that run straight on and leave the slot alone
fn stored_before_use(code: List(IrOp), slot: Int) -> Bool {
  case code {
    [IrFinal(opcode.PutLocal(i)), ..rest] ->
      i == slot || stored_before_use(rest, slot)
    [IrFinal(opcode.GetLocal(i)), ..rest]
    | [IrFinal(opcode.GetBoxed(i)), ..rest]
    | [IrFinal(opcode.PutBoxed(i)), ..rest]
    | [IrFinal(opcode.BoxLocal(i)), ..rest] ->
      i != slot && stored_before_use(rest, slot)
    [IrLine(_), ..rest]
    | [IrBinOp(_), ..rest]
    | [IrGetField(_), ..rest]
    | [IrGetFieldKeep(_), ..rest]
    | [IrPutField(_), ..rest]
    | [opcode.IrDefineField(_), ..rest]
    | [IrFinal(opcode.PushConst(_)), ..rest]
    | [IrFinal(opcode.GetEnv(_)), ..rest]
    | [IrFinal(opcode.Pop), ..rest]
    | [IrFinal(opcode.Dup), ..rest]
    | [IrFinal(opcode.Swap), ..rest]
    | [IrFinal(opcode.Rot3), ..rest]
    | [IrFinal(opcode.UnaryOp(_)), ..rest]
    | [IrFinal(opcode.TypeOf), ..rest]
    | [IrFinal(opcode.GetGlobal(_)), ..rest]
    | [IrFinal(opcode.GetElem), ..rest]
    | [IrFinal(opcode.GetElemKeep), ..rest]
    | [IrFinal(opcode.PutElem), ..rest]
    | [IrFinal(opcode.GetPrivateFieldDyn), ..rest]
    | [IrFinal(opcode.GetPrivateFieldDynKeep), ..rest]
    | [IrFinal(opcode.NewObject), ..rest]
    | [IrFinal(opcode.NewObjectWith(..)), ..rest]
    | [IrFinal(opcode.NewObjectShaped(..)), ..rest]
    | [IrFinal(opcode.ArrayFrom(_)), ..rest]
    | [IrFinal(opcode.ToObject), ..rest]
    | [IrFinal(opcode.ToStringVal), ..rest]
    | [IrFinal(opcode.NewRegExp), ..rest]
    | [IrFinal(opcode.Call(_)), ..rest]
    | [IrFinal(opcode.CallMethod(_)), ..rest]
    | [IrFinal(opcode.CallNew(_)), ..rest]
    | [IrFinal(opcode.MakeClosure(_)), ..rest]
    | [IrFinal(opcode.MakeLazyClosure(..)), ..rest]
    | [IrFinal(opcode.Await), ..rest] -> stored_before_use(rest, slot)
    _ -> False
  }
}

fn peephole(code: List(IrOp), acc: List(IrOp)) -> List(IrOp) {
  case code {
    [] -> list.reverse(acc)

    // postfix update statement on a plain local
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
      let fused = case c == types.mk_int(1), kind {
        True, binop.Add -> Some(IrFinal(opcode.IncLocal(i)))
        True, binop.Sub -> Some(IrFinal(opcode.DecLocal(i)))
        _, _ -> None
      }
      case fused {
        Some(op) -> peephole(rest, [op, ..acc])
        None ->
          peephole(rest, [
            IrFinal(opcode.PutLocal(j)),
            IrBinOp(kind),
            IrFinal(opcode.PushConst(c)),
            IrFinal(opcode.UnaryOp(opcode.Pos)),
            IrFinal(opcode.GetLocal(i)),
            ..acc
          ])
      }
    }

    // prefix update on a plain local
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
      let fused = case c == types.mk_int(1), kind {
        True, binop.Add -> Some(IrFinal(opcode.IncLocal(i)))
        True, binop.Sub -> Some(IrFinal(opcode.DecLocal(i)))
        _, _ -> None
      }
      case fused, rest {
        Some(op), [IrFinal(opcode.Pop), ..rest] -> peephole(rest, [op, ..acc])
        Some(op), _ -> peephole([get, ..rest], [op, ..acc])
        None, _ -> peephole(list.drop(code, 1), [get, ..acc])
      }
    }

    // postfix update on a boxed local, drop dead dup/pop
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
      peephole(rest, [
        IrFinal(opcode.PutBoxed(j)),
        IrBinOp(kind),
        IrFinal(opcode.PushConst(c)),
        IrFinal(opcode.UnaryOp(opcode.Pos)),
        IrFinal(opcode.GetBoxed(i)),
        ..acc
      ])

    // dead dup under a discarded store
    [
      IrFinal(opcode.Dup),
      IrFinal(opcode.PutLocal(i)),
      IrFinal(opcode.Pop),
      ..rest
    ] -> peephole(rest, put_local(acc, i))
    [
      IrFinal(opcode.Dup),
      IrFinal(opcode.PutBoxed(i)),
      IrFinal(opcode.Pop),
      ..rest
    ] -> peephole(rest, [IrFinal(opcode.PutBoxed(i)), ..acc])

    // seed immediately overwritten
    [
      IrFinal(opcode.PushConst(_)),
      IrFinal(opcode.PutLocal(i)),
      IrFinal(opcode.PutLocal(j)),
      ..rest
    ]
      if i == j
    -> peephole(rest, put_local(acc, i))

    // compare and branch
    [
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.GetLocal(b)),
      IrBinOp(kind),
      ..rest
    ] ->
      case fusable_cmp(kind), rest {
        Some(pure), [IrJumpIfFalse(l) as j, ..rest]
        | Some(pure), [IrJumpIfTrue(l) as j, ..rest]
        -> {
          let when = j == IrJumpIfTrue(l)
          case stepped_local(acc, a) {
            Some(#(by, acc)) if a != b ->
              peephole(rest, [
                IrIncLocalCmpLocalJump(a, by, b, pure, l, when),
                ..acc
              ])
            _ ->
              peephole(rest, [IrCmpLocalLocalJump(a, b, pure, l, when), ..acc])
          }
        }
        _, _ ->
          peephole(rest, [IrFinal(opcode.BinOpLocalLocal(kind, a, b)), ..acc])
      }
    [
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.PushConst(c)),
      IrBinOp(kind),
      ..rest
    ] ->
      case fusable_cmp(kind), rest {
        Some(pure), [IrJumpIfFalse(l) as j, ..rest]
        | Some(pure), [IrJumpIfTrue(l) as j, ..rest]
        -> {
          let when = j == IrJumpIfTrue(l)
          case stepped_local(acc, a) {
            Some(#(by, acc)) ->
              peephole(rest, [
                IrIncLocalCmpConstJump(a, by, c, pure, l, when),
                ..acc
              ])
            None ->
              peephole(rest, [IrCmpLocalConstJump(a, c, pure, l, when), ..acc])
          }
        }
        _, _ ->
          peephole(rest, [IrFinal(opcode.BinOpLocalConst(kind, a, c)), ..acc])
      }

    // postfix update in value position
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
      let fused = case c == types.mk_int(1), kind {
        True, binop.Add -> Some(IrFinal(opcode.PostIncLocal(i)))
        True, binop.Sub -> Some(IrFinal(opcode.PostDecLocal(i)))
        _, _ -> None
      }
      case fused, acc, rest {
        Some(IrFinal(opcode.PostIncLocal(_))),
          [IrFinal(opcode.GetLocal(obj)), ..acc],
          [IrFinal(opcode.GetElem), ..rest]
          if obj != i
        -> peephole(rest, [IrFinal(opcode.GetElemPostInc(obj, i)), ..acc])
        Some(op), _, _ -> peephole(rest, [op, ..acc])
        None, _, _ ->
          peephole(list.drop(code, 1), [IrFinal(opcode.GetLocal(i)), ..acc])
      }
    }

    [IrFinal(opcode.GetLocal(i)), IrJumpIfFalse(l), ..rest] ->
      peephole(rest, [IrJumpIfLocal(i, l, when: False), ..acc])
    [IrFinal(opcode.GetLocal(i)), IrJumpIfTrue(l), ..rest] ->
      peephole(rest, [IrJumpIfLocal(i, l, when: True), ..acc])

    // binops with folded operand loads
    [IrFinal(opcode.GetLocal(i)), IrBinOp(kind), ..rest] ->
      peephole([IrFinal(opcode.BinOpLocal(kind, i)), ..rest], acc)
    [IrFinal(opcode.PushConst(c)), IrBinOp(kind), ..rest] ->
      peephole([IrFinal(opcode.BinOpConst(kind, c)), ..rest], acc)
    [IrFinal(opcode.PutLocal(i)), ..rest] -> peephole(rest, put_local(acc, i))
    [IrJump(l), ..rest] ->
      case lands_here(rest, l), acc {
        True, _ -> peephole(rest, acc)
        False, [IrFinal(opcode.IncLocal(i)), ..acc] ->
          peephole(rest, [IrIncLocalJump(i, l), ..acc])
        False, _ -> peephole(rest, [IrJump(l), ..acc])
      }
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
      ] -> peephole([value, ret, ..rest], acc)
    [IrBinOp(kind) as op, IrJumpIfFalse(l) as jump, ..rest]
    | [IrBinOp(kind) as op, IrJumpIfTrue(l) as jump, ..rest] ->
      case fusable_cmp(kind) {
        Some(pure) ->
          peephole(rest, [
            IrCmpJump(pure, l, when: jump == IrJumpIfTrue(l)),
            ..acc
          ])
        None -> peephole(rest, [jump, op, ..acc])
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
      case fusable_cmp(kind) {
        Some(pure) ->
          peephole(rest, [
            IrCmpConstJump(c, pure, l, when: jump == IrJumpIfTrue(l)),
            ..acc
          ])
        None -> peephole(rest, [jump, op, ..acc])
      }
    [
      IrFinal(opcode.GetLocal(obj)),
      IrFinal(opcode.GetLocal(k)),
      IrFinal(opcode.GetElem),
      ..rest
    ] -> peephole(rest, [IrFinal(opcode.GetElemLocals(obj, k)), ..acc])
    [IrFinal(opcode.PutElem), IrFinal(opcode.Pop), ..rest] ->
      peephole(rest, [IrFinal(opcode.PutElemPop), ..acc])
    [IrFinal(opcode.GetLocal(i)), IrGetField(name), IrBinOp(kind), ..rest] ->
      peephole(rest, [
        IrFinal(opcode.BinOpLocalField(kind, i, key.canonical(name))),
        ..acc
      ])

    // field access superinstructions
    [
      IrGetFieldKeep(name),
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.CallMethod(1)),
      ..rest
    ] ->
      case key.canonical(name) {
        key.Named(..) as k ->
          peephole(rest, [IrFinal(opcode.GetFieldCall1(k, a)), ..acc])
        k ->
          peephole(rest, [
            IrFinal(opcode.CallMethod(1)),
            IrFinal(opcode.GetLocal(a)),
            IrFinal(opcode.GetFieldKeep(k)),
            ..acc
          ])
      }
    [
      IrFinal(opcode.GetLocal(_)) as recv,
      IrGetFieldKeep(_) as get,
      IrFinal(opcode.GetLocal(_)) as arg,
      IrFinal(opcode.CallMethod(1)) as call,
      ..rest
    ] -> peephole([get, arg, call, ..rest], [recv, ..acc])
    [
      IrFinal(opcode.GetLocal(i)),
      IrGetFieldKeep(name),
      IrFinal(opcode.CallMethod(0)),
      ..rest
    ] ->
      case key.canonical(name) {
        key.Named(..) as k ->
          peephole(rest, [IrFinal(opcode.GetLocalFieldCall(i, k)), ..acc])
        k ->
          peephole(rest, [
            IrFinal(opcode.CallMethod(0)),
            IrFinal(opcode.GetLocalFieldKeep(i, k)),
            ..acc
          ])
      }
    [IrGetFieldKeep(name), IrFinal(opcode.CallMethod(0)), ..rest] ->
      case key.canonical(name) {
        key.Named(..) as k ->
          peephole(rest, [IrFinal(opcode.GetFieldCall(k)), ..acc])
        k ->
          peephole(rest, [
            IrFinal(opcode.CallMethod(0)),
            IrFinal(opcode.GetFieldKeep(k)),
            ..acc
          ])
      }
    [IrFinal(opcode.GetLocal(i)), IrGetField(name), ..rest] ->
      peephole(rest, [
        IrFinal(opcode.GetLocalField(i, key.canonical(name))),
        ..acc
      ])
    [IrFinal(opcode.GetLocal(i)), IrGetFieldKeep(name), ..rest] ->
      peephole(rest, [
        IrFinal(opcode.GetLocalFieldKeep(i, key.canonical(name))),
        ..acc
      ])
    [
      IrFinal(opcode.GetLocal(o)),
      IrFinal(opcode.GetLocal(v)),
      IrPutField(name),
      IrFinal(opcode.Pop),
      ..rest
    ] ->
      case key.canonical(name) {
        key.Named(..) as k ->
          peephole(rest, [IrFinal(opcode.PutLocalLocalField(o, v, k)), ..acc])
        k ->
          peephole(rest, [
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
      case key.canonical(name) {
        key.Named(..) as k ->
          peephole(rest, [IrFinal(opcode.PutLocalConstField(o, c, k)), ..acc])
        k ->
          peephole(rest, [
            IrFinal(opcode.PutFieldPop(k)),
            IrFinal(opcode.PushConst(c)),
            IrFinal(opcode.GetLocal(o)),
            ..acc
          ])
      }
    [IrPutField(name), IrFinal(opcode.Pop), ..rest] ->
      peephole(rest, [IrFinal(opcode.PutFieldPop(key.canonical(name))), ..acc])

    [op, ..rest] -> peephole(rest, [op, ..acc])
  }
}

fn stepped_local(acc: List(IrOp), i: Int) -> Option(#(Int, List(IrOp))) {
  case acc {
    [IrFinal(opcode.IncLocal(j)), ..acc] if i == j -> Some(#(1, acc))
    [IrFinal(opcode.DecLocal(j)), ..acc] if i == j -> Some(#(-1, acc))
    _ -> None
  }
}

fn put_local(acc: List(IrOp), dst: Int) -> List(IrOp) {
  case acc {
    [IrBinOp(kind), ..acc] -> [IrFinal(opcode.BinOpPut(kind, dst)), ..acc]
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

fn fusable_cmp(kind: binop.BinOp) -> Option(binop.BinOp) {
  case binop.is_comparison(kind) {
    True -> Some(kind)
    False -> None
  }
}

fn lands_here(rest: List(IrOp), l: LabelId) -> Bool {
  case rest {
    [IrLabel(id), ..] if id == l -> True
    [IrLabel(_), ..more] | [IrLine(_), ..more] -> lands_here(more, l)
    _ -> False
  }
}
