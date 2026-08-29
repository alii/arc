import arc/bytecode/binop
import arc/bytecode/key.{type PropertyKey}
import arc/bytecode/opcode.{
  type IrOp, type LabelId, type Op, type Pc, IrAsyncYieldStarNext,
  IrAsyncYieldStarResume, IrBinOp, IrCmpConstJump, IrCmpJump,
  IrCmpLocalConstJump, IrCmpLocalLocalJump, IrFinal, IrGosub,
  IrIncLocalCmpConstJump, IrIncLocalCmpLocalJump, IrIncLocalJump, IrJump,
  IrJumpIfFalse, IrJumpIfLocal, IrJumpIfNotNullish, IrJumpIfNullish,
  IrJumpIfTrue, IrLabel, IrLine, IrPushTry, IrWithDeleteVar, IrWithGetRefValue,
  IrWithGetVar, IrWithGetVarThis, IrWithMakeRef, IrWithPutRefValue, IrWithPutVar,
  Pc,
}
import arc/internal/tuple_array
import arc/rt/bytecode
import arc/rt/types.{type JsVal, JInt}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub fn resolve(
  code: List(IrOp),
  constants: List(JsVal),
  keys: List(PropertyKey),
) -> Resolved {
  let const_arr = tuple_array.from_list(constants)
  let keys_arr = tuple_array.from_list(keys)
  let code = thread_jumps(code, label_suffixes(code, dict.new()), [])
  let code = drop_dead_labels(code, referenced_labels(code, set.new()), [])
  let code = peephole(code, const_arr, keys_arr, [])
  let label_map = build_label_map(code, 0, dict.new())
  let #(ops, lines) = resolve_ops(code, label_map, 0, [], [])
  Resolved(
    bytecode: tuple_array.from_list(ops),
    constants: const_arr,
    keys: keys_arr,
    lines: tuple_array.from_list(lines),
  )
}

pub type Resolved {
  Resolved(
    bytecode: tuple_array.TupleArray(Op),
    constants: tuple_array.TupleArray(JsVal),
    keys: tuple_array.TupleArray(PropertyKey),
    lines: tuple_array.TupleArray(Int),
  )
}

fn is_named(keys: tuple_array.TupleArray(PropertyKey), slot: Int) -> Bool {
  case tuple_array.get_unchecked(slot, keys) {
    key.Named(_) -> True
    key.Index(_) | key.Private(_) -> False
  }
}

/// runs before label resolution so fusion can't break jumps
fn peephole(
  code: List(IrOp),
  consts: tuple_array.TupleArray(JsVal),
  keys: tuple_array.TupleArray(PropertyKey),
  acc: List(IrOp),
) -> List(IrOp) {
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
      let fused = case is_const_one(consts, c), kind {
        True, opcode.Add -> Some(IrFinal(opcode.IncLocal(i)))
        True, opcode.Sub -> Some(IrFinal(opcode.DecLocal(i)))
        _, _ -> None
      }
      case fused {
        Some(op) -> peephole(rest, consts, keys, [op, ..acc])
        None ->
          peephole(rest, consts, keys, [
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
      let fused = case is_const_one(consts, c), kind {
        True, opcode.Add -> Some(IrFinal(opcode.IncLocal(i)))
        True, opcode.Sub -> Some(IrFinal(opcode.DecLocal(i)))
        _, _ -> None
      }
      case fused, rest {
        Some(op), [IrFinal(opcode.Pop), ..rest] ->
          peephole(rest, consts, keys, [op, ..acc])
        Some(op), _ -> peephole([get, ..rest], consts, keys, [op, ..acc])
        None, _ -> peephole(list.drop(code, 1), consts, keys, [get, ..acc])
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
      peephole(rest, consts, keys, [
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
    ] -> peephole(rest, consts, keys, put_local(acc, i))
    [
      IrFinal(opcode.Dup),
      IrFinal(opcode.PutBoxed(i)),
      IrFinal(opcode.Pop),
      ..rest
    ] -> peephole(rest, consts, keys, [IrFinal(opcode.PutBoxed(i)), ..acc])

    // seed immediately overwritten
    [
      IrFinal(opcode.PushConst(_)),
      IrFinal(opcode.PutLocal(i)),
      IrFinal(opcode.PutLocal(j)),
      ..rest
    ]
      if i == j
    -> peephole(rest, consts, keys, put_local(acc, i))

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
              peephole(rest, consts, keys, [
                IrIncLocalCmpLocalJump(a, by, b, pure, l, when),
                ..acc
              ])
            _ ->
              peephole(rest, consts, keys, [
                IrCmpLocalLocalJump(a, b, pure, l, when),
                ..acc
              ])
          }
        }
        _, _ ->
          peephole(rest, consts, keys, [
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
        Some(pure), [IrJumpIfFalse(l) as j, ..rest]
        | Some(pure), [IrJumpIfTrue(l) as j, ..rest]
        -> {
          let when = j == IrJumpIfTrue(l)
          case stepped_local(acc, a) {
            Some(#(by, acc)) ->
              peephole(rest, consts, keys, [
                IrIncLocalCmpConstJump(a, by, c, pure, l, when),
                ..acc
              ])
            None ->
              peephole(rest, consts, keys, [
                IrCmpLocalConstJump(a, c, pure, l, when),
                ..acc
              ])
          }
        }
        _, _ ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.BinOpLocalConst(opcode.classify(kind), a, c)),
            ..acc
          ])
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
      let fused = case is_const_one(consts, c), kind {
        True, opcode.Add -> Some(IrFinal(opcode.PostIncLocal(i)))
        True, opcode.Sub -> Some(IrFinal(opcode.PostDecLocal(i)))
        _, _ -> None
      }
      case fused, acc, rest {
        Some(IrFinal(opcode.PostIncLocal(_))),
          [IrFinal(opcode.GetLocal(obj)), ..acc],
          [IrFinal(opcode.GetElem), ..rest]
          if obj != i
        ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.GetElemPostInc(obj, i)),
            ..acc
          ])
        Some(op), _, _ -> peephole(rest, consts, keys, [op, ..acc])
        None, _, _ ->
          peephole(list.drop(code, 1), consts, keys, [
            IrFinal(opcode.GetLocal(i)),
            ..acc
          ])
      }
    }

    [IrFinal(opcode.GetLocal(i)), IrJumpIfFalse(l), ..rest] ->
      peephole(rest, consts, keys, [IrJumpIfLocal(i, l, False), ..acc])
    [IrFinal(opcode.GetLocal(i)), IrJumpIfTrue(l), ..rest] ->
      peephole(rest, consts, keys, [IrJumpIfLocal(i, l, True), ..acc])

    // binops with folded operand loads
    [IrFinal(opcode.GetLocal(i)), IrBinOp(kind), ..rest] ->
      peephole(
        [IrFinal(opcode.BinOpLocal(opcode.classify(kind), i)), ..rest],
        consts,
        keys,
        acc,
      )
    [IrFinal(opcode.PushConst(c)), IrBinOp(kind), ..rest] ->
      peephole(
        [IrFinal(opcode.BinOpConst(opcode.classify(kind), c)), ..rest],
        consts,
        keys,
        acc,
      )
    [IrFinal(opcode.PutLocal(i)), ..rest] ->
      peephole(rest, consts, keys, put_local(acc, i))
    [IrJump(l), ..rest] ->
      case lands_here(rest, l), acc {
        True, _ -> peephole(rest, consts, keys, acc)
        False, [IrFinal(opcode.IncLocal(i)), ..acc] ->
          peephole(rest, consts, keys, [IrIncLocalJump(i, l), ..acc])
        False, _ -> peephole(rest, consts, keys, [IrJump(l), ..acc])
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
      ] -> peephole([value, ret, ..rest], consts, keys, acc)
    [IrBinOp(kind) as op, IrJumpIfFalse(l) as jump, ..rest]
    | [IrBinOp(kind) as op, IrJumpIfTrue(l) as jump, ..rest] ->
      case fusable_cmp(kind) {
        Some(pure) ->
          peephole(rest, consts, keys, [
            IrCmpJump(pure, l, jump == IrJumpIfTrue(l)),
            ..acc
          ])
        None -> peephole(rest, consts, keys, [jump, op, ..acc])
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
          peephole(rest, consts, keys, [
            IrCmpConstJump(c, pure, l, jump == IrJumpIfTrue(l)),
            ..acc
          ])
        _ -> peephole(rest, consts, keys, [jump, op, ..acc])
      }
    [
      IrFinal(opcode.GetLocal(obj)),
      IrFinal(opcode.GetLocal(k)),
      IrFinal(opcode.GetElem),
      ..rest
    ] ->
      peephole(rest, consts, keys, [
        IrFinal(opcode.GetElemLocals(obj, k)),
        ..acc
      ])
    [IrFinal(opcode.PutElem), IrFinal(opcode.Pop), ..rest] ->
      peephole(rest, consts, keys, [IrFinal(opcode.PutElemPop), ..acc])
    [
      IrFinal(opcode.GetLocal(i)),
      IrFinal(opcode.GetField(k)),
      IrBinOp(kind),
      ..rest
    ] ->
      peephole(rest, consts, keys, [
        IrFinal(opcode.BinOpLocalField(opcode.classify(kind), i, k)),
        ..acc
      ])

    // field access superinstructions
    [
      IrFinal(opcode.GetField2(k)),
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.CallMethod(1)),
      ..rest
    ] ->
      case is_named(keys, k) {
        True ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.GetFieldCall1(k, a)),
            ..acc
          ])
        False ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.CallMethod(1)),
            IrFinal(opcode.GetLocal(a)),
            IrFinal(opcode.GetField2(k)),
            ..acc
          ])
      }
    [
      IrFinal(opcode.GetLocal(_)) as recv,
      IrFinal(opcode.GetField2(_)) as get,
      IrFinal(opcode.GetLocal(_)) as arg,
      IrFinal(opcode.CallMethod(1)) as call,
      ..rest
    ] -> peephole([get, arg, call, ..rest], consts, keys, [recv, ..acc])
    [
      IrFinal(opcode.GetLocal(i)),
      IrFinal(opcode.GetField2(k)),
      IrFinal(opcode.CallMethod(0)),
      ..rest
    ] ->
      case is_named(keys, k) {
        True ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.GetLocalFieldCall(i, k)),
            ..acc
          ])
        False ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.CallMethod(0)),
            IrFinal(opcode.GetLocalField2(i, k)),
            ..acc
          ])
      }
    [IrFinal(opcode.GetField2(k)), IrFinal(opcode.CallMethod(0)), ..rest] ->
      case is_named(keys, k) {
        True ->
          peephole(rest, consts, keys, [IrFinal(opcode.GetFieldCall(k)), ..acc])
        False ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.CallMethod(0)),
            IrFinal(opcode.GetField2(k)),
            ..acc
          ])
      }
    [IrFinal(opcode.GetLocal(i)), IrFinal(opcode.GetField(k)), ..rest] ->
      peephole(rest, consts, keys, [IrFinal(opcode.GetLocalField(i, k)), ..acc])
    [IrFinal(opcode.GetLocal(i)), IrFinal(opcode.GetField2(k)), ..rest] ->
      peephole(rest, consts, keys, [IrFinal(opcode.GetLocalField2(i, k)), ..acc])
    [
      IrFinal(opcode.GetLocal(o)),
      IrFinal(opcode.GetLocal(v)),
      IrFinal(opcode.PutField(k)),
      IrFinal(opcode.Pop),
      ..rest
    ] ->
      case is_named(keys, k) {
        True ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.PutLocalLocalField(o, v, k)),
            ..acc
          ])
        False ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.PutFieldPop(k)),
            IrFinal(opcode.GetLocal(v)),
            IrFinal(opcode.GetLocal(o)),
            ..acc
          ])
      }
    [
      IrFinal(opcode.GetLocal(o)),
      IrFinal(opcode.PushConst(c)),
      IrFinal(opcode.PutField(k)),
      IrFinal(opcode.Pop),
      ..rest
    ] ->
      case is_named(keys, k) {
        True ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.PutLocalConstField(o, c, k)),
            ..acc
          ])
        False ->
          peephole(rest, consts, keys, [
            IrFinal(opcode.PutFieldPop(k)),
            IrFinal(opcode.PushConst(c)),
            IrFinal(opcode.GetLocal(o)),
            ..acc
          ])
      }
    [IrFinal(opcode.PutField(k)), IrFinal(opcode.Pop), ..rest] ->
      peephole(rest, consts, keys, [IrFinal(opcode.PutFieldPop(k)), ..acc])

    [op, ..rest] -> peephole(rest, consts, keys, [op, ..acc])
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

fn lands_here(rest: List(IrOp), l: LabelId) -> Bool {
  case rest {
    [IrLabel(id), ..] if id == l -> True
    [IrLabel(_), ..more] | [IrLine(_), ..more] -> lands_here(more, l)
    _ -> False
  }
}

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
    IrFinal(_) | IrLabel(_) | IrLine(_) | IrBinOp(_) -> []
  }
}

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
      build_label_map(rest, pc, dict.insert(map, id, Pc(pc)))
    [IrLine(_), ..rest] -> build_label_map(rest, pc, map)
    [_, ..rest] -> build_label_map(rest, pc + 1, map)
  }
}

fn label_pc(labels: Dict(LabelId, Pc), label: LabelId) -> Pc {
  let assert Ok(pc) = dict.get(labels, label) as "unbound label"
  pc
}

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

/// appends a sentinel return so fetch stays unchecked
fn resolve_ops(
  code: List(IrOp),
  labels: Dict(LabelId, Pc),
  line: Int,
  acc: List(Op),
  lines: List(Int),
) -> #(List(Op), List(Int)) {
  case code {
    [] -> #(list.reverse([opcode.Return, ..acc]), list.reverse([line, ..lines]))
    [IrLabel(_), ..rest] -> resolve_ops(rest, labels, line, acc, lines)
    [IrLine(l), ..rest] -> resolve_ops(rest, labels, l, acc, lines)
    [op, ..rest] ->
      resolve_ops(rest, labels, line, [resolve_op(op, labels), ..acc], [
        line,
        ..lines
      ])
  }
}

fn resolve_op(op: IrOp, labels: Dict(LabelId, Pc)) -> Op {
  case op {
    IrFinal(op) -> op

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

    IrBinOp(kind) -> opcode.bin_op(kind)

    IrCmpLocalLocalJump(a, b, kind, l, when) ->
      opcode.CmpLocalLocalJump(a, b, kind, label_pc(labels, l), when)
    IrCmpLocalConstJump(a, c, kind, l, when) ->
      opcode.CmpLocalConstJump(a, c, kind, label_pc(labels, l), when)
    IrIncLocalJump(i, l) -> opcode.IncLocalJump(i, label_pc(labels, l))
    IrJumpIfLocal(i, l, when) ->
      opcode.JumpIfLocal(i, label_pc(labels, l), when)
    IrIncLocalCmpConstJump(i, by, c, kind, l, when) ->
      opcode.IncLocalCmpConstJump(i, by, c, kind, label_pc(labels, l), when)
    IrIncLocalCmpLocalJump(i, by, b, kind, l, when) ->
      opcode.IncLocalCmpLocalJump(i, by, b, kind, label_pc(labels, l), when)
    IrCmpJump(kind, l, when) -> opcode.CmpJump(kind, label_pc(labels, l), when)
    IrCmpConstJump(c, kind, l, when) ->
      opcode.CmpConstJump(c, kind, label_pc(labels, l), when)
    IrLabel(_) | IrLine(_) -> panic as "marker occupies no pc"
  }
}

// picks up to two loop-written slots to keep out of the locals tuple
pub fn assign_regs(
  code: tuple_array.TupleArray(Op),
  pinned: Set(Int),
) -> #(tuple_array.TupleArray(Op), bytecode.Regs) {
  let ops = tuple_array.to_list(code)
  let #(scores, pinned) = score_slots(ops, loop_depths(ops), dict.new(), pinned)
  let picked =
    dict.to_list(scores)
    |> list.filter(fn(e) { e.1.1 && !set.contains(pinned, e.0) })
    |> list.sort(fn(a, b) { int.compare(b.1.0, a.1.0) })
    |> list.map(fn(e) { e.0 })
  case picked {
    [] -> #(code, bytecode.NoRegs)
    [a, ..rest] -> {
      let b = case rest {
        [b, ..] -> b
        [] -> -1
      }
      let remap = fn(i) {
        case i == a, i == b {
          True, _ -> -1
          _, True -> -2
          _, _ -> i
        }
      }
      let ops = list.map(ops, opcode.map_slots(_, remap))
      #(tuple_array.from_list(ops), bytecode.Regs(a, b))
    }
  }
}

// nesting depth per pc from backward jumps
fn loop_depths(ops: List(Op)) -> List(Int) {
  let deltas =
    list.index_fold(ops, dict.new(), fn(acc, op, pc) {
      case opcode.jump_target(op) {
        t if t >= 0 && t <= pc ->
          acc
          |> dict.upsert(t, fn(v) { option.unwrap(v, 0) + 1 })
          |> dict.upsert(pc + 1, fn(v) { option.unwrap(v, 0) - 1 })
        _ -> acc
      }
    })
  let #(_, rev) =
    list.index_fold(ops, #(0, []), fn(acc, _op, pc) {
      let d = acc.0 + { dict.get(deltas, pc) |> result.unwrap(0) }
      #(d, [d, ..acc.1])
    })
  list.reverse(rev)
}

// score and whether the slot is written inside a loop
fn score_slots(
  ops: List(Op),
  depths: List(Int),
  scores: Dict(Int, #(Int, Bool)),
  pinned: Set(Int),
) -> #(Dict(Int, #(Int, Bool)), Set(Int)) {
  case ops, depths {
    [op, ..ops], [d, ..depths] -> {
      let w = case d {
        0 -> 1
        1 -> 16
        2 -> 256
        _ -> 4096
      }
      let scores =
        list.fold(opcode.slot_uses(op), scores, fn(scores, used) {
          let #(slot, is_write) = used
          let #(score, hot) =
            dict.get(scores, slot) |> result.unwrap(#(0, False))
          let add = case is_write {
            True -> w * 3
            False -> w
          }
          dict.insert(scores, slot, #(score + add, hot || { is_write && d > 0 }))
        })
      let pinned = list.fold(opcode.pinned_slots(op), pinned, set.insert)
      score_slots(ops, depths, scores, pinned)
    }
    _, _ -> #(scores, pinned)
  }
}
