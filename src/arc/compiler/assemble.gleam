import arc/bytecode/binop
import arc/bytecode/key
import arc/bytecode/opcode.{
  type IrOp, type LabelId, type Op, type Pc, IrAsyncYieldStarNext,
  IrAsyncYieldStarResume, IrBinOp, IrCmpConstJump, IrCmpJump,
  IrCmpLocalConstJump, IrCmpLocalLocalJump, IrDefineAccessor, IrDefineField,
  IrDefineMethod, IrDeleteField, IrFinal, IrGetField, IrGetFieldKeep, IrGosub,
  IrIncLocalCmpConstJump, IrIncLocalCmpLocalJump, IrIncLocalJump, IrJump,
  IrJumpIfFalse, IrJumpIfLocal, IrJumpIfNotNullish, IrJumpIfNullish,
  IrJumpIfTrue, IrLabel, IrLine, IrPushTry, IrPutField, IrWithDeleteVar,
  IrWithGetRefValue, IrWithGetVar, IrWithGetVarThis, IrWithMakeRef,
  IrWithPutRefValue, IrWithPutVar, Pc,
}
import arc/internal/tuple_array
import arc/rt/bytecode
import arc/rt/types.{type JsVal}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub fn assemble(code: List(IrOp), constants: List(JsVal)) -> Assembled {
  let const_arr = tuple_array.from_list(constants)
  let code = thread_jumps(code, label_suffixes(code, dict.new()), [])
  let code = drop_dead_labels(code, referenced_labels(code, set.new()), [])
  let code = peephole(code, const_arr, [])
  let code = add_safepoints(code)
  let label_map = build_label_map(code, 0, dict.new())
  let #(ops, lines) = assemble_ops(code, label_map, 0, [], [])
  Assembled(
    bytecode: tuple_array.from_list(ops),
    constants: const_arr,
    lines: tuple_array.from_list(lines),
  )
}

pub type Assembled {
  Assembled(
    bytecode: tuple_array.TupleArray(Op),
    constants: tuple_array.TupleArray(JsVal),
    lines: tuple_array.TupleArray(Int),
  )
}

// runs before labels become pcs so fusion can't break jumps
fn peephole(
  code: List(IrOp),
  consts: tuple_array.TupleArray(JsVal),
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
        Some(op) -> peephole(rest, consts, [op, ..acc])
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
          peephole(rest, consts, [op, ..acc])
        Some(op), _ -> peephole([get, ..rest], consts, [op, ..acc])
        None, _ -> peephole(list.drop(code, 1), consts, [get, ..acc])
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
      peephole(rest, consts, [
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
    ] -> peephole(rest, consts, put_local(acc, i))
    [
      IrFinal(opcode.Dup),
      IrFinal(opcode.PutBoxed(i)),
      IrFinal(opcode.Pop),
      ..rest
    ] -> peephole(rest, consts, [IrFinal(opcode.PutBoxed(i)), ..acc])

    // seed immediately overwritten
    [
      IrFinal(opcode.PushConst(_)),
      IrFinal(opcode.PutLocal(i)),
      IrFinal(opcode.PutLocal(j)),
      ..rest
    ]
      if i == j
    -> peephole(rest, consts, put_local(acc, i))

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
              peephole(rest, consts, [
                IrIncLocalCmpLocalJump(a, by, b, pure, l, when),
                ..acc
              ])
            _ ->
              peephole(rest, consts, [
                IrCmpLocalLocalJump(a, b, pure, l, when),
                ..acc
              ])
          }
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
        Some(pure), [IrJumpIfFalse(l) as j, ..rest]
        | Some(pure), [IrJumpIfTrue(l) as j, ..rest]
        -> {
          let when = j == IrJumpIfTrue(l)
          case stepped_local(acc, a) {
            Some(#(by, acc)) ->
              peephole(rest, consts, [
                IrIncLocalCmpConstJump(a, by, c, pure, l, when),
                ..acc
              ])
            None ->
              peephole(rest, consts, [
                IrCmpLocalConstJump(a, c, pure, l, when),
                ..acc
              ])
          }
        }
        _, _ ->
          peephole(rest, consts, [
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
          peephole(rest, consts, [IrFinal(opcode.GetElemPostInc(obj, i)), ..acc])
        Some(op), _, _ -> peephole(rest, consts, [op, ..acc])
        None, _, _ ->
          peephole(list.drop(code, 1), consts, [
            IrFinal(opcode.GetLocal(i)),
            ..acc
          ])
      }
    }

    [IrFinal(opcode.GetLocal(i)), IrJumpIfFalse(l), ..rest] ->
      peephole(rest, consts, [IrJumpIfLocal(i, l, when: False), ..acc])
    [IrFinal(opcode.GetLocal(i)), IrJumpIfTrue(l), ..rest] ->
      peephole(rest, consts, [IrJumpIfLocal(i, l, when: True), ..acc])

    // binops with folded operand loads
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
    [IrFinal(opcode.PutLocal(i)), ..rest] ->
      peephole(rest, consts, put_local(acc, i))
    [IrJump(l), ..rest] ->
      case lands_here(rest, l), acc {
        True, _ -> peephole(rest, consts, acc)
        False, [IrFinal(opcode.IncLocal(i)), ..acc] ->
          peephole(rest, consts, [IrIncLocalJump(i, l), ..acc])
        False, _ -> peephole(rest, consts, [IrJump(l), ..acc])
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
      ] -> peephole([value, ret, ..rest], consts, acc)
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
          key.canonical(name),
        )),
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
        key.Named(_) as k ->
          peephole(rest, consts, [IrFinal(opcode.GetFieldCall1(k, a)), ..acc])
        k ->
          peephole(rest, consts, [
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
    ] -> peephole([get, arg, call, ..rest], consts, [recv, ..acc])
    [
      IrFinal(opcode.GetLocal(i)),
      IrGetFieldKeep(name),
      IrFinal(opcode.CallMethod(0)),
      ..rest
    ] ->
      case key.canonical(name) {
        key.Named(_) as k ->
          peephole(rest, consts, [
            IrFinal(opcode.GetLocalFieldCall(i, k)),
            ..acc
          ])
        k ->
          peephole(rest, consts, [
            IrFinal(opcode.CallMethod(0)),
            IrFinal(opcode.GetLocalFieldKeep(i, k)),
            ..acc
          ])
      }
    [IrGetFieldKeep(name), IrFinal(opcode.CallMethod(0)), ..rest] ->
      case key.canonical(name) {
        key.Named(_) as k ->
          peephole(rest, consts, [IrFinal(opcode.GetFieldCall(k)), ..acc])
        k ->
          peephole(rest, consts, [
            IrFinal(opcode.CallMethod(0)),
            IrFinal(opcode.GetFieldKeep(k)),
            ..acc
          ])
      }
    [IrFinal(opcode.GetLocal(i)), IrGetField(name), ..rest] ->
      peephole(rest, consts, [
        IrFinal(opcode.GetLocalField(i, key.canonical(name))),
        ..acc
      ])
    [IrFinal(opcode.GetLocal(i)), IrGetFieldKeep(name), ..rest] ->
      peephole(rest, consts, [
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
      case key.canonical(name) {
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
        IrFinal(opcode.PutFieldPop(key.canonical(name))),
        ..acc
      ])

    [op, ..rest] -> peephole(rest, consts, [op, ..acc])
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
    opcode.Less -> Some(binop.Compare(binop.Less))
    opcode.LessEq -> Some(binop.Compare(binop.LessEq))
    opcode.Greater -> Some(binop.Compare(binop.Greater))
    opcode.GreaterEq -> Some(binop.Compare(binop.GreaterEq))
    opcode.StrictEq -> Some(binop.Equality(binop.StrictEq))
    opcode.StrictNotEq -> Some(binop.Equality(binop.StrictNotEq))
    opcode.LooseEq -> Some(binop.Equality(binop.LooseEq))
    opcode.LooseNotEq -> Some(binop.Equality(binop.LooseNotEq))
    _ -> None
  }
}

fn is_const_one(consts: tuple_array.TupleArray(JsVal), index: Int) -> Bool {
  tuple_array.get_unchecked(index, consts) == types.mk_int(1)
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

// a backward jump target gets a safepoint unless nothing before it allocates
fn add_safepoints(code: List(IrOp)) -> List(IrOp) {
  let heads = loop_heads(code, 0, dict.new(), dict.new())
  case dict.is_empty(heads) {
    True -> code
    False -> {
      let heavy = heavy_prefix(code, 0, 0, dict.new())
      insert_safepoints(code, 0, heads, heavy, [])
    }
  }
}

// label -> #(label position, furthest backward jump position)
fn loop_heads(
  code: List(IrOp),
  i: Int,
  seen: Dict(LabelId, Int),
  heads: Dict(LabelId, #(Int, Int)),
) -> Dict(LabelId, #(Int, Int)) {
  case code {
    [] -> heads
    [IrLabel(l), ..rest] ->
      loop_heads(rest, i + 1, dict.insert(seen, l, i), heads)
    [op, ..rest] -> {
      let heads =
        list.fold(loop_refs(op), heads, fn(heads, l) {
          case dict.get(seen, l) {
            Ok(at) -> dict.insert(heads, l, #(at, i))
            Error(Nil) -> heads
          }
        })
      loop_heads(rest, i + 1, seen, heads)
    }
  }
}

// plain control flow only, not try or generator plumbing
fn loop_refs(op: IrOp) -> List(LabelId) {
  case op {
    IrJump(l)
    | IrJumpIfFalse(l)
    | IrJumpIfTrue(l)
    | IrJumpIfNullish(l)
    | IrJumpIfNotNullish(l)
    | IrCmpLocalLocalJump(label: l, ..)
    | IrCmpLocalConstJump(label: l, ..)
    | IrCmpJump(label: l, ..)
    | IrCmpConstJump(label: l, ..)
    | IrIncLocalJump(label: l, ..)
    | IrIncLocalCmpConstJump(label: l, ..)
    | IrIncLocalCmpLocalJump(label: l, ..)
    | IrJumpIfLocal(label: l, ..) -> [l]
    _ -> []
  }
}

// heavy ops seen before each position
fn heavy_prefix(
  code: List(IrOp),
  i: Int,
  n: Int,
  acc: Dict(Int, Int),
) -> Dict(Int, Int) {
  case code {
    [] -> dict.insert(acc, i, n)
    [op, ..rest] -> {
      let acc = dict.insert(acc, i, n)
      let n = case may_allocate(op) {
        True -> n + 1
        False -> n
      }
      heavy_prefix(rest, i + 1, n, acc)
    }
  }
}

fn insert_safepoints(
  code: List(IrOp),
  i: Int,
  heads: Dict(LabelId, #(Int, Int)),
  heavy: Dict(Int, Int),
  acc: List(IrOp),
) -> List(IrOp) {
  case code {
    [] -> list.reverse(acc)
    [IrLabel(l) as op, ..rest] -> {
      let acc = case dict.get(heads, l) {
        Ok(#(from, to)) ->
          case heavy_between(heavy, from, to) {
            True -> [IrFinal(opcode.Safepoint), op, ..acc]
            False -> [op, ..acc]
          }
        Error(Nil) -> [op, ..acc]
      }
      insert_safepoints(rest, i + 1, heads, heavy, acc)
    }
    [op, ..rest] -> insert_safepoints(rest, i + 1, heads, heavy, [op, ..acc])
  }
}

fn heavy_between(heavy: Dict(Int, Int), from: Int, to: Int) -> Bool {
  let at = fn(i) { dict.get(heavy, i) |> result.unwrap(0) }
  at(to + 1) > at(from)
}

// false for ops that never mint a cell, and for plain calls
fn may_allocate(op: IrOp) -> Bool {
  case op {
    IrFinal(op) ->
      case op {
        opcode.PushConst(_)
        | opcode.Pop
        | opcode.Dup
        | opcode.Swap
        | opcode.Rot3
        | opcode.Unrot4
        | opcode.GetLocal(_)
        | opcode.PutLocal(_)
        | opcode.PutLocalCheckInit(_)
        | opcode.GetGlobal(_)
        | opcode.PutGlobal(_)
        | opcode.GetField(_)
        | opcode.GetFieldKeep(_)
        | opcode.PutField(_)
        | opcode.GetElem
        | opcode.GetElemKeep
        | opcode.PutElem
        | opcode.DeleteField(_)
        | opcode.DeleteElem
        | opcode.Return
        | opcode.Safepoint
        | opcode.Jump(_)
        | opcode.JumpIfFalse(_)
        | opcode.JumpIfTrue(_)
        | opcode.JumpIfNullish(_)
        | opcode.JumpIfNotNullish(_)
        | opcode.Gosub(_)
        | opcode.Ret
        | opcode.Throw
        | opcode.ThrowConstAssign(_)
        | opcode.PushTry(..)
        | opcode.PopTry
        | opcode.GetBoxed(_)
        | opcode.PutBoxed(_)
        | opcode.PutBoxedCheckInit(_)
        | opcode.BinOp(_)
        | opcode.UnaryOp(_)
        | opcode.TypeOf
        | opcode.TypeofGlobal(_)
        | opcode.IncLocal(_)
        | opcode.DecLocal(_)
        | opcode.JumpIfLocal(..)
        | opcode.IncLocalJump(..)
        | opcode.IncLocalCmpConstJump(..)
        | opcode.IncLocalCmpLocalJump(..)
        | opcode.CmpLocalLocalJump(..)
        | opcode.CmpLocalConstJump(..)
        | opcode.CmpJump(..)
        | opcode.CmpConstJump(..)
        | opcode.GetLocalField(..)
        | opcode.GetLocalFieldKeep(..)
        | opcode.PutFieldPop(_)
        | opcode.PutLocalLocalField(..)
        | opcode.PutLocalConstField(..)
        | opcode.BinOpConst(..)
        | opcode.BinOpLocal(..)
        | opcode.BinOpLocalLocal(..)
        | opcode.BinOpLocalConst(..)
        | opcode.PostIncLocal(_)
        | opcode.PostDecLocal(_)
        | opcode.PutElemPop
        | opcode.GetElemLocals(..)
        | opcode.GetElemPostInc(..)
        | opcode.BinOpLocalField(..)
        | opcode.BinOpPut(..)
        | opcode.BinOpConstPut(..)
        | opcode.BinOpLocalPut(..)
        | opcode.BinOpLocalLocalPut(..)
        | // a bytecode callee reaches the return safepoint itself
          opcode.Call(_)
        | opcode.CallMethod(_)
        | opcode.GetFieldCall(_)
        | opcode.GetFieldCall1(..)
        | opcode.GetLocalFieldCall(..)
        | opcode.IteratorNext
        | opcode.ForInNext
        | opcode.Yield -> False
        _ -> True
      }
    IrLine(_)
    | IrLabel(_)
    | IrJump(_)
    | IrJumpIfFalse(_)
    | IrJumpIfTrue(_)
    | IrJumpIfNullish(_)
    | IrJumpIfNotNullish(_)
    | IrPushTry(..)
    | IrGosub(_)
    | IrGetField(_)
    | IrGetFieldKeep(_)
    | IrPutField(_)
    | IrDeleteField(_)
    | IrBinOp(_)
    | IrCmpLocalLocalJump(..)
    | IrCmpLocalConstJump(..)
    | IrCmpJump(..)
    | IrCmpConstJump(..)
    | IrIncLocalJump(..)
    | IrJumpIfLocal(..)
    | IrIncLocalCmpConstJump(..)
    | IrIncLocalCmpLocalJump(..) -> False
    IrAsyncYieldStarNext(_)
    | IrAsyncYieldStarResume(_)
    | IrWithGetVar(..)
    | IrWithGetVarThis(..)
    | IrWithPutVar(..)
    | IrWithDeleteVar(..)
    | IrWithMakeRef(..)
    | IrWithGetRefValue(..)
    | IrWithPutRefValue(..)
    | IrDefineField(_)
    | IrDefineMethod(_)
    | IrDefineAccessor(..) -> True
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
    IrFinal(_)
    | IrLabel(_)
    | IrLine(_)
    | IrGetField(_)
    | IrGetFieldKeep(_)
    | IrPutField(_)
    | IrDeleteField(_)
    | IrDefineField(_)
    | IrDefineMethod(_)
    | IrDefineAccessor(..)
    | IrBinOp(_) -> []
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

fn assemble_try_kind(
  labels: Dict(LabelId, Pc),
  kind: opcode.TryKind(LabelId),
) -> opcode.TryKind(Pc) {
  case kind {
    opcode.Finally(fin_label:) -> opcode.Finally(label_pc(labels, fin_label))
    opcode.CatchOnly -> opcode.CatchOnly
    opcode.IterCloseGuard -> opcode.IterCloseGuard
  }
}

// appends a sentinel return so fetch stays unchecked
fn assemble_ops(
  code: List(IrOp),
  labels: Dict(LabelId, Pc),
  line: Int,
  acc: List(Op),
  lines: List(Int),
) -> #(List(Op), List(Int)) {
  case code {
    [] -> #(list.reverse([opcode.Return, ..acc]), list.reverse([line, ..lines]))
    [IrLabel(_), ..rest] -> assemble_ops(rest, labels, line, acc, lines)
    [IrLine(l), ..rest] -> assemble_ops(rest, labels, l, acc, lines)
    [op, ..rest] ->
      assemble_ops(rest, labels, line, [assemble_op(op, labels), ..acc], [
        line,
        ..lines
      ])
  }
}

fn assemble_op(op: IrOp, labels: Dict(LabelId, Pc)) -> Op {
  case op {
    IrFinal(op) -> op

    IrJump(l) -> opcode.Jump(label_pc(labels, l))
    IrJumpIfFalse(l) -> opcode.JumpIfFalse(label_pc(labels, l))
    IrJumpIfTrue(l) -> opcode.JumpIfTrue(label_pc(labels, l))
    IrJumpIfNullish(l) -> opcode.JumpIfNullish(label_pc(labels, l))
    IrJumpIfNotNullish(l) -> opcode.JumpIfNotNullish(label_pc(labels, l))
    IrPushTry(l, kind) ->
      opcode.PushTry(label_pc(labels, l), assemble_try_kind(labels, kind))
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

    IrGetField(name) -> opcode.GetField(key.canonical(name))
    IrGetFieldKeep(name) -> opcode.GetFieldKeep(key.canonical(name))
    IrPutField(name) -> opcode.PutField(key.canonical(name))
    IrDeleteField(name) -> opcode.DeleteField(key.canonical(name))
    IrDefineField(name) -> opcode.DefineField(key.canonical(name))
    IrDefineMethod(name) -> opcode.DefineMethod(key.canonical(name))
    IrDefineAccessor(name, kind, enumerable) ->
      opcode.DefineAccessor(key.canonical(name), kind, enumerable)

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
        [] -> bytecode.no_register
      }
      let remap = fn(i) {
        case i == a, i == b {
          True, _ -> bytecode.reg_a_slot
          _, True -> bytecode.reg_b_slot
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
        Some(t) if t <= pc ->
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
