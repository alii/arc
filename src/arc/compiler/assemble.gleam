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
import arc/compiler/peephole
import arc/internal/tuple_array
import arc/rt/types.{type JsVal}
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set.{type Set}

pub fn assemble(code: List(IrOp), constants: List(JsVal)) -> Assembled {
  let const_arr = tuple_array.from_list(constants)
  let code = thread_jumps(code, label_suffixes(code, dict.new()), [])
  let code = drop_dead_labels(code, referenced_labels(code, set.new()), [])
  let code = peephole.run(code, const_arr)
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

type LoopSpan {
  LoopSpan(head_at: Int, back_edge_at: Int)
}

// label -> its position and the furthest backward jump to it
fn loop_heads(
  code: List(IrOp),
  i: Int,
  seen: Dict(LabelId, Int),
  heads: Dict(LabelId, LoopSpan),
) -> Dict(LabelId, LoopSpan) {
  case code {
    [] -> heads
    [IrLabel(l), ..rest] ->
      loop_heads(rest, i + 1, dict.insert(seen, l, i), heads)
    [op, ..rest] -> {
      let heads =
        list.fold(loop_refs(op), heads, fn(heads, l) {
          case dict.get(seen, l) {
            Ok(at) -> dict.insert(heads, l, LoopSpan(at, i))
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
  heads: Dict(LabelId, LoopSpan),
  heavy: Dict(Int, Int),
  acc: List(IrOp),
) -> List(IrOp) {
  case code {
    [] -> list.reverse(acc)
    [IrLabel(l) as op, ..rest] -> {
      let acc = case dict.get(heads, l) {
        Ok(LoopSpan(from, to)) ->
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

    IrBinOp(kind) -> opcode.BinOp(kind)

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
