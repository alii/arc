/// Phase 3: Label Resolution
///
/// Converts IrOp (with label IDs for jumps) into final Op (with absolute PC addresses).
/// Two-pass algorithm:
///   Pass 1: Walk IR, skip IrLabel markers, build Dict(label_id → PC)
///   Pass 2: Walk IR, replace IrJump(label) → Jump(pc), drop IrLabel, translate all Ir* → Op
import arc/vm/binop
import arc/vm/internal/tuple_array
import arc/vm/key
import arc/vm/opcode.{
  type IrOp, type LabelId, type Op, type Pc, IrAsyncYieldStarNext,
  IrAsyncYieldStarResume, IrBinOp, IrCmpLocalConstJump, IrCmpLocalLocalJump,
  IrDefineAccessor, IrDefineField, IrDefineMethod, IrDeleteField, IrFinal,
  IrGetField, IrGetField2, IrGosub, IrJump, IrJumpIfFalse, IrJumpIfNullish,
  IrJumpIfTrue, IrLabel, IrPushTry, IrPutField, IrWithDeleteVar,
  IrWithGetRefValue, IrWithGetVar, IrWithGetVarThis, IrWithMakeRef,
  IrWithPutRefValue, IrWithPutVar, Pc,
}
import arc/vm/value.{type JsValue}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

/// Run Phase 3 over one function body's IR: peephole-fuse, then resolve label
/// IDs to absolute PCs. Returns the runnable bytecode array plus the constant
/// pool it indexes into (peephole reads constants, so the two are produced
/// together).
///
/// Variable access is already concrete (the emitter consults the scope tree
/// and emits GetLocal/GetBoxed/GetGlobal/IrWith* directly); only
/// IrLabel/IrJump/IrJumpIfFalse/IrJumpIfTrue/IrJumpIfNullish/IrPushTry (both
/// its catch target and a `Finally` kind's subroutine entry) still need
/// label→PC resolution.
///
/// Assembling the surrounding `value.FuncTemplate` is the CALLER's job
/// (`compiler.resolve_top_level` / `compiler.compile_child`): they own the
/// scope-analysis metadata every other template field comes from, so this
/// module never has to thread sixteen values through untouched.
pub fn resolve(
  code: List(IrOp),
  constants: List(JsValue),
) -> #(tuple_array.TupleArray(Op), tuple_array.TupleArray(JsValue)) {
  let const_arr = tuple_array.from_list(constants)
  let code = peephole(code, const_arr, [])
  let label_map = build_label_map(code, 0, dict.new())
  let ops = resolve_ops(code, label_map, [])
  #(tuple_array.from_list(ops), const_arr)
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
    ] -> peephole(rest, consts, [IrFinal(opcode.PutLocal(i)), ..acc])
    [
      IrFinal(opcode.Dup),
      IrFinal(opcode.PutBoxed(i)),
      IrFinal(opcode.Pop),
      ..rest
    ] -> peephole(rest, consts, [IrFinal(opcode.PutBoxed(i)), ..acc])

    // -- Pattern 4: fused compare-and-branch loop conditions -------------
    [
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.GetLocal(b)),
      IrBinOp(kind),
      IrJumpIfFalse(l),
      ..rest
    ] ->
      case fusable_cmp(kind) {
        Some(pure) ->
          peephole(rest, consts, [IrCmpLocalLocalJump(a, b, pure, l), ..acc])
        None ->
          peephole(
            [
              IrFinal(opcode.GetLocal(b)),
              IrBinOp(kind),
              IrJumpIfFalse(l),
              ..rest
            ],
            consts,
            [IrFinal(opcode.GetLocal(a)), ..acc],
          )
      }
    [
      IrFinal(opcode.GetLocal(a)),
      IrFinal(opcode.PushConst(c)),
      IrBinOp(kind),
      IrJumpIfFalse(l),
      ..rest
    ] ->
      case fusable_cmp(kind) {
        Some(pure) ->
          peephole(rest, consts, [IrCmpLocalConstJump(a, c, pure, l), ..acc])
        None ->
          peephole(
            [
              IrFinal(opcode.PushConst(c)),
              IrBinOp(kind),
              IrJumpIfFalse(l),
              ..rest
            ],
            consts,
            [IrFinal(opcode.GetLocal(a)), ..acc],
          )
      }

    [op, ..rest] -> peephole(rest, consts, [op, ..acc])
  }
}

/// Only the pure relational kinds are fused — their step semantics are
/// exactly binop_direct / binop_with_to_primitive (no In/InstanceOf heap
/// access, no Add string-concat split, no loose-eq coercion table). Returns
/// the narrowed `PureBinOp` the fused opcode carries, so the fusion cannot
/// smuggle an operator the fused step handler can't run.
fn fusable_cmp(kind: opcode.BinOpKind) -> Option(binop.PureBinOp) {
  case kind {
    opcode.Lt -> Some(binop.Compare(binop.LtCmp))
    opcode.LtEq -> Some(binop.Compare(binop.LtEqCmp))
    opcode.Gt -> Some(binop.Compare(binop.GtCmp))
    opcode.GtEq -> Some(binop.Compare(binop.GtEqCmp))
    _ -> None
  }
}

fn is_const_one(consts: tuple_array.TupleArray(JsValue), index: Int) -> Bool {
  tuple_array.get_unchecked(index, consts) == value.JsNumber(value.Finite(1.0))
}

/// Pass 1: Walk the IR, counting real ops and recording label positions.
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

/// Pass 2: Walk the IR, resolve labels to PCs, translate IrOp → Op.
/// Appends a sentinel Return at the end so the interpreter's fetch loop
/// can use unchecked element/2 — termination happens via normal Return
/// dispatch instead of Option/None detection on every instruction.
///
/// Every opcode that needs no resolution rides through as `IrFinal(op)` and is
/// simply unwrapped, so this pass only ever has to know about the handful of
/// IR-only variants below.
fn resolve_ops(
  code: List(IrOp),
  labels: Dict(LabelId, Pc),
  acc: List(Op),
) -> List(Op) {
  case code {
    [] -> list.reverse([opcode.Return, ..acc])

    // Labels are dropped (they were just markers)
    [IrLabel(_), ..rest] -> resolve_ops(rest, labels, acc)

    // Already-final: nothing to resolve. A PC-carrying op (Jump, PushTry, …)
    // is unrepresentable here — every such Op field is a `Pc`, and the emitter
    // only ever holds `LabelId`s (via `emit.fresh_label`), so the type checker
    // rejects an `IrFinal(Jump(..))` at the emit site.
    [IrFinal(op), ..rest] -> resolve_ops(rest, labels, [op, ..acc])

    // Jump ops: resolve label → PC
    [IrJump(l), ..rest] ->
      resolve_ops(rest, labels, [opcode.Jump(label_pc(labels, l)), ..acc])
    [IrJumpIfFalse(l), ..rest] ->
      resolve_ops(rest, labels, [opcode.JumpIfFalse(label_pc(labels, l)), ..acc])
    [IrJumpIfTrue(l), ..rest] ->
      resolve_ops(rest, labels, [opcode.JumpIfTrue(label_pc(labels, l)), ..acc])
    [IrJumpIfNullish(l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.JumpIfNullish(label_pc(labels, l)),
        ..acc
      ])
    [IrPushTry(l, kind), ..rest] -> {
      let op =
        opcode.PushTry(label_pc(labels, l), resolve_try_kind(labels, kind))
      resolve_ops(rest, labels, [op, ..acc])
    }
    [IrGosub(l), ..rest] ->
      resolve_ops(rest, labels, [opcode.Gosub(label_pc(labels, l)), ..acc])
    [IrAsyncYieldStarNext(l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.AsyncYieldStarNext(label_pc(labels, l)),
        ..acc
      ])
    [IrAsyncYieldStarResume(l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.AsyncYieldStarResume(label_pc(labels, l)),
        ..acc
      ])

    // `with`-object access: label-carrying, resolved like jumps
    [IrWithGetVar(name, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.WithGetVar(name, label_pc(labels, l)),
        ..acc
      ])
    [IrWithGetVarThis(name, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.WithGetVarThis(name, label_pc(labels, l)),
        ..acc
      ])
    [IrWithPutVar(name, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.WithPutVar(name, label_pc(labels, l)),
        ..acc
      ])
    [IrWithDeleteVar(name, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.WithDeleteVar(name, label_pc(labels, l)),
        ..acc
      ])
    [IrWithMakeRef(name, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.WithMakeRef(name, label_pc(labels, l)),
        ..acc
      ])
    [IrWithGetRefValue(name, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.WithGetRefValue(name, label_pc(labels, l)),
        ..acc
      ])
    [IrWithPutRefValue(name, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.WithPutRefValue(name, label_pc(labels, l)),
        ..acc
      ])

    // Static property access — precompute the canonical key once
    // (key.canonical_key, THE canonicalizer shared with the runtime) so the
    // interpreter never re-parses the constant string per dispatch.
    [IrGetField(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.GetField(key.canonical_key(name)),
        ..acc
      ])
    [IrGetField2(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.GetField2(key.canonical_key(name)),
        ..acc
      ])
    [IrPutField(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.PutField(key.canonical_key(name)),
        ..acc
      ])
    [IrDeleteField(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DeleteField(key.canonical_key(name)),
        ..acc
      ])
    [IrDefineField(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DefineField(key.canonical_key(name)),
        ..acc
      ])
    [IrDefineMethod(name), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DefineMethod(key.canonical_key(name)),
        ..acc
      ])
    [IrDefineAccessor(name, kind, enumerable), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.DefineAccessor(key.canonical_key(name), kind, enumerable),
        ..acc
      ])

    // Narrow the operator to its handler HERE, once per instruction resolved,
    // rather than once per instruction executed.
    [IrBinOp(kind), ..rest] ->
      resolve_ops(rest, labels, [opcode.bin_op(kind), ..acc])

    // Fused superinstructions (created by the peephole pass above)
    [IrCmpLocalLocalJump(a, b, kind, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.CmpLocalLocalJump(a, b, kind, label_pc(labels, l)),
        ..acc
      ])
    [IrCmpLocalConstJump(a, c, kind, l), ..rest] ->
      resolve_ops(rest, labels, [
        opcode.CmpLocalConstJump(a, c, kind, label_pc(labels, l)),
        ..acc
      ])
  }
}
