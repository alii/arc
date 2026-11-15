import arc/bytecode/opcode.{type Op}
import arc/internal/tuple_array
import arc/rt/bytecode
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/set.{type Set}

// picks up to four slots to keep out of the locals tuple, where every write
// would copy it and a read has to find its place; a use inside a loop counts
// for more
pub fn assign_regs(
  code: tuple_array.TupleArray(Op),
  pinned: Set(Int),
) -> bytecode.Regs {
  let ops = tuple_array.to_list(code)
  // rewritten is what ops that only know the tuple write
  let #(scores, rewritten) =
    score_slots(ops, loop_depths(ops), 1, cold_pcs(code), dict.new(), set.new())
  let picked =
    dict.to_list(scores)
    |> list.filter(fn(e) {
      let #(slot, SlotScore(written:, score:)) = e
      case written {
        True -> !set.contains(pinned, slot) && !set.contains(rewritten, slot)
        // has to pay for being loaded and saved around calls
        False -> score >= worth_loading && !set.contains(rewritten, slot)
      }
    })
    |> list.sort(fn(a, b) {
      let #(_, SlotScore(score: score_a, ..)) = a
      let #(_, SlotScore(score: score_b, ..)) = b
      int.compare(score_b, score_a)
    })
    |> list.map(fn(e) {
      let #(slot, _) = e
      slot
    })
  let none = bytecode.no_register
  case picked {
    [] -> bytecode.NoRegs
    [a] -> bytecode.Regs(a, none, none, none)
    [a, b] -> bytecode.Regs(a, b, none, none)
    [a, b, c] -> bytecode.Regs(a, b, c, none)
    [a, b, c, d, ..] -> bytecode.Regs(a, b, c, d)
  }
}

// nesting depth per pc from backward jumps
fn loop_depths(ops: List(Op)) -> List(Int) {
  let deltas =
    list.index_fold(ops, dict.new(), fn(acc, op, i) {
      let pc = i + 1
      case opcode.jump_target(op) {
        Some(t) if t <= pc ->
          acc
          |> dict.upsert(t, fn(v) { option.unwrap(v, 0) + 1 })
          |> dict.upsert(pc + 1, fn(v) { option.unwrap(v, 0) - 1 })
        _ -> acc
      }
    })
  use <- bool.lazy_guard(dict.is_empty(deltas), fn() {
    list.map(ops, fn(_) { 0 })
  })
  let #(_, rev) =
    list.index_fold(ops, #(0, []), fn(acc, _op, i) {
      let d = acc.0 + { dict.get(deltas, i + 1) |> result.unwrap(0) }
      #(d, [d, ..acc.1])
    })
  list.reverse(rev)
}

// the pcs of each UnpackArray's general arm, up to where its fast arm jumps
fn cold_pcs(code: tuple_array.TupleArray(Op)) -> Set(Int) {
  use cold, op <- list.fold(tuple_array.to_list(code), set.new())
  case op {
    opcode.UnpackArray(miss:, ..) ->
      case tuple_array.element(miss - 1, code) {
        opcode.Jump(done) if done > miss ->
          int.range(from: miss, to: done, with: cold, run: set.insert)
        _ -> cold
      }
    _ -> cold
  }
}

// one read by position in a loop
const worth_loading = 48

// these find an early slot nearly as fast as a register
fn reads_by_literal(op: Op) -> Bool {
  case op {
    opcode.GetLocal(..)
    | opcode.GetLocalField(..)
    | opcode.GetLocalFieldKeep(..)
    | opcode.GetLocalFieldCall(..) -> True
    _ -> False
  }
}

type SlotScore {
  SlotScore(score: Int, written: Bool)
}

fn score_slots(
  ops: List(Op),
  depths: List(Int),
  pc: Int,
  cold: Set(Int),
  scores: Dict(Int, SlotScore),
  rewritten: Set(Int),
) -> #(Dict(Int, SlotScore), Set(Int)) {
  case ops, depths {
    [op, ..ops], [d, ..depths] -> {
      let w = case d {
        0 -> 1
        1 -> 16
        2 -> 256
        _ -> 4096
      }
      let uses = case set.contains(cold, pc) {
        True -> []
        False -> opcode.slot_uses(op)
      }
      let scores =
        list.fold(uses, scores, fn(scores, used) {
          let #(slot, is_write) = used
          let SlotScore(score:, written:) =
            dict.get(scores, slot)
            |> result.unwrap(SlotScore(score: 0, written: False))
          let add = case is_write, reads_by_literal(op) && slot <= 8 {
            True, _ -> w * 8
            False, True -> w
            False, False -> w * 3
          }
          dict.insert(scores, slot, SlotScore(score + add, written || is_write))
        })
      let rewritten = list.fold(opcode.pinned_slots(op), rewritten, set.insert)
      score_slots(ops, depths, pc + 1, cold, scores, rewritten)
    }
    _, _ -> #(scores, rewritten)
  }
}
