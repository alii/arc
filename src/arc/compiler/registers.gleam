import arc/bytecode/opcode.{type Op}
import arc/internal/tuple_array
import arc/rt/bytecode
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/set.{type Set}

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
