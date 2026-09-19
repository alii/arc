// §24.1.5 insertion order via seqs; next_seq never resets

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub opaque type OrderedEntries(k, v) {
  OrderedEntries(
    entries: Dict(k, #(Int, v)),
    order: Dict(Int, k),
    next_seq: Int,
  )
}

pub fn new() -> OrderedEntries(k, v) {
  OrderedEntries(entries: dict.new(), order: dict.new(), next_seq: 0)
}

pub fn insert(
  table: OrderedEntries(k, v),
  key: k,
  val: v,
) -> OrderedEntries(k, v) {
  let OrderedEntries(entries:, order:, next_seq:) = table
  case dict.get(entries, key) {
    Ok(#(seq, _)) ->
      OrderedEntries(
        entries: dict.insert(entries, key, #(seq, val)),
        order:,
        next_seq:,
      )
    Error(Nil) ->
      OrderedEntries(
        entries: dict.insert(entries, key, #(next_seq, val)),
        order: dict.insert(order, next_seq, key),
        next_seq: next_seq + 1,
      )
  }
}

pub fn delete(
  table: OrderedEntries(k, v),
  key: k,
) -> #(Bool, OrderedEntries(k, v)) {
  let OrderedEntries(entries:, order:, next_seq:) = table
  case dict.get(entries, key) {
    Error(Nil) -> #(False, table)
    Ok(#(seq, _)) -> #(
      True,
      OrderedEntries(
        entries: dict.delete(entries, key),
        order: dict.delete(order, seq),
        next_seq:,
      ),
    )
  }
}

pub fn clear(table: OrderedEntries(k, v)) -> OrderedEntries(k, v) {
  OrderedEntries(
    entries: dict.new(),
    order: dict.new(),
    next_seq: table.next_seq,
  )
}

pub fn get(table: OrderedEntries(k, v), key: k) -> Option(v) {
  dict.get(table.entries, key)
  |> result.map(fn(e) { e.1 })
  |> option.from_result
}

pub fn has(table: OrderedEntries(k, v), key: k) -> Bool {
  dict.has_key(table.entries, key)
}

pub fn size(table: OrderedEntries(k, v)) -> Int {
  dict.size(table.entries)
}

pub fn next_seq(table: OrderedEntries(k, v)) -> Int {
  table.next_seq
}

pub fn fold(table: OrderedEntries(k, v), acc: a, f: fn(a, k, v) -> a) -> a {
  use acc, k, entry <- dict.fold(table.entries, acc)
  f(acc, k, entry.1)
}

pub fn live_entries(table: OrderedEntries(k, v)) -> List(#(k, v)) {
  live_entries_from(table, 0)
}

pub fn live_values(table: OrderedEntries(k, v)) -> List(v) {
  live_entries(table) |> list.map(fn(e) { e.1 })
}

pub fn live_entries_from(
  table: OrderedEntries(k, v),
  cursor: Int,
) -> List(#(k, v)) {
  dict.fold(table.entries, [], fn(acc, k, entry) {
    let #(seq, v) = entry
    case seq >= cursor {
      True -> [#(seq, #(k, v)), ..acc]
      False -> acc
    }
  })
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.map(fn(p) { p.1 })
}

// called by name from arc_rt_lang_ffi
pub fn next_from(
  table: OrderedEntries(k, v),
  cursor: Int,
) -> Option(#(Int, k, v)) {
  case cursor >= table.next_seq {
    True -> None
    False ->
      case dict.get(table.order, cursor) {
        Ok(k) -> {
          let assert Ok(#(_seq, v)) = dict.get(table.entries, k)
            as "ordered_entries: order/entries desync"
          Some(#(cursor + 1, k, v))
        }
        Error(Nil) -> next_from(table, cursor + 1)
      }
  }
}
