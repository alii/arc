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
  store: OrderedEntries(k, v),
  key: k,
  val: v,
) -> OrderedEntries(k, v) {
  let OrderedEntries(entries:, order:, next_seq:) = store
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
  store: OrderedEntries(k, v),
  key: k,
) -> #(OrderedEntries(k, v), Bool) {
  let OrderedEntries(entries:, order:, next_seq:) = store
  case dict.get(entries, key) {
    Error(Nil) -> #(store, False)
    Ok(#(seq, _)) -> #(
      OrderedEntries(
        entries: dict.delete(entries, key),
        order: dict.delete(order, seq),
        next_seq:,
      ),
      True,
    )
  }
}

pub fn clear(store: OrderedEntries(k, v)) -> OrderedEntries(k, v) {
  OrderedEntries(
    entries: dict.new(),
    order: dict.new(),
    next_seq: store.next_seq,
  )
}

pub fn get(store: OrderedEntries(k, v), key: k) -> Option(v) {
  dict.get(store.entries, key)
  |> result.map(fn(e) { e.1 })
  |> option.from_result
}

pub fn has(store: OrderedEntries(k, v), key: k) -> Bool {
  dict.has_key(store.entries, key)
}

pub fn size(store: OrderedEntries(k, v)) -> Int {
  dict.size(store.entries)
}

pub fn next_seq(store: OrderedEntries(k, v)) -> Int {
  store.next_seq
}

pub fn fold(store: OrderedEntries(k, v), acc: a, f: fn(a, k, v) -> a) -> a {
  use acc, k, entry <- dict.fold(store.entries, acc)
  f(acc, k, entry.1)
}

pub fn live_entries(store: OrderedEntries(k, v)) -> List(#(k, v)) {
  live_entries_from(store, 0)
}

pub fn live_values(store: OrderedEntries(k, v)) -> List(v) {
  live_entries(store) |> list.map(fn(e) { e.1 })
}

pub fn live_entries_from(
  store: OrderedEntries(k, v),
  cursor: Int,
) -> List(#(k, v)) {
  dict.fold(store.entries, [], fn(acc, k, entry) {
    let #(seq, v) = entry
    case seq >= cursor {
      True -> [#(seq, #(k, v)), ..acc]
      False -> acc
    }
  })
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.map(fn(p) { p.1 })
}

pub fn next_from(
  store: OrderedEntries(k, v),
  cursor: Int,
) -> Option(#(Int, k, v)) {
  case cursor >= store.next_seq {
    True -> None
    False ->
      case dict.get(store.order, cursor) {
        Ok(k) -> {
          let assert Ok(#(_seq, v)) = dict.get(store.entries, k)
            as "ordered_entries: order/entries desync"
          Some(#(cursor + 1, k, v))
        }
        Error(Nil) -> next_from(store, cursor + 1)
      }
  }
}
