//// Insertion-ordered key → value store — the spec's append-only [[MapData]] /
//// [[SetData]] list, backing both Map and Set.
////
//// A `Dict` alone gives O(log n) lookup but loses insertion order, so order
//// is modelled with monotonically increasing sequence numbers:
////
////   - `entries`  live key → value (the actual data)
////   - `seqs`     live key → its insertion sequence number
////   - `order`    insertion sequence number → live key (inverse of `seqs`)
////   - `next_seq` the next sequence number to assign; never reset
////
//// `delete` removes the record from all three dicts; the gap it leaves in the
//// seq space is the spec's "emptied" record, skipped by iterator cursors. A
//// deleted-then-re-inserted key gets a fresh seq past every live iterator's
//// cursor, so it is revisited (§24.1.5 / §24.2.5). `clear` empties the dicts
//// but keeps `next_seq`, so entries added after a clear still land past
//// in-flight cursors. An overwrite of a live key keeps its original seq.
////
//// The type is opaque so the invariant — `order` and `seqs` are exact
//// inverses, both cover exactly the keys of `entries`, and `next_seq` is
//// strictly greater than every assigned seq — cannot be broken from outside.
//// Every state a caller can reach through this API satisfies it; a ghost or
//// missing iteration entry (a desynced `order`/`seqs`) is unrepresentable.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub opaque type OrderedEntries(k, v) {
  OrderedEntries(
    entries: Dict(k, v),
    seqs: Dict(k, Int),
    order: Dict(Int, k),
    next_seq: Int,
  )
}

/// An empty store. `next_seq` starts at 0.
pub fn new() -> OrderedEntries(k, v) {
  OrderedEntries(
    entries: dict.new(),
    seqs: dict.new(),
    order: dict.new(),
    next_seq: 0,
  )
}

/// Insert or overwrite `key`. An existing key keeps its insertion position
/// (its seq is preserved and only the value is replaced); a new key — including
/// a deleted-then-re-added one — appends at `next_seq`, past every live
/// iterator's cursor.
pub fn insert(
  store: OrderedEntries(k, v),
  key: k,
  val: v,
) -> OrderedEntries(k, v) {
  let OrderedEntries(entries:, seqs:, order:, next_seq:) = store
  let #(seqs, order, next_seq) = case dict.has_key(entries, key) {
    True -> #(seqs, order, next_seq)
    False -> #(
      dict.insert(seqs, key, next_seq),
      dict.insert(order, next_seq, key),
      next_seq + 1,
    )
  }
  OrderedEntries(entries: dict.insert(entries, key, val), seqs:, order:, next_seq:)
}

/// Delete `key`, returning the updated store and whether the key was present.
/// The record is removed entirely; the seq gap left behind is the spec's
/// emptied record (skipped by iterator cursors in O(1)).
pub fn delete(
  store: OrderedEntries(k, v),
  key: k,
) -> #(OrderedEntries(k, v), Bool) {
  let OrderedEntries(entries:, seqs:, order:, next_seq:) = store
  case dict.get(seqs, key) {
    Error(Nil) -> #(store, False)
    Ok(seq) -> #(
      OrderedEntries(
        entries: dict.delete(entries, key),
        seqs: dict.delete(seqs, key),
        order: dict.delete(order, seq),
        next_seq:,
      ),
      True,
    )
  }
}

/// Remove every entry. `next_seq` is preserved: clear() empties the spec's
/// records but later appends still land past in-flight iterator cursors, so
/// they remain visited.
pub fn clear(store: OrderedEntries(k, v)) -> OrderedEntries(k, v) {
  OrderedEntries(
    entries: dict.new(),
    seqs: dict.new(),
    order: dict.new(),
    next_seq: store.next_seq,
  )
}

/// Look up the value stored under `key`.
pub fn get(store: OrderedEntries(k, v), key: k) -> Option(v) {
  option.from_result(dict.get(store.entries, key))
}

/// Whether `key` is live in the store.
pub fn has(store: OrderedEntries(k, v), key: k) -> Bool {
  dict.has_key(store.entries, key)
}

/// Number of live entries.
pub fn size(store: OrderedEntries(k, v)) -> Int {
  dict.size(store.entries)
}

/// The next insertion sequence number that would be assigned. Iterator
/// cursors at or past this value are exhausted.
pub fn next_seq(store: OrderedEntries(k, v)) -> Int {
  store.next_seq
}

/// Fold over the live entries in unspecified order. For consumers that don't
/// care about insertion order (GC tracing), avoiding `live_entries`' sort.
pub fn fold(store: OrderedEntries(k, v), acc: a, f: fn(a, k, v) -> a) -> a {
  dict.fold(store.entries, acc, f)
}

/// Live (key, value) entries in forward insertion order.
pub fn live_entries(store: OrderedEntries(k, v)) -> List(#(k, v)) {
  live_entries_from(store, 0)
}

/// Live values in forward insertion order.
pub fn live_values(store: OrderedEntries(k, v)) -> List(v) {
  live_entries(store) |> list.map(fn(e) { e.1 })
}

/// Live (key, value) entries whose insertion sequence number is >= `cursor`,
/// in forward insertion order. Used to drain a partially-consumed iterator.
pub fn live_entries_from(
  store: OrderedEntries(k, v),
  cursor: Int,
) -> List(#(k, v)) {
  dict.to_list(store.order)
  |> list.filter(fn(p) { p.0 >= cursor })
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.filter_map(fn(p) {
    dict.get(store.entries, p.1) |> result.map(fn(v) { #(p.1, v) })
  })
}

/// First live entry at sequence number >= `cursor`, scanning past the gaps
/// that deleted entries leave behind (the spec's "empty" records).
/// Returns #(seq, key, value); the iterator resumes at seq + 1. Each seq
/// value is scanned at most once over an iterator's lifetime, so a full
/// iteration is O(total insertions), amortized O(1) per .next().
pub fn entry_from_seq(
  store: OrderedEntries(k, v),
  cursor: Int,
) -> Option(#(Int, k, v)) {
  case cursor >= store.next_seq {
    True -> None
    False ->
      case dict.get(store.order, cursor) {
        Ok(k) ->
          case dict.get(store.entries, k) {
            Ok(v) -> Some(#(cursor, k, v))
            // order/entries are kept in lockstep by construction; a missing
            // key would mean a stale order entry — skip it rather than yield
            // a dead record.
            Error(Nil) -> entry_from_seq(store, cursor + 1)
          }
        Error(Nil) -> entry_from_seq(store, cursor + 1)
      }
  }
}
