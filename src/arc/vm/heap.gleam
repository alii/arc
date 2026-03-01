import arc/erlang
import arc/vm/value.{type HeapSlot, type Ref, Ref}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

/// Stats returned by the stats() function for introspection.
pub type HeapStats {
  HeapStats(live: Int, free: Int, next: Int, roots: Int)
}

/// The heap: an immutable Dict arena with free list and GC roots.
pub opaque type Heap {
  Heap(
    data: dict.Dict(Int, HeapSlot),
    free: List(Int),
    next: Int,
    roots: Set(Int),
  )
}

/// Create an empty heap.
pub fn new() -> Heap {
  Heap(data: dict.new(), free: [], next: 0, roots: set.new())
}

pub fn serialize(heap: Heap) -> BitArray {
  erlang.term_to_binary(heap)
}

/// "dangerously" because this doesn't validate
/// that the passed data is actually a heap 
pub fn dangerously_deserialize(heap: BitArray) -> Heap {
  erlang.binary_to_term(heap)
}

/// Allocate a slot. Prefers recycled indices from the free list,
/// falls back to bumping `next`.
pub fn alloc(heap: Heap, slot: HeapSlot) -> #(Heap, Ref) {
  case heap.free {
    [id, ..rest] -> {
      let data = dict.insert(heap.data, id, slot)
      #(Heap(..heap, data:, free: rest), Ref(id))
    }
    [] -> {
      let id = heap.next
      let data = dict.insert(heap.data, id, slot)
      #(Heap(..heap, data:, next: id + 1), Ref(id))
    }
  }
}

pub fn info_about_jsvalue(heap: Heap, value: value.JsValue) {
  use ref <- option.map(case value {
    value.JsObject(ref) -> Some(ref)
    _ -> None
  })

  let assert Ok(data) = dict.get(heap.data, ref.id)
  data
}

/// Reserve a Ref without writing any slot. The address is allocated
/// (consumed from the free list or bumped), but no data exists yet.
/// Use `write` to fill it in later. This enables forward references
/// for cyclic structures (e.g. proto ↔ constructor).
pub fn reserve(heap: Heap) -> #(Heap, Ref) {
  case heap.free {
    [id, ..rest] -> #(Heap(..heap, free: rest), Ref(id))
    [] -> {
      let id = heap.next
      #(Heap(..heap, next: id + 1), Ref(id))
    }
  }
}

/// Read a slot by ref. Returns None if the ref is dangling (never allocated
/// or already collected). Uses Option rather than Result since a missing
/// ref is a normal condition (e.g. prototype chain termination), not an error.
pub fn read(heap: Heap, ref: Ref) -> Option(HeapSlot) {
  case dict.get(heap.data, ref.id) {
    Ok(slot) -> Some(slot)
    Error(Nil) -> None
  }
}

/// Overwrite a slot. No-op if the ref doesn't exist in the heap
/// (i.e. was never allocated or reserved).
pub fn write(heap: Heap, ref: Ref, slot: HeapSlot) -> Heap {
  case dict.has_key(heap.data, ref.id) {
    True -> Heap(..heap, data: dict.insert(heap.data, ref.id, slot))
    False -> heap
  }
}

/// Read-modify-write: apply a transform to the slot at ref.
/// Returns the unchanged heap if the ref doesn't exist.
/// Convenience wrapper over read + write for in-place slot mutation.
pub fn update(heap: Heap, ref: Ref, f: fn(HeapSlot) -> HeapSlot) -> Heap {
  case dict.get(heap.data, ref.id) {
    Ok(slot) -> Heap(..heap, data: dict.insert(heap.data, ref.id, f(slot)))
    Error(Nil) -> heap
  }
}

/// Read-modify-write variant that returns an extra value alongside the updated
/// heap. The transform returns #(new_slot, extra). Returns #(heap, default)
/// if ref is missing. Useful when mutation needs to produce a side-channel
/// result (e.g. the old value being replaced).
pub fn update_with(
  heap: Heap,
  ref: Ref,
  default: a,
  f: fn(HeapSlot) -> #(HeapSlot, a),
) -> #(Heap, a) {
  case dict.get(heap.data, ref.id) {
    Ok(slot) -> {
      let #(new_slot, extra) = f(slot)
      #(Heap(..heap, data: dict.insert(heap.data, ref.id, new_slot)), extra)
    }
    Error(Nil) -> #(heap, default)
  }
}

/// Write a slot at a ref unconditionally. Used to fill reserved
/// (forward-reference) slots that have no data in the heap yet.
pub fn fill(heap: Heap, ref: Ref, slot: HeapSlot) -> Heap {
  Heap(..heap, data: dict.insert(heap.data, ref.id, slot))
}

/// Mark a ref as a persistent GC root.
pub fn root(heap: Heap, ref: Ref) -> Heap {
  Heap(..heap, roots: set.insert(heap.roots, ref.id))
}

/// Remove a ref from the persistent GC root set.
pub fn unroot(heap: Heap, ref: Ref) -> Heap {
  Heap(..heap, roots: set.delete(heap.roots, ref.id))
}

/// Number of live (allocated) slots.
pub fn size(heap: Heap) -> Int {
  dict.size(heap.data)
}

/// Detailed heap stats for introspection.
pub fn stats(heap: Heap) -> HeapStats {
  HeapStats(
    live: dict.size(heap.data),
    free: list.length(heap.free),
    next: heap.next,
    roots: set.size(heap.roots),
  )
}

/// Run mark-and-sweep GC using only the persistent root set.
pub fn collect(heap: Heap) -> Heap {
  collect_with_roots(heap, set.new())
}

/// Run mark-and-sweep GC using persistent roots + temporary extra roots
/// (e.g. stack refs the VM passes in).
pub fn collect_with_roots(heap: Heap, extra_roots: Set(Int)) -> Heap {
  let all_roots = set.union(heap.roots, extra_roots)
  let live = mark_from(heap, all_roots)
  sweep(heap, live)
}

/// Mark phase: starting from a root set, return the set of all reachable slot IDs.
fn mark_from(heap: Heap, roots: Set(Int)) -> Set(Int) {
  let frontier = set.to_list(roots)
  mark_loop(heap, frontier, set.new())
}

/// Tail-recursive DFS mark traversal.
/// Frontier = worklist of IDs to visit. Visited = already-marked set.
/// Handles cycles (visited check) and dangling refs (dict.get -> Error, skip).
fn mark_loop(heap: Heap, frontier: List(Int), visited: Set(Int)) -> Set(Int) {
  case frontier {
    [] -> visited
    [id, ..rest] -> {
      case set.contains(visited, id) {
        True -> mark_loop(heap, rest, visited)
        False -> {
          let visited = set.insert(visited, id)
          case dict.get(heap.data, id) {
            Error(_) ->
              // Dangling ref — skip
              mark_loop(heap, rest, visited)
            Ok(slot) -> {
              let child_refs = value.refs_in_slot(slot)
              let child_ids = list.map(child_refs, fn(r) { r.id })
              mark_loop(heap, list.append(child_ids, rest), visited)
            }
          }
        }
      }
    }
  }
}

/// Sweep phase: single pass over data dict. Keep live entries,
/// collect freed indices into the free list.
fn sweep(heap: Heap, live: Set(Int)) -> Heap {
  let #(new_data, new_free) =
    dict.fold(heap.data, #(dict.new(), heap.free), fn(acc, id, slot) {
      let #(data, free) = acc
      case set.contains(live, id) {
        True -> #(dict.insert(data, id, slot), free)
        False -> #(data, [id, ..free])
      }
    })
  Heap(..heap, data: new_data, free: new_free)
}
