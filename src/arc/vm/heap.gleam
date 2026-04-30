import arc/internal/erlang
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
/// Generic over `ctx` because HeapSlot carries host-function closures typed
/// against the VM state. Instantiated as `Heap(State)` at the state.gleam layer.
pub opaque type Heap(ctx) {
  Heap(
    data: dict.Dict(Int, HeapSlot(ctx)),
    free: List(Int),
    next: Int,
    roots: Set(Int),
  )
}

/// Create an empty heap.
pub fn new() -> Heap(ctx) {
  Heap(data: dict.new(), free: [], next: 0, roots: set.new())
}

pub fn serialize(heap: Heap(ctx)) -> BitArray {
  erlang.term_to_binary(heap)
}

/// "dangerously" because this doesn't validate
/// that the passed data is actually a heap
pub fn dangerously_deserialize(heap: BitArray) -> Heap(ctx) {
  erlang.binary_to_term(heap)
}

/// Allocate a slot. Prefers recycled indices from the free list,
/// falls back to bumping `next`.
pub fn alloc(heap: Heap(ctx), slot: HeapSlot(ctx)) -> #(Heap(ctx), Ref) {
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

pub fn info_about_jsvalue(heap: Heap(ctx), value: value.JsValue) {
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
pub fn reserve(heap: Heap(ctx)) -> #(Heap(ctx), Ref) {
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
pub fn read(heap: Heap(ctx), ref: Ref) -> Option(HeapSlot(ctx)) {
  dict.get(heap.data, ref.id) |> option.from_result
}

// -- Typed reader helpers -----------------------------------------------------
// These collapse the common `case read(h, ref) { Some(ObjectSlot(kind: X, ..)) -> ... }`
// pyramids found throughout the VM. Each reads a specific slot shape and returns
// Option — use with option.map/option.then/option.unwrap at callsites.

/// Read an ArrayObject, returning #(length, elements).
pub fn read_array(h: Heap(ctx), ref: Ref) -> Option(#(Int, value.JsElements)) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.ArrayObject(length:), elements:, ..)) ->
      Some(#(length, elements))
    _ -> None
  }
}

/// Read an ArrayObject OR ArgumentsObject, returning #(length, elements).
/// Both have indexed elements and a tracked length.
pub fn read_array_like(
  h: Heap(ctx),
  ref: Ref,
) -> Option(#(Int, value.JsElements)) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.ArrayObject(length:), elements:, ..))
    | Some(value.ObjectSlot(kind: value.ArgumentsObject(length:), elements:, ..)) ->
      Some(#(length, elements))
    _ -> None
  }
}

/// Read a PromiseObject, returning the inner promise_data ref.
pub fn read_promise_data_ref(h: Heap(ctx), ref: Ref) -> Option(Ref) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.PromiseObject(promise_data:), ..)) ->
      Some(promise_data)
    _ -> None
  }
}

/// Read a PromiseSlot's state.
pub fn read_promise_state(
  h: Heap(ctx),
  ref: Ref,
) -> Option(value.PromiseState) {
  case read(h, ref) {
    Some(value.PromiseSlot(state:, ..)) -> Some(state)
    _ -> None
  }
}

/// Read an EnvSlot's captured values.
pub fn read_env(h: Heap(ctx), ref: Ref) -> Option(List(value.JsValue)) {
  case read(h, ref) {
    Some(value.EnvSlot(slots:)) -> Some(slots)
    _ -> None
  }
}

/// Read a BoxSlot's inner value.
pub fn read_box(h: Heap(ctx), ref: Ref) -> Option(value.JsValue) {
  case read(h, ref) {
    Some(value.BoxSlot(value:)) -> Some(value)
    _ -> None
  }
}

/// Read an EvalEnvSlot's var dict.
pub fn read_eval_env(
  h: Heap(ctx),
  ref: Ref,
) -> Option(dict.Dict(String, value.JsValue)) {
  case read(h, ref) {
    Some(value.EvalEnvSlot(vars:)) -> Some(vars)
    _ -> None
  }
}

/// Read a PidObject's Erlang pid.
pub fn read_pid(h: Heap(ctx), ref: Ref) -> Option(value.ErlangPid) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.PidObject(pid:), ..)) -> Some(pid)
    _ -> None
  }
}

/// Read a SubjectObject's pid and tag.
pub fn read_subject(
  h: Heap(ctx),
  ref: Ref,
) -> Option(#(value.ErlangPid, value.ErlangRef)) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.SubjectObject(pid:, tag:), ..)) ->
      Some(#(pid, tag))
    _ -> None
  }
}

/// Read a SelectorObject's (tag, handler) entries.
pub fn read_selector(
  h: Heap(ctx),
  ref: Ref,
) -> Option(List(#(value.ErlangRef, value.JsValue))) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.SelectorObject(entries:), ..)) ->
      Some(entries)
    _ -> None
  }
}

/// Read a RegExpObject, returning #(pattern, flags).
pub fn read_regexp(h: Heap(ctx), ref: Ref) -> Option(#(String, String)) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.RegExpObject(pattern:, flags:), ..)) ->
      Some(#(pattern, flags))
    _ -> None
  }
}

/// Read a StringObject's [[StringData]] slot.
pub fn read_string_object(h: Heap(ctx), ref: Ref) -> Option(String) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.StringObject(value:), ..)) -> Some(value)
    _ -> None
  }
}

/// Read a NumberObject's [[NumberData]] slot.
pub fn read_number_object(h: Heap(ctx), ref: Ref) -> Option(value.JsNum) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.NumberObject(value:), ..)) -> Some(value)
    _ -> None
  }
}

/// Read a BooleanObject's [[BooleanData]] slot.
pub fn read_boolean_object(h: Heap(ctx), ref: Ref) -> Option(Bool) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.BooleanObject(value:), ..)) -> Some(value)
    _ -> None
  }
}

// -----------------------------------------------------------------------------

/// Sentinel ref with no backing slot. `read` returns Error, `write`/`update`
/// no-op. Used when array generics are called with a primitive `this` so
/// mutating methods drop their writes without allocating a wrapper.
pub const sentinel_ref = Ref(-1)

/// Overwrite a slot. No-op on `sentinel_ref`. All other refs are assumed
/// live — callers obtain them only via alloc/reserve.
pub fn write(heap: Heap(ctx), ref: Ref, slot: HeapSlot(ctx)) -> Heap(ctx) {
  case ref.id >= 0 {
    True -> Heap(..heap, data: dict.insert(heap.data, ref.id, slot))
    False -> heap
  }
}

/// Read-modify-write: apply a transform to the slot at ref.
/// Returns the unchanged heap if the ref doesn't exist.
/// Convenience wrapper over read + write for in-place slot mutation.
pub fn update(
  heap: Heap(ctx),
  ref: Ref,
  f: fn(HeapSlot(ctx)) -> HeapSlot(ctx),
) -> Heap(ctx) {
  case dict.get(heap.data, ref.id) {
    Ok(slot) -> Heap(..heap, data: dict.insert(heap.data, ref.id, f(slot)))
    Error(Nil) -> heap
  }
}

/// Replace the `kind` of an ObjectSlot in place. No-op for non-object slots.
pub fn update_kind(
  heap: Heap(ctx),
  ref: Ref,
  kind: value.ExoticKind(ctx),
) -> Heap(ctx) {
  use slot <- update(heap, ref)
  case slot {
    value.ObjectSlot(..) -> value.ObjectSlot(..slot, kind:)
    other -> other
  }
}

/// Write a slot at a ref unconditionally. Used to fill reserved
/// (forward-reference) slots that have no data in the heap yet.
pub fn fill(heap: Heap(ctx), ref: Ref, slot: HeapSlot(ctx)) -> Heap(ctx) {
  Heap(..heap, data: dict.insert(heap.data, ref.id, slot))
}

/// Mark a ref as a persistent GC root.
pub fn root(heap: Heap(ctx), ref: Ref) -> Heap(ctx) {
  Heap(..heap, roots: set.insert(heap.roots, ref.id))
}

/// Remove a ref from the persistent GC root set.
pub fn unroot(heap: Heap(ctx), ref: Ref) -> Heap(ctx) {
  Heap(..heap, roots: set.delete(heap.roots, ref.id))
}

/// The persistent root set (for external use like spawn GC).
pub fn root_set(heap: Heap(ctx)) -> Set(Int) {
  heap.roots
}

/// Number of live (allocated) slots.
pub fn size(heap: Heap(ctx)) -> Int {
  dict.size(heap.data)
}

/// Detailed heap stats for introspection.
pub fn stats(heap: Heap(ctx)) -> HeapStats {
  HeapStats(
    live: dict.size(heap.data),
    free: list.length(heap.free),
    next: heap.next,
    roots: set.size(heap.roots),
  )
}

/// Run mark-and-sweep GC using only the persistent root set.
pub fn collect(heap: Heap(ctx)) -> Heap(ctx) {
  collect_with_roots(heap, set.new())
}

/// Run mark-and-sweep GC using persistent roots + temporary extra roots
/// (e.g. stack refs the VM passes in).
pub fn collect_with_roots(heap: Heap(ctx), extra_roots: Set(Int)) -> Heap(ctx) {
  let all_roots = set.union(heap.roots, extra_roots)
  let live = mark_from(heap, all_roots)
  sweep(heap, live)
}

/// Mark phase: starting from a root set, return the set of all reachable slot IDs.
fn mark_from(heap: Heap(ctx), roots: Set(Int)) -> Set(Int) {
  let frontier = set.to_list(roots)
  mark_loop(heap, frontier, set.new())
}

/// Tail-recursive DFS mark traversal.
/// Frontier = worklist of IDs to visit. Visited = already-marked set.
/// Handles cycles (visited check) and dangling refs (dict.get -> Error, skip).
fn mark_loop(
  heap: Heap(ctx),
  frontier: List(Int),
  visited: Set(Int),
) -> Set(Int) {
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
              // Prepend child ref IDs directly onto frontier — avoids
              // intermediate list from list.map + the O(n) list.append.
              let child_refs = value.refs_in_slot(slot)
              let frontier = prepend_ref_ids(child_refs, rest)
              mark_loop(heap, frontier, visited)
            }
          }
        }
      }
    }
  }
}

/// Prepend Ref IDs onto a list without intermediate allocation.
fn prepend_ref_ids(refs: List(Ref), tail: List(Int)) -> List(Int) {
  case refs {
    [] -> tail
    [Ref(id), ..rest] -> prepend_ref_ids(rest, [id, ..tail])
  }
}

/// Sweep phase: filter the data dict to keep only live entries,
/// collect freed indices into the free list.
fn sweep(heap: Heap(ctx), live: Set(Int)) -> Heap(ctx) {
  let new_data = dict.filter(heap.data, fn(id, _) { set.contains(live, id) })
  // Collect freed IDs for reuse
  let new_free =
    dict.fold(heap.data, heap.free, fn(free, id, _) {
      case set.contains(live, id) {
        True -> free
        False -> [id, ..free]
      }
    })
  Heap(..heap, data: new_data, free: new_free)
}
