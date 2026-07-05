import arc/vm/gc_trace
import arc/vm/internal/elements
import arc/vm/key.{Named}
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
/// against the VM state, and over `host` because HostObject slots carry the
/// embedder's opaque value. Instantiated as `Heap(State(host), host)` at the
/// state.gleam layer.
pub opaque type Heap(ctx, host) {
  Heap(
    data: dict.Dict(Int, HeapSlot(ctx, host)),
    free: List(Int),
    next: Int,
    roots: Set(Int),
    /// GC hook: the engine heap `Ref`s reachable from a `HostObject`'s opaque
    /// value, so the mark phase can trace into host values that point back
    /// into the JS heap. `fn(_) { [] }` by default — correct for the common
    /// case where host values are pure host terms (pids, fds, sockets) with no
    /// engine refs. An embedder whose host values DO hold refs installs its
    /// own via `new_with_host_refs` so GC asks it explicitly, rather than
    /// relying on an unchecked "put refs in properties" convention.
    host_refs: fn(host) -> List(Ref),
    /// Shared singleton EnvSlot for closures that capture nothing. EnvSlots
    /// are immutable after creation (mutable captures are boxed), so every
    /// no-capture closure can point at the same slot. Rooted on first use so
    /// the cached ref can never dangle across GC.
    empty_env: Option(Ref),
    /// Value of `next` at the most recent compact/collect (0 before any).
    /// `grown_since_collect` reads the difference so the VM can trigger
    /// in-run collections only after substantial allocation.
    last_collect_next: Int,
  )
}

/// Lazily-materialised function `.prototype` objects (QuickJS-style autoinit),
/// encoded entirely in the Ref id — no heap entry at all until first write.
///
/// A lazy proto ref has a negative id `-2 - payload` (so it can never collide
/// with real slots `>= 0` or `sentinel_ref` = -1), where
/// `payload = (fn_id * 2^30 + object_proto_id) * 2 + has_constructor`.
/// `fn_id` is the owning closure (the `.constructor` backref and the identity
/// discriminator — distinct closures get distinct proto refs), and
/// `object_proto_id` is the realm's %Object.prototype% (or the generator
/// prototype) captured at closure creation.
///
/// `read` synthesises the `{constructor: fn}` ObjectSlot on demand (pure —
/// structurally the slot eager allocation would have produced); the first
/// write/update materialises it into `data` under the tagged id, which
/// shadows synthesis from then on. The GC marks through tagged ids and pins
/// the owning fn id so it is never recycled while the proto ref is live
/// (which would let a new closure mint the same tagged id).
const lazy_proto_shift = 1_073_741_824

/// Decode a tagged lazy-proto id into #(fn_id, object_proto_id, has_constructor).
fn decode_lazy_proto(id: Int) -> #(Int, Int, Bool) {
  let payload = -2 - id
  let rest = payload / 2
  #(rest / lazy_proto_shift, rest % lazy_proto_shift, payload % 2 == 1)
}

/// Build the ObjectSlot a tagged lazy-proto id stands for — structurally
/// identical to what eager allocation in make_closure used to produce.
fn synth_lazy_proto(id: Int) -> HeapSlot(ctx, host) {
  let #(fn_id, object_proto_id, has_constructor) = decode_lazy_proto(id)
  let properties = case has_constructor {
    True ->
      dict.from_list([
        #(
          Named("constructor"),
          value.builtin_property(value.JsObject(Ref(fn_id))),
        ),
      ])
    False -> dict.new()
  }
  value.ObjectSlot(
    kind: value.OrdinaryObject,
    properties:,
    elements: value.NoElements,
    prototype: Some(Ref(object_proto_id)),
    symbol_properties: [],
    extensible: True,
  )
}

/// Create an empty heap. Host values are assumed to hold no engine refs
/// (`host_refs = fn(_) { [] }`) — correct for the default engine and for
/// embedders whose host values are pure host terms. Embedders whose host
/// values point back into the JS heap use `new_with_host_refs`.
pub fn new() -> Heap(ctx, host) {
  new_with_host_refs(fn(_) { [] })
}

/// Create an empty heap, supplying the GC hook that reports the engine refs
/// reachable from a host value (see the `host_refs` field).
pub fn new_with_host_refs(host_refs: fn(host) -> List(Ref)) -> Heap(ctx, host) {
  Heap(
    data: dict.new(),
    free: [],
    next: 0,
    roots: set.new(),
    host_refs:,
    empty_env: None,
    last_collect_next: 0,
  )
}

/// Allocate a slot. Prefers recycled indices from the free list,
/// falls back to bumping `next`.
pub fn alloc(
  heap: Heap(ctx, host),
  slot: HeapSlot(ctx, host),
) -> #(Heap(ctx, host), Ref) {
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

/// Mint the lazy `.prototype` ref for the closure `fn_ref` — a pure id
/// computation, no heap mutation at all. `read` synthesises the ObjectSlot on
/// demand and any write/update materialises it into `data`. `has_constructor`
/// is True for §10.2.5 MakeConstructor functions and False for generators,
/// whose prototype object has no "constructor" (§27.3.3.1); `object_proto` is
/// the realm's %Object.prototype% (or generator prototype).
///
/// Falls back to eager allocation in the (practically unreachable) case where
/// `object_proto.id` doesn't fit the tag encoding.
pub fn alloc_lazy_proto(
  heap: Heap(ctx, host),
  fn_ref: Ref,
  has_constructor: Bool,
  object_proto: Ref,
) -> #(Heap(ctx, host), Ref) {
  case object_proto.id >= 0 && object_proto.id < lazy_proto_shift {
    True -> {
      let flag = case has_constructor {
        True -> 1
        False -> 0
      }
      let payload = { fn_ref.id * lazy_proto_shift + object_proto.id } * 2
      #(heap, Ref(-2 - { payload + flag }))
    }
    False -> {
      // Out-of-range parent proto id: allocate the real slot eagerly.
      let constructor = case has_constructor {
        True -> Some(fn_ref)
        False -> None
      }
      let properties = case constructor {
        Some(f) ->
          dict.from_list([
            #(Named("constructor"), value.builtin_property(value.JsObject(f))),
          ])
        None -> dict.new()
      }
      alloc(
        heap,
        value.ObjectSlot(
          kind: value.OrdinaryObject,
          properties:,
          elements: value.NoElements,
          prototype: Some(object_proto),
          symbol_properties: [],
          extensible: True,
        ),
      )
    }
  }
}

/// Allocate an EnvSlot for `captured_values`. Closures that capture nothing
/// share one rooted singleton empty EnvSlot — EnvSlots are never mutated
/// (mutable captures are boxed), so the sharing is unobservable.
pub fn alloc_env(
  heap: Heap(ctx, host),
  captured_values: List(value.JsValue),
) -> #(Heap(ctx, host), Ref) {
  case captured_values {
    [] ->
      case heap.empty_env {
        Some(ref) -> #(heap, ref)
        None -> {
          let #(heap, ref) = alloc(heap, value.EnvSlot([]))
          let heap =
            Heap(
              ..heap,
              empty_env: Some(ref),
              roots: set.insert(heap.roots, ref.id),
            )
          #(heap, ref)
        }
      }
    _ -> alloc(heap, value.EnvSlot(captured_values))
  }
}

/// Reserve a Ref without writing any slot. The address is allocated
/// (consumed from the free list or bumped), but no data exists yet.
/// Use `write` to fill it in later. This enables forward references
/// for cyclic structures (e.g. proto ↔ constructor).
pub fn reserve(heap: Heap(ctx, host)) -> #(Heap(ctx, host), Ref) {
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
///
/// A miss on a tagged lazy-proto id (< -1) synthesises the slot on demand;
/// `data` always shadows synthesis, so a materialised (written-to) prototype
/// is never re-synthesised.
pub fn read(heap: Heap(ctx, host), ref: Ref) -> Option(HeapSlot(ctx, host)) {
  case ffi_heap_read(heap.data, ref.id) {
    Some(_) as found -> found
    None ->
      case ref.id < -1 {
        True -> Some(synth_lazy_proto(ref.id))
        False -> None
      }
  }
}

/// Single direct map lookup. read/2 is the hottest function in the VM
/// (every property access walks it several times); going through
/// dict.get + option.from_result costs an extra Result allocation and two
/// calls per read.
@external(erlang, "arc_vm_ffi", "heap_read")
fn ffi_heap_read(
  data: dict.Dict(Int, HeapSlot(ctx, host)),
  id: Int,
) -> Option(HeapSlot(ctx, host))

// -- Typed reader helpers -----------------------------------------------------
// These collapse the common `case read(h, ref) { Some(ObjectSlot(kind: X, ..)) -> ... }`
// pyramids found throughout the VM. Each reads a specific slot shape and returns
// Option — use with option.map/option.then/option.unwrap at callsites.

/// Read an array-like's elements as a dense list of `length` values, holes
/// padded with undefined. Anything that is not an array/arguments object
/// (or a dangling ref) reads as the empty list.
pub fn read_array_values(h: Heap(ctx, host), ref: Ref) -> List(value.JsValue) {
  read_array_like(h, ref)
  |> option.map(fn(p) { elements.to_list_padded(p.1, p.0) })
  |> option.unwrap([])
}

/// Read an ArrayObject OR ArgumentsObject, returning #(length, elements).
/// Both have indexed elements and a tracked length.
pub fn read_array_like(
  h: Heap(ctx, host),
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
pub fn read_promise_data_ref(h: Heap(ctx, host), ref: Ref) -> Option(Ref) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.PromiseObject(promise_data:), ..)) ->
      Some(promise_data)
    _ -> None
  }
}

/// Read a PromiseSlot's state.
pub fn read_promise_state(
  h: Heap(ctx, host),
  ref: Ref,
) -> Option(value.PromiseState) {
  case read(h, ref) {
    Some(value.PromiseSlot(state:, ..)) -> Some(state)
    _ -> None
  }
}

/// Read an EnvSlot's captured values.
pub fn read_env(h: Heap(ctx, host), ref: Ref) -> Option(List(value.JsValue)) {
  case read(h, ref) {
    Some(value.EnvSlot(slots:)) -> Some(slots)
    _ -> None
  }
}

/// Read a BoxSlot's inner value.
pub fn read_box(h: Heap(ctx, host), ref: Ref) -> Option(value.JsValue) {
  case read(h, ref) {
    Some(value.BoxSlot(value:)) -> Some(value)
    _ -> None
  }
}

/// Allocate a `CounterSlot` holding `count` — the promise combinators'
/// remainingElementsCount Record { [[Value]]: n }.
pub fn alloc_counter(
  h: Heap(ctx, host),
  count: Int,
) -> #(Heap(ctx, host), Ref) {
  alloc(h, value.CounterSlot(count:))
}

/// remainingElementsCount += 1. There is nothing to report: an increment can
/// never take the counter to zero.
pub fn increment_counter(h: Heap(ctx, host), ref: Ref) -> Heap(ctx, host) {
  let #(h, _count) = adjust_counter(h, ref, 1)
  h
}

/// remainingElementsCount -= 1, reporting whether it reached zero (i.e.
/// whether this decrement is the one that settles the combinator's promise).
pub fn decrement_counter(
  h: Heap(ctx, host),
  ref: Ref,
) -> #(Heap(ctx, host), Bool) {
  let #(h, count) = adjust_counter(h, ref, -1)
  #(h, count <= 0)
}

/// A counter ref that does not point at a `CounterSlot` is an engine bug —
/// counter refs are only ever minted by `alloc_counter` and only ever handed
/// to these functions, and the GC traces them from the element closures that
/// hold them. Panicking beats the old silent no-op, which lost the decrement
/// and left the combinator's promise pending forever.
fn adjust_counter(
  h: Heap(ctx, host),
  ref: Ref,
  delta: Int,
) -> #(Heap(ctx, host), Int) {
  case read(h, ref) {
    Some(value.CounterSlot(count:)) -> {
      let count = count + delta
      #(write(h, ref, value.CounterSlot(count:)), count)
    }
    _ -> panic as "heap.adjust_counter: ref does not point to a CounterSlot"
  }
}

/// Read an EvalEnvSlot's var dict.
pub fn read_eval_env(
  h: Heap(ctx, host),
  ref: Ref,
) -> Option(dict.Dict(String, value.JsValue)) {
  case read(h, ref) {
    Some(value.EvalEnvSlot(vars:)) -> Some(vars)
    _ -> None
  }
}

/// Read a RegExpObject, returning #(pattern, flags).
pub fn read_regexp(h: Heap(ctx, host), ref: Ref) -> Option(#(String, String)) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.RegExpObject(pattern:, flags:), ..)) ->
      Some(#(pattern, flags))
    _ -> None
  }
}

/// Read a StringObject's [[StringData]] slot.
pub fn read_string_object(h: Heap(ctx, host), ref: Ref) -> Option(String) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.StringObject(value:), ..)) -> Some(value)
    _ -> None
  }
}

/// Read a NumberObject's [[NumberData]] slot.
pub fn read_number_object(h: Heap(ctx, host), ref: Ref) -> Option(value.JsNum) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.NumberObject(value:), ..)) -> Some(value)
    _ -> None
  }
}

/// Read a BooleanObject's [[BooleanData]] slot.
pub fn read_boolean_object(h: Heap(ctx, host), ref: Ref) -> Option(Bool) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.BooleanObject(value:), ..)) -> Some(value)
    _ -> None
  }
}

/// Read a BigIntObject's [[BigIntData]] slot.
pub fn read_bigint_object(
  h: Heap(ctx, host),
  ref: Ref,
) -> Option(value.BigInt) {
  case read(h, ref) {
    Some(value.ObjectSlot(kind: value.BigIntObject(value:), ..)) -> Some(value)
    _ -> None
  }
}

// -----------------------------------------------------------------------------

/// Sentinel ref with no backing slot. `read` returns Error, `write`/`update`
/// no-op. Used when array generics are called with a primitive `this` so
/// mutating methods drop their writes without allocating a wrapper.
pub const sentinel_ref = Ref(-1)

/// Overwrite a slot. No-op on `sentinel_ref`. All other refs are assumed
/// live — callers obtain them only via alloc/reserve (or a tagged lazy-proto
/// id, which a write materialises into `data` under its negative id).
pub fn write(
  heap: Heap(ctx, host),
  ref: Ref,
  slot: HeapSlot(ctx, host),
) -> Heap(ctx, host) {
  case ref.id != -1 {
    True -> Heap(..heap, data: dict.insert(heap.data, ref.id, slot))
    False -> heap
  }
}

/// Read-modify-write: apply a transform to the slot at ref.
/// Returns the unchanged heap if the ref doesn't exist.
/// Convenience wrapper over read + write for in-place slot mutation.
///
/// Updating a lazy function prototype materialises it: the transformed slot
/// lands in `data`, which shadows the lazy entry from then on.
pub fn update(
  heap: Heap(ctx, host),
  ref: Ref,
  f: fn(HeapSlot(ctx, host)) -> HeapSlot(ctx, host),
) -> Heap(ctx, host) {
  case dict.get(heap.data, ref.id) {
    Ok(slot) -> Heap(..heap, data: dict.insert(heap.data, ref.id, f(slot)))
    Error(Nil) ->
      case ref.id < -1 {
        True ->
          Heap(
            ..heap,
            data: dict.insert(heap.data, ref.id, f(synth_lazy_proto(ref.id))),
          )
        False -> heap
      }
  }
}

/// Replace the `kind` of an ObjectSlot in place. No-op for non-object slots.
pub fn update_kind(
  heap: Heap(ctx, host),
  ref: Ref,
  kind: value.ExoticKind(ctx, host),
) -> Heap(ctx, host) {
  use slot <- update(heap, ref)
  case slot {
    value.ObjectSlot(..) -> value.ObjectSlot(..slot, kind:)
    other -> other
  }
}

/// Mark a ref as a persistent GC root.
pub fn root(heap: Heap(ctx, host), ref: Ref) -> Heap(ctx, host) {
  Heap(..heap, roots: set.insert(heap.roots, ref.id))
}

/// Remove a ref from the persistent GC root set.
pub fn unroot(heap: Heap(ctx, host), ref: Ref) -> Heap(ctx, host) {
  Heap(..heap, roots: set.delete(heap.roots, ref.id))
}

/// Number of live (allocated) slots. Unmaterialised lazy function prototypes
/// occupy no heap entry, so they are not counted.
pub fn size(heap: Heap(ctx, host)) -> Int {
  dict.size(heap.data)
}

/// Detailed heap stats for introspection.
pub fn stats(heap: Heap(ctx, host)) -> HeapStats {
  HeapStats(
    live: dict.size(heap.data),
    free: list.length(heap.free),
    next: heap.next,
    roots: set.size(heap.roots),
  )
}

/// Run mark-and-sweep GC using only the persistent root set.
pub fn collect(heap: Heap(ctx, host)) -> Heap(ctx, host) {
  collect_with_roots(heap, set.new())
}

/// Mark-and-sweep with extra roots, then DROP the free list instead of
/// refilling it. For handing a heap across a process boundary (e.g. inside a
/// Completion message): a long allocation-heavy script can leave millions of
/// dead slots, and the recycled ids would otherwise travel as a multi-megaword
/// free list inside the heap record, defeating the point of collecting.
/// Discarding them only wastes id space — `alloc` falls back to bumping
/// `next`, and ids are plain (arbitrary-precision) ints.
pub fn compact(
  heap: Heap(ctx, host),
  extra_roots: Set(Int),
) -> Heap(ctx, host) {
  let all_roots = set.union(heap.roots, extra_roots)
  let live = mark_from(heap, all_roots)
  let new_data = dict.filter(heap.data, fn(id, _) { set.contains(live, id) })
  Heap(..heap, data: new_data, free: [], last_collect_next: heap.next)
}

/// Slots allocated via fresh ids since the last compact/collect. The VM's
/// top-level-return GC trigger compares this against its growth threshold.
pub fn grown_since_collect(heap: Heap(ctx, host)) -> Int {
  heap.next - heap.last_collect_next
}

/// Run mark-and-sweep GC using persistent roots + temporary extra roots
/// (e.g. stack refs the VM passes in).
pub fn collect_with_roots(
  heap: Heap(ctx, host),
  extra_roots: Set(Int),
) -> Heap(ctx, host) {
  let all_roots = set.union(heap.roots, extra_roots)
  let live = mark_from(heap, all_roots)
  Heap(..sweep(heap, live), last_collect_next: heap.next)
}

/// Mark phase: starting from a root set, return the set of all reachable slot IDs.
fn mark_from(heap: Heap(ctx, host), roots: Set(Int)) -> Set(Int) {
  let frontier = set.to_list(roots)
  mark_loop(heap, frontier, set.new())
}

/// Tail-recursive DFS mark traversal.
/// Frontier = worklist of IDs to visit. Visited = already-marked set.
/// Handles cycles (visited check) and dangling refs (dict.get -> Error, skip).
fn mark_loop(
  heap: Heap(ctx, host),
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
            Error(Nil) ->
              // Not in data: either an unmaterialised lazy function prototype
              // (tagged id — keep its constructor fn and parent prototype
              // alive, exactly as the materialised slot's props/prototype
              // fields would) or a genuinely dangling ref (skip).
              case id < -1 {
                True -> {
                  let #(fn_id, object_proto_id, _) = decode_lazy_proto(id)
                  mark_loop(heap, [fn_id, object_proto_id, ..rest], visited)
                }
                False -> mark_loop(heap, rest, visited)
              }
            Ok(slot) -> {
              // Prepend child ref IDs directly onto frontier — avoids
              // intermediate list from list.map + the O(n) list.append.
              let child_refs = gc_trace.refs_in_slot(slot, heap.host_refs)
              let frontier = prepend_ref_ids(child_refs, rest)
              // A materialised lazy proto (tagged id) additionally pins its
              // owning fn id: if the fn were collected and its id recycled, a
              // new closure could mint this same tagged id and alias the
              // stale materialised slot.
              let frontier = case id < -1 {
                True -> {
                  let #(fn_id, _, _) = decode_lazy_proto(id)
                  [fn_id, ..frontier]
                }
                False -> frontier
              }
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
fn sweep(heap: Heap(ctx, host), live: Set(Int)) -> Heap(ctx, host) {
  let new_data = dict.filter(heap.data, fn(id, _) { set.contains(live, id) })
  // Collect freed IDs for reuse. Materialised lazy protos live under negative
  // tagged ids that `alloc` must never hand out, so they stay off the free
  // list.
  let new_free =
    dict.fold(heap.data, heap.free, fn(free, id, _) {
      case id < 0 || set.contains(live, id) {
        True -> free
        False -> [id, ..free]
      }
    })
  Heap(..heap, data: new_data, free: new_free)
}
