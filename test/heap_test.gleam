import arc/vm/heap
import arc/vm/js_elements
import arc/vm/value.{
  ArrayObject, BigInt, BoxSlot, EnvSlot, Finite, FunctionObject, JsBigInt,
  JsNull, JsNumber, JsObject, JsString, JsSymbol, ObjectSlot, OrdinaryObject,
  Ref, SymbolId,
}
import gleam/dict
import gleam/option.{None, Some}
import gleam/set

fn ordinary(props: dict.Dict(String, value.JsValue)) {
  ObjectSlot(
    kind: OrdinaryObject,
    properties: dict.map_values(props, fn(_, v) { value.data_property(v) }),
    symbol_properties: dict.new(),
    elements: js_elements.new(),
    prototype: None,
    extensible: True,
  )
}

pub fn alloc_and_read_roundtrip_test() {
  let h = heap.new()
  let slot = ordinary(dict.new())
  let #(h, ref) = heap.alloc(h, slot)
  let assert Some(got) = heap.read(h, ref)
  assert got == slot
}

pub fn multiple_allocs_distinct_refs_test() {
  let h = heap.new()
  let #(h, r1) = heap.alloc(h, ordinary(dict.new()))
  let #(_h, r2) = heap.alloc(h, ordinary(dict.new()))
  assert r1 != r2
}

pub fn read_nonexistent_test() {
  let h = heap.new()
  let assert None = heap.read(h, Ref(999))
}

pub fn write_and_read_test() {
  let h = heap.new()
  let #(h, ref) = heap.alloc(h, ordinary(dict.new()))
  let new_slot = ordinary(dict.from_list([#("x", JsNumber(Finite(42.0)))]))
  let h = heap.write(h, ref, new_slot)
  let assert Some(got) = heap.read(h, ref)
  assert got == new_slot
}

pub fn write_nonexistent_noop_test() {
  let h = heap.new()
  let slot = ordinary(dict.new())
  // Writing to a ref that doesn't exist should be a no-op
  let h2 = heap.write(h, Ref(999), slot)
  assert heap.size(h2) == 0
}

pub fn gc_rooted_survives_test() {
  let h = heap.new()
  let #(h, ref) = heap.alloc(h, ordinary(dict.new()))
  let h = heap.root(h, ref)
  let h = heap.collect(h)
  assert heap.size(h) == 1
  let assert Some(_) = heap.read(h, ref)
}

pub fn gc_unreachable_collected_test() {
  let h = heap.new()
  let #(h, ref) = heap.alloc(h, ordinary(dict.new()))
  // Not rooted — should be collected
  let h = heap.collect(h)
  assert heap.size(h) == 0
  let assert None = heap.read(h, ref)
}

pub fn gc_transitive_reachability_test() {
  // A -> B, root A => both survive
  let h = heap.new()
  let #(h, ref_b) = heap.alloc(h, ordinary(dict.new()))
  let #(h, ref_a) =
    heap.alloc(h, ordinary(dict.from_list([#("child", JsObject(ref_b))])))
  let h = heap.root(h, ref_a)
  let h = heap.collect(h)
  assert heap.size(h) == 2
  let assert Some(_) = heap.read(h, ref_a)
  let assert Some(_) = heap.read(h, ref_b)
}

pub fn gc_deep_chain_test() {
  // A -> B -> C, root A => all survive
  let h = heap.new()
  let #(h, ref_c) = heap.alloc(h, ordinary(dict.new()))
  let #(h, ref_b) =
    heap.alloc(h, ordinary(dict.from_list([#("next", JsObject(ref_c))])))
  let #(h, ref_a) =
    heap.alloc(h, ordinary(dict.from_list([#("next", JsObject(ref_b))])))
  let h = heap.root(h, ref_a)
  let h = heap.collect(h)
  assert heap.size(h) == 3
  let assert Some(_) = heap.read(h, ref_a)
  let assert Some(_) = heap.read(h, ref_b)
  let assert Some(_) = heap.read(h, ref_c)
}

pub fn gc_unrooted_cycle_collected_test() {
  // A -> B -> A (cycle), neither rooted => both collected
  let h = heap.new()
  // Alloc with placeholder, then fix up
  let #(h, ref_a) = heap.alloc(h, ordinary(dict.new()))
  let #(h, ref_b) =
    heap.alloc(h, ordinary(dict.from_list([#("back", JsObject(ref_a))])))
  let h =
    heap.write(h, ref_a, ordinary(dict.from_list([#("fwd", JsObject(ref_b))])))
  let h = heap.collect(h)
  assert heap.size(h) == 0
}

pub fn gc_rooted_cycle_survives_test() {
  // A -> B -> A (cycle), root A => both survive
  let h = heap.new()
  let #(h, ref_a) = heap.alloc(h, ordinary(dict.new()))
  let #(h, ref_b) =
    heap.alloc(h, ordinary(dict.from_list([#("back", JsObject(ref_a))])))
  let h =
    heap.write(h, ref_a, ordinary(dict.from_list([#("fwd", JsObject(ref_b))])))
  let h = heap.root(h, ref_a)
  let h = heap.collect(h)
  assert heap.size(h) == 2
  let assert Some(_) = heap.read(h, ref_a)
  let assert Some(_) = heap.read(h, ref_b)
}

pub fn free_list_reuse_after_gc_test() {
  let h = heap.new()
  let #(h, _r1) = heap.alloc(h, ordinary(dict.new()))
  let #(h, r2) = heap.alloc(h, ordinary(dict.new()))
  let h = heap.root(h, r2)
  // Collect — r1 should be freed and its index recycled
  let h = heap.collect(h)
  assert heap.size(h) == 1
  let stats = heap.stats(h)
  assert stats.free == 1
  // Next alloc should reuse the freed index
  let #(h, r3) = heap.alloc(h, ordinary(dict.new()))
  assert r3 != r2
  assert heap.size(h) == 2
  let new_stats = heap.stats(h)
  assert new_stats.free == 0
}

pub fn unroot_then_collect_test() {
  let h = heap.new()
  let #(h, ref) = heap.alloc(h, ordinary(dict.new()))
  let h = heap.root(h, ref)
  let h = heap.collect(h)
  assert heap.size(h) == 1
  // Unroot and collect again
  let h = heap.unroot(h, ref)
  let h = heap.collect(h)
  assert heap.size(h) == 0
}

pub fn multiple_independent_heaps_test() {
  // Two heaps are fully independent — mutating one doesn't affect the other
  let h1 = heap.new()
  let h2 = heap.new()
  let slot1 = ordinary(dict.new())
  let slot2 = ordinary(dict.from_list([#("a", JsString("hello"))]))
  let #(h1, r1) = heap.alloc(h1, slot1)
  let #(h2, _r2) = heap.alloc(h2, slot2)
  // h1 has slot1 at index 0, h2 has slot2 at index 0
  let assert Some(got1) = heap.read(h1, r1)
  assert got1 == slot1
  // h2 at same index has different content
  let assert Some(got2) = heap.read(h2, r1)
  assert got2 == slot2
  // Mutating h1 doesn't affect h2
  let h1 = heap.root(h1, r1)
  assert heap.stats(h1).roots == 1
  assert heap.stats(h2).roots == 0
}

pub fn gc_traces_through_array_slot_test() {
  // ArrayObject containing a ref in elements — should be traced
  let h = heap.new()
  let #(h, ref_inner) = heap.alloc(h, ordinary(dict.new()))
  let #(h, ref_arr) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: ArrayObject(3),
        properties: dict.new(),
        symbol_properties: dict.new(),
        elements: js_elements.from_list([
          JsNumber(Finite(1.0)),
          JsObject(ref_inner),
          JsNull,
        ]),
        prototype: None,
        extensible: True,
      ),
    )
  let h = heap.root(h, ref_arr)
  let h = heap.collect(h)
  assert heap.size(h) == 2
  let assert Some(_) = heap.read(h, ref_inner)
}

pub fn gc_traces_through_closure_slot_test() {
  // FunctionObject -> EnvSlot -> captured object — all should be traced
  let h = heap.new()
  let #(h, ref_captured) = heap.alloc(h, ordinary(dict.new()))
  let #(h, ref_env) = heap.alloc(h, EnvSlot(slots: [JsObject(ref_captured)]))
  let #(h, ref_closure) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: FunctionObject(func_index: 0, env: ref_env),
        properties: dict.new(),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: None,
        extensible: True,
      ),
    )
  let h = heap.root(h, ref_closure)
  let h = heap.collect(h)
  assert heap.size(h) == 3
  let assert Some(_) = heap.read(h, ref_captured)
  let assert Some(_) = heap.read(h, ref_env)
}

pub fn collect_with_roots_test() {
  // Object not in persistent roots but passed as temporary root
  let h = heap.new()
  let #(h, ref) = heap.alloc(h, ordinary(dict.new()))
  let h = heap.collect_with_roots(h, set.from_list([ref.id]))
  assert heap.size(h) == 1
  let assert Some(_) = heap.read(h, ref)
  // Without temporary roots, it's collected
  let h = heap.collect(h)
  assert heap.size(h) == 0
}

pub fn mixed_live_dead_partition_test() {
  // 5 objects: root 2 of them (plus 1 transitive), 2 should die
  let h = heap.new()
  let #(h, dead1) = heap.alloc(h, ordinary(dict.new()))
  let #(h, dead2) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: ArrayObject(1),
        properties: dict.new(),
        symbol_properties: dict.new(),
        elements: js_elements.from_list([JsNumber(Finite(1.0))]),
        prototype: None,
        extensible: True,
      ),
    )
  let #(h, live_leaf) = heap.alloc(h, ordinary(dict.new()))
  let #(h, live_parent) =
    heap.alloc(h, ordinary(dict.from_list([#("child", JsObject(live_leaf))])))
  let #(h, live_env) = heap.alloc(h, EnvSlot(slots: [JsString("hi")]))
  let #(h, live_solo) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: FunctionObject(func_index: 1, env: live_env),
        properties: dict.new(),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: None,
        extensible: True,
      ),
    )
  let h = heap.root(h, live_parent)
  let h = heap.root(h, live_solo)
  let h = heap.collect(h)
  assert heap.size(h) == 4
  let assert Some(_) = heap.read(h, live_parent)
  let assert Some(_) = heap.read(h, live_leaf)
  let assert Some(_) = heap.read(h, live_solo)
  let assert Some(_) = heap.read(h, live_env)
  let assert None = heap.read(h, dead1)
  let assert None = heap.read(h, dead2)
}

pub fn stats_test() {
  let h = heap.new()
  let s = heap.stats(h)
  assert s.live == 0
  assert s.free == 0
  assert s.next == 0
  assert s.roots == 0

  let #(h, r1) = heap.alloc(h, ordinary(dict.new()))
  let #(h, _r2) = heap.alloc(h, ordinary(dict.new()))
  let h = heap.root(h, r1)
  let s = heap.stats(h)
  assert s.live == 2
  assert s.free == 0
  assert s.next == 2
  assert s.roots == 1
}

pub fn gc_traces_through_function_object_test() {
  // JsObject(Ref) pointing to a FunctionObject should be traced through properties
  let h = heap.new()
  let #(h, ref_env) = heap.alloc(h, EnvSlot(slots: []))
  let #(h, ref_closure) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: FunctionObject(func_index: 0, env: ref_env),
        properties: dict.new(),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: None,
        extensible: True,
      ),
    )
  let #(h, ref_obj) =
    heap.alloc(
      h,
      ordinary(dict.from_list([#("callback", JsObject(ref_closure))])),
    )
  let h = heap.root(h, ref_obj)
  let h = heap.collect(h)
  assert heap.size(h) == 3
  let assert Some(_) = heap.read(h, ref_closure)
  let assert Some(_) = heap.read(h, ref_env)
}

pub fn shared_env_both_closures_keep_it_alive_test() {
  // Two closures share the same EnvSlot — rooting either keeps the env alive
  let h = heap.new()
  let #(h, ref_env) = heap.alloc(h, EnvSlot(slots: [JsNumber(Finite(0.0))]))
  let #(h, ref_inc) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: FunctionObject(func_index: 0, env: ref_env),
        properties: dict.new(),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: None,
        extensible: True,
      ),
    )
  let #(h, ref_get) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: FunctionObject(func_index: 1, env: ref_env),
        properties: dict.new(),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: None,
        extensible: True,
      ),
    )
  // Root only ref_inc — env should still survive (reachable from ref_inc)
  let h = heap.root(h, ref_inc)
  let h = heap.collect(h)
  assert heap.size(h) == 2
  let assert Some(_) = heap.read(h, ref_inc)
  let assert Some(_) = heap.read(h, ref_env)
  let assert None = heap.read(h, ref_get)
}

pub fn gc_traces_through_box_slot_test() {
  // EnvSlot holds a ref to a BoxSlot (mutable capture) which holds a ref to an object.
  // Rooting the closure should keep: closure -> env -> box -> object all alive.
  let h = heap.new()
  let #(h, ref_obj) = heap.alloc(h, ordinary(dict.new()))
  let #(h, ref_box) = heap.alloc(h, BoxSlot(value: JsObject(ref_obj)))
  let #(h, ref_env) = heap.alloc(h, EnvSlot(slots: [JsObject(ref_box)]))
  let #(h, ref_closure) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: FunctionObject(func_index: 0, env: ref_env),
        properties: dict.new(),
        symbol_properties: dict.new(),
        elements: js_elements.new(),
        prototype: None,
        extensible: True,
      ),
    )
  let h = heap.root(h, ref_closure)
  let h = heap.collect(h)
  assert heap.size(h) == 4
  let assert Some(_) = heap.read(h, ref_closure)
  let assert Some(_) = heap.read(h, ref_env)
  let assert Some(_) = heap.read(h, ref_box)
  let assert Some(_) = heap.read(h, ref_obj)
}

pub fn box_slot_mutation_test() {
  // BoxSlot can be updated (write) and the new value is readable
  let h = heap.new()
  let #(h, ref_box) = heap.alloc(h, BoxSlot(value: JsNumber(Finite(0.0))))
  let assert Some(BoxSlot(value: JsNumber(Finite(0.0)))) = heap.read(h, ref_box)
  let h = heap.write(h, ref_box, BoxSlot(value: JsNumber(Finite(1.0))))
  let assert Some(BoxSlot(value: JsNumber(Finite(1.0)))) = heap.read(h, ref_box)
}

pub fn non_ref_values_dont_prevent_gc_test() {
  // JsSymbol and JsBigInt are not heap refs — they shouldn't keep anything alive
  let h = heap.new()
  let #(h, _ref) =
    heap.alloc(
      h,
      ordinary(
        dict.from_list([
          #("sym", JsSymbol(SymbolId(1))),
          #("big", JsBigInt(BigInt(999_999_999_999))),
          #("str", JsString("hello")),
        ]),
      ),
    )
  // Not rooted — should be collected despite having values in it
  let h = heap.collect(h)
  assert heap.size(h) == 0
}
