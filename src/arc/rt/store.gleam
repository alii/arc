import arc/bytecode/error_kind.{RangeError}
import arc/rt/arena
import arc/rt/limits
import arc/rt/types.{
  type Agent, type Cell, type Handle, type JsOps, type JsVal, type ObjectKey,
  type Property, type Store, type StoreMeta, Agent, DataProperty, Handle, JsOps,
  SBox, Store, StoreMeta,
}
import gleam/dict
import gleam/set

pub fn new() -> Store {
  Store(
    cells: arena.new(),
    next_id: 0,
    alloc_since_gc: 0,
    // young generation size in cells between minor gcs
    gc_threshold: 4096,
    // past the constant birth seqs
    prop_seq: 3,
    shapes: dict.from_list([
      #(0, types.ShapeDesc(0, dict.new(), dict.new())),
    ]),
    next_shape: 1,
    ics: dict.new(),
    plain_write_protos: dict.new(),
    global_epoch: 0,
    ops: unseeded_ops(),
    microtasks: types.job_queue_new(),
    pinned_roots: set.new(),
    meta: StoreMeta(
      live_count: 0,
      next_private_id: 0,
      next_symbol_id: 0,
      next_unit_id: 0,
      unhandled_rejections: [],
      old_gen: arena.new(),
      young_start: 0,
      old_weak_ids: [],
      major_live: 0,
      minors_since_major: 0,
    ),
  )
}

fn unseeded_ops() -> JsOps {
  JsOps(
    get_prop: fn(_, _, _) { unseeded() },
    call: fn(_, _, _, _) { unseeded() },
    to_object: fn(_, _) { unseeded() },
    new_error: fn(_, _, _) { unseeded() },
    eval_hook: fn(_, _, _) { unseeded() },
    call_bytecode: fn(_, _, _, _, _) { unseeded() },
    prepare_call: fn(_, _, _, _) { unseeded() },
    construct_bytecode: fn(_, _, _, _) { unseeded() },
    resume_frame: fn(_, _, _) { unseeded() },
  )
}

// named fn so each stub gets its own type
fn unseeded() -> a {
  panic as "JsOps unseeded — init_realm fills"
}

pub fn cell_new(st: Agent, cell: Cell) -> #(Handle, Agent) {
  let store = st.store
  let id = store.next_id
  let store =
    Store(
      ..store,
      cells: arena.set(id, cell, store.cells),
      next_id: id + 1,
      alloc_since_gc: store.alloc_since_gc + 1,
    )
  #(Handle(id), Agent(..st, store: store))
}

pub fn cell_new_with(
  st: Agent,
  seqs: Int,
  build: fn(Int) -> Cell,
) -> #(Handle, Agent) {
  let store = st.store
  let id = store.next_id
  let store =
    Store(
      ..store,
      cells: arena.set(id, build(store.prop_seq), store.cells),
      next_id: id + 1,
      alloc_since_gc: store.alloc_since_gc + 1,
      prop_seq: store.prop_seq + seqs,
    )
  #(Handle(id), Agent(..st, store: store))
}

pub fn cell_new_pair(
  st: Agent,
  build: fn(Handle, Handle) -> #(Cell, Cell),
) -> #(Handle, Handle, Agent) {
  let store = st.store
  let id = store.next_id
  let a = Handle(id)
  let b = Handle(id + 1)
  let #(cell_a, cell_b) = build(a, b)
  let store =
    Store(
      ..store,
      cells: arena.set(id + 1, cell_b, arena.set(id, cell_a, store.cells)),
      next_id: id + 2,
      alloc_since_gc: store.alloc_since_gc + 2,
    )
  #(a, b, Agent(..st, store: store))
}

@external(erlang, "arc_rt_store_ffi", "cell_get")
pub fn cell_get(st: Agent, h: Handle) -> Cell

pub fn cell_set(st: Agent, h: Handle, cell: Cell) -> Agent {
  let store = st.store
  let Handle(id) = h
  let cells = arena.set(id, cell, store.cells)
  let global_epoch = case cell {
    types.SObject(kind: types.GlobalObj, ..) -> store.global_epoch + 1
    _ -> store.global_epoch
  }
  let plain_write_protos = case dict.has_key(store.plain_write_protos, id) {
    True -> dict.new()
    False -> store.plain_write_protos
  }
  Agent(..st, store: Store(..store, cells:, plain_write_protos:, global_epoch:))
}

// boxes must be sbox so gc traces them
pub fn box_new(st: Agent, value: JsVal) -> #(Handle, Agent) {
  cell_new(st, SBox(value))
}

@external(erlang, "arc_rt_store_ffi", "box_get")
pub fn box_get(st: Agent, h: Handle) -> JsVal

pub fn box_set(st: Agent, h: Handle, value: JsVal) -> Agent {
  cell_set(st, h, SBox(value))
}

pub fn cell_update(st: Agent, h: Handle, f: fn(Cell) -> Cell) -> Agent {
  cell_set(st, h, f(cell_get(st, h)))
}

pub fn cell_free(st: Agent, h: Handle) -> Agent {
  let store = st.store
  let Handle(id) = h
  Agent(..st, store: Store(..store, cells: arena.free(id, store.cells)))
}

pub fn pin_root(st: Agent, h: Handle) -> Agent {
  let store = st.store
  let Handle(id) = h
  Agent(
    ..st,
    store: Store(..store, pinned_roots: set.insert(store.pinned_roots, id)),
  )
}

pub fn next_prop_seq(st: Agent) -> #(Int, Agent) {
  let store = st.store
  #(
    store.prop_seq,
    Agent(..st, store: Store(..store, prop_seq: store.prop_seq + 1)),
  )
}

// spelled out rather than via types.*_property: one call per property made
pub fn frozen_property(st: Agent, value: JsVal) -> #(Property, Agent) {
  let #(seq, st) = next_prop_seq(st)
  let prop =
    DataProperty(
      value:,
      writable: False,
      enumerable: False,
      configurable: False,
      seq:,
    )
  #(prop, st)
}

pub fn plain_property(st: Agent, value: JsVal) -> #(Property, Agent) {
  let #(seq, st) = next_prop_seq(st)
  let prop =
    DataProperty(
      value:,
      writable: True,
      enumerable: True,
      configurable: True,
      seq:,
    )
  #(prop, st)
}

pub fn builtin_property(st: Agent, value: JsVal) -> #(Property, Agent) {
  let #(seq, st) = next_prop_seq(st)
  let prop =
    DataProperty(
      value:,
      writable: True,
      enumerable: False,
      configurable: True,
      seq:,
    )
  #(prop, st)
}

pub fn next_private_id(st: Agent) -> #(Int, Agent) {
  let meta = st.store.meta
  #(
    meta.next_private_id,
    with_meta(st, StoreMeta(..meta, next_private_id: meta.next_private_id + 1)),
  )
}

pub fn next_symbol_id(st: Agent) -> #(Int, Agent) {
  let meta = st.store.meta
  #(
    meta.next_symbol_id,
    with_meta(st, StoreMeta(..meta, next_symbol_id: meta.next_symbol_id + 1)),
  )
}

pub fn next_unit_id(st: Agent) -> #(Int, Agent) {
  let meta = st.store.meta
  #(
    meta.next_unit_id,
    with_meta(st, StoreMeta(..meta, next_unit_id: meta.next_unit_id + 1)),
  )
}

fn with_meta(st: Agent, meta: StoreMeta) -> Agent {
  Agent(..st, store: Store(..st.store, meta:))
}

pub fn enter_call(st: Agent) -> Agent {
  case st.call_depth >= limits.max_call_depth {
    True -> stack_overflow(st)
    False -> Agent(..st, call_depth: st.call_depth + 1)
  }
}

pub fn stack_overflow(st: Agent) -> a {
  let #(e, st) =
    st.store.ops.new_error(st, RangeError, "Maximum call stack size exceeded")
  throw(st, e)
}

pub fn leave_call(st: Agent) -> Agent {
  Agent(..st, call_depth: st.call_depth - 1)
}

@external(erlang, "arc_rt_store_ffi", "throw")
pub fn throw(st: Agent, err_val: JsVal) -> a

// wire key tuple as an object key, no copy
@external(erlang, "arc_rt_store_ffi", "as_object_key")
pub fn as_object_key(key: k) -> ObjectKey

@external(erlang, "arc_rt_store_ffi", "is_handle")
pub fn is_handle(v: JsVal) -> Bool
