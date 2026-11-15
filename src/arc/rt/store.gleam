import arc/bytecode/error_kind.{RangeError}
import arc/host_hooks.{type HostHooks}
import arc/rt/arena
import arc/rt/limits
import arc/rt/types.{
  type Agent, type Cell, type Handle, type JsOps, type JsVal, type ObjectKey,
  type Property, type Store, type StoreMeta, Agent, DataProperty, Handle, JsOps,
  SBox, Store, StoreMeta,
}
import gleam/dict
import gleam/option.{None}
import gleam/set

// young generation size in cells between minor gcs
const gc_threshold = 2048

/// an agent with an empty heap and no realm yet
pub fn new_agent(hooks: HostHooks) -> Agent {
  Agent(
    cells: arena.new(),
    next_id: 0,
    gc_at: gc_threshold,
    // past the constant birth seqs
    prop_seq: 3,
    call_depth: 0,
    frames: [],
    realm: types.unset_realm(),
    store: new(),
    hooks:,
    globals: dict.new(),
  )
}

@external(erlang, "arc_rt_store_ffi", "iter_result_offsets")
fn iter_result_offsets() -> types.ShapeOffsets

// §7.4.16 createiteratorresultobject
@external(erlang, "arc_rt_store_ffi", "new_iter_result")
pub fn new_iter_result(st: Agent, value: JsVal, done: JsVal) -> #(Handle, Agent)

/// gives the process a heap worth running code in, for as long as `run` takes
@external(erlang, "arc_rt_store_ffi", "with_room")
pub fn with_room(run: fn() -> a) -> a

pub fn new() -> Store {
  Store(
    gc_threshold:,
    // 0 is where `new` receivers start, 1 where `{}` starts, and 2 is an
    // iterator result's
    shapes: dict.from_list([
      #(0, types.ShapeDesc(0, dict.new(), dict.new())),
      #(1, types.ShapeDesc(0, dict.new(), dict.new())),
      #(2, types.ShapeDesc(2, iter_result_offsets(), dict.new())),
    ]),
    next_shape: 3,
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
      major_live: 0,
      minors_since_major: 0,
      plain_index_protos: dict.new(),
      async_context_mapping: types.mk_undefined(),
      lazy_code: dict.new(),
      threaded: dict.new(),
      threaded_before: dict.new(),
      thread_keys: dict.new(),
      draft_runs: dict.new(),
      next_template_id: 1,
      unit_texts: dict.new(),
      kept: types.Kept(
        below: 0,
        cells: arena.new(),
        weak_ids: [],
        count: 0,
        pins: 0,
      ),
      template_objects: dict.new(),
      host_fns: dict.new(),
      realms: dict.new(),
      import_hook: None,
      waiters: [],
      tz_zones: dict.new(),
      checked_subject: "",
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

@external(erlang, "arc_rt_store_ffi", "cell_new")
pub fn cell_new(st: Agent, cell: Cell) -> #(Handle, Agent)

pub fn cell_new_with(
  st: Agent,
  seqs: Int,
  build: fn(Int) -> Cell,
) -> #(Handle, Agent) {
  let id = st.next_id
  let cells = arena.set(id, build(st.prop_seq), st.cells)
  #(
    Handle(id),
    Agent(..st, cells:, next_id: id + 1, prop_seq: st.prop_seq + seqs),
  )
}

pub fn cell_new_pair(
  st: Agent,
  build: fn(Handle, Handle) -> #(Cell, Cell),
) -> #(Handle, Handle, Agent) {
  let id = st.next_id
  let a = Handle(id)
  let b = Handle(id + 1)
  let #(cell_a, cell_b) = build(a, b)
  let cells = arena.set(id + 1, cell_b, arena.set(id, cell_a, st.cells))
  #(a, b, Agent(..st, cells:, next_id: id + 2))
}

@external(erlang, "arc_rt_store_ffi", "cell_get")
pub fn cell_get(st: Agent, h: Handle) -> Cell

@external(erlang, "arc_rt_store_ffi", "cell_set")
pub fn cell_set(st: Agent, h: Handle, cell: Cell) -> Agent

// boxes must be sbox so gc traces them
pub fn box_new(st: Agent, value: JsVal) -> #(Handle, Agent) {
  cell_new(st, SBox(value))
}

@external(erlang, "arc_rt_store_ffi", "box_get")
pub fn box_get(st: Agent, h: Handle) -> JsVal

@external(erlang, "arc_rt_store_ffi", "box_set")
pub fn box_set(st: Agent, h: Handle, value: JsVal) -> Agent

pub fn cell_update(st: Agent, h: Handle, f: fn(Cell) -> Cell) -> Agent {
  cell_set(st, h, f(cell_get(st, h)))
}

pub fn cell_free(st: Agent, h: Handle) -> Agent {
  Agent(..st, cells: arena.free(h.id, st.cells))
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
  #(st.prop_seq, Agent(..st, prop_seq: st.prop_seq + 1))
}

// spelled out rather than built with types.*_property, so each property takes one call
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

/// the id the next unit to run will be given
pub fn upcoming_unit_id(st: Agent) -> Int {
  st.store.meta.next_unit_id
}

pub fn next_unit_id(st: Agent) -> #(Int, Agent) {
  let meta = st.store.meta
  #(
    meta.next_unit_id,
    with_meta(st, StoreMeta(..meta, next_unit_id: meta.next_unit_id + 1)),
  )
}

pub fn with_meta(st: Agent, meta: StoreMeta) -> Agent {
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
