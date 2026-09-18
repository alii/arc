import arc/rt/arena
import arc/rt/limits
import arc/rt/types.{
  type Agent, type Cell, type Handle, type JsOps, type JsStore, type JsVal,
  type StoreMeta, Agent, Handle, JsOps, JsStore, RangeErr, SBox, StoreMeta,
} as rt_types
import gleam/dict
import gleam/set

pub fn new() -> JsStore(Agent) {
  JsStore(
    data: arena.new(),
    next: 0,
    alloc_since_gc: 0,
    // young generation size in cells between minor gcs
    gc_threshold: 4096,
    // past the constant birth seqs
    prop_seq: 3,
    shapes: dict.from_list([
      #(0, rt_types.ShapeDesc(0, dict.new(), dict.new())),
    ]),
    next_shape: 1,
    ics: dict.new(),
    free_protos: dict.new(),
    global_epoch: 0,
    ops: unseeded_ops(),
    microtasks: rt_types.jq_new(),
    pinned_roots: set.new(),
    meta: StoreMeta(
      gc_live: 0,
      private_uid: 0,
      symbol_uid: 0,
      unit_uid: 0,
      unhandled_rejections: [],
      old: arena.new(),
      old_next: 0,
      weak_old: [],
      major_live: 0,
      minors_since_major: 0,
    ),
  )
}

fn unseeded_ops() -> JsOps(Agent) {
  JsOps(
    get_prop: fn(_, _, _) { unseeded() },
    call: fn(_, _, _, _) { unseeded() },
    to_object: fn(_, _) { unseeded() },
    new_error: fn(_, _, _) { unseeded() },
    eval_hook: fn(_, _, _) { unseeded() },
    call_bytecode: fn(_, _, _, _, _) { unseeded() },
    bind_call: fn(_, _, _, _) { unseeded() },
    construct_bytecode: fn(_, _, _, _) { unseeded() },
    resume_frame: fn(_, _, _) { unseeded() },
  )
}

// named fn so each stub gets its own type
fn unseeded() -> a {
  panic as "JsOps unseeded — init_realm fills"
}

pub fn t_cell_new(st: Agent, cell: Cell) -> #(Handle, Agent) {
  let js = st.store
  let id = js.next
  let js =
    JsStore(
      ..js,
      data: arena.set(id, cell, js.data),
      next: id + 1,
      alloc_since_gc: js.alloc_since_gc + 1,
    )
  #(Handle(id), Agent(..st, store: js))
}

pub fn t_cell_new_with(
  st: Agent,
  seqs: Int,
  build: fn(Int) -> Cell,
) -> #(Handle, Agent) {
  let js = st.store
  let id = js.next
  let js =
    JsStore(
      ..js,
      data: arena.set(id, build(js.prop_seq), js.data),
      next: id + 1,
      alloc_since_gc: js.alloc_since_gc + 1,
      prop_seq: js.prop_seq + seqs,
    )
  #(Handle(id), Agent(..st, store: js))
}

pub fn t_cell_new_pair(
  st: Agent,
  build: fn(Handle, Handle) -> #(Cell, Cell),
) -> #(Handle, Handle, Agent) {
  let js = st.store
  let id = js.next
  let a = Handle(id)
  let b = Handle(id + 1)
  let #(cell_a, cell_b) = build(a, b)
  let js =
    JsStore(
      ..js,
      data: arena.set(id + 1, cell_b, arena.set(id, cell_a, js.data)),
      next: id + 2,
      alloc_since_gc: js.alloc_since_gc + 2,
    )
  #(a, b, Agent(..st, store: js))
}

@external(erlang, "arc_rt_store_ffi", "t_cell_get")
pub fn t_cell_get(st: Agent, h: Handle) -> Cell

pub fn t_cell_set(st: Agent, h: Handle, cell: Cell) -> Agent {
  let js = st.store
  let Handle(id) = h
  let data = arena.set(id, cell, js.data)
  let global_epoch = case cell {
    rt_types.SObject(kind: rt_types.GlobalObj, ..) -> js.global_epoch + 1
    _ -> js.global_epoch
  }
  let free_protos = case dict.has_key(js.free_protos, id) {
    True -> dict.new()
    False -> js.free_protos
  }
  Agent(..st, store: JsStore(..js, data:, free_protos:, global_epoch:))
}

// boxes must be sbox so gc traces them
pub fn t_box_new(st: Agent, value: JsVal) -> #(Handle, Agent) {
  t_cell_new(st, SBox(value))
}

@external(erlang, "arc_rt_store_ffi", "t_box_get")
pub fn t_box_get(st: Agent, h: Handle) -> JsVal

pub fn t_box_set(st: Agent, h: Handle, value: JsVal) -> Agent {
  t_cell_set(st, h, SBox(value))
}

pub fn t_cell_update(st: Agent, h: Handle, f: fn(Cell) -> Cell) -> Agent {
  t_cell_set(st, h, f(t_cell_get(st, h)))
}

pub fn t_cell_free(st: Agent, h: Handle) -> Agent {
  let js = st.store
  let Handle(id) = h
  Agent(..st, store: JsStore(..js, data: arena.free(id, js.data)))
}

pub fn t_pin_root(st: Agent, h: Handle) -> Agent {
  let js = st.store
  let Handle(id) = h
  Agent(
    ..st,
    store: JsStore(..js, pinned_roots: set.insert(js.pinned_roots, id)),
  )
}

pub fn t_next_prop_seq(st: Agent) -> #(Int, Agent) {
  let js = st.store
  #(js.prop_seq, Agent(..st, store: JsStore(..js, prop_seq: js.prop_seq + 1)))
}

pub fn t_next_private_uid(st: Agent) -> #(Int, Agent) {
  let meta = st.store.meta
  #(
    meta.private_uid,
    with_meta(st, StoreMeta(..meta, private_uid: meta.private_uid + 1)),
  )
}

pub fn t_next_symbol_uid(st: Agent) -> #(Int, Agent) {
  let meta = st.store.meta
  #(
    meta.symbol_uid,
    with_meta(st, StoreMeta(..meta, symbol_uid: meta.symbol_uid + 1)),
  )
}

pub fn t_next_unit_uid(st: Agent) -> #(Int, Agent) {
  let meta = st.store.meta
  #(
    meta.unit_uid,
    with_meta(st, StoreMeta(..meta, unit_uid: meta.unit_uid + 1)),
  )
}

fn with_meta(st: Agent, meta: StoreMeta) -> Agent {
  Agent(..st, store: JsStore(..st.store, meta:))
}

pub fn t_enter_call(st: Agent) -> Agent {
  case st.call_depth >= limits.max_call_depth {
    True -> {
      let #(_, st) = stack_overflow(st)
      st
    }
    False -> Agent(..st, call_depth: st.call_depth + 1)
  }
}

pub fn stack_overflow(st: Agent) -> #(JsVal, Agent) {
  let #(e, st) =
    st.store.ops.new_error(st, RangeErr, "Maximum call stack size exceeded")
  t_throw(st, e)
}

pub fn t_leave_call(st: Agent) -> Agent {
  Agent(..st, call_depth: st.call_depth - 1)
}

@external(erlang, "arc_rt_store_ffi", "t_throw")
pub fn t_throw(st: Agent, err_val: JsVal) -> a
