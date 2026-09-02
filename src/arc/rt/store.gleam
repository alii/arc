import arc/bytecode/key.{type Key}
import arc/rt/arena
import arc/rt/limits
import arc/rt/names
import arc/rt/types.{
  type Agent, type Handle, type JobQueue, type JsOps, type JsSlot, type JsStore,
  type JsVal, Agent, JsCell, JsOps, JsStore, NameTable, RangeErr, SBox,
} as rt_types
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

@external(erlang, "arc_job_queue_ffi", "job_queue_new")
fn jq_new() -> JobQueue

pub fn t_store_new() -> JsStore(Agent) {
  JsStore(
    data: arena.new(),
    next: 0,
    pinned_roots: set.new(),
    alloc_since_gc: 0,
    gc_threshold: 65_536,
    gc_live: 0,
    // past the constant birth seqs
    prop_seq: 3,
    private_uid: list.length(reserved_private),
    symbol_uid: 0,
    ops: unseeded_ops(),
    microtasks: jq_new(),
    unhandled_rejections: [],
    shapes: dict.from_list([
      #(0, rt_types.ShapeDesc(0, dict.new(), dict.new())),
    ]),
    next_shape: 1,
    unit_uid: 0,
    ics: dict.new(),
    free_protos: dict.new(),
    global_epoch: 0,
    names: NameTable(
      // seeded with every fixed name so a lookup is one probe
      numbers: names.fixed_map(),
      texts: list.index_map(reserved_private, fn(text, uid) {
        #(key.private(uid), text)
      })
        |> dict.from_list,
      next: names.fixed_count(),
      pinned: list.index_map(reserved_private, fn(_, uid) {
        #(key.private(uid), Nil)
      })
        |> dict.from_list,
      swept: 0,
      sweep_min: default_names_sweep_min,
      gcs: 0,
    ),
  )
}

pub const default_names_sweep_min: Int = 4096

pub fn t_gc_settings(
  st: Agent,
  gc_threshold gc_threshold: Int,
  names_sweep_min sweep_min: Int,
) -> Agent {
  let js = require_js(st)
  let names = NameTable(..js.names, sweep_min:)
  with_js(st, JsStore(..js, gc_threshold:, names:))
}

// private keys the engine keeps on the global object
const reserved_private = [
  "arc_module_status", "arc_module_errors", "arc_module_cache",
  "arc_module_deferred", "arc_module_pending", "arc_module_referrer",
  "[[IteratingRegExp]]", "[[IteratedString]]", "[[Global]]", "[[Done]]",
]

pub fn reserved_private_key(text: String) -> Key {
  case find_index(reserved_private, text, 0) {
    Some(uid) -> key.private(uid)
    None -> panic as { "not a reserved private name " <> text }
  }
}

fn find_index(items: List(String), text: String, i: Int) -> Option(Int) {
  case items {
    [] -> None
    [first, ..] if first == text -> Some(i)
    [_, ..rest] -> find_index(rest, text, i + 1)
  }
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

fn require_js(st: Agent) -> JsStore(Agent) {
  st.store
}

fn with_js(st: Agent, js: JsStore(Agent)) -> Agent {
  Agent(..st, store: js)
}

pub fn t_cell_new(st: Agent, slot: JsSlot) -> #(Handle, Agent) {
  let js = st.store
  let id = js.next
  let js =
    JsStore(
      ..js,
      data: arena.set(id, slot, js.data),
      next: id + 1,
      alloc_since_gc: js.alloc_since_gc + 1,
    )
  #(JsCell(id), Agent(..st, store: js))
}

pub fn t_cell_new_with(
  st: Agent,
  seqs: Int,
  build: fn(Int) -> JsSlot,
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
  #(JsCell(id), Agent(..st, store: js))
}

pub fn t_cell_new_pair(
  st: Agent,
  build: fn(Handle, Handle) -> #(JsSlot, JsSlot),
) -> #(Handle, Handle, Agent) {
  let js = st.store
  let id = js.next
  let a = JsCell(id)
  let b = JsCell(id + 1)
  let #(slot_a, slot_b) = build(a, b)
  let js =
    JsStore(
      ..js,
      data: arena.set(id + 1, slot_b, arena.set(id, slot_a, js.data)),
      next: id + 2,
      alloc_since_gc: js.alloc_since_gc + 2,
    )
  #(a, b, Agent(..st, store: js))
}

@external(erlang, "arc_rt_store_ffi", "t_cell_get")
pub fn t_cell_get(st: Agent, h: Handle) -> JsSlot

pub fn t_cell_set(st: Agent, h: Handle, slot: JsSlot) -> Agent {
  let js = st.store
  let JsCell(id) = h
  let data = arena.set(id, slot, js.data)
  let global_epoch = case slot {
    rt_types.SObject(kind: rt_types.GlobalObj, ..) -> js.global_epoch + 1
    _ -> js.global_epoch
  }
  case dict.has_key(js.free_protos, id) {
    True ->
      Agent(
        ..st,
        store: JsStore(..js, data:, free_protos: dict.new(), global_epoch:),
      )
    False -> Agent(..st, store: JsStore(..js, data:, global_epoch:))
  }
}

// boxes must be sbox so gc traces them
pub fn t_var_new(st: Agent, value: JsVal) -> #(Handle, Agent) {
  t_cell_new(st, SBox(value))
}

@external(erlang, "arc_rt_store_ffi", "t_var_get")
pub fn t_var_get(st: Agent, h: Handle) -> JsVal

pub fn t_var_set(st: Agent, h: Handle, value: JsVal) -> Agent {
  t_cell_set(st, h, SBox(value))
}

pub fn t_cell_update(st: Agent, h: Handle, f: fn(JsSlot) -> JsSlot) -> Agent {
  t_cell_set(st, h, f(t_cell_get(st, h)))
}

pub fn t_cell_free(st: Agent, h: Handle) -> Agent {
  let js = require_js(st)
  let JsCell(id) = h
  with_js(st, JsStore(..js, data: arena.reset(id, js.data)))
}

pub fn t_pin_root(st: Agent, h: Handle) -> Agent {
  let js = require_js(st)
  let JsCell(id) = h
  with_js(st, JsStore(..js, pinned_roots: set.insert(js.pinned_roots, id)))
}

pub fn t_next_prop_seq(st: Agent) -> #(Int, Agent) {
  let js = require_js(st)
  #(js.prop_seq, with_js(st, JsStore(..js, prop_seq: js.prop_seq + 1)))
}

pub fn t_next_symbol_uid(st: Agent) -> #(Int, Agent) {
  let js = require_js(st)
  #(js.symbol_uid, with_js(st, JsStore(..js, symbol_uid: js.symbol_uid + 1)))
}

pub fn t_next_unit_uid(st: Agent) -> #(Int, Agent) {
  let js = require_js(st)
  #(js.unit_uid, with_js(st, JsStore(..js, unit_uid: js.unit_uid + 1)))
}

pub fn find_name(js: JsStore(st), text: String) -> Option(Int) {
  dict.get(js.names.numbers, text) |> option.from_result
}

pub fn name_number(js: JsStore(st), text: String) -> #(Int, JsStore(st)) {
  case find_name(js, text) {
    Some(n) -> #(n, js)
    None -> {
      let t = js.names
      let n = t.next
      let text = copy_text(text)
      let names =
        NameTable(
          ..t,
          numbers: dict.insert(t.numbers, text, n),
          texts: dict.insert(t.texts, key.name(n), text),
          next: n + 1,
        )
      #(n, JsStore(..js, names:, alloc_since_gc: js.alloc_since_gc + 1))
    }
  }
}

// so the table never keeps a slice of a bigger string alive
@external(erlang, "binary", "copy")
fn copy_text(text: String) -> String

pub fn name_text(js: JsStore(st), n: Int) -> String {
  key_text(js, key.name(n))
}

// index digits, the name text, or a private name's source text
@external(erlang, "arc_rt_val_ffi", "key_text")
pub fn key_text(js: JsStore(st), k: Key) -> String

// non allocating: a text no object has ever been keyed by is none
pub fn t_find_key(st: Agent, text: String) -> Option(Key) {
  case key.index_of_text(text) {
    Some(i) -> Some(key.index(i))
    None -> find_name(require_js(st), text) |> option.map(key.name)
  }
}

// the key for text, naming it on first use
pub fn t_key(st: Agent, text: String) -> #(Key, Agent) {
  case key.index_of_text(text) {
    Some(i) -> #(key.index(i), st)
    None -> {
      let js = require_js(st)
      case find_name(js, text) {
        Some(n) -> #(key.name(n), st)
        None -> {
          let #(n, js) = name_number(js, text)
          #(key.name(n), with_js(st, js))
        }
      }
    }
  }
}

pub fn t_key_of_int(st: Agent, n: Int) -> #(Key, Agent) {
  case key.is_array_index(n) {
    True -> #(key.index(n), st)
    False -> t_key(st, int.to_string(n))
  }
}

pub fn t_key_text(st: Agent, k: Key) -> String {
  key_text(require_js(st), k)
}

pub fn t_key_value(st: Agent, k: Key) -> JsVal {
  rt_types.mk_string(key_text(require_js(st), k))
}

pub fn t_new_private_key(st: Agent, text: String) -> #(Key, Agent) {
  let js = require_js(st)
  let k = key.private(js.private_uid)
  let names = NameTable(..js.names, texts: dict.insert(js.names.texts, k, text))
  let js =
    JsStore(
      ..js,
      private_uid: js.private_uid + 1,
      names:,
      alloc_since_gc: js.alloc_since_gc + 1,
    )
  #(k, with_js(st, js))
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
    require_js(st).ops.new_error(
      st,
      RangeErr,
      "Maximum call stack size exceeded",
    )
  t_throw(st, e)
}

pub fn t_leave_call(st: Agent) -> Agent {
  Agent(..st, call_depth: st.call_depth - 1)
}

@external(erlang, "arc_rt_store_ffi", "t_throw")
pub fn t_throw(st: Agent, err_val: JsVal) -> a
