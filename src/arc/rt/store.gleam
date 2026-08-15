//// `rt_store` — construction + threaded cell ops for the JS heap (SPEC §7.M1b).
////
//// The `JsStore(st)` **type** lives in `rt_types` (leaf, D17); this module
//// owns only its construction (`t_store_new`) and the state-threading `t_*`
//// ops that read/mutate the store carried on `Agent.store`.
////
//// **Return-tuple order is `#(V, St')` — value FIRST (R1).** `t_cell_new` and
//// the three counter ops return a tuple; `t_cell_get` returns a bare `JsSlot`;
//// every other op returns a bare `Agent`. `t_cell_new` NEVER collects
//// (D11 — allocation is O(1) and pure; GC is turn-boundary only).

import arc/rt/types.{
  type Agent, type Handle, type HostHooks, type JobQueue, type JsOps,
  type JsSlot, type JsStore, type JsVal, Agent, JsCell, JsOps, JsStore, RangeErr,
} as rt_types
import arc/vm/limits
import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/set

// ── FFI: opaque JobQueue construction (M8 owns push/pop) ────────────────────

/// Fresh empty microtask queue (Erlang `queue:new/0`). The queue is opaque to
/// Gleam; M8 owns enqueue/dequeue via the same FFI module.
@external(erlang, "arc_job_queue_ffi", "job_queue_new")
fn jq_new() -> JobQueue

// ── construction ────────────────────────────────────────────────────────────

/// Build an empty, realm-less `JsStore` (SPEC §2.2 / §7.M1b). NO realm, NO
/// global object (G18 — those are allocated INTO the store by M6 `init_realm`).
/// `ops` is a panic-stub `JsOps` (unreachable until `init_realm` seeds the
/// real M4/M-CALL fns as its step 1). Total; touches no process dictionary.
pub fn t_store_new(hooks: HostHooks) -> JsStore(Agent) {
  JsStore(
    data: dict.new(),
    free: [],
    next: 0,
    pinned_roots: set.new(),
    alloc_since_gc: 0,
    gc_threshold: 65_536,
    call_depth: 0,
    prop_seq: 0,
    private_uid: 0,
    symbol_uid: 0,
    ops: unseeded_ops(),
    host_hooks: hooks,
    microtasks: jq_new(),
    unhandled_rejections: [],
    console_buf: [],
    shapes: dict.from_list([
      #(0, rt_types.ShapeDesc(0, dict.new(), dict.new())),
    ]),
    next_shape: 1,
  )
}

/// Panic-stub `JsOps` for a store that `init_realm` has not yet seeded. Every
/// field is unreachable under the driver contract (M6 step 1 replaces this
/// before any user code runs), so a call here is an internal invariant bug.
fn unseeded_ops() -> JsOps(Agent) {
  JsOps(
    get_prop: fn(_, _, _) { unseeded() },
    call: fn(_, _, _, _) { unseeded() },
    to_object: fn(_, _) { unseeded() },
    new_error: fn(_, _, _) { unseeded() },
    eval_hook: fn(_, _) { unseeded() },
    call_bytecode: fn(_, _, _, _, _) { unseeded() },
    construct_bytecode: fn(_, _, _, _) { unseeded() },
    resume_frame: fn(_, _, _) { unseeded() },
  )
}

/// Shared panic body for `unseeded_ops`. A named `fn() -> a` so each stub
/// site re-generalises to its own return type (a `let`-bound closure would
/// monomorphise at first use).
fn unseeded() -> a {
  panic as "JsOps unseeded — init_realm fills"
}

// ── store access (private) ──────────────────────────────────────────────────

fn require_js(st: Agent) -> JsStore(Agent) {
  st.store
}

/// The one write path every mutating op below goes through.
fn with_js(st: Agent, js: JsStore(Agent)) -> Agent {
  Agent(..st, store: js)
}

// ── cell ops (arc heap.gleam:115-400) ───────────────────────────────────────

/// Allocate a fresh cell holding `slot`, returning `#(handle, st')` (R1 —
/// value first). Prefers a recycled id from the free list, else bumps `next`.
/// Bumps `alloc_since_gc` for the M2 turn-boundary trigger. **Never collects**
/// (D11) — allocation is O(1) and pure; GC only runs at `call_depth == 0`.
pub fn t_cell_new(st: Agent, slot: JsSlot) -> #(Handle, Agent) {
  let js = require_js(st)
  let #(id, free, next) = case js.free {
    [id, ..rest] -> #(id, rest, js.next)
    [] -> #(js.next, [], js.next + 1)
  }
  let js =
    JsStore(
      ..js,
      data: dict.insert(js.data, id, slot),
      free:,
      next:,
      alloc_since_gc: js.alloc_since_gc + 1,
    )
  #(JsCell(id), with_js(st, js))
}

/// Read the slot at `h`. Fail-closed panic on a dangling handle — every live
/// `Handle` was minted by `t_cell_new` and is not on the free list, so a miss
/// is a use-after-free / GC bug, never a normal path. FFI-backed so the hot
/// emitted-code read path is one map lookup, not `require_js` + `dict.get`.
@external(erlang, "arc_rt_store_ffi", "t_cell_get")
pub fn t_cell_get(st: Agent, h: Handle) -> JsSlot

/// Overwrite the slot at `h` with `slot`, returning the updated state. The
/// handle must be live (`t_cell_new`-minted, not freed); a write to a dead id
/// silently resurrects it, so callers uphold the invariant.
pub fn t_cell_set(st: Agent, h: Handle, slot: JsSlot) -> Agent {
  let js = require_js(st)
  let JsCell(id) = h
  with_js(st, JsStore(..js, data: dict.insert(js.data, id, slot)))
}

/// Read-modify-write the slot at `h` via `f`. Fail-closed panic on a dangling
/// handle (same posture as `t_cell_get`).
pub fn t_cell_update(st: Agent, h: Handle, f: fn(JsSlot) -> JsSlot) -> Agent {
  t_cell_set(st, h, f(t_cell_get(st, h)))
}

/// Return `h`'s id to the free list and drop its slot. Caller guarantees no
/// live reference to `h` remains (the GC's sweep is the normal caller).
pub fn t_cell_free(st: Agent, h: Handle) -> Agent {
  let js = require_js(st)
  let JsCell(id) = h
  with_js(
    st,
    JsStore(..js, data: dict.delete(js.data, id), free: [id, ..js.free]),
  )
}

/// Add `h` to the permanent GC root set (`pinned_roots`). Realm intrinsics
/// and captured-binding cells pin themselves so a turn-boundary collect can
/// never reclaim them (SPEC §2.2).
pub fn t_pin_root(st: Agent, h: Handle) -> Agent {
  let js = require_js(st)
  let JsCell(id) = h
  with_js(st, JsStore(..js, pinned_roots: set.insert(js.pinned_roots, id)))
}

// ── threaded counters (D9, D14) ─────────────────────────────────────────────

/// Next `Property.seq` stamp — the threaded replacement for arc's global
/// `arc_vm_ffi:next_prop_seq` atomic (D14). Returns `#(seq, st')` (R1).
pub fn t_next_prop_seq(st: Agent) -> #(Int, Agent) {
  let js = require_js(st)
  #(js.prop_seq, with_js(st, JsStore(..js, prop_seq: js.prop_seq + 1)))
}

/// Next private-name uid for `t_new_private_name` (D9). Per-evaluation
/// identity; deterministic and replayable (replaces `erlang:unique_integer`).
pub fn t_next_private_uid(st: Agent) -> #(Int, Agent) {
  let js = require_js(st)
  #(js.private_uid, with_js(st, JsStore(..js, private_uid: js.private_uid + 1)))
}

/// Next `UserSymbol` uid — the threaded replacement for arc's `make_ref()`.
pub fn t_next_symbol_uid(st: Agent) -> #(Int, Agent) {
  let js = require_js(st)
  #(js.symbol_uid, with_js(st, JsStore(..js, symbol_uid: js.symbol_uid + 1)))
}

// ── call-depth (D11 gate) ───────────────────────────────────────────────────

/// Enter a JS call: `++call_depth`. `t_maybe_collect` (M2) refuses to run
/// while `call_depth > 0`, which is what makes fn-entry allocation GC-safe.
/// Throws RangeError once `call_depth` reaches `limits.max_call_depth` (arc
/// `call.gleam:174-179`): the one depth choke point for calls, constructs
/// and coroutine resumes.
pub fn t_enter_call(st: Agent) -> Agent {
  let js = require_js(st)
  case js.call_depth >= limits.max_call_depth {
    True -> {
      let #(_, st) = stack_overflow(st)
      st
    }
    False -> with_js(st, JsStore(..js, call_depth: js.call_depth + 1))
  }
}

/// Raise RangeError "Maximum call stack size exceeded". Typed as an op body
/// so `t_call` can run it under its catch and hand back a `ThrowCompletion`.
pub fn stack_overflow(st: Agent) -> #(JsVal, Agent) {
  let #(e, st) =
    require_js(st).ops.new_error(
      st,
      RangeErr,
      "Maximum call stack size exceeded",
    )
  t_throw(st, e)
}

/// Leave a JS call: `--call_depth`. Paired with `t_enter_call` by M-CALL's
/// `t_call_checked` around every compiled-fn invocation.
pub fn t_leave_call(st: Agent) -> Agent {
  let js = require_js(st)
  with_js(st, JsStore(..js, call_depth: js.call_depth - 1))
}

// ── console (M20 harness sink) ──────────────────────────────────────────────

/// Append one console line to `console_buf` (stored reversed — LIFO prepend).
/// The M20 harness reads via `t_console_read`/`t_console_bytes` after the run;
/// nothing is written to real stdio (deterministic, replayable).
pub fn t_console_write(st: Agent, line: BitArray) -> Agent {
  let js = require_js(st)
  with_js(st, JsStore(..js, console_buf: [line, ..js.console_buf]))
}

/// Read the buffered console output in emission order (oldest first).
pub fn t_console_read(st: Agent) -> List(BitArray) {
  let js = require_js(st)
  list.reverse(js.console_buf)
}

/// Read the buffered console output as one contiguous `BitArray` in emission
/// order — what the M20 test harness diffs against expected output.
pub fn t_console_bytes(st: Agent) -> BitArray {
  bit_array.concat(t_console_read(st))
}

// ── exception (D7 / R2) ─────────────────────────────────────────────────────

/// Raise a JS exception carrying the current threaded state, so the catching
/// frame resumes with the store as it was at throw-time. Wire per R2:
/// `erlang:error({wasm_exn, 0, [St, V]})` — St FIRST in the payload list.
@external(erlang, "arc_rt_store_ffi", "t_throw")
pub fn t_throw(st: Agent, err_val: JsVal) -> a
