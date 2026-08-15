//// `rt_gc` — turn-boundary-only mark-sweep over the threaded `JsStore`
//// (SPEC §7.M2, D11). Port of arc `vm/{heap.gleam:442-563, gc_trace.gleam}`
//// onto 2core's `rt_types` shapes. Collection is gated on
//// `call_depth == 0` (D11 — NO fn-entry safepoint): compiled code never has
//// a live frame at a safepoint, and the interpreter, which does collect from
//// inside its root activation, hands that frame's registers in as
//// `extra_roots` (`arc/interp/safepoint`). Parked coroutine frames live in
//// cells and are traced by `refs_in_cell`.
////
//// **G18 divergence from M2.md:** M2.md's `roots_of_state` sketch
//// destructures a `Realm` record for `global_object` / intrinsics. That
//// sketch is STALE — per G18 `JsStore` has no `realm` field and
//// the GC does not read `Agent.realm`. Realm intrinsics reach the root set via
//// `JsStore.pinned_roots`, which M6 `init_realm` populates via
//// `t_pin_root`. The D11 root formula therefore reduces to
//// `pinned_roots ∪ refs_in_term(microtasks) ∪ unhandled_rejections`.

import arc/rt/bytecode.{
  type EnvTuple, type FuncTemplate, type SuspendedFrame, FuncTemplate,
  SuspendedFrame,
}
import arc/rt/types.{
  type Agent, type AsyncGenRequest, type Handle, type Job, type JsElements,
  type JsSlot, type JsStore, type JsVal, type ObjKind, type PromiseReaction,
  type PromiseState, type Property, type ReactionHandler, type Resume,
  type WeakKey, AccessorProperty, Agent, ArgumentsObj, ArrayBufferObj,
  ArrayIterator, ArrayObj, AsyncFromSyncIterator, AsyncGenRequest,
  AsyncGeneratorObj, BigIntObj, BooleanObj, DataProperty, DataViewObj, DateObj,
  Dense, DisposableStackObj, ErrorObj, ForInIterator, GeneratorObj, Handler,
  HostJob, IdentityPassThrough, IntlObj, IteratorHelperObj, JsCell, JsStore,
  KBound, KBytecode, KCompiled, KHost, KNative, MapIterator, MapObj,
  ModuleNamespace, NoElements, NumberObj, Ordinary, PromiseFulfilled, PromiseObj,
  PromisePending, PromiseReaction, PromiseRejected, ProxyObj, RawJsonObj,
  ReactionJob, RegExpObj, ResolveThenableJob, ResumeCompiled, ResumeFrame,
  SAsyncContext, SAsyncGen, SBox, SGenerator, SObject, SPromiseData,
  SShapedObject, SetIterator, SetObj, Sparse, StringIterator, StringObj,
  SymbolObj, TemporalObj, ThrowerPassThrough, TypedArrayObj, WeakMapObj,
  WeakObjKey, WeakSetObj, WeakSymKey, WrapForValidIteratorObj, jq_to_list,
  native_token_refs,
} as rt_types
import arc/vm/internal/ordered_entries
import arc/vm/internal/tree_array as rt_tree_array
import arc/vm/internal/tuple_array.{type TupleArray}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

// ── FFI: deep BEAM-term walk (M2-I8) ────────────────────────────────────────

/// Deep-walk any BEAM term, pushing every `{js_cell, N}` id onto `acc`.
/// Recurses into tuples/lists/maps AND fun captured env via
/// `erlang:fun_info(F, env)` — how a JS closure stored in a cell keeps its
/// captured `Handle` bindings alive. Impl: `arc_rt_gc_ffi.erl`.
@external(erlang, "arc_rt_gc_ffi", "refs_in_term")
pub fn push_term_refs(v: Dynamic, acc: List(Int)) -> List(Int)

/// Identity erase to `Dynamic` for the FFI term walk. Tier-O
/// (`gleam_stdlib:identity/1`); matches the coercion idiom in
/// `link.gleam` / `rt_mem.gleam`.
@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: anything) -> Dynamic

/// `push_term_refs` on a `JsVal` (opaque wire term). Convenience for the
/// per-variant cell tracers (gc-trace-cell).
pub fn push_val_refs(v: JsVal, acc: List(Int)) -> List(Int) {
  push_term_refs(to_dynamic(v), acc)
}

// ── store access (private) ──────────────────────────────────────────────────

fn require_js(st: Agent) -> JsStore(Agent) {
  st.store
}

// ── roots (D11) ─────────────────────────────────────────────────────────────

/// Every persistent GC-root cell id reachable from `st` (D11 root formula).
///
/// SPEC §7.M2's formula `pinned_roots ∪ realm.* ∪ global_object ∪
/// refs_in_term(microtasks) ∪ unhandled_rejections` is satisfied here as
/// `pinned_roots ∪ refs_in_term(microtasks) ∪ unhandled_rejections` because
/// M6 `init_realm` calls `t_pin_root` on every intrinsic + the global
/// object, so `pinned_roots` already SUBSUMES the `realm.*` and
/// `global_object` terms. `JsStore` has NO `realm` field (G18) — M2.md's
/// `let Realm(..) = realm` sketch (M2.md:143-172) is STALE vs G18 and is
/// NOT ported. RULINGS precedence: SPEC §7.M2 + G18 override M2.md (A4).
///
/// Port of arc `state.reachable_root_refs` (`state.gleam:216-313`) MINUS
/// interpreter-only fields (`stack`/`locals`/`call_args`/`new_target`/
/// `eval_env`/`atomics_waiters` — no interpreter frame exists at a D11
/// turn-boundary safepoint) and MINUS the `RealmCtx` destructure (subsumed
/// by `pinned_roots` per above). EXHAUSTIVE `JsStore` destructure (M2-I5 —
/// no `..`): adding a store field is a compile error here until rooted.
pub fn roots_of_state(st: Agent) -> List(Int) {
  let JsStore(
    // ── cell arena bookkeeping — no roots ──
    data: _,
    free: _,
    next: _,
    // Set(Int): realm intrinsics + global_object + captured-binding cells.
    pinned_roots:,
    // ── GC trigger counters — no roots ──
    alloc_since_gc: _,
    gc_threshold: _,
    call_depth: _,
    // ── threaded uid counters — no roots ──
    prop_seq: _,
    private_uid: _,
    symbol_uid: _,
    // fn-record: engine plumbing, no `{js_cell,_}` captured.
    ops: _,
    // Opaque `:queue` of `Job` — port `state.gleam:297-300`.
    microtasks:,
    // List(Int) of promise cell ids — port `state.gleam:282`.
    unhandled_rejections:,
    // ── hidden-class table — Int/BitArray only, no handles ──
    shapes: _,
    next_shape: _,
  ) = require_js(st)
  let acc = set.to_list(pinned_roots)
  let acc = list.append(unhandled_rejections, acc)
  let acc = push_term_refs(to_dynamic(jq_to_list(microtasks)), acc)
  // Embedder closures may capture handles (a class constructor holding its
  // prototype); walk their fun envs, the import hook's included.
  let acc = push_term_refs(to_dynamic(dict.values(st.host_fns)), acc)
  let acc = push_term_refs(to_dynamic(st.import_hook), acc)
  // Global let/const/class bindings of every realm live outside the heap's
  // global objects (intrinsics and template objects are already pinned).
  // The registry's copy of the current realm may be stale, so it is walked
  // from `st.realm`; a stale entry only over-approximates.
  let realms = dict.insert(st.realms, st.realm.id, st.realm)
  dict.fold(realms, acc, fn(acc, _id, realm) {
    dict.fold(realm.lexical_globals, acc, fn(acc, _name, binding) {
      push_val_refs(rt_types.lexical_global_value(binding), acc)
    })
  })
}

// ── cell tracer (arc gc_trace.gleam:61-540) ─────────────────────────────────

/// Every cell id directly reachable from `slot`. Port of arc
/// `gc_trace.refs_in_slot`. EXHAUSTIVE match on `JsSlot` with NO wildcard
/// (SPEC §7.M2 / D8 safety property): adding a `JsSlot` variant is a compile
/// error here, never a silent free-of-live-cell.
pub fn refs_in_cell(slot: JsSlot) -> List(Int) {
  case slot {
    SObject(kind:, proto:, props:, symbol_props:, elements:, extensible: _) -> {
      let acc = push_objkind_refs(kind, [])
      let acc = push_opt_handle(proto, acc)
      let acc = dict.fold(props, acc, fn(a, _, p) { push_property_refs(p, a) })
      let acc =
        list.fold(symbol_props, acc, fn(a, sp) { push_property_refs(sp.1, a) })
      push_elements_refs(elements, acc)
    }
    SShapedObject(shape_id: _, proto:, slots:) -> {
      let acc = push_opt_handle(proto, [])
      rt_types.shape_slots_fold(slots, acc, fn(_, v, a) { push_val_refs(v, a) })
    }
    SBox(value:) -> push_val_refs(value, [])
    SPromiseData(state:, is_handled: _) -> push_promise_state_refs(state, [])
    SGenerator(state: _, resume:) -> push_resume_refs(resume, [])
    SAsyncGen(state: _, resume:, queue: #(front, back)) -> {
      let acc = push_resume_refs(resume, [])
      let acc = list.fold(front, acc, push_request_refs)
      list.fold(back, acc, push_request_refs)
    }
    SAsyncContext(resume:, promise:) -> push_resume_refs(resume, [promise.id])
  }
}

/// Refs a parked coroutine keeps alive. Exhaustive over `Resume`.
fn push_resume_refs(resume: Resume, acc: List(Int)) -> List(Int) {
  case resume {
    // A compiled state machine: the fun's captured env plus its own locals
    // tuple, both opaque compiled terms only the FFI walk can open.
    ResumeCompiled(sm:, rs: _, loc:) ->
      push_term_refs(to_dynamic(loc), push_term_refs(to_dynamic(sm), acc))
    ResumeFrame(frame:) -> push_suspended_frame_refs(frame, acc)
  }
}

/// Refs a parked interpreter frame keeps alive: its locals, operand stack,
/// receiver, home object and its own sloppy-direct-eval var object (reachable
/// from nowhere else, so a resume after a collection would find it gone).
/// Port of arc `gc_trace.push_suspended_frame_refs`. EXHAUSTIVE destructure:
/// a new `SuspendedFrame` field is a compile error here until classified.
pub fn push_suspended_frame_refs(
  frame: SuspendedFrame,
  acc: List(Int),
) -> List(Int) {
  let SuspendedFrame(
    template:,
    // Plain index.
    pc: _,
    locals:,
    stack:,
    // Scalar: pc offsets and a stack depth.
    try_stack: _,
    this:,
    home_object:,
    eval_env:,
    line: _,
    parked: _,
    call_args:,
  ) = frame
  let acc = push_template_refs(template, acc)
  let acc = push_vals_tuple_refs(locals, acc)
  let acc = list.fold(stack, acc, fn(a, v) { push_val_refs(v, a) })
  let acc = push_val_refs(this, acc)
  let acc = push_val_refs(home_object, acc)
  let acc = list.fold(call_args, acc, fn(a, v) { push_val_refs(v, a) })
  case eval_env {
    Some(id) -> [id, ..acc]
    None -> acc
  }
}

/// Refs a code template keeps alive: only its own constant pool can name a
/// value. Nested `functions` are not walked: a nested template is compile-time
/// data until MakeClosure instantiates it as its own `KBytecode` cell, which
/// is then traced here in its own right. EXHAUSTIVE destructure.
fn push_template_refs(template: FuncTemplate, acc: List(Int)) -> List(Int) {
  let FuncTemplate(
    name: _,
    arity: _,
    length: _,
    local_count: _,
    // Opcodes: operands are ints, strings and pool indices.
    bytecode: _,
    constants:,
    functions: _,
    // Parent-slot indices.
    env_descriptors: _,
    is_strict: _,
    is_arrow: _,
    is_derived_constructor: _,
    is_generator: _,
    is_async: _,
    is_constructor: _,
    is_class_constructor: _,
    // Names and slot indices.
    local_names: _,
    lexical: _,
    code_kind: _,
  ) = template
  push_vals_tuple_refs(constants, acc)
}

/// A closure's captured environment: a flat tuple of values.
fn push_env_refs(env: EnvTuple, acc: List(Int)) -> List(Int) {
  push_term_refs(to_dynamic(env), acc)
}

/// A flat tuple of values (locals, constant pool), walked in one FFI pass.
fn push_vals_tuple_refs(vals: TupleArray(JsVal), acc: List(Int)) -> List(Int) {
  push_term_refs(to_dynamic(vals), acc)
}

/// One queued `.next`/`.throw`/`.return` on an async generator: the sent
/// value and the promise capability it settles. EXHAUSTIVE destructure.
fn push_request_refs(acc: List(Int), req: AsyncGenRequest) -> List(Int) {
  let AsyncGenRequest(completion: _, value:, resolve:, reject:) = req
  acc
  |> push_val_refs(value, _)
  |> push_val_refs(resolve, _)
  |> push_val_refs(reject, _)
}

/// Refs reachable from an `ObjKind`. EXHAUSTIVE (SPEC §7.M2 table) — no
/// wildcard. Weak keys (`WeakMapObj`/`WeakSetObj`) are not traced; dead-key
/// entries are pruned post-sweep (SPEC §7.M2 §weak).
fn push_objkind_refs(kind: ObjKind, acc: List(Int)) -> List(Int) {
  case kind {
    Ordinary -> acc
    ArrayObj(length: _) -> acc
    ArgumentsObj(length: _, mapped:) ->
      case mapped {
        Some(hs) -> list.fold(hs, acc, fn(a, h) { [h.id, ..a] })
        None -> acc
      }
    StringObj(value: _) -> acc
    NumberObj(value: _) -> acc
    BooleanObj(value: _) -> acc
    BigIntObj(value: _) -> acc
    SymbolObj(value: _) -> acc
    KCompiled(code:, home_object:, flags: _, fields_init:, simple:) -> {
      let acc = push_opt_handle(home_object, acc)
      let acc = push_opt_handle(fields_init, acc)
      // `code`/`simple` are opaque `CompiledFn`s; the captures live in their
      // fun env, walked via FFI.
      let acc = push_term_refs(to_dynamic(code), acc)
      push_term_refs(to_dynamic(simple), acc)
    }
    KBytecode(template:, env:, home_object:, flags: _, fields_init:) -> {
      let acc = push_opt_handle(home_object, acc)
      let acc = push_opt_handle(fields_init, acc)
      let acc = push_template_refs(template, acc)
      push_env_refs(env, acc)
    }
    KNative(tag:, name: _, length: _, constructible: _) -> {
      let acc = list.fold(native_token_refs(tag), acc, fn(a, h) { [h.id, ..a] })
      // `JsVal` fields in closure-carrying variants (`PromiseNative` etc.)
      // are opaque wire terms — walk them via the FFI term scanner.
      push_term_refs(to_dynamic(tag), acc)
    }
    KBound(target:, bound_this:, bound_args:) -> {
      let acc = push_val_refs(bound_this, [target.id, ..acc])
      list.fold(bound_args, acc, fn(a, v) { push_val_refs(v, a) })
    }
    // Handles the embedder stashed inside its own value keep their cells.
    KHost(payload:) -> push_term_refs(to_dynamic(payload), acc)
    ErrorObj(stack: _) -> acc
    MapObj(entries:) ->
      ordered_entries.fold(entries, acc, fn(a, k, v) {
        push_val_refs(v, push_term_refs(to_dynamic(k), a))
      })
    SetObj(entries:) ->
      ordered_entries.fold(entries, acc, fn(a, k, _) {
        push_term_refs(to_dynamic(k), a)
      })
    // Weak KEYS are not roots (dead-key entries are pruned post-sweep); a
    // WeakMap's VALUES are held strongly, so a value reachable only through
    // its entry survives for as long as the entry does.
    WeakMapObj(entries:) ->
      dict.fold(entries, acc, fn(a, _, v) { push_val_refs(v, a) })
    WeakSetObj(entries: _) -> acc
    DateObj(ms: _) -> acc
    RegExpObj(source: _, flags: _, last_index: _, compiled: _) -> acc
    // Storage is bytes-or-nothing in every variant: no handle refs.
    ArrayBufferObj(storage: _) -> acc
    TypedArrayObj(buffer:, elem_kind: _, byte_offset: _, length: _) -> [
      buffer.id,
      ..acc
    ]
    DataViewObj(buffer:, byte_offset: _, byte_length: _) -> [buffer.id, ..acc]
    RawJsonObj(raw: _) -> acc
    ModuleNamespace(exports:) ->
      dict.fold(exports, acc, fn(a, _, h) { [h.id, ..a] })
    ProxyObj(target:, handler:, revoked: _) -> [target.id, handler.id, ..acc]
    ForInIterator(remaining: _) -> acc
    ArrayIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    MapIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    SetIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    StringIterator(source: _, index: _) -> acc
    PromiseObj(data:) -> [data.id, ..acc]
    GeneratorObj(data:) -> [data.id, ..acc]
    AsyncGeneratorObj(data:) -> [data.id, ..acc]
    AsyncFromSyncIterator(sync_rec:) -> [sync_rec.id, ..acc]
    // ES2025 iterator helpers — payload nests JsVals + Option(IteratorRecord);
    // walk via the FFI term scanner (arc parity: value.gleam refs are opaque).
    IteratorHelperObj(gen_state: _, body:) ->
      push_term_refs(to_dynamic(body), acc)
    WrapForValidIteratorObj(record:) -> push_term_refs(to_dynamic(record), acc)
    // The resolved Intl state is handle-free; only the bound-method cache
    // holds a cell.
    IntlObj(data: _, bound:) -> push_opt_handle(bound, acc)
    // Temporal internal slots are plain integers, calendars and resolved
    // time zones: no handles.
    TemporalObj(data: _) -> acc
    // The resource stack holds user values, dispose methods and callback
    // argument lists — all `JsVal`s; walk the whole state term.
    DisposableStackObj(async: _, state:) ->
      push_term_refs(to_dynamic(state), acc)
  }
}

fn push_property_refs(prop: Property, acc: List(Int)) -> List(Int) {
  case prop {
    DataProperty(value:, writable: _, enumerable: _, configurable: _, seq: _) ->
      push_val_refs(value, acc)
    AccessorProperty(get:, set:, enumerable: _, configurable: _, seq: _) ->
      push_opt_val(set, push_opt_val(get, acc))
  }
}

fn push_elements_refs(elems: JsElements, acc: List(Int)) -> List(Int) {
  case elems {
    NoElements -> acc
    Dense(arr) ->
      rt_tree_array.sparse_fold(fn(_, v, a) { push_val_refs(v, a) }, acc, arr)
    Sparse(d) -> dict.fold(d, acc, fn(a, _, v) { push_val_refs(v, a) })
  }
}

fn push_promise_state_refs(state: PromiseState, acc: List(Int)) -> List(Int) {
  case state {
    PromisePending(reactions:) -> list.fold(reactions, acc, push_reaction_refs)
    PromiseFulfilled(v) -> push_val_refs(v, acc)
    PromiseRejected(v) -> push_val_refs(v, acc)
  }
}

fn push_reaction_refs(acc: List(Int), r: PromiseReaction) -> List(Int) {
  let PromiseReaction(on_fulfill:, on_reject:, child_resolve:, child_reject:) =
    r
  let acc = push_reaction_handler_refs(on_fulfill, acc)
  let acc = push_reaction_handler_refs(on_reject, acc)
  push_val_refs(child_reject, push_val_refs(child_resolve, acc))
}

fn push_reaction_handler_refs(h: ReactionHandler, acc: List(Int)) -> List(Int) {
  case h {
    Handler(fun:) -> push_val_refs(fun, acc)
    IdentityPassThrough | ThrowerPassThrough -> acc
  }
}

/// Refs reachable from a microtask `Job`. Exhaustive over `Job`; each variant
/// fully destructured so a new job kind (or a new ref-carrying field on an
/// existing one) is a compile error here. Port of arc
/// `gc_trace.push_job_refs`. `roots_of_state` walks the materialized queue via
/// `push_term_refs` — this is the typed alternative for callers holding a
/// `Job` directly.
pub fn push_job_refs(job: Job, acc: List(Int)) -> List(Int) {
  case job {
    ReactionJob(handler:, arg:, resolve:, reject:) ->
      acc
      |> push_reaction_handler_refs(handler, _)
      |> push_val_refs(arg, _)
      |> push_val_refs(resolve, _)
      |> push_val_refs(reject, _)
    ResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      acc
      |> push_val_refs(thenable, _)
      |> push_val_refs(then_fn, _)
      |> push_val_refs(resolve, _)
      |> push_val_refs(reject, _)
    // Handles the closure captured live in its fun env.
    HostJob(run:) -> push_term_refs(to_dynamic(run), acc)
  }
}

fn push_opt_handle(oh: Option(Handle), acc: List(Int)) -> List(Int) {
  case oh {
    Some(h) -> [h.id, ..acc]
    None -> acc
  }
}

fn push_opt_val(ov: Option(JsVal), acc: List(Int)) -> List(Int) {
  case ov {
    Some(v) -> push_val_refs(v, acc)
    None -> acc
  }
}

// ── mark / sweep (arc heap.gleam:442-563) ───────────────────────────────────
//
// SPEC §7.M2 "Dropped from arc": `lazy_proto` handling (arc heap.gleam:
// 513-530) — protos are eagerly-allocated real cells here, so `mark_loop`
// has NO tagged-id decode branch and `sweep` has NO `is_real_slot` filter.
// `t_compact` (arc heap.gleam:453-460) is DROPPED — handles are stable,
// sweep-to-free-list only (M2.md:49 keeps it; SPEC §7.M2:618 drops it;
// SPEC wins per RULINGS precedence).

/// Default allocation-count threshold before an automatic collection. Seeds
/// `JsStore.gc_threshold` in `t_store_new`; `t_maybe_collect` reads the
/// per-instance field, not this const. Port of arc `gc_growth_threshold`
/// (interpreter.gleam:5796).
pub const default_gc_threshold: Int = 65_536

/// TURN-BOUNDARY safepoint (D11). Collects only when `call_depth == 0` AND
/// `alloc_since_gc >= gc_threshold` (the store's own `gc_threshold` field).
/// Safepoints: `rt/async.drain` between jobs, the runner / engine after a
/// top-level return, and the interpreter's root-activation `Return`
/// (`arc/interp/safepoint`, via `t_maybe_collect_with`). NEVER at fn-entry.
pub fn t_maybe_collect(st: Agent) -> Agent {
  t_maybe_collect_with(st, [])
}

/// `t_maybe_collect` for a caller that still holds live values the store
/// cannot see: the interpreter's root activation passes its frame registers.
/// Same gate; `extra_roots` only matter when it fires.
pub fn t_maybe_collect_with(st: Agent, extra_roots: List(Handle)) -> Agent {
  let js = require_js(st)
  case js.call_depth == 0 && js.alloc_since_gc >= js.gc_threshold {
    True -> t_collect(st, extra_roots)
    False -> st
  }
}

/// Keep the cells `held` names alive across a stretch where safepoints fire
/// without the holder's roots (a microtask drain run while the engine holds
/// a completion value in Gleam). Pins the ids not already permanent and
/// returns exactly those, for `t_release_roots` to undo this hold and
/// nothing else.
pub fn t_hold_roots(st: Agent, held: List(JsVal)) -> #(Agent, List(Int)) {
  let js = require_js(st)
  let ids =
    list.fold(held, [], fn(acc, v) { push_val_refs(v, acc) })
    |> list.filter(fn(id) { !set.contains(js.pinned_roots, id) })
    |> list.unique
  let pinned = list.fold(ids, js.pinned_roots, set.insert)
  #(Agent(..st, store: JsStore(..js, pinned_roots: pinned)), ids)
}

/// Undo a `t_hold_roots`.
pub fn t_release_roots(st: Agent, ids: List(Int)) -> Agent {
  let js = require_js(st)
  let pinned = list.fold(ids, js.pinned_roots, set.delete)
  Agent(..st, store: JsStore(..js, pinned_roots: pinned))
}

/// Mark-and-sweep the JS heap. Roots = `roots_of_state(st)` ∪ `extra_roots`
/// (the interpreter's live root frame, or a host-driven mid-turn `gc()`).
/// Resets `alloc_since_gc`. NO id renumbering (SPEC §7.M2 invariant).
/// Port of arc `heap.collect_with_roots` (heap.gleam:470-476).
pub fn t_collect(st: Agent, extra_roots: List(Handle)) -> Agent {
  let js = require_js(st)
  let roots =
    list.fold(extra_roots, roots_of_state(st), fn(a, h) { [h.id, ..a] })
  let live = mark_from(js.data, roots)
  let #(data, free) = sweep(js.data, js.free, live)
  let data = prune_weak(data, live)
  Agent(..st, store: JsStore(..js, data:, free:, alloc_since_gc: 0))
}

/// Mark phase: from `roots`, return every reachable cell id. Port of arc
/// `heap.mark_from` (heap.gleam:487-490).
fn mark_from(data: Dict(Int, JsSlot), roots: List(Int)) -> Set(Int) {
  mark_loop(data, roots, set.new())
}

/// Tail-recursive DFS. Cycles: `visited` check. Dangling refs: `dict.get`
/// miss → skip. arc's `lazy_proto.decode_lazy_proto` branches (heap.gleam:
/// 513-530) DELETED — no tagged ids in 2core (SPEC §7.M2 "Dropped from arc").
fn mark_loop(
  data: Dict(Int, JsSlot),
  frontier: List(Int),
  visited: Set(Int),
) -> Set(Int) {
  case frontier {
    [] -> visited
    [id, ..rest] ->
      case set.contains(visited, id) {
        True -> mark_loop(data, rest, visited)
        False -> {
          let visited = set.insert(visited, id)
          case dict.get(data, id) {
            Error(Nil) -> mark_loop(data, rest, visited)
            Ok(slot) ->
              // Prepend child ids directly onto frontier — avoids the
              // reverse+copy `list.append` allocates (arc heap.gleam:519-522).
              mark_loop(data, prepend_ids(refs_in_cell(slot), rest), visited)
          }
        }
      }
  }
}

/// Prepend `ids` onto `tail` without the intermediate reversed copy that
/// `list.append` allocates. Port of arc `heap.prepend_ref_ids` (heap.gleam:
/// 541-546); ids are already `Int` here so no `.id` unwrap is needed.
fn prepend_ids(ids: List(Int), tail: List(Int)) -> List(Int) {
  case ids {
    [] -> tail
    [id, ..rest] -> prepend_ids(rest, [id, ..tail])
  }
}

/// Sweep: keep only live cells, fold dead ids onto the free list. NO id
/// renumbering. arc's `!is_real_slot(id)` guard DELETED — no tagged ids
/// (SPEC §7.M2 "Dropped from arc"). Port of arc `heap.sweep` (heap.gleam:
/// 550-563).
fn sweep(
  data: Dict(Int, JsSlot),
  free: List(Int),
  live: Set(Int),
) -> #(Dict(Int, JsSlot), List(Int)) {
  let new_data = dict.filter(data, fn(id, _) { set.contains(live, id) })
  let new_free =
    dict.fold(data, free, fn(f, id, _) {
      case set.contains(live, id) {
        True -> f
        False -> [id, ..f]
      }
    })
  #(new_data, new_free)
}

/// Post-sweep weak-prune (SPEC §7.M2 §weak): drop `WeakMapObj`/`WeakSetObj`
/// entries whose key-id ∉ `live`. Weak keys are NOT traced during mark, so a
/// key held ONLY by a weak container is swept, and its entry (value
/// included) disappears here in the same collection. Symbol keys are not
/// heap cells and are never pruned.
fn prune_weak(data: Dict(Int, JsSlot), live: Set(Int)) -> Dict(Int, JsSlot) {
  let keep = fn(k: WeakKey) {
    case k {
      WeakObjKey(id:) -> set.contains(live, id)
      WeakSymKey(_) -> True
    }
  }
  dict.map_values(data, fn(_, slot) {
    case slot {
      SObject(kind: WeakMapObj(entries:), ..) ->
        SObject(
          ..slot,
          kind: WeakMapObj(entries: dict.filter(entries, fn(k, _) { keep(k) })),
        )
      SObject(kind: WeakSetObj(entries:), ..) ->
        SObject(..slot, kind: WeakSetObj(entries: set.filter(entries, keep)))
      _ -> slot
    }
  })
}

// ── stats ───────────────────────────────────────────────────────────────────

/// Snapshot of the JS heap's occupancy for diagnostics / test assertions
/// (M2.md:77).
pub type GcStats {
  GcStats(
    /// `dict.size(data)` — cells currently allocated.
    live: Int,
    /// `list.length(free)` — recycled ids awaiting reuse.
    free: Int,
    /// `next` — next never-used id (total ids ever minted).
    next: Int,
    /// `alloc_since_gc` — allocations since the last `t_collect`.
    since_gc: Int,
  )
}

/// Read the current heap occupancy. Total; never mutates.
pub fn stats(st: Agent) -> GcStats {
  let js = require_js(st)
  GcStats(
    live: dict.size(js.data),
    free: list.length(js.free),
    next: js.next,
    since_gc: js.alloc_since_gc,
  )
}

/// True when `h`'s id is in `store.data`. `WeakRef.deref` liveness check
/// (SPEC §7.M2 §weak).
pub fn t_is_live(st: Agent, h: Handle) -> Bool {
  let js = require_js(st)
  let JsCell(id) = h
  dict.has_key(js.data, id)
}
