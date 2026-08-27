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

import arc/internal/ordered_entries
import arc/internal/tree_array
import arc/internal/tuple_array.{type TupleArray}
import arc/rt/arena.{type Arena}
import arc/rt/bytecode.{
  type EnvTuple, type FuncTemplate, type SuspendedFrame, FuncTemplate,
  SuspendedFrame,
}
import arc/rt/types.{
  type Agent, type AsyncGenRequest, type Handle, type IcEntry, type Job,
  type JsElements, type JsSlot, type JsStore, type JsVal, type ObjKind,
  type PromiseReaction, type PromiseState, type Property, type ReactionHandler,
  type Resume, type WeakKey, Agent, ArgumentsObj, ArrayBufferObj, ArrayIterator,
  ArrayObj, AsyncFromSyncIterator, AsyncGenRequest, AsyncGeneratorObj, BigIntObj,
  BooleanObj, DataViewObj, DateObj, Dense, DisposableStackObj, ErrorObj,
  FinRegCell, FinalizationRegistryObj, ForInIterator, GeneratorObj, Handler,
  HostJob, IcCall, IcRead, IdentityPassThrough, IntlObj, IteratorHelperObj,
  JsCell, JsStore, KBound, KBytecode, KCompiled, KHandle, KHost, KNative,
  MapIterator, MapObj, ModuleNamespace, NoElements, NumberObj, Ordinary,
  PromiseFulfilled, PromiseObj, PromisePending, PromiseReaction, PromiseRejected,
  ProxyObj, RawJsonObj, ReactionJob, RegExpObj, ResolveThenableJob,
  ResumeCompiled, ResumeFrame, SAsyncContext, SAsyncGen, SBox,
  SDisposeCapability, SGenerator, SObject, SPromiseData, SShapedObject,
  SetIterator, SetObj, Sparse, StringIterator, StringObj, SymbolObj, TemporalObj,
  ThrowerPassThrough, TypedArrayObj, WeakMapObj, WeakObjKey, WeakRefObj,
  WeakSetObj, WeakSymKey, WrapForValidIteratorObj, classify, jq_to_list,
  native_token_refs,
} as rt_types
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

// ── FFI: deep BEAM-term walk (M2-I8) ────────────────────────────────────────

/// Deep-walk any BEAM term, pushing every `{js_cell, N}` id onto `acc`.
/// Recurses into tuples/lists/maps AND fun captured env via
/// `erlang:fun_info(F, env)` — how a JS closure stored in a cell keeps its
/// captured `Handle` bindings alive. Impl: `arc_rt_gc_ffi.erl`.
@external(erlang, "arc_rt_gc_ffi", "refs_in_term")
pub fn push_term_refs(v: Dynamic, acc: List(Int)) -> List(Int)

/// The ids an object's property map / symbol-property list name: property
/// values and accessor pairs, read by position (`arc_rt_layout.hrl`).
@external(erlang, "arc_rt_gc_ffi", "refs_in_props")
fn push_props_refs(props: Dict(k, Property), acc: List(Int)) -> List(Int)

@external(erlang, "arc_rt_gc_ffi", "refs_in_symbol_props")
fn push_symbol_props_refs(
  props: List(#(k, Property)),
  acc: List(Int),
) -> List(Int)

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
/// `eval_env` — no interpreter frame exists at a D11 turn-boundary
/// safepoint; the waitAsync waiters moved to `Agent.waiters` and ARE walked)
/// and MINUS the `RealmCtx` destructure (subsumed
/// by `pinned_roots` per above). EXHAUSTIVE `JsStore` destructure (M2-I5 —
/// no `..`): adding a store field is a compile error here until rooted.
pub fn roots_of_state(st: Agent) -> List(Int) {
  let JsStore(
    // ── cell arena bookkeeping — no roots ──
    data: _,
    next: _,
    // Set(Int): realm intrinsics + global_object + captured-binding cells.
    pinned_roots:,
    // ── GC trigger counters — no roots ──
    alloc_since_gc: _,
    gc_threshold: _,
    gc_live: _,
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
    // Parse-id counter — no roots.
    unit_uid: _,
    // Inline caches: call entries hold handles but are validated on use
    // (rt_types.IcCall), so they are weak — never roots.
    ics: _,
  ) = require_js(st)
  let acc = set.to_list(pinned_roots)
  let acc = list.append(unhandled_rejections, acc)
  let acc = push_term_refs(to_dynamic(jq_to_list(microtasks)), acc)
  // Embedder closures may capture handles (a class constructor holding its
  // prototype); walk their fun envs, the import hook's included.
  let acc = push_term_refs(to_dynamic(dict.values(st.host_fns)), acc)
  let acc = push_term_refs(to_dynamic(st.import_hook), acc)
  // A parked Atomics.waitAsync waiter's promise capability is usually
  // unreachable from JS by the time the drain collects between jobs.
  let acc = push_term_refs(to_dynamic(st.waiters), acc)
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

/// Push every cell id directly reachable from `slot` onto `acc`. Port of arc
/// `gc_trace.refs_in_slot`. EXHAUSTIVE match on `JsSlot` with NO wildcard
/// (SPEC §7.M2 / D8 safety property): adding a `JsSlot` variant is a compile
/// error here, never a silent free-of-live-cell.
pub fn refs_in_cell(slot: JsSlot, acc: List(Int)) -> List(Int) {
  case slot {
    // `kind` keeps a typed walk (weak containers, code templates) and so do
    // dense elements: an OTP array read other than through its API can see
    // a stale leaf.
    SObject(kind:, proto:, props:, symbol_props:, elements:, extensible: _) ->
      push_objkind_refs(kind, push_opt_handle(proto, acc))
      |> push_props_refs(props, _)
      |> push_symbol_props_refs(symbol_props, _)
      |> push_elements_refs(elements, _)
    SShapedObject(shape_id: _, proto:, slots:) ->
      push_term_refs(to_dynamic(slots), push_opt_handle(proto, acc))
    SBox(value:) -> push_val_refs(value, acc)
    SPromiseData(state:, is_handled: _) -> push_promise_state_refs(state, acc)
    SGenerator(state: _, resume:) -> push_resume_refs(resume, acc)
    SAsyncGen(state: _, resume:, queue: #(front, back)) -> {
      let acc = push_resume_refs(resume, acc)
      let acc = list.fold(front, acc, push_request_refs)
      list.fold(back, acc, push_request_refs)
    }
    SAsyncContext(resume:, promise:) ->
      push_resume_refs(resume, [promise.id, ..acc])
    // The resource stack holds user values, dispose methods and callback
    // argument lists — all `JsVal`s; walk the whole list term.
    SDisposeCapability(resources:) -> push_term_refs(to_dynamic(resources), acc)
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
    parked: _,
    call_args:,
    // Realm id: realms are registry entries, rooted in their own right.
    realm: _,
    // Plain id.
    unit: _,
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
    // Plain ints.
    lines: _,
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
    KCompiled(
      code:,
      home_object:,
      flags: _,
      fields_init:,
      simple:,
      name: _,
      length: _,
      birth:,
    ) -> {
      let acc = push_opt_handle(home_object, acc)
      let acc = push_opt_handle(fields_init, acc)
      let acc = push_birth_refs(birth, acc)
      // `code`/`simple` are opaque `CompiledFn`s; the captures live in their
      // fun env, walked via FFI.
      let acc = push_term_refs(to_dynamic(code), acc)
      push_term_refs(to_dynamic(simple), acc)
    }
    KBytecode(
      template:,
      env:,
      home_object:,
      flags: _,
      fields_init:,
      realm: _,
      unit: _,
      birth:,
    ) -> {
      let acc = push_opt_handle(home_object, acc)
      let acc = push_opt_handle(fields_init, acc)
      let acc = push_birth_refs(birth, acc)
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
    // A pending stack keeps its [[DisposeCapability]] cell alive.
    DisposableStackObj(async: _, state: rt_types.Pending(capability:)) -> [
      capability.id,
      ..acc
    ]
    DisposableStackObj(async: _, state: rt_types.Disposed) -> acc
    // [[CleanupCallback]] and every cell's [[HeldValue]] are strong; the
    // [[WeakRefTarget]] and [[UnregisterToken]] are weak (§26.2.1.1) and NOT
    // traced — a cell whose target dies is dropped in `prune_weak`.
    FinalizationRegistryObj(callback:, cells:) ->
      list.fold(cells, push_val_refs(callback, acc), fn(a, cell) {
        push_val_refs(cell.held, a)
      })
    // [[WeakRefTarget]] is weak (§26.1.1.1) and NOT traced — emptied in
    // `prune_weak` once the target dies.
    WeakRefObj(target: _) -> acc
    // A realm id: the realm's intrinsics are pinned roots already.
    rt_types.ShadowRealmObj(realm: _) -> acc
  }
}

fn push_elements_refs(elems: JsElements, acc: List(Int)) -> List(Int) {
  case elems {
    NoElements -> acc
    Dense(arr) ->
      tree_array.sparse_fold(fn(_, v, a) { push_val_refs(v, a) }, acc, arr)
    Sparse(d) -> push_term_refs(to_dynamic(d), acc)
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

fn push_birth_refs(birth: rt_types.FnBirth, acc: List(Int)) -> List(Int) {
  case birth {
    rt_types.BirthPending(prototype_parent:) ->
      push_opt_handle(prototype_parent, acc)
    rt_types.BirthSettled -> acc
  }
}

// ── mark / sweep (arc heap.gleam:442-563) ───────────────────────────────────
//
// SPEC §7.M2 "Dropped from arc": `lazy_proto` handling (arc heap.gleam:
// 513-530) — protos are eagerly-allocated real cells here, so `mark_loop`
// has NO tagged-id decode branch and `sweep` has NO `is_real_slot` filter.
// Handles are stable: a collection never renumbers ids. Like arc
// `heap.compact` (heap.gleam:459-473) the sweep DISCARDS the dead ids rather
// than keeping a free list: an allocation-heavy turn (test262's
// dst-offset-caching family allocates ~2.4M short-lived Dates) would
// otherwise leave a multi-megaword free list inside the store record, which
// then travels with the Agent across every process boundary and defeats the
// point of collecting. Only the ids past the highest survivor come back, by
// lowering `next`; the rest is wasted id space, and ids are plain ints.

/// Default allocation-count threshold before an automatic collection. Seeds
/// `JsStore.gc_threshold` in `t_store_new`; `t_maybe_collect` reads the
/// per-instance field, not this const. Port of arc `gc_growth_threshold`
/// (interpreter.gleam:5796).
pub const default_gc_threshold: Int = 65_536

/// TURN-BOUNDARY safepoint (D11). Collects only when `call_depth == 0` AND
/// the store is `due` a collection. Safepoints: `rt/async.drain` between
/// jobs, the runner / engine after a top-level return, and the interpreter's
/// root-activation `Return` (`arc/interp/safepoint`, via
/// `t_maybe_collect_with`). NEVER at fn-entry.
pub fn t_maybe_collect(st: Agent) -> Agent {
  t_maybe_collect_with(st, [])
}

/// `t_maybe_collect` for a caller that still holds live values the store
/// cannot see: the interpreter's root activation passes its frame registers.
/// Same gate; `extra_roots` only matter when it fires.
pub fn t_maybe_collect_with(st: Agent, extra_roots: List(Handle)) -> Agent {
  case st.call_depth == 0 && due(require_js(st)) {
    True -> t_collect(st, extra_roots)
    False -> st
  }
}

/// A collection is due once the allocations since the last one reach
/// `gc_threshold`, scaled up by `gc_live / (2 * default_gc_threshold)` when
/// the survivors of the last collection outnumber that: with the default
/// threshold a large heap is next marked after allocating half its live size
/// again, so marking stays proportional to allocation instead of costing the
/// whole live set every `gc_threshold` cells.
pub fn due(js: JsStore(st)) -> Bool {
  js.alloc_since_gc >= js.gc_threshold
  && js.alloc_since_gc * 2 * default_gc_threshold
  >= js.gc_threshold * js.gc_live
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
/// Resets `alloc_since_gc` and records the survivor count for `due`. NO id
/// renumbering (SPEC §7.M2 invariant), but `next` falls back to just past
/// the highest survivor so the ids of a dead tail are minted again and the
/// arena's depth tracks the live set rather than every allocation ever made.
pub fn t_collect(st: Agent, extra_roots: List(Handle)) -> Agent {
  let js = require_js(st)
  let roots =
    list.fold(extra_roots, roots_of_state(st), fn(a, h) { [h.id, ..a] })
  let live = mark_loop(js.data, roots, dict.new())
  let #(data, next) = sweep(js.data, live)
  Agent(
    ..st,
    store: JsStore(
      ..js,
      data:,
      next:,
      alloc_since_gc: 0,
      gc_live: dict.size(live),
      ics: dict.filter(js.ics, fn(_, entry) { is_read_ic(entry) }),
    ),
  )
}

/// Read entries name shapes, which are never recycled; call entries name
/// cell ids, which `sweep` hands out again past the highest survivor.
fn is_read_ic(entry: IcEntry) -> Bool {
  case entry {
    IcRead(..) -> True
    IcCall(..) -> False
  }
}

/// The mark set is a bare map keyed by cell id, probed and grown with the
/// map BIFs directly: this is the one loop that runs per heap edge.
@external(erlang, "maps", "is_key")
fn marked(id: Int, live: Dict(Int, Nil)) -> Bool

@external(erlang, "maps", "put")
fn mark(id: Int, nil: Nil, live: Dict(Int, Nil)) -> Dict(Int, Nil)

/// Mark phase: tail-recursive DFS from `frontier`, returning every reachable
/// cell id. Cycles: `visited` check. Dangling refs: an absent id is skipped.
fn mark_loop(
  data: Arena(JsSlot),
  frontier: List(Int),
  visited: Dict(Int, Nil),
) -> Dict(Int, Nil) {
  case frontier {
    [] -> visited
    [id, ..rest] ->
      case marked(id, visited) {
        True -> mark_loop(data, rest, visited)
        False -> {
          let visited = mark(id, Nil, visited)
          case arena.get_option(id, data) {
            None -> mark_loop(data, rest, visited)
            Some(slot) -> mark_loop(data, refs_in_cell(slot, rest), visited)
          }
        }
      }
  }
}

/// Sweep: rebuild the arena from the live cells alone, in one ascending pass
/// that also applies the weak-container prune, and report the `next` id to
/// mint (one past the highest survivor). Each leaf is built once and the
/// leaves only dead ids occupied are dropped.
fn sweep(data: Arena(JsSlot), live: Dict(Int, Nil)) -> #(Arena(JsSlot), Int) {
  let kept =
    arena.fold(
      fn(id, slot, acc) {
        case marked(id, live) {
          True -> [#(id, prune_weak_slot(slot, live)), ..acc]
          False -> acc
        }
      },
      [],
      data,
    )
  let next = case kept {
    [] -> 0
    [#(id, _), ..] -> id + 1
  }
  #(arena.from_descending(kept), next)
}

/// Post-sweep weak-prune (SPEC §7.M2 §weak): drop `WeakMapObj`/`WeakSetObj`
/// entries whose key-id ∉ `live`. Weak keys are NOT traced during mark, so a
/// key held ONLY by a weak container is swept, and its entry (value
/// included) disappears here in the same collection. Symbol keys are not
/// heap cells and are never pruned. A `FinalizationRegistryObj` cell whose
/// [[WeakRefTarget]] died is dropped (§9.10.3: the target is set to empty and
/// the cleanup job — which this runtime never enqueues — would remove it); a
/// surviving cell whose [[UnregisterToken]] died has the token emptied so a
/// recycled cell id can never match it. Every other slot passes through.
fn prune_weak_slot(slot: JsSlot, live: Dict(Int, Nil)) -> JsSlot {
  case slot {
    SObject(kind: WeakMapObj(entries:), ..) ->
      SObject(
        ..slot,
        kind: WeakMapObj(
          entries: dict.filter(entries, fn(k, _) { weak_key_live(k, live) }),
        ),
      )
    SObject(kind: WeakSetObj(entries:), ..) ->
      SObject(
        ..slot,
        kind: WeakSetObj(
          entries: set.filter(entries, fn(k) { weak_key_live(k, live) }),
        ),
      )
    SObject(kind: FinalizationRegistryObj(callback:, cells:), ..) -> {
      let cells =
        list.filter(cells, fn(c) { option.is_some(weak_live(c.target, live)) })
        |> list.map(fn(c) {
          FinRegCell(..c, token: option.then(c.token, weak_live(_, live)))
        })
      SObject(..slot, kind: FinalizationRegistryObj(callback:, cells:))
    }
    SObject(kind: WeakRefObj(target:), ..) ->
      SObject(
        ..slot,
        kind: WeakRefObj(target: option.then(target, weak_live(_, live))),
      )
    _ -> slot
  }
}

fn weak_key_live(k: WeakKey, live: Dict(Int, Nil)) -> Bool {
  case k {
    WeakObjKey(id:) -> marked(id, live)
    WeakSymKey(_) -> True
  }
}

/// A weakly-held value after the sweep: itself when still usable (a live
/// cell, or a symbol — not a heap cell), None when its cell was swept.
fn weak_live(v: JsVal, live: Dict(Int, Nil)) -> Option(JsVal) {
  case classify(v) {
    KHandle(JsCell(id)) ->
      case marked(id, live) {
        True -> Some(v)
        False -> None
      }
    _ -> Some(v)
  }
}

// ── stats ───────────────────────────────────────────────────────────────────

/// Snapshot of the JS heap's occupancy for diagnostics / test assertions
/// (M2.md:77).
pub type GcStats {
  GcStats(
    /// Cells currently allocated.
    live: Int,
    /// `next` — the next id to mint.
    next: Int,
    /// `alloc_since_gc` — allocations since the last `t_collect`.
    since_gc: Int,
  )
}

/// Read the current heap occupancy. Total; never mutates.
pub fn stats(st: Agent) -> GcStats {
  let js = require_js(st)
  GcStats(
    live: arena.count(js.data),
    next: js.next,
    since_gc: js.alloc_since_gc,
  )
}

/// True when `h`'s id is in `store.data`. `WeakRef.deref` liveness check
/// (SPEC §7.M2 §weak).
pub fn t_is_live(st: Agent, h: Handle) -> Bool {
  let js = require_js(st)
  let JsCell(id) = h
  option.is_some(arena.get_option(id, js.data))
}
