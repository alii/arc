import arc/internal/ordered_entries
import arc/internal/tree_array
import arc/rt/arena.{type Arena}
import arc/rt/bytecode.{type SuspendedFrame, SuspendedFrame}
import arc/rt/prop_map.{type PropMap}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type AsyncGenRequest, type Cell, type Handle, type IcEntry,
  type Job, type JobQueue, type JsElements, type JsVal, type Kept, type ObjKind,
  type PromiseReaction, type PromiseState, type Property, type ReactionHandler,
  type Resume, type WeakKey, Agent, ArgumentsObj, ArrayBufferObj, ArrayIterator,
  ArrayObj, AsyncFromSyncIterator, AsyncGenRequest, AsyncGeneratorObj, BigIntObj,
  BooleanObj, BoundFn, BytecodeFn, CompiledFn, DataViewObj, DateObj, Dense,
  DisposableStackObj, ErrorObj, FinalizationRegistryObj, GeneratorObj, Handle,
  Handler, HostObj, IcCall, IcGlobal, IcInit, IcLiteral, IcOff, IcRead,
  IdentityPassThrough, IntlObj, IteratorHelperObj, KHandle, Kept, MapIterator,
  MapObj, ModuleNamespace, NativeFn, NoElements, NumberObj, Ordinary,
  PromiseFulfilled, PromiseObj, PromisePending, PromiseReaction, PromiseRejected,
  ProxyObj, RawJsonObj, RegExpObj, Registration, ResumeCompiled, ResumeFrame,
  SAsyncContext, SAsyncGen, SBox, SDisposeCapability, SGenerator, SObject,
  SPromiseData, SShapedObject, SetIterator, SetObj, Sparse, Store,
  StringIterator, StringObj, SymbolObj, TemporalObj, ThrowerPassThrough,
  TypedArrayObj, WeakMapObj, WeakObjKey, WeakRefObj, WeakSetObj, WeakSymKey,
  WrapForValidIteratorObj, classify, job_queue_aged, job_queue_fresh,
  job_queue_to_list,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

// every handle id inside any term, fun envs included
@external(erlang, "arc_rt_gc_ffi", "push_refs")
pub fn push_refs(term: a, acc: List(Int)) -> List(Int)

// refs only in what changed since the old copy, since unchanged parts were already seen
@external(erlang, "arc_rt_gc_ffi", "diff_refs")
fn diff_refs(before: a, now: a, acc: List(Int)) -> List(Int)

@external(erlang, "arc_rt_gc_ffi", "push_props_refs")
fn push_props_refs(props: PropMap(Property), acc: List(Int)) -> List(Int)

@external(erlang, "arc_rt_gc_ffi", "push_symbol_props_refs")
fn push_symbol_props_refs(
  props: List(#(k, Property)),
  acc: List(Int),
) -> List(Int)

pub fn agent_roots(st: Agent) -> List(Int) {
  roots_with(st, job_queue_to_list)
}

// exhaustive destructure: a new store field must be rooted here
fn roots_with(st: Agent, jobs: fn(JobQueue) -> List(Job)) -> List(Int) {
  let Store(
    gc_threshold: _,
    shapes: _,
    next_shape: _,
    // ics are validated on use, so weak
    ics: _,
    plain_write_protos: _,
    global_epoch: _,
    ops: _,
    microtasks:,
    pinned_roots:,
    meta: types.StoreMeta(
      live_count: _,
      next_private_id: _,
      next_symbol_id: _,
      next_unit_id: _,
      unhandled_rejections:,
      old_gen: _,
      young_start: _,
      major_live: _,
      minors_since_major: _,
      plain_index_protos: _,
      async_context_mapping:,
      lazy_code: _,
      threaded: _,
      threaded_before: _,
      thread_keys: _,
      draft_runs: _,
      next_template_id: _,
      unit_texts: _,
      kept: _,
      // pinned when made
      template_objects: _,
      host_fns:,
      realms:,
      import_hook:,
      waiters:,
      tz_zones: _,
      checked_subject: _,
    ),
  ) = st.store
  let acc = push_refs(async_context_mapping, set.to_list(pinned_roots))
  let acc = list.append(unhandled_rejections, acc)
  let acc = push_refs(jobs(microtasks), acc)
  let acc = push_refs(dict.values(host_fns), acc)
  let acc = push_refs(import_hook, acc)
  let acc = push_refs(waiters, acc)
  // registry copy of the current realm may be stale
  let realms = dict.insert(realms, st.realm.id, st.realm)
  dict.fold(realms, acc, fn(acc, _id, realm) {
    prop_map.fold(realm.lexical_globals, acc, fn(acc, _name, binding) {
      push_refs(types.lexical_global_value(binding), acc)
    })
  })
}

// exhaustive, no wildcard: a new variant must be traced; called by name from
// arc_rt_gc_ffi
pub fn push_cell_refs(cell: Cell, acc: List(Int)) -> List(Int) {
  case cell {
    SObject(kind:, proto:, props:, symbol_props:, elements:, extensible: _) ->
      push_objkind_refs(kind, push_optional_handle(proto, acc))
      |> push_props_refs(props, _)
      |> push_symbol_props_refs(symbol_props, _)
      |> push_elements_refs(elements, _)
    SShapedObject(shape_id: _, proto:, slots:, offsets: _) ->
      push_refs(slots, push_optional_handle(proto, acc))
    SBox(value:) -> push_refs(value, acc)
    SPromiseData(state:, is_handled: _) -> push_promise_state_refs(state, acc)
    SGenerator(state: _, resume:) -> push_resume_refs(resume, acc)
    SAsyncGen(state: _, resume:, front:, back:) -> {
      let acc = push_resume_refs(resume, acc)
      let acc = list.fold(front, acc, push_request_refs)
      list.fold(back, acc, push_request_refs)
    }
    SAsyncContext(resume:, promise:) ->
      push_resume_refs(resume, [promise.id, ..acc])
    SDisposeCapability(resources:) -> push_refs(resources, acc)
  }
}

fn push_resume_refs(resume: Resume, acc: List(Int)) -> List(Int) {
  case resume {
    ResumeCompiled(machine:, locals:, ..) ->
      push_refs(locals, push_refs(machine, acc))
    ResumeFrame(frame:) -> push_suspended_frame_refs(frame, acc)
  }
}

fn push_suspended_frame_refs(
  frame: SuspendedFrame,
  acc: List(Int),
) -> List(Int) {
  let SuspendedFrame(
    // compiled code holds primitives only
    template: _,
    pc: _,
    locals:,
    stack:,
    try_stack: _,
    eval_env:,
    parked: _,
    call_args:,
    realm: _,
    unit_id: _,
  ) = frame
  let acc = push_refs(locals, acc)
  let acc = list.fold(stack, acc, fn(a, v) { push_refs(v, a) })
  let acc = list.fold(call_args, acc, fn(a, v) { push_refs(v, a) })
  push_refs(eval_env, acc)
}

fn push_request_refs(acc: List(Int), req: AsyncGenRequest) -> List(Int) {
  let AsyncGenRequest(completion: _, value:, resolve:, reject:) = req
  acc
  |> push_refs(value, _)
  |> push_refs(resolve, _)
  |> push_refs(reject, _)
}

// exhaustive; weak keys not traced, see prune_weak_cell
fn push_objkind_refs(kind: ObjKind, acc: List(Int)) -> List(Int) {
  case kind {
    Ordinary | types.GlobalObj -> acc
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
    CompiledFn(
      code:,
      home_object:,
      flags: _,
      fields_init:,
      direct_entry:,
      name: _,
      length: _,
      birth:,
    ) -> {
      let acc = push_optional_handle(home_object, acc)
      let acc = push_optional_handle(fields_init, acc)
      let acc = push_birth_refs(birth, acc)
      let acc = push_refs(code, acc)
      push_refs(direct_entry, acc)
    }
    BytecodeFn(
      template: _,
      env:,
      home_object:,
      flags: _,
      fields_init:,
      realm: _,
      unit_id: _,
      birth:,
    ) -> {
      let acc = push_optional_handle(home_object, acc)
      let acc = push_optional_handle(fields_init, acc)
      let acc = push_birth_refs(birth, acc)
      push_refs(env, acc)
    }
    NativeFn(token:, name: _, length: _, constructible: _) ->
      push_refs(token, acc)
    BoundFn(target:, bound_this:, bound_args:, ..) -> {
      let acc = push_refs(bound_this, [target.id, ..acc])
      list.fold(bound_args, acc, fn(a, v) { push_refs(v, a) })
    }
    HostObj(payload:) -> push_refs(payload, acc)
    ErrorObj(stack: _) -> acc
    MapObj(entries:) ->
      ordered_entries.fold(entries, acc, fn(a, k, v) {
        push_refs(v, push_refs(k, a))
      })
    SetObj(entries:) ->
      ordered_entries.fold(entries, acc, fn(a, k, _) { push_refs(k, a) })
    WeakMapObj(entries:) ->
      dict.fold(entries, acc, fn(a, _, v) { push_refs(v, a) })
    WeakSetObj(entries: _) -> acc
    DateObj(ms: _) -> acc
    RegExpObj(source: _, flags: _, compiled: _) -> acc
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
    ArrayIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    MapIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    SetIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    StringIterator(source: _, index: _) -> acc
    PromiseObj(data:) -> [data.id, ..acc]
    GeneratorObj(data:) -> [data.id, ..acc]
    AsyncGeneratorObj(data:) -> [data.id, ..acc]
    AsyncFromSyncIterator(sync_rec:) -> [sync_rec.id, ..acc]
    IteratorHelperObj(gen_state: _, body:) -> push_refs(body, acc)
    WrapForValidIteratorObj(record:) -> push_refs(record, acc)
    IntlObj(data: _, bound:) -> push_optional_handle(bound, acc)
    TemporalObj(data: _) -> acc
    DisposableStackObj(async: _, state: types.Pending(capability:)) -> [
      capability.id,
      ..acc
    ]
    DisposableStackObj(async: _, state: types.Disposed) -> acc
    FinalizationRegistryObj(callback:, registrations:) ->
      list.fold(registrations, push_refs(callback, acc), fn(a, r) {
        push_refs(r.held, a)
      })
    WeakRefObj(target: _) -> acc
    types.ShadowRealmObj(realm: _) -> acc
  }
}

fn push_elements_refs(elems: JsElements, acc: List(Int)) -> List(Int) {
  case elems {
    NoElements -> acc
    Dense(arr) ->
      tree_array.sparse_fold(fn(_, v, a) { push_refs(v, a) }, acc, arr)
    Sparse(d) -> push_refs(d, acc)
  }
}

fn push_promise_state_refs(state: PromiseState, acc: List(Int)) -> List(Int) {
  case state {
    PromisePending(reactions:) -> list.fold(reactions, acc, push_reaction_refs)
    PromiseFulfilled(v) -> push_refs(v, acc)
    PromiseRejected(v) -> push_refs(v, acc)
  }
}

fn push_reaction_refs(acc: List(Int), r: PromiseReaction) -> List(Int) {
  let PromiseReaction(
    on_fulfill:,
    on_reject:,
    child_resolve:,
    child_reject:,
    snapshot:,
  ) = r
  let acc = push_reaction_handler_refs(on_fulfill, acc)
  let acc = push_reaction_handler_refs(on_reject, acc)
  push_refs(snapshot, push_refs(child_reject, push_refs(child_resolve, acc)))
}

fn push_reaction_handler_refs(h: ReactionHandler, acc: List(Int)) -> List(Int) {
  case h {
    Handler(fun:) -> push_refs(fun, acc)
    IdentityPassThrough | ThrowerPassThrough -> acc
  }
}

fn push_optional_handle(oh: Option(Handle), acc: List(Int)) -> List(Int) {
  case oh {
    Some(h) -> [h.id, ..acc]
    None -> acc
  }
}

fn push_birth_refs(birth: types.FnBirth, acc: List(Int)) -> List(Int) {
  case birth {
    types.BirthPending(prototype_parent:) ->
      push_optional_handle(prototype_parent, acc)
    types.BirthSettled -> acc
  }
}

// turn boundary only, call_depth 0; called by name from arc_aot_run_ffi
pub fn maybe_collect(st: Agent) -> Agent {
  case st.call_depth == 0 && is_due(st) {
    True -> collect_some(st, [])
    False -> st
  }
}

pub fn is_due(st: Agent) -> Bool {
  st.next_id >= st.gc_at
}

/// how many cells may be made between collections
pub fn set_threshold(st: Agent, gc_threshold: Int) -> Agent {
  Agent(
    ..st,
    gc_at: st.gc_at - st.store.gc_threshold + gc_threshold,
    store: Store(..st.store, gc_threshold:),
  )
}

pub fn hold_roots(st: Agent, held: List(JsVal)) -> #(List(Int), Agent) {
  let store = st.store
  let ids =
    list.fold(held, [], fn(acc, v) { push_refs(v, acc) })
    |> list.filter(fn(id) { !set.contains(store.pinned_roots, id) })
    |> list.unique
  let pinned = list.fold(ids, store.pinned_roots, set.insert)
  #(ids, Agent(..st, store: Store(..store, pinned_roots: pinned)))
}

pub fn release_roots(st: Agent, ids: List(Int)) -> Agent {
  let store = st.store
  let pinned = list.fold(ids, store.pinned_roots, set.delete)
  Agent(..st, store: Store(..store, pinned_roots: pinned))
}

// majors keep the old full-gc schedule, minors run in between
pub const minors_per_major: Int = 32

pub fn collect_some(st: Agent, extra_roots: List(Handle)) -> Agent {
  let store = st.store
  let meta = store.meta
  let floor = meta.major_live / 2
  case
    meta.young_start > 0
    && meta.live_count - meta.major_live
    < int.max(2 * store.gc_threshold, floor)
    && meta.minors_since_major * store.gc_threshold
    < int.max(minors_per_major * store.gc_threshold, floor)
  {
    True -> collect_minor(st, extra_roots)
    False -> collect(st, extra_roots)
  }
}

// a full gc; when cells are kept for good it walks only the ids above them
pub fn collect(st: Agent, extra_roots: List(Handle)) -> Agent {
  case st.store.meta.kept.below {
    0 -> collect_all(st, extra_roots)
    _ -> collect_above_kept(st, extra_roots, st.store.meta.kept)
  }
}

/// Everything alive now is kept for good. Later full gcs leave it alone and
/// walk only what was made since.
pub fn keep_all(st: Agent) -> Agent {
  let st = collect_all(st, [])
  let meta = st.store.meta
  let push_weak = fn(id, cell, acc) {
    case is_weak_cell(cell) {
      True -> [id, ..acc]
      False -> acc
    }
  }
  let kept =
    Kept(
      below: st.next_id,
      cells: st.cells,
      weak_ids: arena.fold(push_weak, [], st.cells),
      count: meta.live_count,
      pins: set.size(st.store.pinned_roots),
    )
  rt_store.with_meta(st, types.StoreMeta(..meta, kept:))
}

fn collect_all(st: Agent, extra_roots: List(Handle)) -> Agent {
  let nothing = Kept(below: 0, cells: st.cells, weak_ids: [], count: 0, pins: 0)
  collect_above_kept(st, extra_roots, nothing)
}

// no renumbering, dead ids dropped, next_id falls past highest survivor
fn collect_above_kept(
  st: Agent,
  extra_roots: List(Handle),
  kept: Kept,
) -> Agent {
  let st = drop_handled_rejections(st)
  let store = st.store
  let meta = store.meta
  let Kept(below:, cells: before, weak_ids:, count:, pins: _) = kept
  let Above(cells:, survivors:, next_id:, walked: _) =
    collect_above(
      st,
      extra_roots,
      job_queue_to_list,
      below,
      before,
      weak_ids,
      fn(_) { True },
      Some,
    )
  let live_count = count + survivors
  Agent(
    ..st,
    cells:,
    next_id:,
    gc_at: next_id + store.gc_threshold,
    store: Store(
      ..store,
      ics: dict.filter(store.ics, fn(_, entry) { is_read_ic(entry) }),
      plain_write_protos: dict.new(),
      microtasks: job_queue_aged(store.microtasks),
      meta: types.StoreMeta(
        ..meta,
        live_count:,
        old_gen: cells,
        young_start: next_id,
        major_live: live_count,
        minors_since_major: 0,
        plain_index_protos: dict.new(),
        threaded: dict.new(),
        threaded_before: meta.threaded,
        thread_keys: dict.new(),
        draft_runs: dict.new(),
        checked_subject: "",
      ),
    ),
  )
}

fn drop_handled_rejections(st: Agent) -> Agent {
  let meta = st.store.meta
  let unhandled_rejections = {
    use id <- list.filter(meta.unhandled_rejections)
    case arena.get_option(id, st.cells) {
      Some(SPromiseData(is_handled:, ..)) -> !is_handled
      _ -> False
    }
  }
  rt_store.with_meta(st, types.StoreMeta(..meta, unhandled_rejections:))
}

// young ids are young_start and up; old cells reach them only if written since
fn collect_minor(st: Agent, extra_roots: List(Handle)) -> Agent {
  let store = st.store
  let meta = store.meta
  let Above(cells:, survivors:, next_id: _, walked:) =
    collect_above(
      st,
      extra_roots,
      // a job never changes, so an old one holds only old cells
      job_queue_fresh,
      meta.young_start,
      meta.old_gen,
      // what an old one points at is old, or was just found by its change
      [],
      holds_weak_targets,
      fn(_) { None },
    )
  // the next one will likely walk as much, so as much has to be made first
  let thresholds = int.max(1, walked / { 2 * store.gc_threshold })
  Agent(
    ..st,
    cells:,
    gc_at: st.next_id + thresholds * store.gc_threshold,
    store: Store(
      ..store,
      microtasks: job_queue_aged(store.microtasks),
      meta: types.StoreMeta(
        ..meta,
        live_count: meta.live_count + survivors,
        old_gen: cells,
        young_start: st.next_id,
        minors_since_major: meta.minors_since_major + thresholds,
        checked_subject: "",
      ),
    ),
  )
}

type Above {
  Above(
    cells: Arena(Cell),
    survivors: Int,
    next_id: Int,
    // how much had to be gone through to find the roots
    walked: Int,
  )
}

// a gc of the ids from w up; cells below w reach them only where they differ
// from `before`. Weak containers that `prunes` picks lose their dead entries
fn collect_above(
  st: Agent,
  extra_roots: List(Handle),
  jobs: fn(JobQueue) -> List(Job),
  w: Int,
  before: Arena(Cell),
  weak: List(Int),
  prunes: fn(Cell) -> Bool,
  // where ids are handed out from next, given one past the highest survivor
  restart: fn(Int) -> Option(Int),
) -> Above {
  let cells = st.cells
  let roots =
    list.fold(extra_roots, roots_with(st, jobs), fn(a, h) { [h.id, ..a] })
  // less the pins from before keeping, which every engine has
  let held =
    list.length(roots)
    - int.min(st.store.meta.kept.pins, set.size(st.store.pinned_roots))
  let roots = {
    use acc, id <- list.fold(arena.diff_below(w, before, cells), roots)
    case arena.get_option(id, cells), arena.get_option(id, before) {
      Some(cell), Some(before) -> diff_refs(before, cell, acc)
      Some(cell), None -> push_cell_refs(cell, acc)
      None, _ -> acc
    }
  }
  let walked = held + diffed(roots)
  let #(live, survivors, weak, past) = mark(cells, roots, w, weak)
  let is_live = fn(id) { id < w || is_marked(id, live) }
  let next_id = restart(past) |> option.unwrap(st.next_id)
  let kept = arena.keep_marked(w, live, next_id, cells)
  // ids are never reused, so stale weak keys only cost memory until a major
  let kept = {
    use kept, id <- list.fold(weak, kept)
    case arena.get_option(id, kept) {
      Some(cell) ->
        case prunes(cell) {
          True -> arena.set(id, prune_weak_cell(cell, is_live), kept)
          False -> kept
        }
      None -> kept
    }
  }
  Above(cells: kept, survivors:, next_id:, walked:)
}

// how much diff_refs says it went through
@external(erlang, "arc_rt_gc_ffi", "diffed")
fn diffed(roots: List(Int)) -> Int

fn holds_weak_targets(cell: Cell) -> Bool {
  case cell {
    SObject(kind: WeakRefObj(..), ..)
    | SObject(kind: FinalizationRegistryObj(..), ..) -> True
    _ -> False
  }
}

// called by name from arc_rt_gc_ffi
pub fn is_weak_cell(cell: Cell) -> Bool {
  case cell {
    SObject(kind: WeakMapObj(..), ..)
    | SObject(kind: WeakSetObj(..), ..)
    | SObject(kind: FinalizationRegistryObj(..), ..)
    | SObject(kind: WeakRefObj(..), ..) -> True
    _ -> False
  }
}

// call ics name cell ids that sweep hands out again
fn is_read_ic(entry: IcEntry) -> Bool {
  case entry {
    IcRead(..) | IcOff -> True
    // evaluated code mints literal sites without end, and a refill is cheap
    IcLiteral(..) -> False
    IcCall(..) | IcInit(..) | IcGlobal(..) -> False
  }
}

// the live ids from w up, what they weigh, the weak containers among them
// added to weak, and one past the highest
@external(erlang, "arc_rt_gc_ffi", "mark")
fn mark(
  cells: Arena(Cell),
  roots: List(Int),
  w: Int,
  weak: List(Int),
) -> #(arena.Marks, Int, List(Int), Int)

@external(erlang, "arc_rt_gc_ffi", "is_marked")
fn is_marked(id: Int, marks: arena.Marks) -> Bool

// drop weak entries and registrations whose target died
fn prune_weak_cell(cell: Cell, is_live: fn(Int) -> Bool) -> Cell {
  case cell {
    SObject(kind: WeakMapObj(entries:), ..) ->
      SObject(
        ..cell,
        kind: WeakMapObj(
          entries: dict.filter(entries, fn(k, _) { weak_key_live(k, is_live) }),
        ),
      )
    SObject(kind: WeakSetObj(entries:), ..) ->
      SObject(
        ..cell,
        kind: WeakSetObj(
          entries: set.filter(entries, fn(k) { weak_key_live(k, is_live) }),
        ),
      )
    SObject(kind: FinalizationRegistryObj(callback:, registrations:), ..) -> {
      let registrations =
        list.filter(registrations, fn(r) {
          option.is_some(weak_live(r.target, is_live))
        })
        |> list.map(fn(r) {
          Registration(
            ..r,
            unregister_token: option.then(r.unregister_token, weak_live(
              _,
              is_live,
            )),
          )
        })
      SObject(..cell, kind: FinalizationRegistryObj(callback:, registrations:))
    }
    SObject(kind: WeakRefObj(target:), ..) ->
      SObject(
        ..cell,
        kind: WeakRefObj(target: option.then(target, weak_live(_, is_live))),
      )
    _ -> cell
  }
}

fn weak_key_live(k: WeakKey, is_live: fn(Int) -> Bool) -> Bool {
  case k {
    WeakObjKey(id:) -> is_live(id)
    WeakSymKey(_) -> True
  }
}

fn weak_live(v: JsVal, is_live: fn(Int) -> Bool) -> Option(JsVal) {
  case classify(v) {
    KHandle(Handle(id)) ->
      case is_live(id) {
        True -> Some(v)
        False -> None
      }
    _ -> Some(v)
  }
}

pub fn alloc_since_gc(st: Agent) -> Int {
  st.next_id - st.store.meta.young_start
}

pub type GcStats {
  GcStats(live_count: Int, next_id: Int, alloc_since_gc: Int)
}

pub fn stats(st: Agent) -> GcStats {
  GcStats(
    live_count: arena.count(st.cells),
    next_id: st.next_id,
    alloc_since_gc: alloc_since_gc(st),
  )
}

pub fn is_live(st: Agent, h: Handle) -> Bool {
  let Handle(id) = h
  option.is_some(arena.get_option(id, st.cells))
}
