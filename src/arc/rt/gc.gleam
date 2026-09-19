import arc/internal/ordered_entries
import arc/internal/tree_array
import arc/rt/arena.{type Arena}
import arc/rt/bytecode.{
  type FuncTemplate, type SuspendedFrame, FuncTemplate, SuspendedFrame,
}
import arc/rt/types.{
  type Agent, type AsyncGenRequest, type Cell, type Handle, type IcEntry,
  type JsElements, type JsVal, type ObjKind, type PromiseReaction,
  type PromiseState, type Property, type ReactionHandler, type Resume,
  type Store, type WeakKey, Agent, ArgumentsObj, ArrayBufferObj, ArrayIterator,
  ArrayObj, AsyncFromSyncIterator, AsyncGenRequest, AsyncGeneratorObj, BigIntObj,
  BooleanObj, BoundFn, BytecodeFn, CompiledFn, DataViewObj, DateObj, Dense,
  DisposableStackObj, ErrorObj, FinalizationRegistryObj, GeneratorObj, Handle,
  Handler, HostObj, IcCall, IcGlobal, IcInit, IcOff, IcRead, IdentityPassThrough,
  IntlObj, IteratorHelperObj, KHandle, MapIterator, MapObj, ModuleNamespace,
  NativeFn, NoElements, NumberObj, Ordinary, PromiseFulfilled, PromiseObj,
  PromisePending, PromiseReaction, PromiseRejected, ProxyObj, RawJsonObj,
  RegExpObj, Registration, ResumeCompiled, ResumeFrame, SAsyncContext, SAsyncGen,
  SBox, SDisposeCapability, SGenerator, SObject, SPromiseData, SShapedObject,
  SetIterator, SetObj, Sparse, Store, StringIterator, StringObj, SymbolObj,
  TemporalObj, ThrowerPassThrough, TypedArrayObj, WeakMapObj, WeakObjKey,
  WeakRefObj, WeakSetObj, WeakSymKey, WrapForValidIteratorObj, classify,
  job_queue_to_list,
}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

// every handle id inside any term, fun envs included
@external(erlang, "arc_rt_gc_ffi", "push_refs")
pub fn push_refs(term: a, acc: List(Int)) -> List(Int)

// refs only in what changed since the old copy; unchanged parts are old news
@external(erlang, "arc_rt_gc_ffi", "diff_refs")
fn diff_refs(before: a, now: a, acc: List(Int)) -> List(Int)

@external(erlang, "arc_rt_gc_ffi", "push_props_refs")
fn push_props_refs(props: Dict(k, Property), acc: List(Int)) -> List(Int)

@external(erlang, "arc_rt_gc_ffi", "push_symbol_props_refs")
fn push_symbol_props_refs(
  props: List(#(k, Property)),
  acc: List(Int),
) -> List(Int)

// exhaustive destructure: a new store field must be rooted here
pub fn agent_roots(st: Agent) -> List(Int) {
  let Store(
    cells: _,
    next_id: _,
    alloc_since_gc: _,
    gc_threshold: _,
    prop_seq: _,
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
      old_weak_ids: _,
      major_live: _,
      minors_since_major: _,
    ),
  ) = st.store
  let acc = set.to_list(pinned_roots)
  let acc = list.append(unhandled_rejections, acc)
  let acc = push_refs(job_queue_to_list(microtasks), acc)
  let acc = push_refs(dict.values(st.host_fns), acc)
  let acc = push_refs(st.import_hook, acc)
  let acc = push_refs(st.waiters, acc)
  // registry copy of the current realm may be stale
  let realms = dict.insert(st.realms, st.realm.id, st.realm)
  dict.fold(realms, acc, fn(acc, _id, realm) {
    dict.fold(realm.lexical_globals, acc, fn(acc, _name, binding) {
      push_refs(types.lexical_global_value(binding), acc)
    })
  })
}

// exhaustive, no wildcard: a new variant must be traced
fn push_cell_refs(cell: Cell, acc: List(Int)) -> List(Int) {
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
    template:,
    pc: _,
    locals:,
    stack:,
    try_stack: _,
    this:,
    home_object:,
    eval_env:,
    parked: _,
    call_args:,
    realm: _,
    unit_id: _,
  ) = frame
  let acc = push_template_refs(template, acc)
  let acc = push_refs(locals, acc)
  let acc = list.fold(stack, acc, fn(a, v) { push_refs(v, a) })
  let acc = push_refs(this, acc)
  let acc = push_refs(home_object, acc)
  let acc = list.fold(call_args, acc, fn(a, v) { push_refs(v, a) })
  case eval_env {
    Some(id) -> [id, ..acc]
    None -> acc
  }
}

fn push_template_refs(template: FuncTemplate, acc: List(Int)) -> List(Int) {
  let FuncTemplate(
    name: _,
    arity: _,
    length: _,
    local_count: _,
    bytecode: _,
    constants:,
    lines: _,
    functions: _,
    env_descriptors: _,
    is_strict: _,
    is_arrow: _,
    is_derived_constructor: _,
    is_generator: _,
    is_async: _,
    is_constructor: _,
    is_class_constructor: _,
    local_names: _,
    lexical: _,
    code_kind: _,
    regs: _,
  ) = template
  push_refs(constants, acc)
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
      template:,
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
      let acc = push_template_refs(template, acc)
      push_refs(env, acc)
    }
    NativeFn(token:, name: _, length: _, constructible: _) ->
      push_refs(token, acc)
    BoundFn(target:, bound_this:, bound_args:) -> {
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
    RegExpObj(source: _, flags: _, last_index: _, compiled: _) -> acc
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
  let PromiseReaction(on_fulfill:, on_reject:, child_resolve:, child_reject:) =
    r
  let acc = push_reaction_handler_refs(on_fulfill, acc)
  let acc = push_reaction_handler_refs(on_reject, acc)
  push_refs(child_reject, push_refs(child_resolve, acc))
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
  case st.call_depth == 0 && due(st.store) {
    True -> collect_some(st, [])
    False -> st
  }
}

// minor gcs are cheap, so a fixed young generation size
pub fn due(store: Store) -> Bool {
  store.alloc_since_gc >= store.gc_threshold
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
pub const minors_per_major: Int = 16

pub fn collect_some(st: Agent, extra_roots: List(Handle)) -> Agent {
  let store = st.store
  let meta = store.meta
  let floor = meta.major_live / 2
  case
    meta.young_start > 0
    && meta.live_count - meta.major_live < int.max(store.gc_threshold, floor)
    && meta.minors_since_major * store.gc_threshold
    < int.max(minors_per_major * store.gc_threshold, floor)
  {
    True -> collect_minor(st, extra_roots)
    False -> collect(st, extra_roots)
  }
}

// full; no renumbering, dead ids dropped, next_id falls past highest survivor
pub fn collect(st: Agent, extra_roots: List(Handle)) -> Agent {
  let store = st.store
  let roots = list.fold(extra_roots, agent_roots(st), fn(a, h) { [h.id, ..a] })
  let live = mark_reachable(store.cells, roots, dict.new())
  let Swept(cells:, next_id:, weak_ids: weak) = sweep(store.cells, live)
  let live_count = dict.size(live)
  Agent(
    ..st,
    store: Store(
      ..store,
      cells:,
      next_id:,
      alloc_since_gc: 0,
      ics: dict.filter(store.ics, fn(_, entry) { is_read_ic(entry) }),
      plain_write_protos: dict.new(),
      meta: types.StoreMeta(
        ..store.meta,
        live_count:,
        old_gen: cells,
        young_start: next_id,
        old_weak_ids: weak,
        major_live: live_count,
        minors_since_major: 0,
      ),
    ),
  )
}

// young ids are young_start and up; old cells reach them only if written since
fn collect_minor(st: Agent, extra_roots: List(Handle)) -> Agent {
  let store = st.store
  let meta = store.meta
  let w = meta.young_start
  let cells = store.cells
  let roots = list.fold(extra_roots, agent_roots(st), fn(a, h) { [h.id, ..a] })
  let roots =
    list.fold(arena.diff_below(w, meta.old_gen, cells), roots, fn(acc, id) {
      case arena.get_option(id, cells), arena.get_option(id, meta.old_gen) {
        Some(cell), Some(before) -> diff_refs(before, cell, acc)
        Some(cell), None -> push_cell_refs(cell, acc)
        None, _ -> acc
      }
    })
  let #(live, weak) = mark_young(cells, roots, w, dict.new(), meta.old_weak_ids)
  let is_live = fn(id) { id < w || marked(id, live) }
  let kept = case dict.size(live) * 2 > store.next_id - w {
    True -> reset_dead(cells, w, store.next_id, live)
    False ->
      list.sort(dict.keys(live), int.compare)
      |> list.fold(arena.truncate(w, cells), fn(acc, id) {
        arena.set(id, arena.get(id, cells), acc)
      })
  }
  // ids are never reused, so stale weak keys only cost memory until a major
  let #(kept, weak) =
    list.fold(weak, #(kept, []), fn(acc, id) {
      let #(kept, weak) = acc
      case arena.get_option(id, kept) {
        Some(SObject(kind: WeakRefObj(..), ..) as cell)
        | Some(SObject(kind: FinalizationRegistryObj(..), ..) as cell) -> #(
          arena.set(id, prune_weak_cell(cell, is_live), kept),
          [id, ..weak],
        )
        Some(_) -> #(kept, [id, ..weak])
        None -> acc
      }
    })
  Agent(
    ..st,
    store: Store(
      ..store,
      cells: kept,
      alloc_since_gc: 0,
      meta: types.StoreMeta(
        ..meta,
        live_count: meta.live_count + dict.size(live),
        old_gen: kept,
        young_start: store.next_id,
        old_weak_ids: weak,
        minors_since_major: meta.minors_since_major + 1,
      ),
    ),
  )
}

fn reset_dead(
  cells: Arena(Cell),
  id: Int,
  next: Int,
  live: Dict(Int, Nil),
) -> Arena(Cell) {
  case id >= next {
    True -> cells
    False ->
      case marked(id, live) {
        True -> reset_dead(cells, id + 1, next, live)
        False -> reset_dead(arena.free(id, cells), id + 1, next, live)
      }
  }
}

// live young ids, plus any weak containers among them added to weak
fn mark_young(
  cells: Arena(Cell),
  frontier: List(Int),
  w: Int,
  visited: Dict(Int, Nil),
  weak: List(Int),
) -> #(Dict(Int, Nil), List(Int)) {
  case frontier {
    [] -> #(visited, weak)
    [id, ..rest] ->
      case id < w || marked(id, visited) {
        True -> mark_young(cells, rest, w, visited, weak)
        False -> {
          let visited = mark(id, Nil, visited)
          case arena.get_option(id, cells) {
            None -> mark_young(cells, rest, w, visited, weak)
            Some(cell) -> {
              let weak = case is_weak_cell(cell) {
                True -> [id, ..weak]
                False -> weak
              }
              mark_young(cells, push_cell_refs(cell, rest), w, visited, weak)
            }
          }
        }
      }
  }
}

fn is_weak_cell(cell: Cell) -> Bool {
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
    IcCall(..) | IcInit(..) | IcGlobal(..) -> False
  }
}

// hot: probed once per heap edge
@external(erlang, "maps", "is_key")
fn marked(id: Int, live: Dict(Int, Nil)) -> Bool

@external(erlang, "maps", "put")
fn mark(id: Int, nil: Nil, live: Dict(Int, Nil)) -> Dict(Int, Nil)

fn mark_reachable(
  cells: Arena(Cell),
  frontier: List(Int),
  visited: Dict(Int, Nil),
) -> Dict(Int, Nil) {
  case frontier {
    [] -> visited
    [id, ..rest] ->
      case marked(id, visited) {
        True -> mark_reachable(cells, rest, visited)
        False -> {
          let visited = mark(id, Nil, visited)
          case arena.get_option(id, cells) {
            None -> mark_reachable(cells, rest, visited)
            Some(cell) ->
              mark_reachable(cells, push_cell_refs(cell, rest), visited)
          }
        }
      }
  }
}

type Swept {
  Swept(cells: Arena(Cell), next_id: Int, weak_ids: List(Int))
}

fn sweep(cells: Arena(Cell), live: Dict(Int, Nil)) -> Swept {
  let is_live = fn(id) { marked(id, live) }
  let #(kept, weak) =
    arena.fold(
      fn(id, cell, acc) {
        case marked(id, live) {
          True -> {
            let #(kept, weak) = acc
            case is_weak_cell(cell) {
              True -> #([#(id, prune_weak_cell(cell, is_live)), ..kept], [
                id,
                ..weak
              ])
              False -> #([#(id, cell), ..kept], weak)
            }
          }
          False -> acc
        }
      },
      #([], []),
      cells,
    )
  let next = case kept {
    [] -> 0
    [#(id, _), ..] -> id + 1
  }
  Swept(cells: arena.from_descending(kept), next_id: next, weak_ids: weak)
}

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

pub type GcStats {
  GcStats(live_count: Int, next_id: Int, alloc_since_gc: Int)
}

pub fn stats(st: Agent) -> GcStats {
  let store = st.store
  GcStats(
    live_count: arena.count(store.cells),
    next_id: store.next_id,
    alloc_since_gc: store.alloc_since_gc,
  )
}

pub fn is_live(st: Agent, h: Handle) -> Bool {
  let store = st.store
  let Handle(id) = h
  option.is_some(arena.get_option(id, store.cells))
}
