import arc/bytecode/key.{type Key}
import arc/internal/ordered_entries
import arc/internal/tree_array
import arc/internal/tuple_array.{type TupleArray}
import arc/rt/arena.{type Arena}
import arc/rt/bytecode.{type EnvTuple, type SuspendedFrame, SuspendedFrame}
import arc/rt/names
import arc/rt/types.{
  type Agent, type AsyncGenRequest, type Handle, type IcEntry, type Job,
  type JsElements, type JsSlot, type JsStore, type JsVal, type ObjKind,
  type PromiseReaction, type PromiseState, type Property, type ReactionHandler,
  type Resume, type WeakKey, Agent, ArgumentsObj, ArrayBufferObj, ArrayIterator,
  ArrayObj, AsyncFromSyncIterator, AsyncGenRequest, AsyncGeneratorObj, BigIntObj,
  BooleanObj, DataViewObj, DateObj, Dense, DisposableStackObj, ErrorObj,
  FinRegCell, FinalizationRegistryObj, ForInIterator, GeneratorObj, Handler,
  HostJob, IcCall, IcGlobal, IcInit, IcOff, IcRead, IdentityPassThrough, IntlObj,
  IteratorHelperObj, JsCell, JsStore, KBound, KBytecode, KCompiled, KHandle,
  KHost, KNative, MapIterator, MapObj, ModuleNamespace, NoElements, NumberObj,
  Ordinary, PromiseFulfilled, PromiseObj, PromisePending, PromiseReaction,
  PromiseRejected, ProxyObj, RawJsonObj, ReactionJob, RegExpObj,
  ResolveThenableJob, ResumeCompiled, ResumeFrame, SAsyncContext, SAsyncGen,
  SBox, SDisposeCapability, SGenerator, SObject, SPromiseData, SShapedObject,
  SetIterator, SetObj, Sparse, StringIterator, StringObj, SymbolObj, TemporalObj,
  ThrowerPassThrough, TypedArrayObj, WeakMapObj, WeakObjKey, WeakRefObj,
  WeakSetObj, WeakSymKey, WrapForValidIteratorObj, classify, jq_to_list,
  native_token_refs,
} as rt_types
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

@external(erlang, "arc_rt_gc_ffi", "refs_in_term")
pub fn push_term_refs(v: Dynamic, acc: List(Int)) -> List(Int)

@external(erlang, "arc_rt_gc_ffi", "refs_in_props")
fn push_props_refs(props: Dict(k, Property), acc: List(Int)) -> List(Int)

@external(erlang, "arc_rt_gc_ffi", "refs_in_symbol_props")
fn push_symbol_props_refs(
  props: List(#(k, Property)),
  acc: List(Int),
) -> List(Int)

@external(erlang, "arc_rt_gc_ffi", "keys_in_term")
fn push_term_keys(v: Dynamic, acc: Dict(Key, Nil)) -> Dict(Key, Nil)

@external(erlang, "arc_rt_gc_ffi", "keys_in_keyed")
fn push_keyed_keys(m: Dict(Key, v), acc: Dict(Key, Nil)) -> Dict(Key, Nil)

@external(erlang, "arc_rt_gc_ffi", "keys_in_keyed")
fn push_keyed_term_keys(m: Dynamic, acc: Dict(Key, Nil)) -> Dict(Key, Nil)

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(a: anything) -> Dynamic

pub fn push_val_refs(v: JsVal, acc: List(Int)) -> List(Int) {
  push_term_refs(to_dynamic(v), acc)
}

fn require_js(st: Agent) -> JsStore(Agent) {
  st.store
}

// every root outside the arena, read by both the cell mark and the name sweep
pub type Roots {
  Roots(ids: List(Int), terms: List(Dynamic), keyed: List(Dynamic))
}

// exhaustive destructures: a new field must be classed as a root or not here
pub fn roots_of_state(st: Agent) -> Roots {
  let Agent(
    store:,
    realm:,
    // pinned when made, see rt/lang
    template_objects: _,
    frames: _,
    hooks: _,
    host_fns:,
    realms:,
    import_hook:,
    waiters:,
    call_depth: _,
  ) = st
  let JsStore(
    data: _,
    next: _,
    pinned_roots:,
    alloc_since_gc: _,
    gc_threshold: _,
    gc_live: _,
    prop_seq: _,
    private_uid: _,
    symbol_uid: _,
    ops: _,
    microtasks:,
    unhandled_rejections:,
    shapes:,
    next_shape: _,
    unit_uid: _,
    // ics are validated on use, so weak for cells and names alike
    ics: _,
    free_protos: _,
    global_epoch: _,
    names:,
  ) = store
  let ids = list.append(unhandled_rejections, set.to_list(pinned_roots))
  let terms = [
    to_dynamic(jq_to_list(microtasks)),
    to_dynamic(dict.values(host_fns)),
    to_dynamic(import_hook),
    to_dynamic(waiters),
  ]
  let keyed = [to_dynamic(names.pinned), ..shape_keyed(shapes)]
  // registry copy of the current realm may be stale
  dict.insert(realms, realm.id, realm)
  |> dict.fold(Roots(ids:, terms:, keyed:), realm_roots)
}

pub fn shape_keyed(shapes: Dict(Int, rt_types.ShapeDesc)) -> List(Dynamic) {
  dict.fold(shapes, [], fn(acc, _, desc) {
    [to_dynamic(desc.offsets), to_dynamic(desc.transitions), ..acc]
  })
}

// intrinsic handles are pinned when the realm is built
fn realm_roots(acc: Roots, _id: Int, realm: rt_types.Realm) -> Roots {
  let rt_types.Realm(
    object: _,
    function: _,
    array: _,
    string: _,
    number: _,
    boolean: _,
    symbol: _,
    bigint: _,
    error: _,
    type_error: _,
    reference_error: _,
    range_error: _,
    syntax_error: _,
    eval_error: _,
    uri_error: _,
    aggregate_error: _,
    map: _,
    set: _,
    weak_map: _,
    weak_set: _,
    weak_ref: _,
    finalization_registry: _,
    date: _,
    regexp: _,
    promise: _,
    proxy: _,
    array_buffer: _,
    data_view: _,
    typed_arrays: _,
    math: _,
    json: _,
    reflect: _,
    console: _,
    atomics: _,
    iterator_proto: _,
    array_iter_proto: _,
    string_iter_proto: _,
    map_iter_proto: _,
    set_iter_proto: _,
    async_iterator_proto: _,
    async_from_sync_proto: _,
    iterator: _,
    iterator_helper_proto: _,
    wrap_for_valid_proto: _,
    generator: _,
    generator_fn: _,
    async_fn: _,
    async_gen: _,
    throw_type_error: _,
    global_object: _,
    shared_array_buffer: _,
    id: _,
    lexical_globals:,
    suppressed_error: _,
  ) = realm
  Roots(..acc, terms: [to_dynamic(lexical_globals), ..acc.terms], keyed: [
    to_dynamic(lexical_globals),
    ..acc.keyed
  ])
}

fn mark_roots(
  roots: Roots,
  extra: List(Handle),
  terms: List(Dynamic),
) -> List(Int) {
  let ids = list.fold(extra, roots.ids, fn(a, h) { [h.id, ..a] })
  list.fold(terms, ids, fn(a, t) { push_term_refs(t, a) })
}

fn key_roots(
  roots: Roots,
  terms: List(Dynamic),
  acc: Dict(Key, Nil),
) -> Dict(Key, Nil) {
  list.fold(terms, acc, fn(a, t) { push_term_keys(t, a) })
  |> list.fold(roots.keyed, _, fn(a, m) { push_keyed_term_keys(m, a) })
}

// exhaustive, no wildcard: a new variant must be traced
pub fn refs_in_cell(slot: JsSlot, acc: List(Int)) -> List(Int) {
  case slot {
    SObject(kind:, proto:, props:, symbol_props:, elements:, extensible: _) ->
      push_objkind_refs(kind, push_opt_handle(proto, acc))
      |> push_props_refs(props, _)
      |> push_symbol_props_refs(symbol_props, _)
      |> push_elements_refs(elements, _)
    SShapedObject(shape_id: _, proto:, slots:, offsets: _) ->
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
    SDisposeCapability(resources:) -> push_term_refs(to_dynamic(resources), acc)
  }
}

// exhaustive twin of refs_in_cell: js values name no keys, templates,
// private names, host terms and keyed maps do
fn keys_in_cell(slot: JsSlot, acc: Dict(Key, Nil)) -> Dict(Key, Nil) {
  case slot {
    SObject(
      kind:,
      proto: _,
      props:,
      symbol_props: _,
      elements: _,
      extensible: _,
    ) -> push_objkind_keys(kind, push_keyed_keys(props, acc))
    SShapedObject(shape_id: _, proto: _, slots: _, offsets:) ->
      push_keyed_keys(offsets, acc)
    // a boxed class binding holds a private name
    SBox(value:) -> push_term_keys(to_dynamic(value), acc)
    SPromiseData(state: _, is_handled: _) -> acc
    SGenerator(state: _, resume:) -> push_term_keys(to_dynamic(resume), acc)
    SAsyncGen(state: _, resume:, queue: _) ->
      push_term_keys(to_dynamic(resume), acc)
    SAsyncContext(resume:, promise: _) ->
      push_term_keys(to_dynamic(resume), acc)
    SDisposeCapability(resources: _) -> acc
  }
}

fn push_objkind_keys(kind: ObjKind, acc: Dict(Key, Nil)) -> Dict(Key, Nil) {
  case kind {
    Ordinary | rt_types.GlobalObj -> acc
    ArrayObj(length: _) -> acc
    ArgumentsObj(length: _, mapped: _) -> acc
    StringObj(value: _) -> acc
    NumberObj(value: _) -> acc
    BooleanObj(value: _) -> acc
    BigIntObj(value: _) -> acc
    SymbolObj(value: _) -> acc
    KCompiled(
      code:,
      home_object: _,
      flags: _,
      fields_init: _,
      simple:,
      name: _,
      length: _,
      birth: _,
    ) ->
      push_term_keys(to_dynamic(code), acc)
      |> push_term_keys(to_dynamic(simple), _)
    KBytecode(
      template:,
      env:,
      home_object: _,
      flags: _,
      fields_init: _,
      realm: _,
      unit: _,
      birth: _,
    ) ->
      push_term_keys(to_dynamic(template), acc)
      |> push_term_keys(to_dynamic(env), _)
    KNative(tag:, name: _, length: _, constructible: _) ->
      push_native_keys(tag, acc)
    KBound(target: _, bound_this: _, bound_args: _) -> acc
    KHost(payload:) -> push_term_keys(to_dynamic(payload), acc)
    ErrorObj(stack: _) -> acc
    MapObj(entries: _) -> acc
    SetObj(entries: _) -> acc
    WeakMapObj(entries: _) -> acc
    WeakSetObj(entries: _) -> acc
    DateObj(ms: _) -> acc
    RegExpObj(source: _, flags: _, last_index: _, compiled: _) -> acc
    ArrayBufferObj(storage: _) -> acc
    TypedArrayObj(buffer: _, elem_kind: _, byte_offset: _, length: _) -> acc
    DataViewObj(buffer: _, byte_offset: _, byte_length: _) -> acc
    RawJsonObj(raw: _) -> acc
    ModuleNamespace(exports: _) -> acc
    ProxyObj(target: _, handler: _, revoked: _) -> acc
    ForInIterator(remaining: _) -> acc
    ArrayIterator(target: _, index: _, kind: _) -> acc
    MapIterator(target: _, index: _, kind: _) -> acc
    SetIterator(target: _, index: _, kind: _) -> acc
    StringIterator(source: _, index: _) -> acc
    PromiseObj(data: _) -> acc
    GeneratorObj(data: _) -> acc
    AsyncGeneratorObj(data: _) -> acc
    AsyncFromSyncIterator(sync_rec: _) -> acc
    // zip helpers carry object keys
    IteratorHelperObj(gen_state: _, body:) ->
      push_term_keys(to_dynamic(body), acc)
    WrapForValidIteratorObj(record: _) -> acc
    IntlObj(data: _, bound: _) -> acc
    TemporalObj(data: _) -> acc
    DisposableStackObj(async: _, state: _) -> acc
    FinalizationRegistryObj(callback: _, cells: _) -> acc
    WeakRefObj(target: _) -> acc
    rt_types.ShadowRealmObj(realm: _) -> acc
  }
}

// the regexp constructor caches RegExp.prototype's own keys
fn push_native_keys(
  tag: rt_types.NativeToken,
  acc: Dict(Key, Nil),
) -> Dict(Key, Nil) {
  case tag {
    rt_types.RegExpN(rt_types.RegExpConstructor(proto_props: Some(props), ..)) ->
      push_keyed_keys(props, acc)
    _ -> push_term_keys(to_dynamic(tag), acc)
  }
}

fn push_resume_refs(resume: Resume, acc: List(Int)) -> List(Int) {
  case resume {
    ResumeCompiled(sm:, rs: _, loc:) ->
      push_term_refs(to_dynamic(loc), push_term_refs(to_dynamic(sm), acc))
    ResumeFrame(frame:) -> push_suspended_frame_refs(frame, acc)
  }
}

pub fn push_suspended_frame_refs(
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
    unit: _,
  ) = frame
  let acc = push_term_refs(to_dynamic(template), acc)
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

fn push_env_refs(env: EnvTuple, acc: List(Int)) -> List(Int) {
  push_term_refs(to_dynamic(env), acc)
}

fn push_vals_tuple_refs(vals: TupleArray(JsVal), acc: List(Int)) -> List(Int) {
  push_term_refs(to_dynamic(vals), acc)
}

fn push_request_refs(acc: List(Int), req: AsyncGenRequest) -> List(Int) {
  let AsyncGenRequest(completion: _, value:, resolve:, reject:) = req
  acc
  |> push_val_refs(value, _)
  |> push_val_refs(resolve, _)
  |> push_val_refs(reject, _)
}

// exhaustive; weak keys not traced, see prune_weak_slot
fn push_objkind_refs(kind: ObjKind, acc: List(Int)) -> List(Int) {
  case kind {
    Ordinary | rt_types.GlobalObj -> acc
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
      let acc = push_term_refs(to_dynamic(template), acc)
      push_env_refs(env, acc)
    }
    KNative(tag:, name: _, length: _, constructible: _) -> {
      let acc = list.fold(native_token_refs(tag), acc, fn(a, h) { [h.id, ..a] })
      push_term_refs(to_dynamic(tag), acc)
    }
    KBound(target:, bound_this:, bound_args:) -> {
      let acc = push_val_refs(bound_this, [target.id, ..acc])
      list.fold(bound_args, acc, fn(a, v) { push_val_refs(v, a) })
    }
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
    WeakMapObj(entries:) ->
      dict.fold(entries, acc, fn(a, _, v) { push_val_refs(v, a) })
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
    ForInIterator(remaining: _) -> acc
    ArrayIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    MapIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    SetIterator(target:, index: _, kind: _) -> [target.id, ..acc]
    StringIterator(source: _, index: _) -> acc
    PromiseObj(data:) -> [data.id, ..acc]
    GeneratorObj(data:) -> [data.id, ..acc]
    AsyncGeneratorObj(data:) -> [data.id, ..acc]
    AsyncFromSyncIterator(sync_rec:) -> [sync_rec.id, ..acc]
    IteratorHelperObj(gen_state: _, body:) ->
      push_term_refs(to_dynamic(body), acc)
    WrapForValidIteratorObj(record:) -> push_term_refs(to_dynamic(record), acc)
    IntlObj(data: _, bound:) -> push_opt_handle(bound, acc)
    TemporalObj(data: _) -> acc
    DisposableStackObj(async: _, state: rt_types.Pending(capability:)) -> [
      capability.id,
      ..acc
    ]
    DisposableStackObj(async: _, state: rt_types.Disposed) -> acc
    FinalizationRegistryObj(callback:, cells:) ->
      list.fold(cells, push_val_refs(callback, acc), fn(a, cell) {
        push_val_refs(cell.held, a)
      })
    WeakRefObj(target: _) -> acc
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

pub const default_gc_threshold: Int = 65_536

// turn boundary only (call_depth == 0), never at fn entry
pub fn t_maybe_collect(st: Agent) -> Agent {
  t_maybe_collect_with(st, [])
}

pub fn t_maybe_collect_with(st: Agent, extra_roots: List(Handle)) -> Agent {
  case st.call_depth == 0 && due(require_js(st)) {
    True -> t_collect(st, extra_roots)
    False -> st
  }
}

// threshold scales with live size so marking tracks allocation
pub fn due(js: JsStore(st)) -> Bool {
  js.alloc_since_gc >= js.gc_threshold
  && js.alloc_since_gc * 2 * default_gc_threshold
  >= js.gc_threshold * js.gc_live
}

pub fn t_hold_roots(st: Agent, held: List(JsVal)) -> #(Agent, List(Int)) {
  let js = require_js(st)
  let ids =
    list.fold(held, [], fn(acc, v) { push_val_refs(v, acc) })
    |> list.filter(fn(id) { !set.contains(js.pinned_roots, id) })
    |> list.unique
  let pinned = list.fold(ids, js.pinned_roots, set.insert)
  #(Agent(..st, store: JsStore(..js, pinned_roots: pinned)), ids)
}

pub fn t_release_roots(st: Agent, ids: List(Int)) -> Agent {
  let js = require_js(st)
  let pinned = list.fold(ids, js.pinned_roots, set.delete)
  Agent(..st, store: JsStore(..js, pinned_roots: pinned))
}

pub fn t_collect(st: Agent, extra_roots: List(Handle)) -> Agent {
  t_collect_frames(st, extra_roots, [], False)
}

// also sweeps unused names regardless of table size
pub fn t_collect_full(st: Agent, extra_roots: List(Handle)) -> Agent {
  t_collect_frames(st, extra_roots, [], True)
}

pub fn names_due(js: JsStore(st)) -> Bool {
  let n = js.names
  n.sweep_min == 0 || dict.size(n.texts) >= int.max(n.sweep_min, 2 * n.swept)
}

// no renumbering; dead ids dropped, next falls past highest survivor
pub fn t_collect_frames(
  st: Agent,
  extra_roots: List(Handle),
  frame_terms: List(Dynamic),
  sweep_names: Bool,
) -> Agent {
  let js = require_js(st)
  let roots = roots_of_state(st)
  let terms = list.append(frame_terms, roots.terms)
  let live =
    mark_loop(js.data, mark_roots(roots, extra_roots, terms), dict.new())
  let #(data, next) = sweep(js.data, live)
  let js =
    JsStore(
      ..js,
      data:,
      next:,
      alloc_since_gc: 0,
      gc_live: dict.size(live),
      ics: dict.filter(js.ics, fn(_, entry) { is_read_ic(entry) }),
      free_protos: dict.new(),
    )
  case sweep_names || names_due(js) {
    True -> Agent(..st, store: sweep_names_with(js, roots, terms))
    False -> Agent(..st, store: js)
  }
}

// over swept cells; numbers are never reused
fn sweep_names_with(
  js: JsStore(Agent),
  roots: Roots,
  terms: List(Dynamic),
) -> JsStore(Agent) {
  let keys =
    arena.fold(
      fn(_, slot, acc) { keys_in_cell(slot, acc) },
      dict.new(),
      js.data,
    )
    |> key_roots(roots, terms, _)
  let fixed = names.fixed_count()
  let numbers =
    dict.filter(js.names.numbers, fn(_, n) {
      n < fixed || marked(key.name(n), keys)
    })
  let texts = dict.filter(js.names.texts, fn(k, _) { marked(k, keys) })
  let names =
    rt_types.NameTable(..js.names, numbers:, texts:, swept: dict.size(texts))
  JsStore(..js, names:)
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

// drop weak entries and registry cells whose target died
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

pub type GcStats {
  GcStats(
    live: Int,
    next: Int,
    since_gc: Int,
    // dynamic name and private key texts held, and those pinned for good
    names: Int,
    pinned_names: Int,
  )
}

pub fn stats(st: Agent) -> GcStats {
  let js = require_js(st)
  GcStats(
    live: arena.count(js.data),
    next: js.next,
    since_gc: js.alloc_since_gc,
    names: dict.size(js.names.texts),
    pinned_names: dict.size(js.names.pinned),
  )
}

pub fn t_is_live(st: Agent, h: Handle) -> Bool {
  let js = require_js(st)
  let JsCell(id) = h
  option.is_some(arena.get_option(id, js.data))
}
