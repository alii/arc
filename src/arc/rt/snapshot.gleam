import arc/host_hooks.{type HostHooks}
import arc/rt/arena
import arc/rt/builtins as rt_builtins
import arc/rt/builtins/regexp as b_regexp
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Cell, type Handle, type Job, type Realm, type ShapeDesc,
  type Store, Agent, CompiledFn, Handle, HostJob, NativeFn, ReactionJob,
  RegExpConstructor, RegExpN, RegExpObj, ResolveThenableJob, ResumeCompiled,
  ResumeFrame, SAsyncContext, SAsyncGen, SBox, SDisposeCapability, SGenerator,
  SObject, SPromiseData, SShapedObject, Store,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/set.{type Set}

// bump on any change to the image or runtime records
pub const abi_version = 15

pub type SnapshotError {
  SnapshotContainsCompiledCode(handle: Handle)
  SnapshotContainsHostJob
  SnapshotContainsWaiter
}

pub type DeserializeError {
  MalformedBinary
  IncompatibleSnapshot
}

type StoreImage {
  StoreImage(
    cells: Dict(Int, Cell),
    free: List(Int),
    next_id: Int,
    pinned_roots: Set(Int),
    alloc_since_gc: Int,
    gc_threshold: Int,
    prop_seq: Int,
    next_private_id: Int,
    next_symbol_id: Int,
    microtasks: List(Job),
    unhandled_rejections: List(Int),
    shapes: Dict(Int, ShapeDesc),
    next_shape: Int,
    next_unit_id: Int,
  )
}

type RealmImage {
  RealmImage(
    current: Realm,
    realms: Dict(Int, Realm),
    template_objects: Dict(String, Handle),
  )
}

@external(erlang, "arc_rt_snapshot_ffi", "encode")
fn encode(version: Int, store: StoreImage, realms: RealmImage) -> BitArray

@external(erlang, "arc_rt_snapshot_ffi", "decode")
fn decode(
  version: Int,
  data: BitArray,
) -> Result(#(StoreImage, RealmImage), DeserializeError)

pub fn serialize(st: Agent) -> Result(BitArray, SnapshotError) {
  let Agent(
    store:,
    realm:,
    template_objects:,
    frames: _,
    hooks: _,
    host_fns: _,
    realms:,
    import_hook: _,
    waiters:,
    call_depth: _,
    tz_zones: _,
  ) = st
  use Nil <- result.try(case waiters {
    [] -> Ok(Nil)
    [_, ..] -> Error(SnapshotContainsWaiter)
  })
  let Store(
    cells:,
    next_id:,
    alloc_since_gc:,
    gc_threshold:,
    prop_seq:,
    shapes:,
    next_shape:,
    ics: _,
    plain_write_protos: _,
    global_epoch: _,
    ops: _,
    microtasks:,
    pinned_roots:,
    meta: types.StoreMeta(
      live_count: _,
      next_private_id:,
      next_symbol_id:,
      next_unit_id:,
      unhandled_rejections:,
      old_gen: _,
      young_start: _,
      old_weak_ids: _,
      major_live: _,
      minors_since_major: _,
    ),
  ) = store
  let microtasks = types.job_queue_to_list(microtasks)
  let cells =
    arena.fold(
      fn(id, cell, acc) { dict.insert(acc, id, drop_regexp_matcher(cell)) },
      dict.new(),
      cells,
    )
  use Nil <- result.try(check_cells(cells))
  use Nil <- result.try(check_jobs(microtasks))
  let store =
    StoreImage(
      cells:,
      free: [],
      next_id:,
      pinned_roots:,
      alloc_since_gc:,
      gc_threshold:,
      prop_seq:,
      next_private_id:,
      next_symbol_id:,
      microtasks:,
      unhandled_rejections:,
      shapes:,
      next_shape:,
      next_unit_id:,
    )
  let realms = RealmImage(current: realm, realms:, template_objects:)
  Ok(encode(abi_version, store, realms))
}

pub fn deserialize(
  data: BitArray,
  hooks: HostHooks,
) -> Result(Agent, DeserializeError) {
  use #(store, realms) <- result.map(decode(abi_version, data))
  let RealmImage(current: realm, realms:, template_objects:) = realms
  Agent(
    store: restore(store),
    realm:,
    template_objects:,
    frames: [],
    hooks:,
    host_fns: dict.new(),
    realms:,
    import_hook: None,
    waiters: [],
    call_depth: 0,
    tz_zones: dict.new(),
  )
  |> rt_builtins.seed_ops
}

fn restore(image: StoreImage) -> Store {
  let StoreImage(
    cells:,
    free: _,
    next_id:,
    pinned_roots:,
    alloc_since_gc:,
    gc_threshold:,
    prop_seq:,
    next_private_id:,
    next_symbol_id:,
    microtasks:,
    unhandled_rejections:,
    shapes:,
    next_shape:,
    next_unit_id:,
  ) = image
  let fresh = rt_store.new()
  Store(
    ..fresh,
    cells: dict.fold(cells, arena.new(), fn(acc, id, cell) {
      arena.set(id, cell, acc)
    }),
    next_id:,
    alloc_since_gc:,
    gc_threshold:,
    prop_seq:,
    shapes:,
    next_shape:,
    microtasks: list.fold(
      microtasks,
      types.job_queue_new(),
      types.job_queue_push,
    ),
    pinned_roots:,
    meta: types.StoreMeta(
      ..fresh.meta,
      next_private_id:,
      next_symbol_id:,
      next_unit_id:,
      unhandled_rejections:,
    ),
  )
}

fn check_cells(cells: Dict(Int, Cell)) -> Result(Nil, SnapshotError) {
  dict.fold(cells, Ok(Nil), fn(found, id, cell) {
    case found, holds_compiled_code(cell) {
      Ok(Nil), True -> Error(SnapshotContainsCompiledCode(Handle(id)))
      _, _ -> found
    }
  })
}

fn holds_compiled_code(cell: Cell) -> Bool {
  case cell {
    SObject(kind: CompiledFn(..), ..) -> True
    SGenerator(resume: ResumeCompiled(..), ..)
    | SAsyncGen(resume: ResumeCompiled(..), ..)
    | SAsyncContext(resume: ResumeCompiled(..), ..) -> True
    SGenerator(resume: ResumeFrame(..), ..)
    | SAsyncGen(resume: ResumeFrame(..), ..)
    | SAsyncContext(resume: ResumeFrame(..), ..) -> False
    SObject(..)
    | SBox(..)
    | SPromiseData(..)
    | SShapedObject(..)
    | SDisposeCapability(..) -> False
  }
}

// compiled re pattern is otp-release specific
fn drop_regexp_matcher(cell: Cell) -> Cell {
  case cell {
    SObject(kind: RegExpObj(..) as kind, ..) ->
      SObject(..cell, kind: RegExpObj(..kind, compiled: b_regexp.uncompiled()))
    SObject(
      kind: NativeFn(token: RegExpN(RegExpConstructor(..) as ctor), ..) as kind,
      ..,
    ) ->
      SObject(
        ..cell,
        kind: NativeFn(
          ..kind,
          token: RegExpN(
            RegExpConstructor(..ctor, proto_props: None, compiled: dict.new()),
          ),
        ),
      )
    _ -> cell
  }
}

fn check_jobs(jobs: List(Job)) -> Result(Nil, SnapshotError) {
  let has_host_job =
    list.any(jobs, fn(job) {
      case job {
        HostJob(..) -> True
        ReactionJob(..) | ResolveThenableJob(..) -> False
      }
    })
  case has_host_job {
    True -> Error(SnapshotContainsHostJob)
    False -> Ok(Nil)
  }
}
