import arc/host_hooks.{type HostHooks}
import arc/rt/arena
import arc/rt/builtins as rt_builtins
import arc/rt/builtins/regexp as b_regexp
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type Job, type JsSlot, type JsStore, type NameTable,
  type Realm, type ShapeDesc, Agent, HostJob, JsCell, JsStore, KCompiled,
  ReactionJob, RegExpObj, ResolveThenableJob, ResumeCompiled, ResumeFrame,
  SAsyncContext, SAsyncGen, SBox, SDisposeCapability, SGenerator, SObject,
  SPromiseData, SShapedObject,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

// bump on any change to the image or runtime records
pub const abi_version = 14

pub type SnapshotError {
  SnapshotContainsCompiledCode(cell: Handle)
  SnapshotContainsHostJob
  SnapshotContainsWaiter
}

pub type DeserializeError {
  MalformedBinary
  IncompatibleSnapshot
}

type StoreImage {
  StoreImage(
    data: Dict(Int, JsSlot),
    free: List(Int),
    next: Int,
    pinned_roots: Set(Int),
    alloc_since_gc: Int,
    gc_threshold: Int,
    prop_seq: Int,
    private_uid: Int,
    symbol_uid: Int,
    microtasks: List(Job),
    unhandled_rejections: List(Int),
    shapes: Dict(Int, ShapeDesc),
    next_shape: Int,
    unit_uid: Int,
    names: NameTable,
  )
}

type RealmImage {
  RealmImage(
    current: Realm,
    realms: Dict(Int, Realm),
    template_objects: Dict(String, Handle),
  )
}

@external(erlang, "arc_snapshot_ffi", "encode")
fn encode(version: Int, store: StoreImage, realms: RealmImage) -> BitArray

@external(erlang, "arc_snapshot_ffi", "decode")
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
  ) = st
  use Nil <- result.try(case waiters {
    [] -> Ok(Nil)
    [_, ..] -> Error(SnapshotContainsWaiter)
  })
  let JsStore(
    data:,
    next:,
    pinned_roots:,
    alloc_since_gc:,
    gc_threshold:,
    gc_live: _,
    prop_seq:,
    private_uid:,
    symbol_uid:,
    ops: _,
    microtasks:,
    unhandled_rejections:,
    shapes:,
    next_shape:,
    unit_uid:,
    ics: _,
    free_protos: _,
    global_epoch: _,
    names:,
  ) = store
  let microtasks = types.jq_to_list(microtasks)
  let data =
    arena.fold(
      fn(id, slot, acc) { dict.insert(acc, id, drop_regexp_matcher(slot)) },
      dict.new(),
      data,
    )
  use Nil <- result.try(check_cells(data))
  use Nil <- result.try(check_jobs(microtasks))
  let store =
    StoreImage(
      data:,
      free: [],
      next:,
      pinned_roots:,
      alloc_since_gc:,
      gc_threshold:,
      prop_seq:,
      private_uid:,
      symbol_uid:,
      microtasks:,
      unhandled_rejections:,
      shapes:,
      next_shape:,
      unit_uid:,
      names:,
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
  )
  |> rt_builtins.seed_ops
}

fn restore(image: StoreImage) -> JsStore(Agent) {
  let StoreImage(
    data:,
    free: _,
    next:,
    pinned_roots:,
    alloc_since_gc:,
    gc_threshold:,
    prop_seq:,
    private_uid:,
    symbol_uid:,
    microtasks:,
    unhandled_rejections:,
    shapes:,
    next_shape:,
    unit_uid:,
    names:,
  ) = image
  JsStore(
    data: dict.fold(data, arena.new(), fn(acc, id, slot) {
      arena.set(id, slot, acc)
    }),
    next:,
    pinned_roots:,
    alloc_since_gc:,
    gc_threshold:,
    gc_live: 0,
    prop_seq:,
    private_uid:,
    symbol_uid:,
    ops: rt_store.t_store_new().ops,
    microtasks: list.fold(microtasks, types.jq_new(), types.jq_push),
    unhandled_rejections:,
    shapes:,
    next_shape:,
    unit_uid:,
    ics: dict.new(),
    free_protos: dict.new(),
    global_epoch: 0,
    names:,
  )
}

fn check_cells(data: Dict(Int, JsSlot)) -> Result(Nil, SnapshotError) {
  let offender: Option(Int) =
    dict.fold(data, None, fn(found, id, slot) {
      case found, holds_compiled_code(slot) {
        Some(_), _ -> found
        None, True -> Some(id)
        None, False -> None
      }
    })
  case offender {
    Some(id) -> Error(SnapshotContainsCompiledCode(JsCell(id)))
    None -> Ok(Nil)
  }
}

fn holds_compiled_code(slot: JsSlot) -> Bool {
  case slot {
    SObject(kind: KCompiled(..), ..) -> True
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
fn drop_regexp_matcher(slot: JsSlot) -> JsSlot {
  case slot {
    SObject(kind: RegExpObj(source:, flags:, last_index:, compiled: _), ..) ->
      SObject(
        ..slot,
        kind: RegExpObj(
          source:,
          flags:,
          last_index:,
          compiled: b_regexp.uncompiled_regexp(),
        ),
      )
    SObject(
      kind: types.KNative(
        tag: types.RegExpN(types.RegExpConstructor(legacy:, ..)),
        name:,
        length:,
        constructible:,
      ),
      ..,
    ) ->
      SObject(
        ..slot,
        kind: types.KNative(
          tag: types.RegExpN(types.RegExpConstructor(
            legacy:,
            proto_props: None,
            compiled: dict.new(),
          )),
          name:,
          length:,
          constructible:,
        ),
      )
    _ -> slot
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
