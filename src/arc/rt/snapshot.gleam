//// An `Agent` as one binary and back.
////
//// The image holds every `JsStore` field except `ops`, plus the current
//// realm, the realm registry and the template map. It holds no BEAM funs: `JsOps` is re-seeded with
//// the runtime's entries on `deserialize` (the interpreter links its own on
//// top), `hooks` are supplied by the caller, `host_fns` are re-registered by
//// the embedder (ids line up by registration order), the `import_hook` is
//// re-installed and `frames` are empty at an engine boundary. RegExp objects are written without their compiled
//// matcher (an OTP-release-specific `re` pattern) and recompile on first
//// exec. A store that itself holds compiled code (a
//// `KCompiled` function, a coroutine parked in a compiled state machine, a
//// queued `HostJob`) cannot be written, because a fun is bound to one loaded
//// version of its module; `serialize` fails and names the cell.
////
//// The bytes are `<<"arc-engine", abi_version:32, term>>` where `term` is
//// `term_to_binary({arc_snapshot, abi_version, store, realms})`. The header
//// lets `deserialize` reject foreign or stale bytes before decoding; it is
//// not an authenticity check, so only feed it bytes this library produced.

import arc/host_hooks.{type HostHooks}
import arc/internal/tree_array
import arc/rt/builtins as rt_builtins
import arc/rt/builtins/regexp as b_regexp
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type Job, type JsSlot, type JsStore, type Realm,
  type ShapeDesc, Agent, HostJob, JsCell, JsStore, KCompiled, ReactionJob,
  RegExpObj, ResolveThenableJob, ResumeCompiled, ResumeFrame, SAsyncContext,
  SAsyncGen, SBox, SDisposeCapability, SGenerator, SObject, SPromiseData,
  SShapedObject,
}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

/// Identifies the shape of the image below and of every runtime record
/// inside it. Bump on any change to `StoreImage`, `RealmImage`, `JsSlot`,
/// `ObjKind`, `Realm` or the wire value; older images then decode as
/// `IncompatibleSnapshot` instead of being misread.
///
///   1..3  interpreter-heap snapshots (`arc/engine` before the shared store)
///   4     interpreter heap inside the `{arc_snapshot, ..}` term
///   5     shared-store image
///   6     parse ids: `unit_uid` counter, `unit` on closures and frames
pub const abi_version = 6

/// Why `serialize` refused to write the agent.
pub type SnapshotError {
  /// `cell` is a compiled function, or a generator / async body suspended
  /// inside compiled code.
  SnapshotContainsCompiledCode(cell: Handle)
  /// A `HostJob` closure is still queued; drain microtasks first.
  SnapshotContainsHostJob
  /// An `Atomics.waitAsync` waiter with no timeout is still parked; its
  /// wake source (a later `Atomics.notify`) does not survive the image.
  SnapshotContainsWaiter
}

/// Why `deserialize` rejected a binary.
pub type DeserializeError {
  /// No snapshot header at all: random bytes, a bare Erlang term, or an
  /// unaligned bit array. Nothing was decoded.
  MalformedBinary
  /// A snapshot, but of another `abi_version`, or a corrupt payload behind
  /// a valid header.
  IncompatibleSnapshot
}

/// `JsStore` without `ops` and `call_depth`, with the microtask queue as a
/// plain list so the image does not depend on OTP's `queue` representation.
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

/// Write `st` as a snapshot binary. Call at an engine boundary: no frame is
/// running and `call_depth` is not recorded.
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
  ) = store
  let microtasks = types.jq_to_list(microtasks)
  let data =
    tree_array.sparse_fold(
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
    )
  let realms = RealmImage(current: realm, realms:, template_objects:)
  Ok(encode(abi_version, store, realms))
}

/// Read an agent back from `serialize` output, on `hooks`, with no host
/// functions registered, no import hook installed and the runtime's `JsOps`
/// seeded. The interpreter's `JsOps` entries are not linked; the caller does
/// that, as for a fresh agent.
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
  ) = image
  JsStore(
    data: dict.fold(data, rt_store.data_new(), fn(acc, id, slot) {
      tree_array.set(id, slot, acc)
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

/// A RegExp's cached matcher is a `re` compiled pattern, valid only on the
/// OTP release that built it, so the image keeps source and flags and the
/// restored object recompiles on its first exec.
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
