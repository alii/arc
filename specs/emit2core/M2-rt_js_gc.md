# M2 `rt_js_gc` — mark-sweep over the threaded JsStore

## Files
- **`../2core/src/twocore/runtime/rt_js_gc.gleam`** — pure-Gleam mark/sweep + root enumeration + exhaustive per-`JsSlot` tracer.
- **`../2core/src/twocore_rt_js_gc_ffi.erl`** — one function: the deep `Dynamic`-term walk (`refs_in_term/2`) that finds every `{js_cell, N}` inside an arbitrary BEAM term, **including inside a fun's captured env** via `erlang:fun_info(F, env)`. FFI naming/location matches `twocore_rt_state_ffi.erl`, `twocore_rt_exn_ffi.erl` etc. at `2core/src/*.erl`.

## Imports / dependency edges
```gleam
import gleam/dict
import gleam/list
import gleam/set.{type Set}
import gleam/option.{type Option, None, Some}
import gleam/dynamic.{type Dynamic}
import twocore/runtime/rt_state.{type InstanceState, InstanceState}
import twocore/runtime/rt_js_types.{
  type JsSlot, type Handle, type Realm, type Job,
  type PromiseState, type PromiseReaction, type ReactionHandler, type ObjKind,
}
import twocore/runtime/rt_js_store.{type JsStore, JsStore, handle_id}
```
**DAG:** M2 → M1a (`rt_js_types`) + M1b (`rt_js_store`) + `rt_state`. **M1 does NOT import M2.** M8 (`rt_js_async`) is the ONLY runtime module importing M2 — `t_drain_microtasks` calls `t_maybe_collect` between jobs (D11). M9's `resolve_js` dispatches ALL allocating ops (`"new_object"`, `"cell_new"`, …) directly to `rt_js_store.t_cell_new` — allocation NEVER triggers collection.

## Safepoint model — turn-boundary only (D11)
Collection runs **only between turns**, never mid-expression. `t_maybe_collect(st) -> st` is called from exactly two sites: (1) `rt_js_async.t_drain_microtasks` between jobs, (2) M19's `js_main` after top-level eval returns. It is gated on `store.call_depth == 0` — a re-entrant native call that drains microtasks mid-turn is a no-op for GC. This is a direct port of arc's `maybe_collect_at_toplevel` gate (`interpreter.gleam:5806-5827`, `call_stack == []`).

**Why this is sound without stack-walking:** at `call_depth == 0` no compiled-JS frame is live, so no BEAM local variable can hold a `{js_cell, N}` handle that isn't already reachable from `roots_of_state(st)` (Frame's `ActiveFunc`/`HomeObject` are cells in the store; captured bindings are `SBox` cells in `pinned_roots`; suspended generator/async locals live in the `resume` fun's captured env, traced via `refs_in_term`). `t_cell_new` NEVER collects — it only bumps `alloc_since_gc`. There is **no** `t_alloc = maybe_collect |> cell_new` wrapper.

## The `extra_roots` parameter — v1 dormant, v2 escape hatch
`t_collect(st, extra_roots: List(Handle))` keeps arc's `collect_with_roots` signature (`heap.gleam:470-476`) for the explicit host `gc()` builtin and for M20 tests that force collection with known live handles. Under the D11 turn-boundary model, automatic collection always passes `extra_roots = []` — at `call_depth == 0` there are no transient locals to root. **v2 escape hatch:** if a test262 case OOMs allocating within one synchronous turn, `extra_roots` becomes load-bearing for a mid-turn safepoint (e.g. loop back-edge) — the parameter is kept for exactly that contingency. The persistent root set is `store.pinned_roots` (populated via M1b `t_pin_root` at closure-create + realm-init time) plus the `Realm` handle fields — there is **no** `unroot` API (pins are for-VM-lifetime).

**Invariant M2-I1 (GC-safety):** `t_collect(st, extra_roots)` may free cell `N` iff `N` is unreachable from `roots_of_state(st) ∪ {handle_id(h) | h ∈ extra_roots}`. `t_maybe_collect` calls `t_collect(st, [])` and additionally requires `store.call_depth == 0`.

## Public API (exact signatures)

```gleam
//// rt_js_gc — mark-sweep over the threaded JsStore. Pure JsStore→JsStore
//// transform; the only FFI is the Dynamic deep-walk `refs_in_term`.

/// Default allocation-count threshold before an automatic collection.
/// Ported from arc gc_growth_threshold (interpreter.gleam:5796).
pub const default_gc_threshold: Int = 65_536

/// Full collection: mark from persistent roots (derived from `st`) + `extra_roots`,
/// sweep dead cells onto `st.js.free`, reset `alloc_since_gc` to 0.
/// Port of arc heap.collect_with_roots (heap.gleam:470-476).
pub fn t_collect(st: InstanceState, extra_roots: List(Handle)) -> InstanceState

/// Collect + DROP the free list (`free = []`) — for handing InstanceState across
/// a process boundary (gen_server reply). Port of arc heap.compact (heap.gleam:453-460).
pub fn t_compact(st: InstanceState, extra_roots: List(Handle)) -> InstanceState

/// Turn-boundary trigger: if `st.js.call_depth == 0 && st.js.alloc_since_gc >= default_gc_threshold`,
/// run `t_collect(st, [])`; else identity. Called ONLY by rt_js_async.t_drain_microtasks
/// (between jobs) and M19 js_main (after top-level eval). NEVER called from allocation.
/// Port of arc maybe_collect_at_toplevel (interpreter.gleam:5806-5827) INCLUDING the
/// call_stack==[] / call_depth==0 gate.
pub fn t_maybe_collect(st: InstanceState) -> InstanceState

/// Persistent-root enumeration: every Handle id reachable from `st` fields
/// OTHER than `st.js.data` itself. EXHAUSTIVE destructure of the `js` sub-record
/// (no `..`) — adding a Handle-carrying field to JsStore/JsRealm is a compile
/// error here. Port of arc state.reachable_root_refs (state.gleam:216-313)
/// minus interpreter-only fields (stack/locals/call_stack/constants).
pub fn roots_of_state(st: InstanceState) -> List(Int)

/// Per-cell child-handle enumeration. EXHAUSTIVE match on JsSlot — NO wildcard
/// arm. Port of arc gc_trace.refs_in_slot (gc_trace.gleam:222-543).
pub fn refs_in_cell(slot: JsSlot) -> List(Int)

/// Push every {js_cell, N} id reachable inside a Dynamic BEAM term onto acc.
/// Recurses into tuples, lists, maps, and — critically — fun captured env
/// (erlang:fun_info(F, env)). This is how a JS closure stored as an object
/// property keeps its captured cell handles alive.
pub fn push_term_refs(v: Dynamic, acc: List(Int)) -> List(Int)

/// Store-level stats (for tests / heapStats builtin).
pub type GcStats { GcStats(live: Int, free: Int, next: Int, since_gc: Int) }
pub fn stats(st: InstanceState) -> GcStats
```

## Private functions (exact list, with arc provenance)

| fn | arity | port of (arc file:line) | notes |
|---|---|---|---|
| `mark_from` | `(JsStore, List(Int)) -> Set(Int)` | `heap.gleam:487-490` | seed frontier, call `mark_loop` |
| `mark_loop` | `(JsStore, List(Int), Set(Int)) -> Set(Int)` | `heap.gleam:495-538` | tail-recursive DFS. **DROP** both `lazy_proto.decode_lazy_proto` branches (heap.gleam:513-517, 527-530) — protos are real cells |
| `prepend_ids` | `(List(Int), List(Int)) -> List(Int)` | `heap.gleam:541-546` | already `Int`, no unwrap needed |
| `sweep` | `(JsStore, Set(Int)) -> JsStore` | `heap.gleam:550-563` | **DROP** `!lazy_proto.is_real_slot(id)` guard (heap.gleam:557); every id is real |
| `union_roots` | `(List(Int), List(Handle)) -> List(Int)` | `heap.gleam:482-484` | fold `handle_id` over extra_roots |
| `push_option_handle` | `(Option(Handle), List(Int)) -> List(Int)` | `gc_trace.gleam:100-108` | |
| `push_property_refs` | `(Property, List(Int)) -> List(Int)` | `gc_trace.gleam:35-52` | DataProperty/AccessorProperty |
| `push_reaction_refs` | `(PromiseReaction, List(Int)) -> List(Int)` | `gc_trace.gleam:483-490` | full destructure |
| `push_reaction_handler` | `(ReactionHandler, List(Int)) -> List(Int)` | `gc_trace.gleam:24-32` | `Handler(fun)` traces the fun via `push_term_refs` |
| `push_job_refs` | `(Job, List(Int)) -> List(Int)` | `gc_trace.gleam:82-98` | exhaustive over M8's `Job` type |
| `push_objkind_refs` | `(ObjKind, List(Int)) -> List(Int)` | `gc_trace.gleam:263-460` | exhaustive over M4's `ObjKind` |

## `refs_in_cell` — the exhaustive `JsSlot` match

Contract with M1a: `JsSlot` variants (`rt_js_types` defines; M2 exhaustively traces). Expected variant set derived from arc `HeapSlot` (`value.gleam:4159-4269`) minus interpreter-only slots:

```gleam
pub fn refs_in_cell(slot: JsSlot) -> List(Int) {
  case slot {
    // Port of gc_trace.gleam:235-461 (ObjectSlot arm)
    rt_js_types.SObject(kind:, props:, elements:, proto:, sym_props:, extensible: _, private_brands: _) -> {
      let acc = dict.fold(props, [], fn(a, _k, p) { push_property_refs(p, a) })
      let acc = list.fold(sym_props, acc, fn(a, pair) { push_property_refs(pair.1, a) })
      let acc = dict.fold(elements, acc, fn(a, _i, v) { push_term_refs(v, a) })
      let acc = push_option_handle(proto, acc)
      push_objkind_refs(kind, acc)
    }
    // Port of gc_trace.gleam:464 (BoxSlot)
    rt_js_types.SBox(value: v) -> push_term_refs(v, [])
    // Port of gc_trace.gleam:471-493 (PromiseSlot) — reactions live INSIDE Pending (§2.4)
    rt_js_types.SPromise(state:, is_handled: _) ->
      case state {
        rt_js_types.Pending(rs) -> list.fold(rs, [], push_reaction_refs)
        rt_js_types.Fulfilled(v) | rt_js_types.Rejected(v) -> push_term_refs(v, [])
      }
    // NEW (no arc equivalent — CPS state machine per M18): resume is a BEAM fun
    // whose captured env holds every live local at the await/yield point.
    // Tracing it via push_term_refs (fun_info env walk) IS the stack-map.
    rt_js_types.SGenerator(state: _, resume:, gen_cell:) ->
      [handle_id(gen_cell), ..push_term_refs(resume, [])]
    rt_js_types.SAsyncGen(state: _, resume:, queue:, gen_cell:) ->
      [handle_id(gen_cell), ..push_term_refs(dynamic.from(queue), push_term_refs(resume, []))]
  }
}
```

**DROPPED arc `HeapSlot` variants** (interpreter-only, not in `JsSlot`): `EnvSlot` (closures use BEAM fun captures — traced via `push_term_refs` on the fun), `EvalEnvSlot` (direct-eval; not in v1 scope), `ForInIteratorSlot` (key list is a BEAM local list of binaries, no handles), `CounterSlot` (use `BoxCell(Int)`), `AsyncFunctionSlot`/`GeneratorSlot`'s `SuspendedFrame` (replaced by CPS `resume` fun), `RealmSlot` (single realm is a struct field on `JsStore`, not a cell — see `roots_of_state`).

**`push_objkind_refs`** exhaustively matches M4's `ObjKind` (port of `gc_trace.gleam:263-460`). Ref-carrying kinds: `FunctionObject(home: Option(Handle))`, `BoundFunction(target: Handle, bound_this, bound_args)`, `ProxyObject(target, handler)`, `PromiseObject(data: Handle)`, `GeneratorObject(data: Handle)`, `AsyncGeneratorObject(data: Handle)`, `ArrayIteratorObject(source: Handle)`, `SetIteratorObject/MapIteratorObject(source: Handle)`, `MapObject(entries)`/`SetObject(entries)` (fold `push_term_refs` over entries), `WeakMapObject`/`WeakSetObject` (traced **strongly** — same rationale as `gc_trace.gleam:318-332`: freed ids are recycled, so a weak key must be pruned before its id reaches the free list, and no ephemeron pass exists), `DataViewObject(buffer: Handle)`, `TypedArrayObject(buffer: Handle)`, `ModuleNamespace(exports: Dict(_, Handle))`. Ref-free kinds listed explicitly (no `_`): `Ordinary`, `ArrayObject`, `ArgumentsObject`, `ErrorObject(_)`, `StringObject(_)`, `NumberObject(_)`, `BooleanObject(_)`, `BigIntObject(_)`, `SymbolObject(_)`, `DateObject(_)`, `RegExpObject(..)`, `ArrayBufferObject(_)`, `StringIteratorObject(_)`.

**DROPPED arc `ExoticKind` machinery**: entire `NativeFunction` payload tracing (`gc_trace.gleam:555-1568`, ~1000 lines of per-builtin-enum tracers). Compiled builtins are BEAM funs traced via `push_term_refs` — no `NativeFnSlot` enum exists. `IntlObject`/`Temporal*`/`ShadowRealmObject`/`FinalizationRegistryObject`/`DisposableStackObject`/`IteratorHelperObject` — not in M6's builtin set.

## `roots_of_state` — persistent-root enumeration

Formula (must match SPEC.md §7.M2): `pinned_roots ∪ realm.* handles ∪ global_object ∪ refs_in_term(microtasks) ∪ refs_in_term(unhandled_rejections)`. Exhaustive destructure of `st.js` (the `JsStore` sub-record M1b adds to `InstanceState`). Port of arc `state.reachable_root_refs` (`state.gleam:216-313`) minus interpreter fields:

```gleam
pub fn roots_of_state(st: InstanceState) -> List(Int) {
  let assert Some(js) = st.js_store   // panic "js op on InstanceState with no JsStore" on None (§2.1 G7)
  let JsStore(
    data: _, free: _, next: _, alloc_since_gc: _, call_depth: _,  // store bookkeeping
    ops: _, host_hooks: _,                                        // fn-records; no handles
    pinned_roots:,            // Set(Int) — closures' captured-binding cells + realm intrinsics
    realm:,
    microtasks:,              // JobQueue — port state.gleam:297-300
    unhandled_rejections:,    // List(Handle) — port state.gleam:282
    // …remaining non-Handle fields destructured with `field: _` (no `..`)…
  ) = js
  let rt_js_types.Realm(
    global_object:,           // Handle — port state.gleam:271
    // Every prototype/constructor handle — replaces arc's heap.roots persistent set
    object_proto:, function_proto:, array_proto:, string_proto:, number_proto:,
    boolean_proto:, symbol_proto:, bigint_proto:, error_proto:, type_error_proto:,
    range_error_proto:, syntax_error_proto:, reference_error_proto:, eval_error_proto:,
    uri_error_proto:, aggregate_error_proto:, promise_proto:, map_proto:, set_proto:,
    date_proto:, regexp_proto:, generator_proto:, async_generator_proto:,
    iterator_proto:, async_iterator_proto:, array_iterator_proto:,
    string_iterator_proto:, map_iterator_proto:, set_iterator_proto:,
    array_buffer_proto:, data_view_proto:, typed_array_protos:,  // Dict(Kind, Handle)
    object_ctor:, array_ctor:, function_ctor:, promise_ctor:, // ... every ctor
    symbol_registry: _,       // Dict(String, SymbolId) — SymbolId is a BEAM ref, no handle
  ) = realm
  let acc = set.to_list(pinned_roots)
  let acc = [handle_id(global_object), handle_id(object_proto), /* … every proto/ctor … */ ..acc]
  let acc = dict.fold(typed_array_protos, acc, fn(a, _k, h) { [handle_id(h), ..a] })
  let acc = push_term_refs(dynamic.from(microtasks), acc)
  push_term_refs(dynamic.from(unhandled_rejections), acc)
}
```

The `Realm` field list is the M6 contract; the exhaustive destructure (no `..` on `Realm`) means M6 adding a prototype is a compile error here until rooted — the safety property from arc `state.gleam:210-215` and `gc_trace.gleam:5-9`. `JsStore` non-Handle fields are listed with `field: _` (not `..`) for the same reason.

## FFI: `twocore_rt_js_gc_ffi.erl`

```erlang
-module(twocore_rt_js_gc_ffi).
-export([refs_in_term/2]).

%% Deep walk: push every js_cell id reachable inside Term onto Acc. Recurses
%% into tuples/lists/maps and — the load-bearing case — a fun's captured env,
%% so a JS closure stored in a cell keeps its captured handles alive.
refs_in_term({js_cell, N}, Acc) when is_integer(N) -> [N | Acc];
refs_in_term(F, Acc) when is_function(F) ->
    {env, Env} = erlang:fun_info(F, env),
    lists:foldl(fun refs_in_term/2, Acc, Env);
refs_in_term(T, Acc) when is_tuple(T) ->
    lists:foldl(fun refs_in_term/2, Acc, tuple_to_list(T));
refs_in_term([H | T], Acc) -> refs_in_term(T, refs_in_term(H, Acc));
refs_in_term(M, Acc) when is_map(M) ->
    maps:fold(fun(K, V, A) -> refs_in_term(V, refs_in_term(K, A)) end, Acc, M);
refs_in_term(_, Acc) -> Acc.  % atom | number | binary | ref | pid | port | []
```
`push_term_refs` is `@external(erlang, "twocore_rt_js_gc_ffi", "refs_in_term")`.

## Algorithm bodies (direct port, simplifications noted)

**`t_collect`** (port `heap.gleam:470-476`):
```gleam
pub fn t_collect(st: InstanceState, extra_roots: List(Handle)) -> InstanceState {
  let roots = union_roots(roots_of_state(st), extra_roots)
  let live = mark_from(st.js, roots)
  let js = sweep(st.js, live)
  InstanceState(..st, js: JsStore(..js, alloc_since_gc: 0))
}
```

**`mark_loop`** (port `heap.gleam:495-538`, lazy_proto branches deleted):
```gleam
fn mark_loop(store: JsStore, frontier: List(Int), visited: Set(Int)) -> Set(Int) {
  case frontier {
    [] -> visited
    [id, ..rest] -> case set.contains(visited, id) {
      True -> mark_loop(store, rest, visited)
      False -> {
        let visited = set.insert(visited, id)
        case dict.get(store.data, id) {
          Error(Nil) -> mark_loop(store, rest, visited)  // dangling — skip
          Ok(cell) -> mark_loop(store, prepend_ids(refs_in_cell(cell), rest), visited)
        }
      }
    }
  }
}
```

**`sweep`** (port `heap.gleam:550-563`, `is_real_slot` guard deleted):
```gleam
fn sweep(store: JsStore, live: Set(Int)) -> JsStore {
  let new_data = dict.filter(store.data, fn(id, _) { set.contains(live, id) })
  let new_free = dict.fold(store.data, store.free, fn(free, id, _) {
    case set.contains(live, id) { True -> free  False -> [id, ..free] }
  })
  JsStore(..store, data: new_data, free: new_free)
}
```

**`t_maybe_collect`** (port `interpreter.gleam:5806-5827`, `call_depth == 0` gate KEPT):
```gleam
pub fn t_maybe_collect(st: InstanceState) -> InstanceState {
  let assert Some(js) = st.js_store   // panic "js op on InstanceState with no JsStore" on None (§2.1 G7)
  case js.call_depth == 0 && js.alloc_since_gc >= default_gc_threshold {
    True -> t_collect(st, [])
    False -> st
  }
}
```

## Invariants M2 upholds

- **M2-I1 (soundness):** after `t_collect(st, ex)`, for every `h ∈ roots_of_state(st) ∪ ex`, `dict.get(st'.js.data, handle_id(h)) == Ok(_)` and transitively for every id in `refs_in_cell` of any live cell.
- **M2-I2 (free-list correctness):** `sweep` adds exactly `dict.keys(old.data) \ live` to `free`; `t_compact` sets `free = []`. Every id on `free` is absent from `data`.
- **M2-I3 (counter reset):** `t_collect`/`t_compact` set `alloc_since_gc = 0`. `t_maybe_collect` on the `False` branch is identity (does not touch the counter).
- **M2-I4 (purity):** `t_collect` is a pure `InstanceState → InstanceState` function — no pdict, no ets, no message send. Safe to call from any process holding the threaded state.
- **M2-I5 (exhaustiveness discipline):** every `case` over `JsSlot`, `ObjKind`, `Property`, `PromiseState`, `ReactionHandler`, `Job`, and the `JsStore`/`Realm` record destructures has **NO `_` wildcard and NO `..` on ref-carrying arms**. Adding a variant or Handle-typed field anywhere in M1/M4/M6/M8's types is a compile error in this file. This is the file's whole reason to exist (arc `gc_trace.gleam:5-12`).
- **M2-I6 (no lazy_proto):** every prototype is a real allocated `ObjectCell`; ids are non-negative, `mark_loop` never synthesizes a slot, `sweep` recycles every dead id.
- **M2-I7 (weak collections traced strong):** `WeakMapObject`/`WeakSetObject` entries are traced as strong refs (over-retention is spec-legal; false hits from id recycling are not — arc `gc_trace.gleam:318-326`).
- **M2-I8 (fun-env tracing):** a BEAM fun stored in any cell field keeps every `{js_cell, N}` in its captured environment alive via `refs_in_term`'s `erlang:fun_info(F, env)` walk. This is how MakeClosure-captured mutable-binding cells and CPS resume-continuation locals stay live across GC.

## Simplifications vs arc (explicit deletions)

| arc feature | file:line | M2 disposition |
|---|---|---|
| `heap.roots: Set(Int)` + `root`/`unroot` | `heap.gleam:26,416-423` | **REPLACE** with `JsStore.pinned_roots: Set(Int)` + M1b `t_pin_root` (add-only; no `unroot`) |
| `host_refs: fn(host) -> List(Ref)` | `heap.gleam:34,394` | **DROP** — no opaque host type param; funs traced via `refs_in_term` |
| `empty_env: Option(Ref)` | `heap.gleam:39` | **DROP** — no `EnvSlot` |
| `last_collect_next` / `grown_since_collect` | `heap.gleam:43,464` | **REPLACE** with `alloc_since_gc: Int` on `JsStore` (M1 field), reset to 0 in `t_collect` |
| `lazy_proto` decode in mark | `heap.gleam:513-530` | **DROP** — real cells only |
| `lazy_proto.is_real_slot` in sweep | `heap.gleam:557` | **DROP** |
| `native_fn_refs` + 30 per-builtin tracers | `gc_trace.gleam:555-1568` | **DROP** — builtins are BEAM funs |
| `push_suspended_frame_refs` | `gc_trace.gleam:201` | **DROP** — CPS resume fun replaces `SuspendedFrame` |
| interpreter `call_stack==[]` gate | `interpreter.gleam:5808-5818` | **KEEP** as `store.call_depth == 0` — the D11 turn-boundary invariant; `t_maybe_collect` is a no-op unless it holds |

## Test surface (M20 hooks)

- `stats(st).live` before/after `t_collect` with a known-dead-cell setup.
- Round-trip: alloc N cells, drop refs to half, `t_collect`, assert freed ids on `free` list, re-alloc reuses them.
- Fun-env tracing: `let f = MakeClosure capturing {js_cell, 5}; store f in cell 1; drop local ref to 5; t_collect([cell 1]); assert cell 5 live`.
- Exhaustiveness: compile-time only (no runtime test — the Gleam compiler is the test).