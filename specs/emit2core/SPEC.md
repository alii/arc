# ARC→2CORE FINAL DESIGN SPEC

**Target file:** `../2core/specs/ARC-2CORE-FINAL.md`
**Status:** Single source of truth for 20 parallel implementation modules. Every module below is buildable in its final form from this document alone.

---

## §0 Overview

arc's JS parser + scope analyzer + 97.4%-test262 interpreter is re-targeted to emit 2core IR directly, compiled to native BEAM via 2core's existing `ir → ir_lower → emit_core → CoreErlang → .beam` pipeline. The JS heap becomes a **threaded** `JsStore` (a field on 2core's `InstanceState`), replacing the current pdict-based `rt_js`. arc's mark-sweep GC ports to operate on the threaded store. Async/generators compile to a full state-machine transform (no interpreter fallback). Compiled functions are native BEAM funs stored inside JS Function-object cells.

**Repos:** this repo (parser+scope reused verbatim; new `emit_2core/` backend), the sibling `2core` repo (new `rt_js_*` runtime modules; ~6 targeted `emit_core.gleam` changes; `pipeline.gleam` gains `compile_ir` entry).

---

## §1 Cross-Cutting Decisions (D-numbered, cited by modules)

Eighteen unit specs surfaced conflicts. Each is resolved once here; downstream modules cite by D-number.

| # | Decision | Rationale / evidence |
|---|---|---|
| **D1** | **Return-tuple order = `{Result, St'}` — value FIRST, state SECOND.** Every threaded rt_js op returns `#(JsVal, InstanceState)` (or `#(a, InstanceState)` for typed results); every compiled JS function returns `{V, St'}`. | 2core's entire Threaded seam uses this order: `emit_core.gleam:1559` `CTuple([pkg, CVar(cur)])`, `:1936` `PTuple([PVar(vvar), PVar(stvar)])`, `:3508-3509` unpack, `:570` pure-export adapter, `rt_state.t_grow` etc. Task brief's `{State', Result}` was a summary error — the operative constraint is "2core's Threaded state_strategy", which IS `{Result, St'}`. Flipping would rewrite `emit_return`/`emit_threaded_call_unpack`/`emit_value_state_pair`/`apply_cont` + every existing `t_*` in rt_state/rt_mem/rt_table. **M8.md/M14.md state-first is REJECTED.** |
| **D2** | **State is INVISIBLE at the IR level.** The frontend emits `CallHost("js","get_prop",[recv,key])` with NO St arg, `CallClosure(f,[frame,args])` with NO St arg, `MakeClosure(name,caps,arity:2)`. `emit_core` under `Threading(cur)` transparently prepends `cur`, unpacks `{V,St'}`, rebinds. `Emitter2` has NO `state_var` field. | Cleaner frontend IR; reuses `emit_value_state_pair`/`emit_threaded_call_unpack` verbatim; concentrates all state threading in ~5 emit_core functions (M9). Half the unit specs assumed explicit-St-as-IR-Var; those are mechanically simplified by dropping the St bookkeeping. **M13-M18 drafts using `st_cur`/`state_var` are STALE.** |
| **D3** | **`js_profile: Bool` on `Binding`.** When `True`, `state_reaching_closure` short-circuits to "every function", and `MakeClosure`/`CallClosure`/`CallHost("js",…)` emit their threaded variants unconditionally. | `emit_core.gleam:1490-1492` MakeClosure is UN-threaded today; `:1530-1535` CallClosure is state-neutral; `:3554` CallHost("js") is state-neutral. JS closures cannot be state-neutral (every call may allocate). Uniform threading avoids per-function analysis. WASM never emits MakeClosure/CallClosure (`ir.gleam:873`) → zero regression. Flag name per D18. |
| **D4** | **JS function value = `{js_cell, Int}` cell handle** whose `ObjKind = KFunction{code: fun/3, home_object, flags, captures}`. NOT a raw BEAM fun. `is_callable(v)` reads the cell kind. | Raw-fun repr breaks `===`: `fun(X)->X end =:= fun(X)->X end` → `true` on BEAM (same module/index/uniq), so `(function(){}) === (function(){})` would wrongly be `true`. Also: `f.prototype`, `f.name`, `f.bind()`, `new Proxy(f,…)` all require per-function object storage. arc precedent: `value.gleam:3384-3388` FunctionObject. Fast-path: M12 emits `CallDirect` for statically-resolved same-module `FunctionDeclaration` calls. |
| **D5** | **`Frame` 4-tuple as param 2.** Compiled fn signature at BEAM level: `'jsf_N'/(m+3) = fun(St, Cap₀..Cap_{m-1}, Frame, Args) -> {V, St'}` where `Frame = {ThisVal, ActiveFunc, HomeObject, NewTarget}` in `arc/vm/lexical.gleam:30-35` `all_lexical_refs` order. IR-level `ir.Function.params = [caps..., "_frame", "_args"]` (emit_core prepends St). | Preserves arity-3-after-captures uniform shape while carrying all four lexicals. Rejects fields-on-InstanceState (arc `call.gleam:1187-1206` save/set/restore per call — hostile to Threading, adds InstanceState churn). Arrows CAPTURE the enclosing Frame tuple (or its components per `FunctionInfo.lexical_captures`, `scope.gleam:248`). |
| **D6** | **Exception ABI: state-in-payload via emit_core prepend.** IR-level: `TagDecl("js_exn",[TTerm])` (ONE param), `Throw("js_exn",[v])`, `CatchHandler(OnTag("js_exn"),["_e"],None,h)`. emit_core under `Threading(cur)`: `emit_throw` prepends `CVar(cur)` → wire = `{wasm_exn, 0, [St, V]}`; `emit_catch_clause` prepends fresh `"_st_c"` to payload names, emits handler under `Threading("_st_c")`. | Only mechanism preserving store mutations before throw (`try{o.x=1;throw 0}catch{o.x}` must see 1). Reuses `rt_exn` wire shape (`twocore_rt_exn_ffi.erl:43` `{wasm_exn, TagId, Payload}`). `emit_core.gleam:5128` "Threaded+EH categorised-unsupported" is lifted by M9's ~15-line patch. **IR-level `TagDecl` arity=1; wire payload=2 (emit_core owns the encoding); no IR validator checks this — verified `emit_core.gleam:5097`.** |
| **D7** | **rt_js ops that throw JS errors RAISE directly** via `twocore_rt_js_store_ffi:t_throw(St, ErrVal) -> no_return()` = `erlang:error({wasm_exn, 0, [St, ErrVal]})`. NOT a `Result`-return. | One exception channel; zero-cost hot path; 2core's existing `emit_try` `try_dispatch` (`emit_core.gleam:5192-5219`) catches it unchanged. `St` in the raised term is post-Error-allocation, so the catch handler's store contains the Error object. Replaces today's `twocore_rt_js_ffi.erl:75` `type_error(Detail) -> erlang:error({js_error, ...})`. **M4.md `JsR(a) = Result(...)` and M18.md:338 Result-return are REJECTED.** |
| **D8** | **`rt_js_types.gleam` leaf module** holds every type reachable from `JsSlot` (Handle, PropertyKey, SymbolId, WellKnown, ObjectKey, Property, ParsedDesc, JsElements, ObjKind, FnFlags, PromiseState, PromiseReaction, Job, ReactionHandler, GeneratorState, AsyncGenState, Realm, BuiltinPair). `rt_js_store.gleam` defines `JsStore` + cell ops. `rt_js_gc`/`rt_js_obj`/`rt_js_async` all import `rt_js_types` — no cycles. | arc precedent: `value.gleam` 4884-line leaf. M2's `refs_in_cell` must exhaustively match `JsSlot` with NO wildcard (arc `gc_trace.gleam:5-9` safety property) — one defining module makes this a compile error, not a runtime miss. |
| **D9** | **Private fields = `PropertyKey.Private(BitArray)` in the object's own props dict**, NOT a brand-set + side-table. `t_new_private_name(st, "#x") -> #(st', {js_key_priv, <<"#x", 0, UidBin>>})` bumps `JsStore.private_uid`. Brand check = `dict.has_key(props, priv_key)`. | arc's proven model (`key.gleam:160-210`, `object.gleam:1763-1778`): one mechanism covers fields/methods/accessors; `#x in obj` = own-prop presence; reflection auto-skips via key-variant filter; per-evaluation identity via threaded uid counter (replaces arc's `erlang:unique_integer` — deterministic, replayable). |
| **D10** | **String encoding = UTF-8 `binary()`, codepoint-indexed.** `.length` = Unicode codepoint count (NOT UTF-16 code-unit count). | arc stores strings as Gleam `String` = UTF-8 binary and indexes by codepoint (`arc_string_ffi.erl:12-15,30-34`). This is a documented spec deviation arc ships at 97.4% (`js_string.gleam:10-13`). M20's byte-for-byte differential vs arc-interpreter REQUIRES matching arc — UTF-16 would diverge on `"😀".length` (arc:1, spec:2). BEAM-native, zero-copy `ConstBinary`. |
| **D11** | **GC safepoint = TURN BOUNDARY ONLY.** `t_maybe_collect(st) -> st` is called ONLY by `t_drain_microtasks` between jobs AND once after `js_main` returns; it is gated on `store.call_depth == 0`. **NO** function-entry prologue call — M14 emits **no** `maybe_collect` `CallHost`; `t_cell_new` **never** collects. Roots = `JsStore.pinned_roots` ∪ `realm.*` ∪ `global_object` ∪ `refs_in_term(microtasks)` ∪ `refs_in_term(unhandled_rejections)`. NO stack-walking, NO shadow-stack. | Matches arc exactly (`interpreter.gleam:5806-5827` `maybe_collect_at_toplevel` gated on `call_stack == [] && call_depth == 0`). A fn-entry safepoint is UNSOUND: at entry the caller's live temporaries (let-bound `Handle`s in BEAM regs) are invisible to any root set, so collecting there would free reachable cells. At turn boundary the JS stack is empty by construction → the persistent root set is complete. `t_collect(st, extra_roots)` keeps its `extra_roots` param as a v2 escape hatch (host-driven mid-turn `gc()` if a test262 case ever OOMs). A pathological `while(true){[]}` never collects until it returns — same as arc today. |
| **D12** | **arc→2core one-way path dependency.** `arc/gleam.toml` gains `twocore = { path = "../2core" }`. 2core does NOT depend on arc. All 26 arc `*_ffi.erl` files needed at runtime are COPIED into `2core/src/twocore_rt_js_*_ffi.erl` per §10's table. | `2core/gleam.toml:15-22` deps = stdlib+erlang+argv+simplifile only. rt_js modules cannot `@external` to arc modules. |
| **D13** | **emit_2core module cycles broken via `EmitDispatch` fn-record on `Emitter2`; `Build(a)` monad pinned.** `expr↔stmt↔fn↔destructure↔class↔exn↔async` are mutually recursive. `Emitter2.dispatch: EmitDispatch` is a **7-field** record — `emit_expr`, `emit_stmts`, `emit_pattern`, `emit_function`, `emit_class`, `emit_async_body`, `emit_destructure` — set once by M19's `init_emitter`. **Placement:** `EmitDispatch` type is defined in **M10 `emit_2core/state.gleam`** (leaf, alongside `Emitter2` which references it) and is **NOT `opaque`** (M19 constructs it directly). `Build(a)` monad is defined in **M11 `emit_2core/anf.gleam`** with the canonical signature `pub type Build(a) = fn(Emitter2, fn(Emitter2, a) -> ir.Expr) -> ir.Expr` (M12.md:32) — the `#(Emitter2, ir.Expr)`-returning variant and the direct-CPS `Emit = #(ir.Expr, Emitter2)` shape are REJECTED. | arc precedent: `state.gleam:645-684` `state.ctx.call` fn-pointer. Gleam forbids module cycles; single-mega-file rejected for maintainability. Leaf placement mirrors D8 (`rt_js_types.gleam` holds every type reachable from `JsSlot`) — `Emitter2.dispatch: EmitDispatch` makes `EmitDispatch` reachable from `Emitter2`, so it lives in the same module. 3→7 fields: M14 func, M16 class, M18 async, M15 destructure each need a cross-module callback the original 3 don't cover. |
| **D14** | **`prop_seq` counter is threaded on `JsStore.prop_seq: Int`**, NOT arc's global FFI atomic (`arc_vm_ffi.erl:next_prop_seq`). Every `Property` construction takes `st` and stamps the seq. | Process-global counter incompatible with pure threading + replayability. |
| **D15** | **`Unsupported` terminal set (NOT "phase 2"):** direct `eval`, `with` statement, `Function()` constructor, `Proxy` construction. All throw `EvalError`/`SyntaxError` at compile-time (emit) or runtime (Proxy ctor). | HANDOFF §6 rejected these; arc's direct-eval requires runtime re-compilation into the live scope — architecturally incompatible with AOT. Indirect `eval` (global scope) is supported via a runtime hook M19 seeds. |
| **D16** | **`JsVal` is OPAQUE at the Gleam type level; `classify/1` FFI is the ONE decode point.** `rt_js_types` declares `pub type JsVal` (opaque, wire = the §2.3 tagged-tuple encoding) plus a `pub type JsValKind` sum (`KUndef`, `KNull`, `KBool(Bool)`, `KNum(JsNum)`, `KStr(String)`, `KBig(Int)`, `KSym(SymbolId)`, `KHandle(Handle)`). All rt_js Gleam matches on `JsValKind`, never on the wire term. | A single ~15-clause Erlang FFI (`twocore_rt_js_val_ffi:classify/1`) owns the wire↔Gleam mapping; every other module stays wire-agnostic. Removes `Dynamic` from every `Property`/`SBox`/`ObjKind` field (§2.4). Changing the wire encoding = one FFI edit, zero Gleam churn. |
| **D17** | **rt_js module cycles broken via `JsOps(st)` fn-record, type-parameterized over the threaded state.** `rt_js_types` (LEAF — no `rt_state` import) defines `pub type JsOps(st) { JsOps(get_prop: fn(st,JsVal,ObjectKey)->#(JsVal,st), call: fn(st,JsVal,JsVal,List(JsVal))->#(JsVal,st), …) }` AND `pub type JsStore(st) { JsStore(…, ops: JsOps(st), host_hooks: HostHooks, call_depth: Int, microtasks: JobQueue, …) }`. `rt_state` imports `rt_js_types` and ties the knot: `js_store: Option(JsStore(InstanceState))` on `InstanceState` + `pub type JsOpsC = JsOps(InstanceState)` — recursion through fn types across the module boundary. Downstream `rt_js_val`/`rt_js_obj`/etc. reach up-stack via `store.ops.get_prop(st,…)` instead of `@external` to a mangled Erlang module name. `init_realm` (M6 step 1) seeds `store.ops` once with the concrete M4/M-CALL fns. | arc precedent: `value.gleam:2064,4159` + `state.gleam:29-46,92,317` `RealmCtx(host)`/`State(host)` type param. Gleam forbids module cycles; the `@external "twocore@runtime@rt_js_obj"` by-Erlang-name hack (M3 draft) is rename-unsafe and invisible to `gleam check` — REJECTED. Compile-proof: three-file check (leaf `JsOps(st)`/`JsStore(st)` + `rt_state` knot + downstream `t_get_prop` consumer) type-checks under `gleam check`. |
| **D18** | **`Binding` flag is named `js_profile: Bool`** (NOT `uniform_state_threading`). `profiles.js_direct()` sets it `True`; `safe_default()` seeds `False`. All emit_core gates (`emit_make_closure`/`emit_call_closure`/`emit_call_host`/`emit_throw`/`emit_catch_clause` §9.8-9.11) test `ctx.binding.js_profile`. | The flag governs more than state threading (it also selects the `"js"` host-namespace dispatch and the D6 EH prepend), so the name says what profile it enables, not one mechanism it toggles. WASM byte-identity guarantee: `js_profile: False` ⇒ every §9 rewrite is a no-op. |

---

## §2 Canonical Types

### §2.1 `InstanceState` extension — `2core/src/twocore/runtime/rt_state.gleam:134-144`

```gleam
pub type InstanceState {
  InstanceState(
    mems: List(Dynamic),
    globals: Dict(String, Int),
    tables: List(Dynamic),
    dropped_data: Set(Int),
    dropped_elem: Set(Int),
    ref_globals: Dict(String, Dynamic),
    func_imports: List(Dynamic),
    /// NEW (M1). `None` under `js_profile: False` (inert; WASM path);
    /// `Some(store)` under `True`. rt_state imports rt_js_types (leaf) for
    /// the `JsStore(st)` type name and ties the knot at `st = InstanceState`
    /// (D17) — layering rule (:32-36) is satisfied because rt_js_types has
    /// zero rt_* dependencies (D8).
    js_store: Option(JsStore(InstanceState)),
  )
}
```
Accessors (rt_state.gleam, following `t_mem_at`/`t_with_mem_at` pattern `:560-577`):
```gleam
pub fn t_js_store(st: InstanceState) -> Option(JsStore(InstanceState))
pub fn t_with_js_store(st: InstanceState, js: JsStore(InstanceState)) -> InstanceState
```
`build_full` (`:711-725`) seeds `js_store: None`. Byte-identity note (`:106`): growing InstanceState changes NO emitted `.core` for WASM modules — accepted one-time record-arity bump. **No identity-coerce externs** (G7): the field is typed, so `store_of`/`coerce_from`/`coerce_to` FFI hacks are DELETED — every rt_js op that needs the store pattern-matches `Some(js)` and panics with `"js op on InstanceState with no JsStore"` on `None` (unreachable under `js_profile: True`).

### §2.2 `JsStore` — `2core/src/twocore/runtime/rt_js_store.gleam`

```gleam
import gleam/dict.{type Dict}
import gleam/set.{type Set}
import twocore/runtime/rt_js_types.{
  type JsSlot, type Handle, type JsOps, type HostHooks, type JobQueue,
}

pub opaque type JsStore(st) {
  JsStore(
    // ── cell arena (arc heap.gleam:21-45) ──
    data: Dict(Int, JsSlot),
    free: List(Int),                  // recycled ids, LIFO
    next: Int,                        // next never-used id (starts 0)
    pinned_roots: Set(Int),           // permanent GC roots: realm intrinsics + captured-binding cells
    // ── GC trigger (M2) ──
    alloc_since_gc: Int,              // bumped by t_cell_new; reset by t_collect
    gc_threshold: Int,                // default 65_536 (arc interpreter.gleam:5796)
    call_depth: Int,                  // ++ on t_call_checked entry, -- on exit; t_maybe_collect gate (D11)
    // ── threaded counters (D9, D14) ──
    prop_seq: Int,                    // Property creation-order stamp (replaces arc_vm_ffi:next_prop_seq)
    private_uid: Int,                 // t_new_private_name counter
    symbol_uid: Int,                  // UserSymbol id counter (replaces make_ref)
    // ── cycle-breaking upcalls + host (D17, G16) ──
    ops: JsOps(st),                   // fn-record: rt_js_val→rt_js_obj upcalls without an import cycle
    host_hooks: HostHooks,            // embedder capabilities (clock, rng, print, sleep); seeded by t_store_new
    // ── async (M8) ──
    microtasks: JobQueue,             // opaque Erlang :queue via twocore_rt_js_queue_ffi
    unhandled_rejections: List(Int),  // promise cell ids
    // ── test harness (M20) ──
    console_buf: List(BitArray),      // reversed; console.log appends here, NOT io:format
  )
}
```
`realm`/`global_object`/`symbol_registry` are **NOT** on `JsStore` (G18): a `JsStore` exists before any realm does. `t_store_new(hooks: HostHooks) -> JsStore` returns a realm-less store; `init_realm(store: JsStore) -> #(JsStore, Realm)` (M6) allocates the realm INTO the store and returns the `Realm` handle-record separately. Callers that need the realm hold it alongside `InstanceState` (M19 driver) or read it via `t_cell_get` on a pinned handle.

### §2.3 Value encoding (FROZEN — the ABI) — `rt_js_types.gleam` + `twocore_rt_js_val_ffi.erl`

**This table is the WIRE encoding** (D16) — the raw BEAM term shape that crosses FFI and lives in compiled `.core`. rt_js Gleam code NEVER pattern-matches these terms directly; it works with `JsValKind` (§2.4) obtained via `classify/1`. `twocore_rt_js_val_ffi:classify/1` is the ONE decode point — a ~15-clause Erlang function that reads this wire form and returns a `JsValKind` constructor. Changing a wire row means changing exactly `classify/1` + the corresponding `mk_*` encoder; nothing in rt_js Gleam recompiles.

| JS type | BEAM term | Discriminator (Erlang guard) |
|---|---|---|
| undefined | atom `undefined` | `V =:= undefined` |
| null | atom `null` | `V =:= null` |
| boolean | `true` \| `false` | `is_boolean(V)` |
| number (finite) | `integer()` \| `float()` | `is_number(V)` |
| number NaN/+∞/−∞ | atom `js_nan` \| `js_inf` \| `js_neg_inf` | `V =:= js_nan` etc. |
| string | UTF-8 `binary()` (D10) | `is_binary(V)` |
| bigint | `{js_bigint, integer()}` | `element(1,V) =:= js_bigint` |
| symbol | `{js_sym, SymbolId}` where `SymbolId` = Gleam wire of `rt_js_types.SymbolId` | `element(1,V) =:= js_sym` |
| object / function (D4) | `{js_cell, integer()}` | `element(1,V) =:= js_cell` |
| TDZ sentinel (internal) | atom `js_tdz` | never a JS value; every coercion → engine panic |

Symbol wire is uniform: position 2 is always the `SymbolId` sum-type's own wire form (`{well_known, Atom}` / `{user_symbol, Int, Desc}` / `{registered, Binary}`) — the encoder does NOT flatten well-known symbols to a bare atom. Well-known atom set: `sym_iterator`, `sym_async_iterator`, `sym_to_primitive`, `sym_to_string_tag`, `sym_has_instance`, `sym_is_concat_spreadable`, `sym_species`, `sym_match`, `sym_match_all`, `sym_replace`, `sym_search`, `sym_split`, `sym_unscopables`, `sym_dispose`, `sym_async_dispose` — mirrors arc `value.gleam:42-69` `WellKnown`.

### §2.4 `JsVal` + `JsSlot` + `ObjKind` — `rt_js_types.gleam` (D8, D16)

```gleam
// ── D16: opaque value + classify ──
/// Opaque JS value. Wire encoding per §2.3; Gleam code NEVER matches on the
/// term shape — it calls `classify` to get a `JsValKind`, and builds values
/// via the `mk_*` FFI constructors below.
pub type JsVal

pub type JsNum { JInt(Int)  JFloat(Float)  JNan  JPosInf  JNegInf }

/// The result of `classify(JsVal)`. Exactly one variant per §2.3 wire row.
pub type JsValKind {
  KUndef  KNull  KBool(Bool)  KNum(JsNum)  KStr(String)
  KBig(Int)  KSym(SymbolId)  KHandle(Handle)  KTdz
}

@external(erlang, "twocore_rt_js_val_ffi", "classify")
pub fn classify(v: JsVal) -> JsValKind
@external(erlang, "twocore_rt_js_val_ffi", "mk_undefined")
pub fn mk_undefined() -> JsVal
// … mk_null, mk_bool, mk_number(JsNum), mk_string, mk_bigint, mk_symbol,
//   mk_object(Handle), mk_tdz — one encoder per wire row.

// ── opaque BEAM-fun handles (G10) ──
/// Compiled JS function body: BEAM `fun(St, Frame, Args) -> {V, St'}` (D4/D5).
/// Opaque so Gleam cannot call it directly — invocation goes through
/// `t_call_checked` (M-CALL), which owns arity/frame/args marshalling.
pub type CompiledFn
/// Opaque Erlang `:queue.queue(Job)`. Constructed/drained via
/// `twocore_rt_js_queue_ffi` only (M8).
pub type JobQueue

pub type Handle { JsCell(id: Int) }   // wire term = {js_cell, Int} (R4)

pub type PropertyKey { Index(n: Int)  Named(name: String)  Private(text: BitArray) }
pub type ObjectKey   { StringKey(PropertyKey)  SymbolKey(SymbolId) }

pub type Property {
  DataProperty(value: JsVal, writable: Bool, enumerable: Bool, configurable: Bool, seq: Int)
  AccessorProperty(get: Option(JsVal), set: Option(JsVal), enumerable: Bool, configurable: Bool, seq: Int)
}

/// §6.2.6 Property Descriptor after `ToPropertyDescriptor` — every field
/// is `Option` (absent ≠ undefined). Consumed by `t_define_own_property`.
pub type ParsedDesc {
  ParsedDesc(value: Option(JsVal), get: Option(JsVal), set: Option(JsVal),
             writable: Option(Bool), enumerable: Option(Bool), configurable: Option(Bool))
}

pub type JsElements { NoElements  Dense(TreeArray(JsVal))  Sparse(Dict(Int, JsVal)) }

pub type FnFlags {
  FnFlags(is_constructor: Bool, is_class_constructor: Bool, is_derived_constructor: Bool,
          is_arrow: Bool, is_method: Bool, is_generator: Bool, is_async: Bool)
}

/// D17: rt_js_val (leaf) needs to call rt_js_obj.get_prop / rt_js_call.call
/// for `ToPrimitive`/`OrdinaryToPrimitive`, but importing them is a cycle.
/// Type-parameterized over the threaded state so rt_js_types stays a LEAF
/// (no rt_state import). rt_state ties the knot: `pub type JsOpsC =
/// JsOps(InstanceState)`. M6 `init_realm` seeds `store.ops` once with the
/// concrete M4/M-CALL fns; rt_js_val calls `store.ops.get_prop(st, recv, key)`.
pub type JsOps(st) {
  JsOps(
    get_prop:    fn(st, JsVal, ObjectKey) -> #(JsVal, st),
    call:        fn(st, JsVal, JsVal, List(JsVal)) -> #(JsVal, st),
    to_object:   fn(st, JsVal) -> #(Handle, st),
    new_error:   fn(st, ErrorKind, String) -> #(JsVal, st),
    eval_hook:   fn(st, String) -> #(JsVal, st),   // indirect eval (M19)
  )
}

/// Embedder capabilities. Seeded once into `JsStore.host_hooks` by
/// `t_store_new(hooks)`; read by Date/Math/console/performance natives (M6).
/// Port of arc `host_hooks.gleam:141-187` reduced to the deterministic-harness set.
pub type HostHooks {
  HostHooks(
    monotonic_now: fn() -> Int,       // ms; Date.now, performance.now
    random:        fn() -> Float,     // Math.random; harness seeds a PRNG
    sleep_ms:      fn(Int) -> Nil,    // Atomics.wait, timers (v2)
    print:         fn(String) -> Nil, // console.* sink (harness → console_buf)
  )
}

pub type ToPrimHint  { HintDefault  HintString  HintNumber }
pub type IterHint    { IterSync  IterAsync }
pub type ArrayIterKind { ArrayIterKeys  ArrayIterValues  ArrayIterEntries }
pub type MapIterKind   { MapIterKeys  MapIterValues  MapIterEntries }
pub type SetIterKind   { SetIterValues  SetIterEntries }
pub type MethodInstallKind { MIMethod  MIGetter  MISetter  MIStatic  MIStaticGetter  MIStaticSetter }   // R11

/// R10/G20: `KNative` dispatch key. NOT `Int`. Full constructor set is
/// enumerated by M6 (~180 variants, one per built-in native); the type
/// lives here so `ObjKind` can reference it. Stub shown; M6 owns the body.
pub type NativeToken { /* ObjectCreate | ArrayIsArray | MathFloor | … */ }

pub type TypedArrayKind {
  Int8  Uint8  Uint8Clamped  Int16  Uint16  Int32  Uint32
  Float32  Float64  BigInt64  BigUint64
}
/// Realm's typed-array constructor/prototype pairs, indexed by kind.
pub type TypedArrays { TypedArrays(by_kind: Dict(TypedArrayKind, BuiltinPair)) }

/// SameValueZero-normalized Map/Set key (arc `value.gleam:967-1027`):
/// -0 → +0, NaN equals NaN, objects by Handle identity.
pub type MapKey {
  MKString(String)  MKNumber(Float)  MKNan  MKInfinity  MKNegInfinity
  MKBool(Bool)  MKNull  MKUndefined  MKObject(Handle)  MKSymbol(SymbolId)  MKBigInt(Int)
}

pub type ObjKind {
  Ordinary
  ArrayObj(length: Int)
  ArgumentsObj(length: Int, mapped: Option(List(Handle)))   // mapped-mode param cells
  StringObj(value: String)  NumberObj(value: JsNum)  BooleanObj(value: Bool)
  BigIntObj(value: Int)  SymbolObj(value: SymbolId)
  KFunction(
    code: CompiledFn,                 // opaque BEAM fun/3 (D4, D5, G10)
    home_object: Option(Handle),      // [[HomeObject]] — set only by t_make_method
    flags: FnFlags,
    fields_init: Option(Handle),      // [[Fields]] — instance-initializer closure (M7/M16)
    captures: List(Handle),           // GC-trace: cells captured via MakeClosure (I8)
  )
  KNative(tag: NativeToken, name: String, length: Int, constructible: Bool)   // R10
  KBound(target: Handle, bound_this: JsVal, bound_args: List(JsVal))
  ErrorObj(stack: String)
  MapObj(entries: OrderedEntries(MapKey, JsVal))            // arc ordered_entries — vendored (§10)
  SetObj(entries: OrderedEntries(MapKey, JsVal))            // value = key round-tripped
  WeakMapObj(entries: Dict(Int, JsVal))                     // key = handle id; NOT a strong ref (M2 §weak)
  WeakSetObj(entries: Set(Int))
  DateObj(ms: JsNum)  RegExpObj(source: String, flags: String, last_index: Int, compiled: CompiledRegExp)
  ArrayBufferObj(bytes: BitArray, detached: Bool)
  TypedArrayObj(buffer: Handle, offset: Int, len: Int, kind: TypedArrayKind)
  DataViewObj(buffer: Handle, offset: Int, len: Int)
  ModuleNamespace(exports: Dict(String, JsVal))
  ProxyObj(target: Handle, handler: Handle, revoked: Bool)
  ForInIterator(remaining: List(String))
  ArrayIterator(target: Handle, index: Int, kind: ArrayIterKind)
  MapIterator(target: Handle, index: Int, kind: MapIterKind)
  SetIterator(target: Handle, index: Int, kind: SetIterKind)
  StringIterator(source: String, index: Int)
  AsyncFromSyncIterator(sync_rec: Handle)
}

pub type JsSlot {
  SObject(kind: ObjKind, proto: Option(Handle), props: Dict(PropertyKey, Property),
          symbol_props: List(#(SymbolId, Property)), elements: JsElements, extensible: Bool)
  SBox(value: JsVal)                                        // captured mutable binding cell
  SPromise(state: PromiseState, is_handled: Bool)
  SGenerator(state: GeneratorState, resume: CompiledFn, gen_cell: Handle)
  SAsyncGen(state: AsyncGenState, resume: CompiledFn,
            queue: #(List(AsyncGenRequest), List(AsyncGenRequest)), gen_cell: Handle)
}

pub type PromiseState { PromisePending(reactions: List(PromiseReaction))  PromiseFulfilled(JsVal)  PromiseRejected(JsVal) }
pub type PromiseReaction { PromiseReaction(on_fulfill: ReactionHandler, on_reject: ReactionHandler, child_resolve: JsVal, child_reject: JsVal) }
pub type ReactionHandler { Handler(fun: JsVal)  IdentityPassThrough  ThrowerPassThrough }
pub type Job { ReactionJob(ReactionHandler, arg: JsVal, resolve: JsVal, reject: JsVal)  ResolveThenableJob(thenable: JsVal, then_fn: JsVal, resolve: JsVal, reject: JsVal) }
pub type AsyncGenRequest { AsyncGenRequest(completion: GeneratorCompletion, value: JsVal, resolve: JsVal, reject: JsVal) }
pub type GeneratorCompletion { GenNext  GenReturn  GenThrow }
pub type GeneratorState { GenSuspendedStart  GenSuspendedYield  GenExecuting  GenCompleted }
pub type AsyncGenState  { AGSuspendedStart  AGSuspendedYield  AGExecuting  AGAwaitingReturn  AGCompleted }
```
`IteratorRecord` is **not a struct here** — `t_get_iterator` allocates a cell and returns a `Handle`; the record's `[[Iterator]]`/`[[NextMethod]]`/`[[Done]]` live as three own-props on that cell's `SObject` (M4). `CompiledRegExp` is an opaque handle to the vendored regex engine's compiled pattern (§10).

### §2.5 `Realm` — `rt_js_types.gleam`

```gleam
pub type BuiltinPair { BuiltinPair(prototype: Handle, constructor: Handle) }

pub type Realm {
  Realm(
    object: BuiltinPair, function: BuiltinPair, array: BuiltinPair,
    string: BuiltinPair, number: BuiltinPair, boolean: BuiltinPair, symbol: BuiltinPair, bigint: BuiltinPair,
    error: BuiltinPair, type_error: BuiltinPair, reference_error: BuiltinPair, range_error: BuiltinPair,
    syntax_error: BuiltinPair, eval_error: BuiltinPair, uri_error: BuiltinPair, aggregate_error: BuiltinPair,
    map: BuiltinPair, set: BuiltinPair, weak_map: BuiltinPair, weak_set: BuiltinPair,
    date: BuiltinPair, regexp: BuiltinPair, promise: BuiltinPair, proxy: BuiltinPair,
    array_buffer: BuiltinPair, data_view: BuiltinPair, typed_arrays: TypedArrays,
    math: Handle, json: Handle, reflect: Handle, console: Handle, atomics: Handle,
    iterator_proto: Handle, array_iter_proto: Handle, string_iter_proto: Handle,
    map_iter_proto: Handle, set_iter_proto: Handle, async_iterator_proto: Handle,
    generator: BuiltinPair, generator_fn: BuiltinPair, async_fn: BuiltinPair, async_gen: BuiltinPair,
    throw_type_error: Handle,
    global_object: Handle,
  )
}
```
Derived from `arc/src/arc/vm/builtins/common.gleam:197-274`. **Invariant:** `init_realm` is deterministic — same handle ids every run (enables snapshot testing).

---

## §3 IR Calling Convention (D1+D2+D3+D5 combined)

### §3.1 Compiled JS function shape

**IR level** (what `emit_2core` produces):
```
ir.Function(
  name:   "jsf_<N>",
  params: [Local("cap_0",TTerm), …, Local("cap_{m-1}",TTerm),
           Local("_frame",TTerm), Local("_args",TTerm)],   // m+2 params
  result: [TTerm],                                          // single value V
  locals: [], body: <ANF Expr>
)
```

**BEAM level** (what emit_core produces under `Threaded` + `js_profile:True` — D18):
```erlang
'jsf_N'/(m+3) = fun(St, Cap_0, ..., Cap_{m-1}, Frame, Args) -> {V, St'}
```
emit_core prepends `St` (`emit_core.gleam:641-652`), wraps return in `{Pkg, St'}` (`:1559`).

### §3.2 Closure creation — `MakeClosure` (M9 change)

**IR:** `MakeClosure("jsf_N", [cap_val_0, …, cap_val_{m-1}], arity: 2)`

**emit_core** (`emit_core.gleam:1493-1523` REWRITE under `Threading(_)` ∧ `js_profile`):
```erlang
fun(St, Frame, Args) -> apply 'jsf_N'/(m+3)(St, Cap_0, ..., Cap_{m-1}, Frame, Args) end
```
Closure runtime arity = **3**. St is a RUNTIME arg, NOT captured.

### §3.3 Closure invocation — `CallClosure` (M9 change)

**IR:** `CallClosure(callee_val, [frame_val, args_val])`

**emit_core** (`emit_core.gleam:1536-1546` REWRITE): under `Threading(cur)`, emit
```erlang
case apply Callee(cur, Frame, Args) of {V, St'} -> <cont under Threading(St')> end
```
via existing `emit_value_state_pair` (`:1919-1940`).

### §3.4 rt_js op invocation — `CallHost("js", op, args)` (M9 change)

**IR:** `CallHost("js", "get_prop", [recv, key])` — NO St arg.

**emit_core** (`emit_core.gleam:3549-3567` REWRITE): `resolve_js(op) -> #(module_atom, fn_name, JsOpKind)`; under `Threading(cur)` ∧ kind≠`JPure`:
```erlang
case call '<module>':'t_<op>'(cur, Recv, Key) of {V, St'} -> <cont under Threading(St')> end
```
Pure ops (`type_of`, `to_boolean`, `strict_eq`, `empty_list`, sentinels): unchanged single-return, `sc` unchanged.

### §3.5 The `Frame` tuple (D5)

`Frame = {ThisVal, ActiveFunc, HomeObject, NewTarget}` — plain Erlang 4-tuple. Built by `rt_js_call.t_call`/`t_construct` (§7.M-CALL); read by function prologue via `TermOp(TupleGet(i), [Var("_frame")])` with **0-based** indices: `this=0`, `active_func=1`, `home_object=2`, `new_target=3` (R7).

| Call kind | Frame construction |
|---|---|
| `f(args)` (regular) | `{recv_or_undef, callee_h, undefined, undefined}` |
| `new F(args)` | `{new_instance_h, callee_h, undefined, new_target_h}` |
| `super.m(args)` | `{this_val, method_h, home_proto_h, undefined}` |
| `super(args)` (ctor) | `{js_tdz, ctor_h, undefined, new_target_h}` — this bound after |
| Arrow | Frame tuple CAPTURED from enclosing non-arrow (or components per `lexical_captures`) |

---

## §4 Exception ABI (D6+D7)

| Aspect | Spec | Source ref |
|---|---|---|
| Tag decl (IR) | `ir.Module.tags = [TagDecl("js_exn", [TTerm])]` — ONE tag ⇒ module-local index `0` | `ir.gleam:125-127`; M19 emits it |
| BEAM wire term | `{wasm_exn, 0, [St, V]}` at ERROR class via `erlang:error/1` | `twocore_rt_exn_ffi.erl:43` shape reused |
| IR `throw v` | `ir.Throw("js_exn", [v_val])` | `ir.gleam:848` |
| emit_core `emit_throw` (M9) | Under `Threading(cur)`: prepend `CVar(cur)` → `throw_exn(0, [St, V])` | `emit_core.gleam:5097-5106` patched |
| IR catch | `CatchHandler(OnTag("js_exn"), ["_e"], None, handler_ir)` | `ir.gleam:991-998` |
| emit_core `emit_catch_clause` (M9) | Under `Threading(_)`: prepend fresh `"_st_c"` to payload pattern; emit handler under `Threading("_st_c")` | `emit_core.gleam:5226-5280` patched |
| rt_js raise | `twocore_rt_js_store_ffi:t_throw(St, ErrVal) -> erlang:error({wasm_exn, 0, [St, ErrVal]})` | new; replaces `twocore_rt_js_ffi.erl:75` |
| Non-JS errors | `match_tag` rejects → `try_dispatch` re-raises via `rt_exn:reraise` | `emit_core.gleam:5200-5211`; T7 sandbox floor preserved |
| `finally` | Barrier-duplication: no IR `after`/gosub; every Break/Continue/Return crossing a `Barrier2` INLINES the finally AST before the transfer; throw path = inner `Try(OnTag)` that runs finally then rethrows | arc `emit.gleam:2287-2510` `BarrierFrame`; §7.M17 |

**Payload order is `[St, V]` — St FIRST** (R2). Matches the arg-1-everywhere convention (St is always the first runtime arg); `t_call_protected` (`emit_core.gleam:627`) already unpacks in this order. Any draft showing `[V, St]` is STALE.

---

## §5 Module Dependency DAG

```mermaid
graph TD
  host[rt_js_host] --> store[rt_js_store M1b]
  types[rt_js_types M1a] --> store
  types --> gc[rt_js_gc M2]
  types --> val[rt_js_val M3]
  types --> inspect[rt_js_inspect]
  store --> inspect
  types --> obj[rt_js_obj M4]
  types --> ops[rt_js_ops M5]
  types --> class[rt_js_class M7]
  types --> async[rt_js_async M8]
  types --> call[rt_js_call M-CALL]
  store --> gc
  store --> obj
  store --> call
  val --> ops
  val --> obj
  obj --> call
  obj --> class
  obj --> builtins[rt_js_builtins M6]
  call --> builtins
  call --> async
  ops --> builtins
  class --> builtins
  async --> builtins
  gc --> builtins
  store --> M9[emit_core patches M9]
  builtins --> M9

  scope[arc/scope reused] --> state[emit_2core/state M10]
  state --> anf[emit_2core/anf M11]
  anf --> expr[expr M12]
  anf --> stmt[stmt M13]
  anf --> func[func M14]
  anf --> destr[destructure M15]
  anf --> klass[class M16]
  anf --> exn[exn M17]
  anf --> asyncE[async M18]
  expr -.EmitDispatch.-> stmt
  stmt -.EmitDispatch.-> expr
  func --> asyncE
  exn --> stmt
  state --> module[module M19]
  expr --> module
  stmt --> module
  func --> module
  M9 --> module
  builtins --> module
  module --> harness[test harness M20]
```

**Leaf modules (no rt_js deps):** `rt_js_types`, `rt_js_host` (HostHooks type only), arc `scope`/`parser`/`ast`/`ast_util` (reused verbatim).
**M-CALL** = `rt_js_call.gleam` (new module owning `t_call`/`t_construct`/`t_throw` — resolved from open questions in M4/M6/M8).
**Cycle-break (D17):** `rt_js_val` does NOT import `rt_js_obj` — the M3→M4 back-edge (`t_to_primitive` needing `get_prop`/`call`) is broken via the `JsOps` fn-record on `JsStore`, seeded by `init_realm` (M6). The prior `@external`-by-Erlang-name hack is REMOVED.
**rt_js_inspect** = `console.log`/`util.inspect` stringifier; deps `rt_js_types`+`rt_js_store` only (reads slots, no mutation).

---

## §6 Build-Order Plan

| Wave | Modules | Blocks on | Parallelizable within wave |
|---|---|---|---|
| **A0** | `arc/gleam.toml` + `twocore` dep; `rt_state.gleam` +js_store field; `instance.gleam` +Binding fields | — | trivial edits, one PR |
| **A** | M1a `rt_js_types`, M1b `rt_js_store`, M3 `rt_js_val`, M10 `state`, M11 `anf`, §10 FFI copies | A0 | 5 parallel (types must land first within wave; ~1hr stagger) |
| **B** | M2 `rt_js_gc`, M4 `rt_js_obj`, M5 `rt_js_ops`, M-CALL `rt_js_call`, M7 `rt_js_class`, M8 `rt_js_async`, M9 `emit_core` patches, M12 `expr`, M13 `stmt`, M14 `func`, M15 `destructure`, M17 `exn` | A | 12 parallel |
| **C** | M6 `rt_js_builtins` (largest surface), M16 `class`, M18 `async`, M19 `module`, `profiles.js_direct()` | B | 4 parallel (M6 is critical path — start first) |
| **D** | M20 differential + test262 harness | C | 1 |

**Critical path:** A0 → M1a → M1b → M4 → M-CALL → M6 → M19 → M20. M6 is the long pole (~400 native methods).

---

## §7 Per-Module Specifications

### M1a — `rt_js_types.gleam` (LEAF)

**File:** `2core/src/twocore/runtime/rt_js_types.gleam`
**Deps:** `gleam/dict`, `gleam/option` only. NO `gleam/dynamic` — every value field is `JsVal` (D16).
**Content:** Every type in §2.2-§2.5 — the complete closed set:
- **Value ABI (D16):** `JsVal` (opaque — wraps the §2.3 wire term), `JsValKind` (the ~15-arm sum `classify/1` returns), `JsNum` (`JInt(Int) | JFloat(Float) | JNan | JPosInf | JNegInf`).
- **Store (D8 — moved here from M1b so `JsOps` can name it):** `JsStore`, `JsOps` (fn-record breaking the val↔obj↔call cycle, D17), `HostHooks` (`monotonic_now`, `random`, `sleep_ms`, `print` — port field list from `arc/src/arc/vm/host_hooks.gleam:142+`), `JobQueue` (opaque), `CompiledFn` (opaque wrapper over the BEAM `fun/3` in `KFunction.code`/`SGenerator.resume`).
- **Heap:** `Handle`, `JsSlot`, `ObjKind`, `FnFlags`, `JsElements`, `Property`, `ParsedDesc`, `PropertyKey`, `SymbolId`, `WellKnown`, `ObjectKey`.
- **Collections:** `MapKey` (SameValueZero-normalized key for `MapObj`/`SetObj` — port `arc/src/arc/vm/value.gleam:967-1027`), `OrderedEntries(k, v)` (vendored from `arc/internal/ordered_entries.gleam`), `ArrayIterKind`/`MapIterKind`/`SetIterKind` sums (replaces raw `kind: Int`).
- **Coercion hints:** `ToPrimHint` (`HintDefault | HintString | HintNumber`), `IterHint` (`IterSync | IterAsync`).
- **Class/realm:** `BuiltinPair`, `Realm`, `TypedArrays`, `MethodInstallKind` (`MIMethod | MIGetter | MISetter | MIStatic | MIStaticGetter | MIStaticSetter`), `NativeToken` (sum type — one variant per M6 native; NOT `Int`), `IteratorRecord` (stored in a cell — see M4).
- **Async:** `PromiseState`, `PromiseReaction`, `ReactionHandler`, `Job`, `GeneratorState`, `AsyncGenState`.

Plus pure helpers ported from arc leaves:
- `canonical_key(String) -> PropertyKey`, `index_key(Int) -> PropertyKey`, `key_to_text(PropertyKey) -> String`, `is_private_key(PropertyKey) -> Bool` — `arc/src/arc/vm/key.gleam:62-201`
- 15 well-known-symbol consts + `symbol_description/1` — `arc/src/arc/vm/value.gleam:81-168`
- `prop_enumerable/prop_configurable/prop_seq/with_seq_of` — `value.gleam:3814-3890`

**Canonical names (PINNED — every M-file uses these spellings, no aliases):** `JsSlot` (not JsCell), `ObjKind` (not ExoticKind), `KFunction`/`KNative`/`KBound` (K-prefix), `SObject`/`SBox`/`SPromise`/`SGenerator`/`SAsyncGen` (S-prefix). The `Handle` constructor stays `JsCell(id: Int)` for wire-compat with `{js_cell, N}`.

**Invariant:** ZERO imports from other `rt_js_*` modules. Adding an `ObjKind` or `JsSlot` variant here is a compile error in M2's `refs_in_cell` (no wildcard).

---

### M1b — `rt_js_store.gleam`

**File:** `2core/src/twocore/runtime/rt_js_store.gleam` + `2core/src/twocore_rt_js_store_ffi.erl`
**Deps:** `rt_js_types`, `rt_state`, `gleam/dict`, `gleam/set`.
**Source ports:** `arc/src/arc/vm/heap.gleam:21-45,115-130,186-400`.

```gleam
// JsStore TYPE lives in rt_js_types (M1a). This module owns construction + cell ops.

// ── construction ──
pub fn t_store_new(hooks: HostHooks) -> JsStore
    // Empty store — NO realm, NO global_object (those live on Realm, populated by M6's init_realm).
    // Seeds: data={}, free=[], next=0, pinned_roots=∅, alloc_since_gc=0, gc_threshold=65_536,
    //        prop_seq=0, private_uid=0, symbol_uid=0, ops=<unseeded — M6 step 1 fills>,
    //        host_hooks=hooks, call_depth=0, microtasks=jq_new(), unhandled_rejections=[], console_buf=[].

// ── cell ops (arc heap.gleam:115-400) ──
pub fn t_cell_new(st: InstanceState, slot: JsSlot) -> #(Handle, InstanceState)
    // pop free-list else bump next; ++alloc_since_gc. NEVER collects (D11) — allocation is O(1) and pure.
pub fn t_cell_get(st: InstanceState, h: Handle) -> JsSlot            // fail-closed panic on bad id
pub fn t_cell_set(st: InstanceState, h: Handle, slot: JsSlot) -> InstanceState
pub fn t_cell_update(st: InstanceState, h: Handle, f: fn(JsSlot) -> JsSlot) -> InstanceState
pub fn t_cell_free(st: InstanceState, h: Handle) -> InstanceState    // push id → free-list
pub fn t_pin_root(st: InstanceState, h: Handle) -> InstanceState     // add to pinned_roots (closures, realm)

// ── threaded counters (D9, D14) ──
pub fn t_next_prop_seq(st: InstanceState) -> #(Int, InstanceState)
pub fn t_next_private_uid(st: InstanceState) -> #(Int, InstanceState)
pub fn t_next_symbol_uid(st: InstanceState) -> #(Int, InstanceState)

// ── call-depth (D11 gate) ──
pub fn t_enter_call(st: InstanceState) -> InstanceState              // ++call_depth
pub fn t_leave_call(st: InstanceState) -> InstanceState              // --call_depth

// ── console (M20) ──
pub fn t_console_write(st: InstanceState, line: BitArray) -> InstanceState
pub fn t_console_read(st: InstanceState) -> List(BitArray)
pub fn t_console_bytes(st: InstanceState) -> BitArray

// ── exception (D7) ──
@external(erlang, "twocore_rt_js_store_ffi", "t_throw")
pub fn t_throw(st: InstanceState, err_val: JsVal) -> a               // erlang:error({wasm_exn,0,[St,Err]})
```

**DELETED (G7):** `store_of`/`coerce_from`/`coerce_to` identity-extern hack — `InstanceState.js_store` is now `Option(JsStore)` typed, so plain field access. **DELETED:** `null_realm`/`handle_invalid` sentinels — no half-initialized JsStore is ever observable; `t_store_new` returns a fully-formed store with `ops` unseeded, and `init_realm` is called before any user code runs.

**FFI** (`twocore_rt_js_store_ffi.erl`):
```erlang
-export([t_throw/2, is_handle/1, handle_id/1]).
t_throw(St, V) -> erlang:error({wasm_exn, 0, [St, V]}).
is_handle({js_cell, N}) when is_integer(N) -> true; is_handle(_) -> false.
handle_id({js_cell, N}) -> N.
```

**Invariants:** Handle ids never reused while live; free-list ∩ live = ∅; JsStore is a pure Gleam value (no pdict, no atomics, no ETS); `t_cell_new` NEVER collects (D11).

---

### M2 — `rt_js_gc.gleam`

**File:** `2core/src/twocore/runtime/rt_js_gc.gleam` + `2core/src/twocore_rt_js_gc_ffi.erl`
**Deps:** `rt_js_types`, `rt_js_store`, `rt_state`.
**Source ports:** `arc/src/arc/vm/heap.gleam:442-563` (`collect_with_roots`/`mark_from`/`mark_loop`/`sweep`), `arc/src/arc/vm/gc_trace.gleam:61-540` (`refs_in_slot`/`push_value_ref`), `arc/src/arc/vm/state.gleam:216-313` (`reachable_root_refs` exhaustive-destructure discipline).

```gleam
pub fn t_maybe_collect(st: InstanceState) -> InstanceState
    // TURN-BOUNDARY ONLY (D11). Gate: store.call_depth == 0 AND alloc_since_gc >= gc_threshold → t_collect(st, []); else st.
    // Called from exactly two places: M8 t_drain_microtasks (between jobs) and M19 js_main (after top-level return).
    // NEVER emitted at fn prologues. Matches arc interpreter.gleam:5806-5827 call_stack==[] gate.
pub fn t_collect(st: InstanceState, extra_roots: List(Handle)) -> InstanceState
    // Explicit host entrypoint (globalThis.gc(), test harness). extra_roots reserved for v2 mid-turn escape hatch.
    // roots = roots_of_state(st) ∪ extra_roots; mark_from(store, roots); sweep → free-list; alloc_since_gc := 0
pub fn roots_of_state(st: InstanceState) -> List(Int)
    // = pinned_roots ∪ realm.* handles ∪ global_object ∪ refs_in_term(microtasks) ∪ refs_in_term(unhandled_rejections)
pub fn refs_in_cell(slot: JsSlot) -> List(Int)
    // EXHAUSTIVE match on JsSlot + ObjKind, NO wildcard arm (arc gc_trace.gleam:5-9 discipline).
    // Adding a JsSlot/ObjKind variant in M1a MUST be a compile error here.
```

**FFI** (`twocore_rt_js_gc_ffi.erl`) — the ONE Erlang piece Gleam cannot express:
```erlang
-export([refs_in_term/1, refs_in_fun_env/1]).
%% Deep walk: any Dynamic → List(Int) of every {js_cell,N} reachable.
refs_in_term(V) -> refs_in_term(V, []).
refs_in_term({js_cell, N}, Acc) -> [N | Acc];
refs_in_term(T, Acc) when is_tuple(T) -> lists:foldl(fun refs_in_term/2, Acc, tuple_to_list(T));
refs_in_term(L, Acc) when is_list(L) -> lists:foldl(fun refs_in_term/2, Acc, L);
refs_in_term(M, Acc) when is_map(M) -> maps:fold(fun(K,V,A) -> refs_in_term(V, refs_in_term(K, A)) end, Acc, M);
refs_in_term(F, Acc) when is_function(F) -> {env, Env} = erlang:fun_info(F, env), lists:foldl(fun refs_in_term/2, Acc, Env);
refs_in_term(_, Acc) -> Acc.
```

**`refs_in_cell` per-variant trace rules** (exhaustive; every Handle-carrying field):

| Variant | Handles traced |
|---|---|
| `SObject(kind,proto,props,syms,elems,_)` | `proto` + `refs_in_objkind(kind)` + every `Property.value/get/set` via `refs_in_term` + every element via `refs_in_term` |
| `SBox(v)` | `refs_in_term(v)` |
| `SPromise(state,_)` | `Pending(rs)` → each reaction's `on_*`/`child_*` via `refs_in_term`; `Fulfilled(v)`/`Rejected(v)` → `refs_in_term(v)` |
| `SGenerator(_,resume,gen_cell)` | `[gen_cell.id]` + `refs_in_fun_env(resume)` |
| `SAsyncGen(_,resume,queue,gen_cell)` | `[gen_cell.id]` + `refs_in_fun_env(resume)` + `refs_in_term(queue)` |
| `ObjKind.KFunction(code,home,_,fi,caps)` | `home` + `fi` + `caps` + `refs_in_fun_env(code)` |
| `ObjKind.KBound(t,bt,ba)` | `[t.id]` + `refs_in_term(bt)` + `refs_in_term(ba)` |
| `ObjKind.MapObj(es)` / `SetObj(es)` | `refs_in_term(es)` |
| `ObjKind.WeakMapObj(_)` / `WeakSetObj(_)` | **NONE** — weak refs are NOT roots (M2 §weak: post-sweep, prune dead keys) |
| `ObjKind.TypedArrayObj(buf,…)` / `DataViewObj(buf,…)` | `[buf.id]` |
| `ObjKind.ProxyObj(t,h,_)` | `[t.id, h.id]` |
| `ObjKind.*Iterator(target,…)` | `[target.id]` |
| others | `[]` |

**Weak semantics:** After `sweep`, iterate `WeakMapObj`/`WeakSetObj` cells: remove entries whose key-id ∉ live set. `WeakRef.deref` = `t_is_live(st, h) -> Bool` (id ∈ store.data).

**Dropped from arc:** `lazy_proto` handling (`heap.gleam:513-530`) — protos are eagerly-allocated real cells; `compact` (`heap.gleam:453-460`) — handles are stable, sweep-to-free-list only.

**Invariants:** After collect, every id in `store.data` is reachable from `roots_of_state`; `free` = swept ids; NO id renumbering.

---

### M3 — `rt_js_val.gleam`

**File:** `2core/src/twocore/runtime/rt_js_val.gleam` + `2core/src/twocore_rt_js_val_ffi.erl`
**Deps:** `rt_js_types`, `rt_state` only. NO import of `rt_js_obj`/`rt_js_call` — cycle broken via `store.ops` (D17).
**Source ports:** `arc/src/arc/vm/value.gleam:464+` primitives, `arc/src/arc/vm/ops/coerce.gleam`, `arc/src/arc/vm/key.gleam` ToPropertyKey; upgrades `twocore_rt_js_ffi.erl:19-468`.

**`classify/1` — the ONE decode point (D16):**
```gleam
@external(erlang, "twocore_rt_js_val_ffi", "classify")
pub fn classify(v: JsVal) -> JsValKind
```
FFI is ~15 head clauses over the §2.3 wire encoding:
```erlang
classify(undefined) -> k_undef;
classify(null) -> k_null;
classify(true) -> {k_bool, true};             classify(false) -> {k_bool, false};
classify(N) when is_integer(N) -> {k_num, {j_int, N}};
classify(N) when is_float(N) -> {k_num, {j_float, N}};
classify(js_nan) -> {k_num, j_nan};
classify(js_inf) -> {k_num, j_pos_inf};       classify(js_neg_inf) -> {k_num, j_neg_inf};
classify(B) when is_binary(B) -> {k_str, B};
classify({js_bigint, N}) -> {k_big, N};
classify({js_sym, S}) -> {k_sym, S};
classify({js_cell, N}) -> {k_handle, {js_cell, N}};
classify(js_tdz) -> k_tdz.
```
Every `rt_js_*` Gleam function pattern-matches on `JsValKind`, never on the wire term.

**Pure predicates (no St):**
```gleam
pub fn to_boolean(v: JsVal) -> Bool            // total, pure (§7.1.2) — via classify
pub fn same_value(a: JsVal, b: JsVal) -> Bool  / same_value_zero(a, b) -> Bool
pub fn prim_to_number(v: JsVal) -> Result(JsNum, CoerceError)
    // Primitive-only ToNumber. Object → Error(NeedsToPrimitive). Symbol → Error(SymbolToNumber).
    // String parse via parse_float FFI. Callers that hit NeedsToPrimitive route through t_to_number.
pub fn to_int32(n: JsNum) -> Int  / to_uint32(n: JsNum) -> Int   // arc/internal/int_math.gleam wrap
```

**Threaded coercions** — call user @@toPrimitive/valueOf/toString via `store.ops.get_prop` + `store.ops.call` (D17 — NOT `@external` to a mangled Gleam module name):
```gleam
pub fn t_to_primitive(st, v: JsVal, hint: ToPrimHint) -> #(JsVal, InstanceState)
pub fn t_to_number(st, v: JsVal) -> #(JsNum, InstanceState)
pub fn t_to_numeric(st, v: JsVal) -> #(JsVal, InstanceState)            // number|bigint (§7.1.3)
pub fn t_to_string(st, v: JsVal) -> #(BitArray, InstanceState)
pub fn t_to_property_key(st, v: JsVal) -> #(ObjectKey, InstanceState)
pub fn t_to_object(st, v: JsVal) -> #(Handle, InstanceState)            // primitive-box via realm; throw on null/undef
pub fn t_to_int32(st, v: JsVal) -> #(Int, InstanceState)  / t_to_uint32 / t_to_length / t_to_integer_or_infinity
pub fn t_require_object_coercible(st, v: JsVal) -> #(JsVal, InstanceState)
```

**Threaded type_of** (D4 — reads cell to distinguish function):
```gleam
pub fn t_type_of(st, v: JsVal) -> #(BitArray, InstanceState)
    // KHandle(h) → read ObjKind: KFunction|KNative|KBound|ProxyObj(callable)→"function", else "object"
pub fn t_is_callable(st, v: JsVal) -> #(Bool, InstanceState)
```

**Error helpers** (D7):
```gleam
pub fn t_throw_type_error(st, msg: BitArray) -> a       // alloc TypeError via store.ops.new_error, then rt_js_store.t_throw
pub fn t_throw_range_error(st, msg) -> a  / t_throw_reference_error / t_throw_syntax_error
```

**String encoding note (D10):** UTF-8 `binary()`, codepoint-indexed. Ill-formed byte sequences on the string→codepoint path are replaced with U+FFFD (REPLACEMENT CHARACTER) — NOT WTF-8, NOT lone-surrogate passthrough.

**FFI ports (§10):** `twocore_rt_js_val_ffi.erl` (`classify/1`), `twocore_rt_js_number_ffi.erl` (`js_number_to_string/1` = §7.1.12.1), `twocore_rt_js_string_ffi.erl` (codepoint length/charAt/indexOf), `twocore_rt_js_float_ffi.erl` (`parse_float/1` for ToNumber(string)).

**Invariants:** `to_boolean` total+pure; every threaded coercion returns updated St even on pure path; matches arc-interpreter output byte-for-byte (D10).

---

### M4 — `rt_js_obj.gleam`

**File:** `2core/src/twocore/runtime/rt_js_obj.gleam`
**Deps:** `rt_js_types`, `rt_js_store`, `rt_js_val`. NO import of `rt_js_call` (would cycle: `rt_js_call` already imports `rt_js_obj`) — accessor getter/setter and @@iterator invocation reach `t_call_checked` via `store.ops.call` (D17).
**Source ports:** `arc/src/arc/vm/ops/object.gleam:107-262` OrdinaryGet, `arc/src/arc/vm/ops/mop.gleam` [[DefineOwnProperty]]/[[GetOwnProperty]], `arc/src/arc/vm/internal/elements.gleam:1-373`.

```gleam
// ── allocation ──
pub fn t_new_object(st, proto: Option(Handle)) -> #(Handle, InstanceState)
pub fn t_new_array(st, elems: List(JsVal)) -> #(Handle, InstanceState)
pub fn t_new_error(st, ctor: Handle, msg: BitArray) -> #(Handle, InstanceState)
pub fn t_new_function(st, code: CompiledFn, flags: FnFlags, name: BitArray, len: Int, captures: List(Handle)) -> #(Handle, InstanceState)
    // allocs KFunction cell + non-enumerable name/length props + writable .prototype (if is_constructor)

// ── property MOP (§10.1) ──
pub fn t_get_prop(st, recv: JsVal, key: ObjectKey) -> #(JsVal, InstanceState)
    // OrdinaryGet: primitive→auto-box via realm; proto walk; Accessor→ store.ops.call(st, getter, recv, []) (D17 upcall); array-index→elements
pub fn t_get_prop_with_receiver(st, base: Handle, key, receiver: JsVal) -> #(JsVal, InstanceState)  // for super
pub fn t_set_prop(st, recv: JsVal, key: ObjectKey, v: JsVal) -> #(JsVal, InstanceState)
    // OrdinarySet: proto-walk for setter; Accessor→ store.ops.call(st, setter, recv, [v]) (D17 upcall); ArrayObj length magic
pub fn t_set_prop_with_receiver(st, base: Handle, key, v, receiver) -> #(JsVal, InstanceState)
pub fn t_define_prop(st, obj: Handle, key: ObjectKey, desc: ParsedDesc) -> #(Bool, InstanceState)  // §10.1.6.3
pub fn t_has_prop(st, obj: JsVal, key) -> #(Bool, InstanceState)        // proto-walking
pub fn t_delete_prop(st, obj: Handle, key) -> #(Bool, InstanceState)
pub fn t_own_keys(st, obj: Handle) -> #(List(ObjectKey), InstanceState)
    // ES enumeration order: integer-index ascending, then string insertion (by seq), then symbols
pub fn t_get_proto(st, obj: Handle) -> #(Option(Handle), InstanceState)
pub fn t_set_proto(st, obj: Handle, proto: Option(Handle)) -> #(Bool, InstanceState)  // cyclic-check

// ── iteration (§7.4) ──
pub fn t_get_iterator(st, obj: JsVal, hint: IterHint) -> #(Handle, InstanceState)
    // @@iterator/@@asyncIterator lookup + store.ops.call (D17). Returns Handle to a fresh cell holding
    // IteratorRecord{iterator: Handle, next_method: JsVal, done: Bool} — NOT a bare tuple.
pub fn t_iter_next(st, iter_rec: Handle) -> #(#(Bool, JsVal), InstanceState)       // returns (done, value) pair
pub fn t_iter_close(st, iter_rec: Handle, abrupt: Bool) -> InstanceState           // .return(); swallows throw only if abrupt
pub fn t_for_in_keys(st, obj: JsVal) -> #(List(BitArray), InstanceState)           // proto-chain enumerable strings, dedup

// ── low-level (used by rt_js_class/rt_js_builtins) ──
pub fn t_own_property_of(st, obj: Handle, key) -> #(Option(Property), InstanceState)
pub fn t_define_own_data(st, obj: Handle, key, v, w, e, c) -> InstanceState
pub fn t_define_own_accessor(st, obj: Handle, key, get, set, e, c) -> InstanceState
```

**Leaf submodules shipped with M4:**
- `rt_js_elements.gleam` — port of `arc/src/arc/vm/internal/elements.gleam:1-373` (Dense/Sparse via `twocore_rt_js_tree_array_ffi.erl`)
- `rt_js_key.gleam` — re-exports from `rt_js_types` + `array_index_of_float` (`arc/key.gleam:120-127`)
- Vendored `arc/internal/ordered_entries.gleam` + `arc_tree_array_ffi.erl` — backs `MapObj: OrderedEntries(MapKey, JsVal)` and `SetObj: OrderedEntries(MapKey, JsVal)` (value = the original key `JsVal` round-tripped, preserving -0/NaN for iteration; §2.4). `MapKey` normalizes `-0→+0` and boxes NaN so `Dict` equality matches SameValueZero.

**Invariants:** `t_get_prop` on non-object primitives boxes via `t_to_object` first (throws for null/undefined); array `length` invariant maintained by `t_set_prop` on `ArrayObj`; every op returns updated St; `t_own_keys` never returns `Private(_)` keys.

---

### M-CALL — `rt_js_call.gleam` (NEW module, resolves M4/M6/M8 open)

**File:** `2core/src/twocore/runtime/rt_js_call.gleam`
**Deps:** `rt_js_types`, `rt_js_store`, `rt_js_obj`.
**Source ports:** `arc/src/arc/vm/exec/call.gleam:1258-1716` (`call_value`/`do_construct`), `arc/src/arc/vm/exec/frame.gleam:80-139`.

```gleam
/// Frame is a plain 4-tuple at the wire level (D5) — typed here for Gleam callers.
pub type Frame { Frame(this: JsVal, active_func: JsVal, home_object: JsVal, new_target: JsVal) }

pub type Completion { NormalCompletion(JsVal)  ThrowCompletion(JsVal) }

/// The ONE re-entry point. Applies a JS callable, catches {wasm_exn,0,[St,V]}
/// into a 2-armed Completion. Every rt_js module that calls user code goes through here.
pub fn t_call(st, callee: JsVal, this: JsVal, args: List(JsVal)) -> #(Completion, InstanceState)

/// = t_call; on ThrowCompletion, re-raise via t_throw. For internal callers (getters,
/// @@toPrimitive, iterator .next) that want the exception to propagate unchanged.
pub fn t_call_checked(st, callee: JsVal, this: JsVal, args: List(JsVal)) -> #(JsVal, InstanceState)

pub fn t_construct(st, callee: JsVal, args: List(JsVal), new_target: JsVal) -> #(Handle, InstanceState)
```

**`t_call` dispatch** (case on `t_cell_get(callee).kind` after `classify → KHandle` guard; brackets with `t_enter_call`/`t_leave_call`):
| ObjKind | Action |
|---|---|
| `KFunction(code,home,flags,…)` | `flags.is_class_constructor` → throw TypeError. Build `Frame = {this_resolved, callee_h, home?undefined, undefined}`; `t_call_protected(St, code, Frame, args_as_cons_list)` → Completion |
| `KNative(tag,…)` | `dispatch_native(st, tag, this, args)` (M6's giant case) |
| `KBound(target,bt,ba)` | `t_call(st, target, bt, ba ++ args)` |
| `ProxyObj(t,h,_)` | `t_get_prop(h,"apply")` → if callable, `t_call(trap, h, [t, this, args_array])`; else `t_call(t, this, args)` |
| non-callable | throw TypeError `<x> is not a function` |

**`t_construct`** (§10.2.2):
| ObjKind | Action |
|---|---|
| `KFunction(…,flags,fields_init,…)` | `!flags.is_constructor` → TypeError. Base: alloc `this` from `t_get_prop(new_target,"prototype")`; call `fields_init` if Some. Derived: `this = js_tdz`. Build `Frame = {this, callee_h, undefined, new_target}`; apply code; return-override rules (`interpreter.gleam:3034-3071`): base = obj-ret overrides else this; derived = obj-ret overrides, undef→this (TDZ-checked ReferenceError), else TypeError |
| `KNative(tag,…,constructible:True)` | `dispatch_native_construct(st, tag, args, new_target)` |
| `KBound(target,_,ba)` | `t_construct(st, target, ba++args, if new_target==callee then target else new_target)` |
| `ProxyObj` | construct trap |

**FFI** — `t_call_protected/4` in `twocore_rt_js_call_ffi.erl`:
```erlang
t_call_protected(St, Code, Frame, Args) ->
    try Code(St, Frame, Args) of {V, St2} -> {{normal_completion, V}, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {{throw_completion, E}, St2} end.
```

**Invariants:** `Frame` tuple always 4 elements; `t_call` NEVER applies a raw fun without a cell wrapper (D4); `this` resolution (sloppy undefined→global) done HERE, not in prologue.

---

### M5 — `rt_js_ops.gleam`

**File:** `2core/src/twocore/runtime/rt_js_ops.gleam` + `twocore_rt_js_ops_ffi.erl`
**Deps:** `rt_js_types`, `rt_js_val` (threaded coercions), `rt_js_obj` (for instanceof proto-walk).
**Source ports:** extend `twocore_rt_js_ffi.erl:63-353` (nadd/nsub/nmul/ndiv/nmod/js_cmp/loose_eq); port `arc/src/arc/vm/ops/operators.gleam:106-410`, `arc/src/arc/vm/ops/numeric.gleam:134-329`, `arc/src/arc/vm/ops/instanceof.gleam:28-211`.

Arithmetic/compare THREADED (ToPrimitive on objects re-enters JS):
```gleam
pub fn t_add/t_sub/t_mul/t_div/t_mod/t_pow(st, a: JsVal, b: JsVal) -> #(JsVal, InstanceState)
pub fn t_neg/t_plus/t_bitnot(st, a: JsVal) -> #(JsVal, InstanceState)
pub fn t_bitand/t_bitor/t_bitxor/t_shl/t_shr/t_ushr(st, a, b) -> #(JsVal, InstanceState)  // ToInt32 wrap; result ∈ [-2^31, 2^31)
pub fn t_lt/t_le/t_gt/t_ge(st, a, b) -> #(Int, InstanceState)
    // Abstract Relational (§7.2.13). String branch = BYTE-WISE binary compare on the UTF-8 bytes (D10) —
    // NO utf16_compare, NO codepoint decode. Matches arc byte-for-byte.
pub fn t_eq(st, a, b) -> #(Int, InstanceState)                        // Abstract Equality — threaded (ToPrimitive)
pub fn t_instance_of(st, a, b) -> #(Int, InstanceState)               // @@hasInstance + OrdinaryHasInstance proto walk
pub fn t_in(st, key, obj) -> #(Int, InstanceState)
```

**Strict equality is PURE** (§8 → JPure classification):
```gleam
pub fn strict_eq(a: JsVal, b: JsVal) -> Bool
    // No St. NaN≠NaN, +0===-0, {js_cell,N}==={js_cell,N} by id. Via classify — never reads the store.
pub fn strict_ne(a: JsVal, b: JsVal) -> Bool
```

**Fast-path IR pattern each op emits under** (HANDOFF §5:167 — M11's `guarded_binop` helper):
```
Let([an], TermTest(IsNumber, a),
  Let([bn], TermTest(IsNumber, b),
    If(Num(I32And, [an, bn]), [TTerm],
       NumTerm(NAdd, [a, b]),
       CallHost("js", "add", [a, b]))))
```

**Invariants:** bitops always return BEAM integer in [-2^31, 2^31); Symbol operands throw TypeError (falls out of `t_to_numeric`).

---

### M6 — `rt_js_builtins.gleam` (largest module)

**Files:**
```
2core/src/twocore/runtime/rt_js_builtins.gleam                     — init_realm, dispatch_native, well-known-symbol consts
2core/src/twocore/runtime/rt_js_builtins/common.gleam              — MethodSpec, alloc_proto, init_type, init_namespace, alloc_methods (arc common.gleam:291-943)
2core/src/twocore/runtime/rt_js_builtins/helpers.gleam             — require_new_target, require_brand, require_callable, arg_at (arc helpers.gleam:29-192)
2core/src/twocore/runtime/rt_js_builtins/{object,function,array,string,number,boolean,symbol,error,
  math,json,reflect,console,map,set,weak,date,regexp,promise,iterator,generator,bigint,
  array_buffer,typed_array,data_view,global_fns}.gleam
2core/src/twocore_rt_js_builtins_ffi.erl                           — apply_native_tag/4 dispatch
```
**Deps:** `rt_js_types`, `rt_js_store`, `rt_js_val`, `rt_js_obj`, `rt_js_ops`, `rt_js_call`, `rt_js_async`, `rt_js_gc`.

```gleam
pub fn init_realm(store: JsStore) -> #(JsStore, Realm)
    // Operates at JsStore level (NOT InstanceState) — called before any InstanceState exists.
    // Step 1 (FIRST, before any allocation): seed store.ops = JsOps(get_prop: rt_js_obj.t_get_prop,
    //         call: rt_js_call.t_call_checked, to_object: rt_js_obj.t_box_primitive,
    //         new_error: rt_js_obj.t_new_error_kind, eval_hook: <stub → throw EvalError; M19 replaces>).
    //         Field set MUST match §2.4 JsOps exactly. This is the ONE place JsOps is populated (D17).
    // Step 2: alloc every prototype+constructor cell, wire proto chains, populate methods, build globalThis.
    // Deterministic handle ids. Pins all as roots. Port arc/builtins.gleam:54-635.
    // Returns (store', realm) — caller (M19) writes both into InstanceState.

pub fn dispatch_native(st, tag: NativeToken, this: JsVal, args: List(JsVal)) -> #(JsVal, InstanceState)
    // The ONE giant exhaustive case on NativeToken → per-method impl. Called by rt_js_call.t_call for KNative.
pub fn dispatch_native_construct(st, tag: NativeToken, args: List(JsVal), new_target: JsVal) -> #(Handle, InstanceState)
```

**Native method encoding (D4):** each builtin method is a `KNative(tag: NativeToken, name, length, constructible)` cell. `NativeToken` is a Gleam sum type (one nullary variant per method) — NOT a dense `Int`. NOT a raw BEAM fun (D4 identity + allows `Function.prototype.toString` to return `"function name() { [native code] }"`).

**Host-hook consumers:** `Date.now`/`Date()` read `store.host_hooks.monotonic_now`; `Math.random` reads `store.host_hooks.random`; `console.*` writes via `store.host_hooks.print` (M20 harness supplies a deterministic capture); `performance.now` reads `monotonic_now`. NO direct `erlang:system_time`/`rand:uniform`/`io:format`.

**FINAL method enumeration** (per-prototype, derived from `arc/src/arc/vm/builtins/*.gleam` pub fns — every method arc implements; `dispatch_native` case is exhaustive):

| Prototype/Namespace | Methods (name/length) |
|---|---|
| `Object.prototype` | `hasOwnProperty/1, isPrototypeOf/1, propertyIsEnumerable/1, toLocaleString/0, toString/0, valueOf/0` |
| `Object` (static) | `assign/2, create/2, defineProperties/2, defineProperty/3, entries/1, freeze/1, fromEntries/1, getOwnPropertyDescriptor/2, getOwnPropertyDescriptors/1, getOwnPropertyNames/1, getOwnPropertySymbols/1, getPrototypeOf/1, hasOwn/2, is/2, isExtensible/1, isFrozen/1, isSealed/1, keys/1, preventExtensions/1, seal/1, setPrototypeOf/2, values/1` |
| `Function.prototype` | `apply/2, bind/1, call/1, toString/0, [@@hasInstance]/1` |
| `Array.prototype` | `at/1, concat/1, copyWithin/2, entries/0, every/1, fill/1, filter/1, find/1, findIndex/1, findLast/1, findLastIndex/1, flat/0, flatMap/1, forEach/1, includes/1, indexOf/1, join/1, keys/0, lastIndexOf/1, map/1, pop/0, push/1, reduce/1, reduceRight/1, reverse/0, shift/0, slice/2, some/1, sort/1, splice/2, toLocaleString/0, toReversed/0, toSorted/1, toSpliced/2, toString/0, unshift/1, values/0, with/2, [@@iterator]/0` |
| `Array` (static) | `from/1, isArray/1, of/0, [@@species]` |
| `String.prototype` | `at/1, charAt/1, charCodeAt/1, codePointAt/1, concat/1, endsWith/1, includes/1, indexOf/1, lastIndexOf/1, localeCompare/1, match/1, matchAll/1, normalize/0, padEnd/1, padStart/1, repeat/1, replace/2, replaceAll/2, search/1, slice/2, split/2, startsWith/1, substring/2, toLowerCase/0, toString/0, toUpperCase/0, trim/0, trimEnd/0, trimStart/0, valueOf/0, [@@iterator]/0` |
| `String` (static) | `fromCharCode/1, fromCodePoint/1, raw/1` |
| `Number.prototype` | `toExponential/1, toFixed/1, toPrecision/1, toString/1, valueOf/0` |
| `Number` (static) | `isFinite/1, isInteger/1, isNaN/1, isSafeInteger/1, parseFloat/1, parseInt/2` + consts `EPSILON, MAX_SAFE_INTEGER, MAX_VALUE, MIN_SAFE_INTEGER, MIN_VALUE, NaN, NEGATIVE_INFINITY, POSITIVE_INFINITY` |
| `Boolean.prototype` | `toString/0, valueOf/0` |
| `Symbol.prototype` | `toString/0, valueOf/0, description(get), [@@toPrimitive]/1` |
| `Symbol` (static) | `for/1, keyFor/1` + 15 well-known consts |
| `Error.prototype` (+ 7 subclasses) | `toString/0, message, name` |
| `Math` | `abs, acos, acosh, asin, asinh, atan, atan2, atanh, cbrt, ceil, clz32, cos, cosh, exp, expm1, floor, fround, hypot, imul, log, log10, log1p, log2, max, min, pow, random, round, sign, sin, sinh, sqrt, tan, tanh, trunc` + consts `E, LN10, LN2, LOG10E, LOG2E, PI, SQRT1_2, SQRT2` |
| `JSON` | `parse/2, stringify/3` |
| `Reflect` | `apply/3, construct/2, defineProperty/3, deleteProperty/2, get/2, getOwnPropertyDescriptor/2, getPrototypeOf/1, has/2, isExtensible/1, ownKeys/1, preventExtensions/1, set/3, setPrototypeOf/2` |
| `Map.prototype` | `clear/0, delete/1, entries/0, forEach/1, get/1, has/1, keys/0, set/2, size(get), values/0, [@@iterator]/0` |
| `Set.prototype` | `add/1, clear/0, delete/1, entries/0, forEach/1, has/1, keys/0, size(get), values/0, [@@iterator]/0` |
| `WeakMap.prototype` | `delete/1, get/1, has/1, set/2` |
| `WeakSet.prototype` | `add/1, delete/1, has/1` |
| `Date.prototype` | `getDate, getDay, getFullYear, getHours, getMilliseconds, getMinutes, getMonth, getSeconds, getTime, getTimezoneOffset, getUTCDate…, setDate…, toISOString, toJSON, toString, valueOf, [@@toPrimitive]` |
| `Date` (static) | `now/0, parse/1, UTC/7` |
| `RegExp.prototype` | `exec/1, test/1, toString/0, [@@match], [@@matchAll], [@@replace], [@@search], [@@split]` + getters `flags, global, ignoreCase, multiline, source, sticky, unicode, dotAll, hasIndices` |
| `Promise.prototype` | `then/2, catch/1, finally/1` |
| `Promise` (static) | `all/1, allSettled/1, any/1, race/1, reject/1, resolve/1, [@@species]` |
| `%IteratorPrototype%` | `[@@iterator]/0` |
| `%ArrayIteratorPrototype%` etc. | `next/0` |
| `%GeneratorPrototype%` | `next/1, return/1, throw/1` (delegate to M8) |
| `%AsyncGeneratorPrototype%` | `next/1, return/1, throw/1` (delegate to M8) |
| `ArrayBuffer.prototype` | `byteLength(get), slice/2` |
| `%TypedArray%.prototype` | `at, buffer(get), byteLength(get), byteOffset(get), copyWithin, entries, every, fill, filter, find, findIndex, forEach, includes, indexOf, join, keys, lastIndexOf, length(get), map, reduce, reduceRight, reverse, set, slice, some, sort, subarray, values, [@@iterator]` |
| `DataView.prototype` | `buffer(get), byteLength(get), byteOffset(get), getInt8…getFloat64, setInt8…setFloat64` |
| `console` | `log, error, warn, info, debug, trace, assert, dir, table, count, countReset, time, timeEnd, timeLog, group, groupEnd` — all write to `console_buf` (M20), NOT `io:format` |
| Global fns | `parseInt/2, parseFloat/1, isNaN/1, isFinite/1, encodeURI, encodeURIComponent, decodeURI, decodeURIComponent, eval/1 (throws EvalError — D15), globalThis, undefined, NaN, Infinity` |

**Invariants:** `init_realm` is the ONLY place intrinsics are allocated; every prototype's `[[Prototype]]` chain terminates at `Object.prototype→null`; every method cell has enumerable:False.

---

### M7 — `rt_js_class.gleam`

**File:** `2core/src/twocore/runtime/rt_js_class.gleam`
**Deps:** `rt_js_types`, `rt_js_store`, `rt_js_obj`, `rt_js_val`, `rt_js_call`.
**Source ports:** `arc/vm/exec/interpreter.gleam:3875-3945` SetupDerivedClass, `:3034-3071` ctor-return, `arc/vm/value.gleam:3691-3714` mint_private_key.

```gleam
pub fn t_new_private_name(st, source: BitArray) -> #(JsVal, InstanceState)
    // {js_key_priv, <<Source, 0, UidBin>>}; bumps JsStore.private_uid (D9)

pub fn t_class_create(st, ctor_code: CompiledFn, super: Option(Handle), name: BitArray,
                      is_derived: Bool, captures: List(Handle)) -> #(#(Handle, Handle), InstanceState)
    // Returns (ctor_h, proto_h). §15.7.14: alloc proto (proto→super.prototype or Object.prototype);
    // alloc ctor as KFunction{is_class_constructor:T, is_derived_constructor, home_object:Some(proto_h)};
    // ctor.prototype = proto (non-writable); proto.constructor = ctor; ctor.__proto__ = super or Function.prototype.
    // For derived: t_get_prop(super,"prototype") — OBSERVABLE, may re-enter (via t_call_checked).

pub fn t_define_method(st, target: Handle, key: ObjectKey, fn_h: Handle, kind: MethodInstallKind) -> InstanceState
    // MethodInstallKind = MIMethod | MIGetter | MISetter | MIStatic | MIStaticGetter | MIStaticSetter (M1a).
    // Sets home_object on fn_h's KFunction to target; defines as enumerable:False data/accessor prop.

pub fn t_private_get(st, obj: JsVal, priv_key: JsVal) -> #(JsVal, InstanceState)
    // If !dict.has_key(props, Private(k)) → throw TypeError. Return DataProperty.value or call Accessor.get.
pub fn t_private_set(st, obj, priv_key, v) -> #(JsVal, InstanceState)
pub fn t_private_define(st, obj, priv_key, v) -> InstanceState  // for field-init; throws if already present (double-init)

// ── JRead ops (§8) — take St, return bare V, do NOT mutate store ──
pub fn t_private_in(st, obj: JsVal, priv_key: JsVal) -> Bool          // `#x in obj` — pure read
pub fn t_fn_home_object(st, fn_h: Handle) -> JsVal
pub fn t_is_constructor(st, v: JsVal) -> Bool

pub fn t_super_get(st, home: Handle, receiver: JsVal, key: ObjectKey) -> #(JsVal, InstanceState)
    // = t_get_prop_with_receiver(t_get_proto(home), key, receiver)
pub fn t_super_set(st, home, receiver, key, v) -> #(JsVal, InstanceState)
pub fn t_super_call(st, active_func: Handle, args: List(JsVal), new_target: JsVal) -> #(Handle, InstanceState)
    // = t_construct(t_get_proto(active_func), args, new_target); returns bound-this for derived ctor
```

**Invariants:** super lookups start from `home_object.[[Prototype]]`, NOT `this.[[Prototype]]`; `t_class_create` never re-enters user code except via `t_get_prop(super,"prototype")`; class body always strict.

---

### M8 — `rt_js_async.gleam`

**File:** `2core/src/twocore/runtime/rt_js_async.gleam` + `2core/src/twocore_rt_js_queue_ffi.erl`
**Deps:** `rt_js_types`, `rt_js_store`, `rt_js_obj`, `rt_js_call`, `rt_js_gc`.
**Source ports:** `arc/vm/builtins/promise.gleam:89-718`, `arc/vm/exec/event_loop.gleam:84-243`, `arc/vm/exec/promises.gleam:1715-1778` setup_await, `arc/vm/exec/async_generators.gleam:61-150`, `arc/vm/internal/job_queue.gleam` + `arc_job_queue_ffi.erl:1-17`.

```gleam
// ── JobQueue (opaque; Erlang queue) ──
@external pub fn jq_new() -> JobQueue  / jq_push / jq_pop / jq_is_empty / jq_to_list

// ── Promise ──
pub fn t_new_promise_capability(st) -> #(#(Handle, Handle, Handle), InstanceState)
    // (promise_h, resolve_fn_h, reject_fn_h) — resolve/reject are KNative cells closing over promise_h
pub fn t_promise_resolve(st, promise_h: Handle, v: JsVal) -> InstanceState
    // §27.2.1.3.2: if v is thenable → enqueue ResolveThenableJob; else transition Fulfilled + enqueue reactions
pub fn t_promise_reject(st, promise_h, reason) -> InstanceState
pub fn t_promise_then(st, promise_h, on_ful: JsVal, on_rej: JsVal) -> #(Handle, InstanceState)

// ── Microtask queue ──
pub fn t_enqueue_job(st, job: Job) -> InstanceState
pub fn t_drain_microtasks(st) -> InstanceState
    // Loop: pop; execute job via t_call; t_maybe_collect(st); until empty.
    // The between-jobs t_maybe_collect call is the D11 GC safepoint (call_depth==0 here).

// ── State-machine resume ABI (interface for M18) ──
// A compiled `<name>__sm` function has BEAM arity m+4:
//     fun(St, Cap₀..Cap_{m-1}, Rs, Sent, Loc) -> {Step, St'}
// where Rs = resume-state Int, Sent = {mode, value} injected by next/return/throw,
// Loc = the suspended-locals tuple. IR-level params = [caps…, "_rs", "_sent", "_loc"] (D2 — St prepended by emit_core).
pub fn t_async_start(st, sm_code: CompiledFn, captures: List(Handle), frame: Frame, args: List(JsVal)) -> #(Handle, InstanceState)
    // Alloc result promise + gen_state cell; call sm_code(St, caps…, 0, {next, undefined}, initial_locals); drive first step.
pub fn t_await(st, gen_h: Handle, awaited: JsVal, next_state: Int, locals: JsVal) -> InstanceState
    // PromiseResolve(awaited).then(resume_fulfill, resume_reject) where resume_* are KNative cells
    // that re-invoke gen_h's stored sm closure with (next_state, {mode, sent}, locals)

pub fn t_gen_start(st, sm_code: CompiledFn, captures, frame, args) -> #(Handle, InstanceState)
    // Alloc SGenerator cell; store resume closure; return generator object.
pub fn t_gen_next(st, gen_h, sent) -> #(Handle, InstanceState)    // returns iter-result cell {value,done}
pub fn t_gen_return(st, gen_h, v) -> #(Handle, InstanceState)
pub fn t_gen_throw(st, gen_h, e) -> #(Handle, InstanceState)

pub fn t_asyncgen_start / t_asyncgen_next / t_asyncgen_return / t_asyncgen_throw
    // Request-queue model (arc value.gleam:4243-4258); each returns a promise_h.
```

**Step protocol** (returned by M18's compiled `<name>__sm` function — `yield*` is lowered INSIDE the state machine as a self-looping state, so there is NO `{delegate, …}` variant):
```
Step = {return, V}
     | {throw,  V}                          // uncaught within the coroutine's own try regions
     | {yield,  V, NextState:Int, Loc'}
     | {await,  V, NextState:Int, Loc'}
```

**Invariants:** `t_drain_microtasks` is called by M19's `js_main` AFTER top-level eval and by host after each callback; NEVER mid-expression. Job queue is a JsStore field (traced by M2). `t_await` on already-settled promise still enqueues (§27.5.3 — always async).

---

### M9 — `emit_core.gleam` patches (the 2core-side seam)

**File:** `2core/src/twocore/backend/emit_core.gleam` (edit) + `2core/src/twocore/runtime/instance.gleam` (edit) + `2core/src/twocore/runtime/profiles.gleam` (edit).

**§9.1 `Binding` extensions** — `instance.gleam:242-277`, after `js_runtime_module: String`:
```gleam
    js_store_module: String,    // "twocore@runtime@rt_js_store"
    js_val_module: String,      // "twocore@runtime@rt_js_val"
    js_obj_module: String,      // "twocore@runtime@rt_js_obj"
    js_ops_module: String,      // "twocore@runtime@rt_js_ops"
    js_call_module: String,     // "twocore@runtime@rt_js_call"
    js_class_module: String,    // "twocore@runtime@rt_js_class"
    js_async_module: String,    // "twocore@runtime@rt_js_async"
    js_gc_module: String,       // "twocore@runtime@rt_js_gc"
    js_builtins_module: String, // "twocore@runtime@rt_js_builtins"
    js_inspect_module: String,  // "twocore@runtime@rt_js_inspect"
    js_profile: Bool,           // D3 (D18: renamed from uniform_state_threading)
```
`safe_default()` (`instance.gleam:288-327`) seeds all with mangled names + `js_profile: False`.

**§9.2 New profile** — `profiles.gleam` after `js()`:
```gleam
pub fn js_direct() -> Binding {
  Binding(..compose(safe(), Threaded, Paged, TablePaged),
          js_profile: True, meter: MeterOff)
}
```

**§9.3 New types** — `emit_core.gleam` after `js_capability` const `:3602`:
```gleam
type JsRtModule { JsStore  JsVal  JsObj  JsOps  JsCall  JsClass  JsAsync  JsGc  JsBuiltins  JsInspect }
type JsOpKind {
  JPure          // no St; single return; sc unchanged
  JRead          // takes St; returns bare V; sc UNCHANGED (read-only store access)
  JMut           // takes St; returns {V, St'}; rebinds sc
  JMutUnit       // takes St; returns St' bare (no value); rebinds sc
}
type ResolveJsError { UnknownJsOp(String) }
```

**§9.4 `resolve_js` REPLACE** (`emit_core.gleam:3621-3657`) — see §8 for full table. Shape:
```gleam
fn resolve_js(op: String) -> Option(#(JsRtModule, String, JsOpKind))
```

**§9.5 `emit_call_host` "js" arm REWRITE** (`:3549-3567`):
```gleam
"js" -> case resolve_js(name) {
  None -> Error(UnknownJsOp(name))   // propagate; NEVER panic on user-reachable op names
  Some(#(rtmod, fn_name, kind)) -> {
    let mod = js_module_atom(ctx.binding, rtmod)
    case kind, sc {
      JPure, _ -> apply_cont_call(CCall(mod, fn_name, cargs), st, 1, sc, k)
      JRead, Threading(cur) -> apply_cont_call(CCall(mod, fn_name, [CVar(cur), ..cargs]), st, 1, sc, k)
      JMut, Threading(cur) -> emit_value_state_pair(CCall(mod, fn_name, [CVar(cur), ..cargs]), st, k)
      JMutUnit, Threading(cur) -> emit_state_rebind(CCall(mod, fn_name, [CVar(cur), ..cargs]), st, k)
      JRead, NoState | JMut, NoState | JMutUnit, NoState -> panic as "threaded js op under NoState — js_direct profile misconfigured"
    }
  }
}
```
`JRead` prepends `St` but the callee returns a bare value — no `{V,St'}` unpack, `sc` stays bound to `cur`. `JMutUnit` lowering is a **bare `St'` rebind** (`let St2 = call(...); <cont under Threading(St2)>`), NOT `emit_threaded_record_effect`'s `Result(_,_)` wrap — the rt fn returns `InstanceState` directly, not `#(Nil, InstanceState)`.

**§9.6 `state_reaching_closure` short-circuit** (`:801-814`):
```gleam
pub fn state_reaching_closure(functions, binding: Binding) -> Set(String) {
  case binding.js_profile {
    True -> set.from_list(list.map(functions, fn(f) { f.name }))
    False -> // …existing seed+fixpoint…
  }
}
```
(Call site at `:379` gains `binding` param; also `iface.describe/2` per `:794-800`.)

**§9.7 `expr_touches_state` seeds** (`:851-921`) — add:
```gleam
CallClosure(..) -> True   // under uniform threading, any closure call may allocate
CallHost("js", op, _) -> case resolve_js(op) { Some(#(_,_,JPure)) | Some(#(_,_,JRead)) -> False  _ -> True }
```

**§9.8 `emit_make_closure` REWRITE** (`:1493-1523`) — under `Threading(_)` ∧ `ctx.binding.js_profile` ∧ target ∈ `fn_state_reaching`:
```
CFun([St, A_1, ..., A_arity],
     CApply(FName(fn_name, m+1+arity), [CVar(St), Cap_1, ..., Cap_m, CVar(A_1), ..., CVar(A_arity)]))
```
Closure runtime arity = `arity + 1`.

**§9.9 `emit_call_closure` REWRITE** (`:1536-1546`) — under `Threading(cur)` ∧ `js_profile`:
```gleam
emit_value_state_pair(CApplyExpr(callee, [CVar(cur), ..cargs]), st, k)
```

**§9.10 `emit_throw` PATCH** (`:5097-5106`) — add `sc` param; under `Threading(cur)` ∧ `ctx.binding.js_profile`, prepend `CVar(cur)` to payload:
```gleam
Threading(cur) if ctx.binding.js_profile -> CCall(rt_exn, "throw_exn", [CInt(tag_id), CList([CVar(cur), ..carg_vals])])
```
Gated on `js_profile` so WASM's `Threading` path stays byte-identical.

**§9.11 `emit_catch_clause` PATCH** (`:5226-5280`) — under `Threading(_)` ∧ `ctx.binding.js_profile`, prepend fresh `st_caught` to `payload` binder list; emit handler body with `sc = Threading(st_caught)`. `collect_vars` already inserts payload names into reserved (`:7172-7182`). Gated on `js_profile` (not bare `Threading`) for WASM byte-identity.

**Invariants:** D3a preserved (literal case, no data-driven `apply(Mod,Fn,Args)`); WASM path byte-identical when `js_profile: False`.

---

### M10 — `emit_2core/state.gleam`

**File:** `arc/src/arc/compiler/emit_2core/state.gleam`
**Deps:** `arc/compiler/scope`, `arc/parser/ast`, `twocore/ir`, `gleam/*`.
**Source ports:** `arc/src/arc/compiler/emit.gleam:125-310,1295-1470,2226-2314,3313-3336`.

```gleam
pub type Emitter2 {
  Emitter2(
    // ── scope cursor (verbatim port emit.gleam:206-228,298-304) ──
    tree: scope.ScopeTree,
    fn_scope: scope.ScopeId,
    cur_scope: scope.ScopeId,
    scope_cursor: List(scope.ScopeId),
    child_fn_cursor: List(scope.ScopeId),
    // ── name generation (per-function; ir.gleam:37-41 uniqueness) ──
    next_var: Int,           // "t{n}"
    next_label: Int,         // "L{n}"
    next_fn: Int,            // "jsf_{n}" — module-global (survives enter_function)
    // ── control-flow stack ──
    frame_stack: List(Frame2),
    pending_label: Option(String),
    // ── function accumulator ──
    fns_acc: List(ir.Function),
    // ── mode flags ──
    strict: Bool,
    is_async: Bool,          // routes await/for-await to M18
    is_generator: Bool,
    // ── slot mapping (D2: no state_var — St is emit_core's problem) ──
    slot_vars: Dict(Int, String),      // scope-slot → current IR var name (unboxed rebindable)
    initialized: Set(Int),             // TDZ elision (emit.gleam:1841)
    // ── class context (for M12 super/private/new.target) ──
    class_stack: List(ClassCtx),
    // ── mutual-recursion break (D13) ──
    dispatch: EmitDispatch,
    // ── constants ──
    consts: RealmConsts,
  )
}

pub type Frame2 {
  Loop2(ir_break: String, ir_continue: String, js_label: Option(String),
        carried: List(Int), iter_close: Option(String))
  Switch2(ir_break: String, js_label: Option(String), carried: List(Int))
  Labeled2(ir_break: String, js_label: String, carried: List(Int))
  Barrier2(finally_body: Option(#(List(ast.StmtWithLine), ScopeSave2)),
           iter_close: Option(String))
}

/// D13 mutual-recursion break. NOT opaque — M19 constructs this record literally.
pub type EmitDispatch {
  EmitDispatch(
    emit_expr:        fn(Emitter2, ast.Expression) -> #(ir.Expr, Emitter2),
    emit_stmts:       fn(Emitter2, List(ast.StmtWithLine), K) -> #(ir.Expr, Emitter2),
    emit_pattern:     fn(Emitter2, ast.Pattern, ir.Value, BindMode) -> #(ir.Expr, Emitter2),
    emit_function:    fn(Emitter2, FnShape, Option(String), List(ast.Pattern), FnBody, scope.ScopeId) -> #(Emitter2, ir.Value),
    emit_class:       fn(Emitter2, Option(String), Option(String), Option(ast.Expression), List(ast.ClassElement)) -> #(ir.Expr, Emitter2),
    emit_async_body:  fn(Emitter2, CoroutineKind, List(ast.Pattern), FnBody, scope.ScopeId, List(ir.Value)) -> #(Emitter2, ir.Value),
    emit_destructure: fn(Emitter2, ast.Pattern, ir.Value, BindMode) -> #(ir.Expr, Emitter2),
  )
}
pub type K = fn(Emitter2) -> ir.Expr

pub type RealmConsts {
  RealmConsts(undef: ir.Value, null: ir.Value, true_: ir.Value, false_: ir.Value,
              nan: ir.Value, pos_inf: ir.Value, neg_inf: ir.Value, tdz: ir.Value,
              empty_bin: ir.Value, js_tag: String)
}

pub type ClassCtx {
  ClassCtx(brand_vars: Dict(String, ir.Value), proto_home_cell: ir.Value,
           static_home_cell: ir.Value, ctor_self_cell: ir.Value,
           inner_name_cell: Option(ir.Value), is_derived: Bool)
}

pub type ScopeSave2 { ScopeSave2(cur_scope: scope.ScopeId, scope_cursor: List(scope.ScopeId), slot_vars: Dict(Int, String)) }
```

**Public helpers:**
```gleam
pub fn new_emitter(tree, root_scope, strict, dispatch) -> Emitter2
pub fn fresh_var(e) -> #(String, Emitter2)  / fresh_label / fresh_fn_name
pub fn slot_var_name(slot: Int) -> String                   // "js_local_{slot}"
pub fn get_slot_var(e, slot) -> String  / set_slot_var(e, slot, name) -> Emitter2
pub fn enter_scope(e) -> #(Emitter2, ScopeSave2)  / leave_scope(e, save) -> Emitter2
pub fn enter_for_scope / leave_for_scope                    // emit.gleam:1450-1470
pub fn enter_function(e, fn_scope_id) -> Emitter2           // resets next_var/next_label; keeps next_fn
pub fn pop_child_fn(e) -> #(scope.ScopeId, Emitter2)
pub fn push_frame(e, Frame2) -> Emitter2  / pop_frame(e) -> Emitter2
pub fn add_function(e, ir.Function) -> Emitter2
pub fn fn_info(e) -> scope.FunctionInfo
pub fn assigned_unboxed_slots(e, stmts) -> List(Int)        // for LoopParam carried set
```

**Invariants:** every fresh name unique within current `ir.Function` (`ir.gleam:419-420`); `frame_stack` balanced on every path; `next_fn` module-global. **Arch-A (D2):** `Emitter2` has NO `state_var`/`st_cur` field — the emitter is state-oblivious; `emit_core` (M9) alone owns St threading.

---

### M11 — `emit_2core/anf.gleam`

**File:** `arc/src/arc/compiler/emit_2core/anf.gleam`
**Deps:** M10, `twocore/ir`.

```gleam
/// CPS builder over Emitter2. Tail continuation receives final Emitter2 + result;
/// signature pinned to M12.md:32 — the continuation returns bare ir.Expr.
pub type Build(a) = fn(Emitter2, fn(Emitter2, a) -> ir.Expr) -> ir.Expr

pub fn pure(v: a) -> Build(a)
pub fn then(b: Build(a), f: fn(a) -> Build(b)) -> Build(b)
pub fn map(b: Build(a), f: fn(a) -> b) -> Build(b)
pub fn seq(bs: List(Build(a))) -> Build(List(a))

/// Let-bind an ir.Expr yielding 1 TTerm; returns fresh Var. (D2: no St bookkeeping)
pub fn bind(rhs: ir.Expr) -> Build(ir.Value)
pub fn bind_n(rhs: ir.Expr, n: Int) -> Build(List(ir.Value))

/// CallHost("js", op, args) → bound Var. Args carry NO St — emit_core (M9) classifies
/// via resolve_js and injects/unpacks state per JsOpKind. (D2 Arch-A)
pub fn host(op: String, args: List(ir.Value)) -> Build(ir.Value)
pub fn host_unit(op: String, args: List(ir.Value)) -> Build(Nil)        // JMutUnit ops (cell_set etc.)

/// Emit the WIRE ObjectKey tuple for a compile-time-known property key (§2.3 encoding).
/// Static member access (`obj.foo`, method defs) uses this — canonicalized at compile time.
pub fn object_key_lit(pk: PropertyKey) -> ir.Expr            // → make_tuple([ConstAtom("string_key"), make_tuple([ConstAtom(<variant>), ConstBinary(..)/ConstI32(..)])])

pub fn bind_if(cond: ir.Value, t: Build(ir.Value), f: Build(ir.Value)) -> Build(ir.Value)
pub fn bind_block(body: fn(label: String) -> Build(ir.Value)) -> Build(ir.Value)

pub fn cons_list(vs: List(ir.Value)) -> Build(ir.Value)      // MakeCons chain over host("empty_list",[])
pub fn make_tuple(vs: List(ir.Value)) -> Build(ir.Value)     // TermOp(MakeTuple, vs)
pub fn tuple_get(v: ir.Value, i: Int) -> ir.Expr             // TermOp(TupleGet(i), [v]) — i is 0-BASED

/// HANDOFF §5 fast-path: TermTest(IsNumber)×2 → NumTerm fast / CallHost slow
pub fn guarded_binop(fast: ir.NumTermOp, slow_op: String, a: ir.Value, b: ir.Value) -> Build(ir.Value)
pub fn guarded_cmp(fast: ir.NumTermOp, slow_op: String, a, b) -> Build(ir.Value)

pub fn truthy_if(v: ir.Value, t: Build(ir.Value), f: Build(ir.Value)) -> Build(ir.Value)  // host("truthy",[v]) → If
pub fn nullish_if(v, t, f) -> Build(ir.Value)

pub fn run(b: Build(ir.Value), e: Emitter2) -> #(ir.Expr, Emitter2)  // supplies fn(e,v){Values([v])}
```

---

### M12 — `emit_2core/expr.gleam`

**File:** `arc/src/arc/compiler/emit_2core/expr.gleam`
**Deps:** M10, M11, `arc/parser/ast`, `arc/compiler/scope`, `arc/compiler/ast_util`.
**Source ports:** `arc/src/arc/compiler/emit.gleam:4800+` expression cases; HANDOFF §5 lowering table.

```gleam
pub fn emit_expr(e: Emitter2, ex: ast.Expression) -> #(ir.Expr, Emitter2)
pub fn emit_named_expr(e, ex, name: String) -> #(ir.Expr, Emitter2)    // §8.4 NamedEvaluation
pub fn emit_lvalue_rw(e, target: ast.Expression) -> #(ir.Expr, LvaluePut, Emitter2)
pub fn emit_property_key(e, key: ast.PropertyKey) -> #(ir.Expr, Emitter2)
    // Static key (Identifier/StringLit/NumberLit) → bind(object_key_lit(canonical_key(...)))
    // Computed key → bind(emit_expr(k), λv. host("to_property_key",[v]))
pub fn emit_args_list(e, args: List(ast.Expression)) -> #(ir.Expr, Emitter2)  // spread → host("spread_into_list")
```

**Errors:** unlowerable syntax (bare `super` outside class, spread in unsupported position, `new.target` outside function) returns `Error(EarlySyntaxError(msg))` — NEVER `panic`.

**Per-`ast.Expression` variant lowering** (D2 Arch-A — NO St in IR):

| Variant | IR |
|---|---|
| `NumberLit(Finite f)` | int-in-[-2^31,2^31) → `Convert(BoxInt(W32), ConstI32(i))`; else `host("float_lit", [ConstF64(bits)])` (M3 unboxes to native float) |
| `NumberLit(NaN/Inf)` | `ConstAtom("js_nan"/"js_inf"/"js_neg_inf")` |
| `StringLit s` | `ConstBinary(utf8 bytes)` |
| `Bool/Null/Undef` | `ConstAtom` |
| `BigIntLit n` | `make_tuple([ConstAtom("js_bigint"), ConstI64(n)])` |
| `TemplateLiteral` | fold `host("to_string",[part])` + `host("string_concat",[acc, s])` |
| `Identifier name` | `scope.lookup(tree, cur_scope, name)`: `Plain(Local{boxed:F, slot})` → `Var(slot_var(slot))`; `Plain(Local{boxed:T, slot})` → `host("cell_get",[Var(slot_var)])`; `Plain(Global)` → `host("global_get",[ConstBinary(name)])`; `WithChain(frames)` → nested `bind_if(host("has_prop"),…)` probes |
| `Binary(op,l,r)` | `+ - * < <= > >= ==` → `guarded_binop/guarded_cmp`; `=== !==` → `host("strict_eq"/"strict_ne",[l,r])` (JPure — resolve_js emits bare apply, NO state pair); `/ % ** & \| ^ << >> >>> in instanceof` → `host("<op>",[l,r])` |
| `Logical(&&,l,r)` | `bind(l, λv. truthy_if(v, emit_expr(r), pure(v)))` (short-circuit); `\|\|` symmetric; `??` via `nullish_if` |
| `Unary` | `! → truthy_if(v,false,true)`; `typeof → host("type_of",[v])`; `- + ~ → host`; `void → seq(v, undef)`; `delete → host("delete_prop")` |
| `Update(++/--)` | `emit_lvalue_rw` → old; `host("add",[old, ConstI32(1)])` → new; put(new); prefix?new:old |
| `Assignment(=)` | Identifier: unboxed → `set_slot_var` rebind; boxed → `host_unit("cell_set")`; global → `host_unit("global_set")`. Member → `host("set_prop")`. Destructure-target → M15 delegate. |
| `Assignment(+=…)` | via `emit_lvalue_rw` |
| `Conditional` | `bind(cond, λc. truthy_if(c, then, else))` |
| `Member(obj,.k)` | `bind(object_key_lit(canonical_key(k)), λpk. host("get_prop",[obj, pk]))` |
| `Member(obj,[k])` | `bind(emit_expr(k), λkv. bind(host("to_property_key",[kv]), λpk. host("get_prop",[obj, pk])))` |
| `OptionalChain` | `bind_block(λexit. bind(base, λb. nullish_if(b, Break(exit,[undef]), continue chain)))` |
| `Call(callee,args)` | See §3.3/§3.5: eval callee (special: `super()` → `host("super_call")`; member-call binds this); build args cons-list; if `scope.lookup(callee)` = `FunctionDecl` never-reassigned → `CallDirect("jsf_N",[frame,args])` fast path; else `host("call",[callee, this, args])` (rt_js_call.t_call) |
| `New(callee,args)` | `host("construct",[callee, args_list, callee])` |
| `Array(elems)` | `host("new_array",[cons_list(elems)])` (spread → `host("spread_into_list")`) |
| `Object(props)` | `bind(host("new_object",[proto?]), λo. seq(per-prop host("set_prop"/"define_prop"), pure(o)))` |
| `TaggedTemplate` | `host("call",[tag, undef, cons_list([raw_array, ..cooked])])` |
| `Sequence` | fold `seq(discard, next)` |
| `This` | `tuple_get(Var("_frame"), 0)` |
| `MetaProperty(NewTarget)` | `tuple_get(Var("_frame"), 3)` |
| `Super` (in member) | `host("super_get",[tuple_get(_frame,2), tuple_get(_frame,0), key])` |
| `FunctionExpression`/`Arrow` | `e.dispatch.emit_function` (M14) |
| `ClassExpression` | `e.dispatch.emit_class` (M16) |
| `Await`/`Yield` | `e.dispatch.emit_async_body` path (M18; only when `e.is_async`/`is_generator`) |

---

### M13 — `emit_2core/stmt.gleam`

**File:** `arc/src/arc/compiler/emit_2core/stmt.gleam`
**Source ports:** `arc/src/arc/compiler/emit.gleam:1100-2780,3685-4130,5080-6030`.

```gleam
pub fn emit_stmts(e: Emitter2, ss: List(ast.StmtWithLine), k: K) -> #(ir.Expr, Emitter2)
pub fn emit_stmt(e, s: ast.Statement, k: K) -> #(ir.Expr, Emitter2)
```

| Variant | IR |
|---|---|
| `VarDeclaration(kind, decls)` | Per decl: emit init (via `emit_named_expr`); Identifier: unboxed → `Let([slot_var],Values([v]),k)` + `set_slot_var`; boxed → `host_unit("cell_set",[cell_var,v])` (var-hoist prologue already `cell_new(undef)`'d it). Pattern → M15. |
| `ExpressionStatement` | `bind(emit_expr, λ_. k)` |
| `If(c,t,f)` | `bind(emit_expr(c), λv. truthy_if(v, emit_block(t,k), emit_block(f,k)))` — NB k duplicated (join) or via `Block` label |
| `While(c,b)` | `carried = assigned_unboxed_slots(b)`; `Block(Lbrk,[TTerm×n], Loop(Lcont, LoopParams(carried), [], truthy_if(c, {body; Continue(Lcont,[carried'])}, Break(Lbrk,[carried]))))` then rebind slot_vars from Block result. **NO St LoopParam** (D2 Arch-A). |
| `DoWhile` | Loop with test at end |
| `For(init;test;upd)` | `enter_for_scope` if lex; init; `While(test, {body; upd})`; per-iteration rebind (`emit.gleam:5618`) |
| `ForOf(lhs,rhs,body)` | `bind(host("get_iterator",[rhs, sync]), λit. push_frame(Loop2{iter_close:Some(it)}); Loop: bind(host("iter_next",[it]), λ#(done,val). If(done, Break, {emit_pattern(lhs,val); body; Continue})); pop; host_unit("iter_close",[it]))` — abrupt exit inlines `iter_close` via `Barrier2` |
| `ForIn` | `bind(host("for_in_keys",[obj]), λkeys. Loop over cons-list)` (eager list) |
| `ForAwaitOf` | delegate M18 (loop body contains await on `iter_next`) |
| `Switch(disc,cases)` | JS fallthrough + non-const cases: `bind(disc, λd. Block(Lbrk, sequential If(host("strict_eq",[d,case_k]))-chain with fallthrough via nested Blocks))` — strict_eq is JPure. All-int-literal fast case → `ir.Switch(Convert(UnboxInt,d),…)` |
| `Break(label?)` | Walk `frame_stack` to target. `frame_stack == []` OR label not found → `Error(BreakOutsideLoop(label))`. For each `Barrier2{finally}` crossed → INLINE `emit_stmts(finally)`; for each `iter_close` → `host_unit("iter_close")`; then `ir.Break(target.ir_break, [carried_vars])` |
| `Continue(label?)` | Same barrier walk; `[]` → `Error(ContinueOutsideLoop(label))`; `ir.Continue(target.ir_continue, [carried_vars])` |
| `Labeled(l, s)` | Set `pending_label`; if `s` is loop → consumed by loop's Frame2; else `Block(L,[], push Labeled2; emit s; pop)` |
| `Return(v?)` | Walk barriers; `ir.Return([v ?? undef])` — bare 1-value; emit_core wraps `{V, St'}` (D2 Arch-A) |
| `Throw(e)` / `TryStatement` | delegate M17 |
| `ClassDeclaration` | delegate M16 |
| `FunctionDeclaration` | Already hoisted by M14's prologue; stmt is no-op here |
| `Block(ss)` | `enter_scope`; hoist inner FunctionDeclarations (BlockDeclarationInstantiation); `emit_stmts(ss, k)`; `leave_scope` |
| `With` | `EmitError.Unsupported("with")` (D15) |
| `Debugger`/`Empty` | `k(e)` |

---

### M14 — `emit_2core/func.gleam`

**File:** `arc/src/arc/compiler/emit_2core/func.gleam`
**Source ports:** `arc/src/arc/compiler/emit.gleam:3190-3650`; capture order `arc/src/arc/compiler.gleam:595-622`.

```gleam
pub type FnShape {
  FnDecl(is_gen: Bool, is_async: Bool)  FnExpr(self_name: Option(String), is_gen, is_async)
  Arrow(is_async: Bool)  Method(is_gen, is_async)  ClassCtor(derived: Bool, has_field_init: Bool)  ClassInitFn
}

pub fn emit_function(e: Emitter2, shape: FnShape, js_name: Option(String),
                     params: List(ast.Pattern), body: FnBody, fn_scope_id: ScopeId)
  -> #(Emitter2, ir.Value)
    // Returns closure VALUE (Var bound to host("fn_new",[MakeClosure(...), flags, name, len, captures_list]))
    // If is_gen|is_async → delegates ENTIRE body to M18.emit_coroutine_fn.

pub fn build_capture_values(e, info: FunctionInfo) -> List(ir.Value)
    // In PARENT frame: for each (name, parent_slot) in info.captures (list order),
    // then all_lexical_refs order for lexical_captures. Boxed → cell-handle Var; unboxed → Var direct.

pub fn emit_prologue(e, params, info: FunctionInfo, shape) -> Build(Nil)
    // 1. Unpack _args cons-list: per param i → Let([p_i], ListHead(tail_i), …); rest = remaining tail
    // 2. Defaults: bind_if(host("strict_eq",[p_i, undef]), emit_expr(default), pure(p_i))
    // 3. Destructured params → e.dispatch.emit_destructure (M15)
    // 4. Box any is_boxed param: host("cell_new",[p_i]) → set_slot_var
    // 5. Frame unpack (non-arrow): Let([this], TupleGet(0, _frame), Let([af], TupleGet(1, _frame),
    //    Let([ho], TupleGet(2, _frame), Let([nt], TupleGet(3, _frame), …))))  — indices 0-BASED (G19)
    //    Arrow: skip — this/nt come from captures
    // 6. arguments object (if references_arguments): host("new_arguments",[_args, mapped_cells?])
    // 7. Box locals: for each is_boxed non-param local → host("cell_new",[undef])
    // 8. Hoist FunctionDeclarations: emit each, cell_set/set_slot_var
    // NB: NO t_maybe_collect call here (G1/D11) — GC safepoint is turn-boundary only (M8 t_drain_microtasks).
```

**IR function shape (D2 Arch-A):** `params = [cap_0..cap_{m-1}, "_frame", "_args"]`, `result = [TTerm]`, `body = prologue; emit_stmts(body, λe. Return([undef]))`. NO St param at IR level.

**Closure site:** `bind(MakeClosure("jsf_N", capture_vals, arity: 2), λfun. host("fn_new",[fun, flags_tuple, name_bin, len_i32, cons_list(capture_cell_handles)]))` — the returned `ir.Value` is the KFunction cell handle (D4).

**Capture order** (canonical): `info.captures` list-order, THEN `all_lexical_refs`-order subset present in `info.lexical_captures` (`arc/compiler.gleam:595-622`; `arc/vm/lexical.gleam:30-35`).

---

### M15 — `emit_2core/destructure.gleam`

**File:** `arc/src/arc/compiler/emit_2core/destructure.gleam`
**Source ports:** `arc/src/arc/compiler/emit.gleam:6038-6912`.

```gleam
pub type BindMode { Declare(kind: scope.BindingKind)  AssignExisting }

pub fn emit_pattern(e, pat: ast.Pattern, source: ir.Value, mode: BindMode) -> #(ir.Expr, Emitter2)
```

| Pattern | IR |
|---|---|
| `IdentifierPattern(n)` | `Declare` unboxed → `Let([slot_var], Values([source]), k)` + set_slot_var; boxed → `host_unit("cell_set",[cell_var, source])`. `AssignExisting` → M12's `emit_assign_identifier` |
| `AssignmentPattern(inner, default)` | `bind_if(host("strict_eq",[source, undef]), emit_named_expr(default, name?), pure(source))` → recurse inner |
| `ArrayPattern(elems)` | `bind(host("get_iterator",[source, sync]), λit. Try([], per-elem: bind(host("iter_next",[it]), λ#(done,v). if hole skip; if rest → collect_remaining; else recurse(elem, v)), [OnTag("js_exn"): host_unit("iter_close",[it]); rethrow]); host_unit("iter_close",[it]))` |
| `ObjectPattern(props)` | `host_unit("require_object_coercible",[source])`; per prop: `bind(host("get_prop",[source, key]), λv. recurse)`; `RestProperty` → `host("copy_data_props",[source, seen_keys_list])` |
| `RestElement(inner)` | (context-provided remaining) |

**Invariant:** evaluation order matches §13.15.5 (iterator protocol observably called; defaults evaluated only when undefined). **Arch-A (D2):** no `state_var` — every host op is emitted state-oblivious.

---

### M16 — `emit_2core/class.gleam`

**File:** `arc/src/arc/compiler/emit_2core/class.gleam`
**Source ports:** `arc/src/arc/compiler/emit.gleam:7074-7726`; reuses `ast_util.gleam:481-738` verbatim.

```gleam
pub fn emit_class(e, binding_name: Option(String), display_name: Option(String),
                  super_class: Option(ast.Expression), body: List(ast.ClassElement))
  -> #(ir.Expr, Emitter2)  // result Value = ctor cell handle
```

**Sequence** (§15.7.14):
1. `enter_scope` (class body scope); push `ClassCtx`; strict:=True.
2. Mint private names: per `#x` in `class_private_names(body)` → `bind(host("new_private_name",[ConstBinary("#x")]), λpk. bind_local("#x", pk))`.
3. Eval `super_expr` (or None).
4. Emit constructor via M14 (`ClassCtor(derived, has_field_init)`); default = `derived ? (…a)=>super(…a) : (){}`.
5. `bind(host("class_create",[ctor_code_fun, super?, name, is_derived, captures_list]), λ#(ctor_h, proto_h). …)`.
6. Fill `proto_home_cell`/`static_home_cell`/`ctor_self_cell` cells (were `cell_new(undef)`'d in step 1).
7. For each computed method key: `bind(emit_property_key(k), λkv. bind_local(computed_field_const(i), kv))`.
8. For each method: `emit_function(Method{…})` → `host_unit("define_method",[is_static?ctor_h:proto_h, key, fn_h, kind])`.
9. Instance fields → build field-init closure (`ClassInitFn`); `host_unit("set_fields_init",[ctor_h, init_h])`.
10. Static fields + static blocks → emit inline in source order with `Frame = {ctor_h, ctor_h, ctor_h, undef}`.
11. Fill `inner_name_cell` with `ctor_h` (TDZ→initialized).
12. `pop_frame`; `leave_scope`; result = `ctor_h`.

**Frame-relative access (D2 Arch-A — NO `lex_*` CallHost):** `super.k` read → `host("super_get",[tuple_get(Var("_frame"),2), tuple_get(Var("_frame"),0), key])` (home_object=idx 2, receiver=this=idx 0, both 0-based). `new.target` → `tuple_get(Var("_frame"),3)`. Private access: `#x` read → `host("private_get",[recv, Var(class_scope_binding("#x"))])`; `#x in obj` → `host("private_in",[obj, Var(..)])` (JRead). Private-name uid = threaded `Int` counter (`t_new_private_name` bumps `JsStore.private_uid`).

---

### M17 — `emit_2core/exn.gleam`

**File:** `arc/src/arc/compiler/emit_2core/exn.gleam`
**Source ports:** `arc/src/arc/compiler/emit.gleam:2287-2510` BarrierFrame crossing.

```gleam
pub const js_exn_tag = "js_exn"

pub fn emit_throw(e, arg: ast.Expression) -> #(ir.Expr, Emitter2)
    // = bind(emit_expr(arg), λv. Throw(js_exn_tag, [v]))  — 1 IR arg; emit_core prepends St (D6)

pub fn emit_try(e, block, tail: ast.TryTail, k: K) -> #(ir.Expr, Emitter2)

pub fn cross_barriers(e, target_depth: Int, then: ir.Expr) -> #(ir.Expr, Emitter2)
    // For each Barrier2{finally} between top and target: INLINE emit_stmts(finally); for iter_close: host_unit.
    // Called by M13's emit_break/emit_continue/emit_return before emitting the transfer.
```

**`emit_try` cases** (`CatchHandler` binds exactly ONE name — `["_e"]`; wire payload `[St,V]` is emit_core's concern):
- `TryCatch(block, param, catch_body)` → `Try([TTerm], emit_stmts(block), [CatchHandler(OnTag(js_exn_tag), ["_e"], None, {enter catch scope; emit_pattern(param, Var("_e"), Declare(Let)); emit_stmts(catch_body)})])`
- `TryFinally(block, finally)` → push `Barrier2(Some(finally, scope_save))`; emit as: `Try([TTerm], emit_stmts(block), [CatchHandler(OnTag(js_exn_tag), ["_e"], None, {emit_stmts(finally); Throw(js_exn_tag,[Var("_e")])})])` then `emit_stmts(finally)` (normal path); pop barrier.
- `TryCatchFinally` → nest: outer Try(finally) wrapping inner Try(catch).

**Finally-overrides-completion:** a `return`/`throw` INSIDE the inlined finally is emitted normally — it naturally supersedes the pending transfer since the transfer's `ir.Break/Return` comes AFTER the inline.

**Code-size note:** finally duplicated N+2 times for N crossing transfers. Pathological nesting (10-deep, many labels) is exponential — no mitigation in v1 (matches arc's Gosub-free semantics; add letrec-join if test262 surfaces a size failure).

---

### M18 — `emit_2core/async.gleam` (state-machine transform)

**File:** `arc/src/arc/compiler/emit_2core/async.gleam`
**Prior art:** regenerator, TypeScript `__generator`, Kotlin `ContinuationImpl.label`.
**Source anchors:** `arc/vm/value.gleam:4034-4048` SuspendedFrame, `:4080-4155` gen states.

```gleam
pub type CoroutineKind { CorGenerator  CorAsync  CorAsyncGen }

pub fn emit_coroutine_fn(e, kind: CoroutineKind, params, body, fn_scope_id, captures)
  -> #(Emitter2, ir.Value)  // returns the outer-fn closure value
```

**§18.1 Emitted shape — TWO `ir.Function`s:**

Outer `jsf_N` (params = `[caps…, _frame, _args]`):
```
prologue (unpack args, box locals-live-across-split into gen_state cell props);
bind(host("<kind>_start", [MakeClosure("jsf_N__sm", caps, 3), _frame, _args, initial_locals_tuple]), λh.
  Return([h]))   // h = generator cell OR result-promise cell
```

State-machine `jsf_N__sm` (params = `[caps…, "_rs", "_sent", "_loc"]` — D2 Arch-A: NO St param; rs:Int state id, sent:{mode:Int, value}, loc:locals-tuple):
```
Let([mode], TupleGet(0, _sent), Let([sent_v], TupleGet(1, _sent),
  Loop("Lresume", [LoopParam("rs",TTerm,_rs), LoopParam("loc",TTerm,_loc)], [TTerm],
    Switch(Convert(UnboxInt(W32), rs), [TTerm],
      [SwitchArm(0, Try([TTerm], <state-0 body>, [<arm-catch>])),
       SwitchArm(1, Try([TTerm], <state-1 body>, [<arm-catch>])), …],
      default: Return([make_tuple([ConstAtom("throw"), host("new_error",["invalid gen state"])])])))))
```

**Step wire type** (sm return value — 4 variants, NO Result wrapper):
```
Step = {return, V} | {throw, V} | {yield, V, Ns:Int, Loc'} | {await, V, Ns:Int, Loc'}
```

**§18.2 Split-point analysis (pass 1):** Walk body AST; assign state-id N=1..K to entry AFTER each `Await`/`Yield`/`YieldDelegate`/`ForAwait`-step. State 0 = entry. Track `TryEntry` regions containing ≥1 split (regions with 0 splits stay as ordinary M17 `ir.Try`).

**§18.3 Live-across-split (pass 2):** For each local slot in `FunctionInfo`, mark as HOISTED iff its scope binding spans any split point. Hoisted locals live at fixed indices in the `loc` tuple (stable layout — slot i = same local at every state). Non-hoisted stay as IR `Var`s.

**§18.4 Per-state-arm emission:** Each `SwitchArm(N, body_N)` is wrapped `Try([TTerm], body_N, [CatchHandler(OnTag(js_exn_tag), ["_e"], None, <arm-catch_N>)])` — a runtime throw from any CallHost inside the arm is caught locally and converted to a Step. `body_N`:
1. Restore hoisted locals: `Let([x_i], TupleGet(i, loc), …)`.
2. If arm N>0: `If(mode == 1 (throw), <Continue(Lresume,[catch_state, loc-with-caught=sent_v]) or Return([{throw, sent_v}]) if no enclosing try-region>, If(mode == 2 (return), <Continue(Lresume,[finally_state, loc-with-pending=return])>, continue))`.
3. Emit straight-line IR (via M12/M13 with `LocalStrategy = Hoisted(gen_state_cell)`) up to next split. **Before each throwing CallHost**, snapshot `loc_snap = MakeTuple([…live hoisted…])` so `<arm-catch_N>` has current locals.
4. At split: pack live hoisted into `loc' = MakeTuple([…])`; `Return([make_tuple([ConstAtom("await"|"yield"), val, ConstI32(next_state), loc'])])`.
5. Loop back-edge crossing a split: instead of `Continue`, emit split (state N → state at loop head).

`<arm-catch_N>`: if state N is inside a try-region → `Continue(Lresume, [catch_state, loc_snap-with-caught=_e])`; else → `Return([make_tuple([ConstAtom("throw"), Var("_e")])])`.

**§18.5 try/finally across splits:** `TryEntry{catch_state, finally_state, after_state}`. Throw inside try-region state → mode=1 dispatch (§18.4 step 2) jumps to `catch_state` (locals include caught value). `finally` is a state; a `pending: {kind:normal|return|throw|break, carry}` slot in `loc` tuple records what to do at finally-end (regenerator's `tryLocs` model).

**§18.6 yield* delegation:** Lowered ENTIRELY inside the sm — NO `{delegate,…}` Step variant, and NO dedicated host op — the arm dispatches on `mode` in-IR using only §8 ops (`get_iterator`/`get_prop`/`call`/`iter_close`). `yield* expr` emits a **self-looping state** `Nd`: (a) on entry from prior state, `iter_h = host("get_iterator",[expr, hint])` and `inner = host("get_prop",[iter_h, object_key_lit("iterator")])` are stored in `loc`; (b) arm `Nd` body: `mname = Switch(mode, [0→"next", 1→"throw", 2→"return"])`; `meth = host("get_prop",[inner, object_key_lit(mname)])`; **if `mode≠0 ∧ host("strict_eq",[meth, undef])`** → `mode==1`: `host_unit("iter_close",[iter_h, True])` then `Return([{throw, sent_v}])`; `mode==2`: `Continue(Lresume,[Nd+1, loc-with-result=sent_v])`. Otherwise `res = host("call",[meth, inner, cons_list([sent_v])])`; `done = host("truthy",[host("get_prop",[res, object_key_lit("done")])])`; `v = host("get_prop",[res, object_key_lit("value")])`; `If(done, mode==2 ? Return([{return, v}]) : Continue(Lresume,[Nd+1, loc-with-result=v]), Return([{yield, v, Nd, loc}]))` — resumption re-enters `Nd` with the caller's next `(mode, sent)`, forwarding it verbatim to the inner iterator.

**§18.7 async-gen `yield x`:** = `Await(x)` then `Yield` (tc39/ecma262#2819; `arc/emit.gleam:4936-4939`) — two consecutive splits.

**Invariants:** `loc` tuple layout stable per function; state 0 ignores `sent`; every `SwitchArm` body is wrapped in exactly one `ir.Try` (per-arm catch); non-split try uses M17's `ir.Try` normally (nested inside the arm-Try). sm return value is always a bare `Step` tuple — NEVER a `Result(..)`.

---

### M19 — `emit_2core.gleam` + `emit_2core/module.gleam` (module driver)

**Files:** `arc/src/arc/compiler/emit_2core.gleam` (façade) + `arc/src/arc/compiler/emit_2core/module.gleam` (driver) + `2core/src/twocore/pipeline.gleam` additions + `2core/src/twocore/instance/profiles.gleam` addition + `arc/gleam.toml` edit.

**§19.1 Façade types**
```gleam
pub type SourceKind { AsScript  AsModule }
pub type CompileOpts { CompileOpts(module_name: String, source_kind: SourceKind, entry_name: String) }
pub type EmitError { ParseFailed(parser.ParseError)  Unsupported(String)  EarlySyntaxError(String)  Internal(String) }
pub type CompiledUnit { CompiledUnit(module: ir.Module, tree: scope.ScopeTree, is_strict: Bool) }

pub fn compile(prog: ast.Program, sb: scope.ScopeBuilder, opts: CompileOpts) -> Result(CompiledUnit, EmitError)
pub fn compile_source(source: String, opts: CompileOpts) -> Result(CompiledUnit, EmitError)
pub fn binding() -> instance.Binding   // = profiles.js_direct()
```

**§19.2 `init_emitter` — the ONE place `EmitDispatch` is constructed (D13)**
```gleam
// module.gleam
pub fn init_emitter(tree: scope.ScopeTree, opts: CompileOpts) -> Emitter2 {
  let dispatch = state.EmitDispatch(
    emit_expr:        expr.emit_expr,
    emit_stmts:       stmt.emit_stmts,
    emit_pattern:     destructure.emit_pattern,
    emit_function:    func.emit_function,
    emit_class:       class.emit_class,
    emit_async_body:  async.emit_coroutine_fn,
    emit_destructure: destructure.emit_pattern,   // alias; M15 owns both bind-modes
  )
  state.new_emitter(tree, tree.root_scope, opts.source_kind == AsModule, dispatch)
}
```
All seven fields are wired here; no emit module imports another emit module directly.

**§19.3 `js_main` signature (D2 + D3)**
- **IR level:** `ir.Function{ name:"js_main", params:["_frame","_args"], result:[TTerm], body:… }` — two IR params, state invisible.
- **Wire level:** `emit_core` under `Threading(cur)` ∧ `js_profile:True` prepends `St` ⇒ BEAM arity **3**.
- **Invocation:** `erlang:apply(Mod, js_main, [St0, {undefined,undefined,undefined,undefined}, []])` — `_frame` is the 4-tuple `{this,active_func,home_object,new_target}` (all `undefined` at top level, M-CALL), `_args` is `[]`.
- **Return:** `{JsVal, InstanceState}` on normal completion; uncaught top-level throw surfaces as `error:{wasm_exn,0,[St',E]}` (§4).

**§19.4 Tag emission (D6)**
`ir.Module.tags = [TagDecl("js_exn", [TTerm])]` — exactly one entry, module-local index `0`. IR-level payload arity is 1 (`[v]`); `emit_core` owns the wire encoding `{wasm_exn,0,[St,V]}`. M17 emits `Throw("js_exn",[v])` / `CatchHandler(OnTag("js_exn"),["_e"],…)` against this constant; `pub const js_exn_tag = "js_exn"` is re-exported here for callers.

**§19.5 Driver algorithm (`compile`)**
1. `scope.finalize(sb, AnalyzeOpts{top_lex:LexLocal, fallthrough:ToGlobal, strict: opts.source_kind==AsModule, …})` (`arc/compiler.gleam:281-288`).
2. `let e0 = init_emitter(tree, opts)`.
3. **Hoist** top-level `FunctionDeclaration`s (§8.1.4 GlobalDeclarationInstantiation): for each, `bind(e.dispatch.emit_function(…), λfn_h. host_unit("global_set",[ConstBinary(name), fn_h]))` — emitted BEFORE any statement so forward refs resolve.
4. `let #(body_ir, e1) = e0.dispatch.emit_stmts(e0, prog.body, fn(_e){ ir.Values([anf.undef()]) })`.
5. Tail: `host_unit("drain_microtasks", [])` then `ir.Return([anf.undef()])`. NO `init_realm` call in the body — realm init is `run_js_beam`'s responsibility (§19.8) so multiple scripts share one realm.
6. `let js_main = ir.Function{name:"js_main", exported:True, params:["_frame","_args"], param_types:[TTerm,TTerm], result:[TTerm], locals:[], body: seq([..hoists, body_ir, drain, ret])}`.
7. Assemble `ir.Module{ name:opts.module_name, uses_numerics:True, memories:[], globals:[], tags:[TagDecl("js_exn",[TTerm])], imports:[], functions:[js_main, ..list.reverse(e1.fns_acc)], exports:[ExportFn(opts.entry_name,"js_main")], start:None }`.
8. Wrap `Ok(CompiledUnit(module, tree, is_strict))`; any `EarlySyntaxError`/`Unsupported` bubbled from a sub-emitter short-circuits via `Result`.

**§19.6 ESM scope — v1 is single-unit (Q7 resolved)**
`AsModule` toggles strict mode + top-level lexical scoping only. v1 does NOT implement a module graph:
- `ImportDeclaration` → `Error(Unsupported("ESM import (v1 single-unit)"))`.
- `ExportNamedDeclaration`/`ExportDefault` with a local binding → emit the binding, drop the export marker (no linker to consume it).
- `export … from "…"` / `import()` expression → `Error(Unsupported("ESM re-export / dynamic import (v1)"))`.
- `ModuleNamespace` `ObjKind` exists in M1a as a stub (`t_get_prop` on it throws `TypeError`).
- **Deferred to v2:** HostResolveImportedModule / HostLoadImportedModule hooks, module linking, live bindings, top-level `await`, `import.meta`. Tracked in §11.

**§19.7 Indirect-eval hook (D15)**
`JsStore.ops` (D17) carries `eval_hook: fn(InstanceState, String) -> #(JsVal, InstanceState)`. M6's `eval` KNative dispatch reads it:
- Default (`rt_js_builtins.init_realm`) seeds a stub that throws `EvalError("eval not available")`.
- M20's harness overrides it with `fn(st, src){ compile_source(src,…) |> compile_ir |> load |> run_js_beam_in(st,_) }` — indirect eval runs in the GLOBAL scope of the SAME realm.
- Direct `eval` (identifier `eval` called syntactically with local-scope access) remains `Unsupported` (D15) — detected by M12 at emit time.

**§19.8 `pipeline.gleam` additions**
```gleam
pub fn compile_ir(module: ir.Module, binding: Binding) -> Result(BitArray, PipelineError)
    // = ir_to_lowered_core → emit_core(binding) → core_to_beam. Skips wasm-frontend stages.

pub fn run_js_beam(beam: BitArray, mod_atom: Atom, hooks: HostHooks) -> #(InstanceState, DiffRun)
    // Fresh realm. St0 = rt_state.fresh_full(…) |> js_store := rt_js_store.t_store_new(hooks)
    //   |> rt_js_builtins.init_realm; then run_js_beam_in(St0, beam, mod_atom).

pub fn run_js_beam_in(st: InstanceState, beam: BitArray, mod_atom: Atom) -> #(InstanceState, DiffRun)
    // Existing realm ($262.evalScript, indirect eval). code:load_binary; try apply(Mod, js_main,
    //   [st, {undefined×4}, []]) catch error:{wasm_exn,0,[St2,E]} -> {St2, Threw(E)} end;
    //   St' = rt_js_gc.t_maybe_collect(St')  — the second D11 turn-boundary safepoint;
    //   drain result into DiffRun{stdout: t_console_bytes(St'), result}.
```

**§19.9 `profiles.js_direct()` (D3/D18)**
```gleam
pub fn js_direct() -> Binding {
  Binding(..safe_default(),
    threading_initial: ThreadFromCaller,
    js_profile: True,                 // D18 canonical flag name
    meter: MeterOff,
    host_modules: js_rt_module_table())  // resolve_js target atoms
}
```
`js_profile:True` is what gates M9 §9.8-§9.11 threaded-closure emission; the WASM profile leaves it `False` for byte-identical output.

**Invariants:** `js_main` never appears in `fns_acc` (driver prepends it once); every `ir.Function` in the output has `params` beginning with its capture list then `["_frame","_args"]` (or `["_rs","_sent","_loc"]` for `*__sm`); `tags` list length is exactly 1.

---

### M20 — Test harness

**Files:** `arc/test/emit_2core_harness.gleam`, `arc/test/emit_2core_diff_test.gleam`, `arc/test/emit_2core_test262.gleam`, `arc/test/arc_emit_2core_test_ffi.erl`, `2core/test/twocore/runtime/rt_js_*_test.gleam`.

```gleam
pub type DiffRun { DiffRun(stdout: BitArray, result: Result(Dynamic, String)) }
pub fn run_compiled(source: String) -> DiffRun
pub fn run_interpreted(source: String) -> DiffRun    // via arc/engine, group_leader capture
pub fn diff(src: String) -> DiffVerdict
pub fn test_hooks() -> HostHooks   // deterministic: fixed epoch clock, seeded xorshift RNG, stub sleep
```

**Deterministic `HostHooks`:** the harness constructs `HostHooks{ monotonic_now: fn(){ 1_700_000_000_000 }, random: seeded_xorshift(0xC0FFEE), sleep_ms: fn(_){ Nil }, print: fn(s){ push console_buf } }` and passes it to BOTH `run_js_beam` (via `t_store_new(hooks)`) and the arc interpreter (`engine.new_with_hooks`). This is what makes `Date.now()`, `Math.random()`, and `performance.now()` output byte-comparable in the differential. The harness ALSO seeds `store.ops.eval_hook` (§19.7) so `$262.evalScript` and indirect `eval` work under test262.

**`run_compiled` flow:** `parser.parse_script` → `emit_2core.compile_source` → `pipeline.compile_ir(mod, profiles.js_direct())` → `code:load_binary` → `run_js_beam_in(St0, beam, mod)` where `St0` was built with `test_hooks()`; on `error:{wasm_exn,0,[St',E]}` recover partial output from `St'.js_store.console_buf` → `rt_js_store.t_console_bytes(St')`.

**`run_interpreted`:** `capture_stdout(fn(){ engine.new_with_hooks(test_hooks()) |> engine.eval(src) })` via group_leader FFI redirect (arc's `console.gleam:45` does `io.println`).

**Corpus:** reuse `2core/test/twocore/js/corpus/` categories + new `async/`, `class/`, `destructure/`, `exn/` dirs. Keystone tests (must-Match): fibonacci, closures-counter, prototype-chain, try-finally-return, async-await-chain, generator-delegation, class-private-super.

**test262 mode:** fork `arc/test/test262_exec.gleam`; swap execute step from interpreter to `run_compiled`; concatenate harness+test source before single compile (simple; recompiles harness per test — acceptable v1); separate snapshot `test/emit_2core_test262_snapshot.txt`. Skip: `$262.agent`, `$262.createRealm`, `Proxy`, direct-eval (D15).

---

## §8 Complete `resolve_js` Op Table (M9)

`resolve_js(op) -> Option(#(JsRtModule, String, JsOpKind))`. Every `CallHost("js", op, …)` any emit module produces:

| Op | Module | Fn | Kind | Arity | Used by |
|---|---|---|---|---|---|
| `cell_new` | JsStore | `t_cell_new` | JMut | 1 | M13 var-decl, M14 prologue |
| `cell_get` | JsStore | `t_cell_get` | JRead | 1 | M12 boxed-local read |
| `cell_set` | JsStore | `t_cell_set` | JMutUnit | 2 | M12/M13 boxed write |
| `pin_root` | JsStore | `t_pin_root` | JMutUnit | 1 | M14 closure-capture |
| `tuple_set` | JsStore | `t_tuple_set` | JMutUnit | 3 | M18 sm locals snapshot |
| `console_log` | JsStore | `t_console_write` | JMutUnit | 1 | M6 console |
| `collect` | JsGc | `t_collect` | JMutUnit | 0 | explicit gc() |
| `to_primitive` | JsVal | `t_to_primitive` | JMut | 2 | M5 |
| `to_number` | JsVal | `t_to_number` | JMut | 1 | M12 unary+ |
| `to_string` | JsVal | `t_to_string` | JMut | 1 | M12 template |
| `to_property_key` | JsVal | `t_to_property_key` | JMut | 1 | M12 computed member |
| `to_object` | JsVal | `t_to_object` | JMut | 1 | M15 obj-pattern |
| `type_of` | JsVal | `t_type_of` | JMut | 1 | M12 typeof (D4 — reads cell) |
| `is_callable` | JsVal | `t_is_callable` | JMut | 1 | M12 |
| `require_object_coercible` | JsVal | `t_require_object_coercible` | JMutUnit | 1 | M15 |
| `throw_type_error` | JsVal | `t_throw_type_error` | JMutUnit | 1 | (diverges) |
| `truthy` | JsVal | `to_boolean_i32` | JPure | 1 | M11 truthy_if |
| `is_nullish` | JsVal | `is_nullish` | JPure | 1 | M11 nullish_if |
| `float_lit` | JsVal | `float_from_bits` | JPure | 1 | M12 number lit |
| `empty_list` | JsVal | `empty_list` | JPure | 0 | M11 cons_list |
| `string_concat` | JsVal | `string_concat` | JPure | 2 | M12 template |
| `inspect` | JsInspect | `t_inspect` | JRead | 1 | M6 console/util.inspect |
| `new_object` | JsObj | `t_new_object` | JMut | 1 | M12 obj-lit |
| `new_array` | JsObj | `t_new_array` | JMut | 1 | M12 arr-lit |
| `new_error` | JsObj | `t_new_error` | JMut | 2 | M17 |
| `fn_new` | JsObj | `t_new_function` | JMut | 5 | M14 (D4) |
| `new_arguments` | JsObj | `t_new_arguments` | JMut | 2 | M14 |
| `get_prop` | JsObj | `t_get_prop` | JMut | 2 | M12 |
| `set_prop` | JsObj | `t_set_prop` | JMut | 3 | M12 |
| `define_prop` | JsObj | `t_define_prop` | JMut | 3 | M12 obj-lit |
| `has_prop` | JsObj | `t_has_prop` | JMut | 2 | M12 `in` |
| `delete_prop` | JsObj | `t_delete_prop` | JMut | 2 | M12 delete |
| `own_keys` | JsObj | `t_own_keys` | JMut | 1 | M15 rest |
| `copy_data_props` | JsObj | `t_copy_data_props` | JMut | 2 | M12 spread, M15 rest |
| `get_iterator` | JsObj | `t_get_iterator` | JMut | 2 | M13/M15 |
| `iter_next` | JsObj | `t_iter_next` | JMut | 1 | M13/M15 |
| `iter_close` | JsObj | `t_iter_close` | JMutUnit | 2 | M13/M15/M17 (iter, abrupt:Bool) |
| `for_in_keys` | JsObj | `t_for_in_keys` | JMut | 1 | M13 |
| `spread_into_list` | JsObj | `t_spread_into_list` | JMut | 2 | M12 args |
| `global_get` | JsObj | `t_global_get` | JMut | 1 | M12 |
| `global_set` | JsObj | `t_global_set` | JMutUnit | 2 | M12/M13 |
| `module_get` | JsObj | `t_module_get` | JMut | 2 | M19 ESM |
| `module_set` | JsObj | `t_module_set` | JMutUnit | 3 | M19 ESM |
| `add`/`sub`/`mul`/`div`/`mod`/`pow` | JsOps | `t_*` | JMut | 2 | M11 guarded_binop slow |
| `neg`/`plus`/`bitnot` | JsOps | `t_*` | JMut | 1 | M12 |
| `bitand`/`bitor`/`bitxor`/`shl`/`shr`/`ushr` | JsOps | `t_*` | JMut | 2 | M12 |
| `lt`/`le`/`gt`/`ge`/`eq` | JsOps | `t_*` | JMut | 2 | M11 guarded_cmp slow |
| `strict_eq`/`strict_ne` | JsOps | `t_strict_eq`/`t_strict_ne` | JPure | 2 | M11/M12/M15 |
| `op_in` | JsOps | `t_in` | JMut | 2 | M12 `in` (ToPropertyKey + HasProperty) |
| `instance_of` | JsOps | `t_instance_of` | JMut | 2 | M12 |
| `call` | JsCall | `t_call_checked` | JMut | 3 | M12 (D4) |
| `construct` | JsCall | `t_construct` | JMut | 3 | M12 new |
| `new_private_name` | JsClass | `t_new_private_name` | JMut | 1 | M16 |
| `class_create` | JsClass | `t_class_create` | JMut | 5 | M16 |
| `define_method` | JsClass | `t_define_method` | JMutUnit | 4 | M16 |
| `set_fields_init` | JsClass | `t_set_fields_init` | JMutUnit | 2 | M16 |
| `private_get`/`private_set`/`private_define`/`private_has` | JsClass | `t_*` | JMut | 2-3 | M12/M16 |
| `private_in` | JsClass | `t_private_in` | JRead | 2 | M12 `#x in obj` |
| `fn_home_object` | JsClass | `t_fn_home_object` | JRead | 1 | M16 super-binding |
| `is_constructor` | JsClass | `t_is_constructor` | JRead | 1 | M12/M-CALL |
| `super_get`/`super_set`/`super_call` | JsClass | `t_*` | JMut | 3-4 | M12 |
| `async_start`/`gen_start`/`asyncgen_start` | JsAsync | `t_*` | JMut | 4 | M18 |
| `async_iter_next` | JsAsync | `t_async_iter_next` | JMut | 1 | M18 for-await |
| `await` | JsAsync | `t_await` | JMutUnit | 4 | M18 sm |
| `drain_microtasks` | JsAsync | `t_drain_microtasks` | JMutUnit | 0 | M19 |
| `init_realm` | JsBuiltins | `init_realm` | JMutUnit | 0 | M19 |

Note: `maybe_collect` is NOT a `CallHost("js",…)` op — per D11 turn-boundary GC, only `t_drain_microtasks` (M8) calls `t_maybe_collect` internally.

---

## §9 M18 Worked Example

Input:
```js
async function f() {
  let x = 1;
  try { x = await p; } finally { g(); }
  return x;
}
```

**Split analysis:** 1 split (the `await`). States: 0=entry, 1=after-await. `x` is live-across-split → hoisted to `loc[0]`. Try-region contains the split → `TryEntry{id:0, catch_state:None, finally_state:2, after_state:3}`. So states: 0=entry, 1=resume-in-try, 2=finally, 3=after-finally. `pending` slot at `loc[1]`.

**Outer `jsf_f`** (params `[_frame, _args]`):
```
Let([loc0], MakeTuple([ConstI32(1), ConstAtom("normal")]),           // x=1, pending=normal
  Let([sm], MakeClosure("jsf_f__sm", [], 3),
    Let([h], CallHost("js","async_start",[sm, _frame, _args, loc0]),
      Return([h]))))                                                  // h = result promise
```

**State-machine `jsf_f__sm`** (params `[_rs, _sent, _loc]`):
```
Let([mode], TupleGet(0,_sent), Let([sv], TupleGet(1,_sent),
Loop("Lr", [LoopParam("rs",_,_rs), LoopParam("loc",_,_loc)], [TTerm],
Switch(UnboxInt(rs), [TTerm], [

  SwitchArm(0,    // entry: enter try, eval p, suspend
    Let([x], TupleGet(0,loc),
    Let([p], CallHost("js","global_get",[ConstBinary("p")]),
    Let([loc'], MakeTuple([x, ConstAtom("normal")]),
    Return([MakeTuple([ConstAtom("await"), p, ConstI32(1), loc'])]))))),

  SwitchArm(1,    // resume inside try; sent = {mode, awaited-result}
    Let([x], TupleGet(0,loc),
    If(I32Eq(mode, ConstI32(1)),   // awaited promise rejected
       // no catch → pending={throw, sv}; jump to finally
       Let([loc'], MakeTuple([x, MakeTuple([ConstAtom("throw"), sv])]),
         Continue("Lr", [ConstI32(2), loc'])),
       // fulfilled: x = sv; try body done → pending=normal; jump to finally
       Let([loc'], MakeTuple([sv, ConstAtom("normal")]),
         Continue("Lr", [ConstI32(2), loc']))))),

  SwitchArm(2,    // finally: g(); then dispatch on pending
    Let([x], TupleGet(0,loc), Let([pend], TupleGet(1,loc),
    Let([_], CallHost("js","call",[CallHost("js","global_get",[ConstBinary("g")]), ConstAtom("undefined"), empty_list]),
    // dispatch pending
    If(TermTest(IsAtom, pend),          // "normal"
       Continue("Lr", [ConstI32(3), loc]),
       // pending is {throw, e} tuple
       Let([e], TupleGet(1, pend),
         Return([MakeTuple([ConstAtom("throw"), e])]))))))),

  SwitchArm(3,    // after-finally: return x
    Let([x], TupleGet(0,loc),
    Return([MakeTuple([ConstAtom("return"), x])]))),

], default: Return([MakeTuple([ConstAtom("throw"), CallHost("js","new_error",[…"bad state"])])])))))
```

M8's `t_async_start` calls `sm(St, 0, {0,undef}, loc0)`; sees `{await, p, 1, loc'}`; does `PromiseResolve(p).then(λv. sm(St', 1, {0,v}, loc'), λe. sm(St', 1, {1,e}, loc'))`; on `{return, x}` → resolve result-promise with x; on `{throw, e}` → reject.

---

## §10 arc FFI Port Table (D12)

Mechanical rewrite on copy: `{finite, F}`→`F`, `na_n`→`js_nan`, `infinity`→`js_inf`, `neg_infinity`→`js_neg_inf`.

| arc file | 2core file | LoC | owner | shared by |
|---|---|---|---|---|
| `arc/vm/internal/arc_tree_array_ffi.erl` | `twocore_rt_js_tree_array_ffi.erl` | 66 | M4 (Elements) | M6 |
| `arc/internal/ordered_entries.gleam` | `twocore/runtime/rt_js_ordered_entries.gleam` (vendored Gleam, not FFI) | ~180 | M4 (MapObj/SetObj) | M1a (MapKey) |
| **NEW** — no arc source | `twocore_rt_js_val_ffi.erl` — `classify/1` (D16) | ~15 clauses | M3 | M4 M5 |
| `arc/vm/internal/arc_job_queue_ffi.erl` | `twocore_rt_js_queue_ffi.erl` | 16 | M8 | M1 |
| `arc/vm/arc_string_ffi.erl` | `twocore_rt_js_string_ffi.erl` | 245 | M3 | M6 |
| `arc/vm/builtins/arc_number_ffi.erl` | `twocore_rt_js_number_ffi.erl` | 225 | M3 | M6 |
| `arc/vm/builtins/arc_math_ffi.erl` | `twocore_rt_js_math_ffi.erl` | 144 (§JsNum) | M6 | M5 (pow, is_neg_zero) |
| `arc/parser/arc_float_ffi.erl` | `twocore_rt_js_float_ffi.erl` | 108 (§JsNum) | M3 | M6 |
| `arc/vm/arc_bytes_ffi.erl` | `twocore_rt_js_bytes_ffi.erl` | 49 | M6 (regexp) | — |
| `arc/vm/builtins/arc_regexp_ffi.erl` + `arc_regex_{props,uni17,…}_ffi.erl` (7 files) | `twocore_rt_js_regexp_ffi.erl` + siblings | ~6400 | M6 | — |
| `arc/vm/builtins/arc_tz_ffi.erl` + `arc_tzif.erl` + `arc_clock_ffi.erl` | `twocore_rt_js_tz_ffi.erl` + siblings | ~800 | M6 (Date) | — |
| `arc/vm/builtins/arc_typed_array_ffi.erl` | `twocore_rt_js_typed_array_ffi.erl` | 145 (§JsNum) | M6 | — |
| `arc/vm/builtins/arc_uri_ffi.erl` | `twocore_rt_js_uri_ffi.erl` | 177 | M6 (encodeURI) | — |
| `arc/internal/arc_escape_ffi.erl` + `arc_unicode_ffi.erl` | `twocore_rt_js_unicode_ffi.erl` | ~200 | M3 | M6 |
| `arc/vm/internal/arc_tuple_array_ffi.erl` | **NOT PORTED** | — | (bytecode-interpreter only) | — |
| `arc/vm/arc_vm_ffi.erl:next_prop_seq` | **NOT PORTED** | — | replaced by threaded `JsStore.prop_seq` (D14) | — |
| `arc/vm/arc_vm_ffi.erl:make_ref/0` | **NOT PORTED** | — | replaced by threaded `JsStore.symbol_uid` (D14); no FFI file exports `make_ref/0` | — |

---

## §11 Open Questions / Risks

Open design questions raised by the adversarial review (spec-review PART 6) with the review's RECOMMENDED resolution recorded. These are picks, not unknowns — each is adopted by the D-table / RULINGS.md; listed here for traceability.

| Q# | Question | Recommended resolution | Adopted as |
|---|---|---|---|
| **Q1** | GC safepoint model — fn-entry prologue vs turn-boundary-only? | **Turn-boundary-only.** `t_maybe_collect` gated on `store.call_depth==0`, called ONLY from `t_drain_microtasks` between jobs + after `js_main` return. NO fn-entry prologue. Matches arc `interpreter.gleam:5806-5827` `call_stack==[]` gate. `extra_roots` reserved for v2. | D11 |
| **Q2** | JsVal representation — `Dynamic` everywhere vs opaque type? | **Opaque `JsVal` + `classify/1` FFI.** rt_js Gleam works with `JsValKind` sum; `classify/1` (~15 Erlang clauses) is the ONE decode point. Eliminates `Dynamic` from Property/SBox/Dense/Sparse/etc. | D16 |
| **Q3** | Exception wire-payload order — `[St,V]` or `[V,St]`? | **`[St,V]` — St first.** Matches arg-1-everywhere; `t_call_protected` (`emit_core.gleam:627`) already unpacks this order. | D6 / R2 |
| **Q4** | `Binding` flag name for the JS threading profile? | **`js_profile: Bool`** (NOT `uniform_state_threading`). §9.10/§9.11 gate on `ctx.binding.js_profile`; WASM path byte-identical when `False`. | D18 / R3 |
| **Q5** | String `<` comparison semantics? | **Byte-wise binary compare** (UTF-8 = code-POINT order). Carries arc's known divergence from spec UTF-16 code-UNIT order for astral vs U+E000..U+FFFF. `utf16_compare` DELETED. | D10 |
| **Q6** | `yield*` lowering — separate `{delegate,…}` step vs inside-SM? | **Inside-SM self-looping state.** No `{delegate}` Step variant; `yield*` compiles to a self-looping SwitchArm that drives the inner iterator until done. | §7.M18 |
| **Q7** | ESM scope for v1? | **Single-unit only.** `import`/`export` statements → `Error(Unsupported("ESM v1"))`; `ModuleNamespace` ObjKind stubbed. Deferred: HostResolveImportedModule, live bindings, TLA. | §7.M19 |
| **Q8** | `PropertyKey.Named` encoding — `BitArray` or `String`? | **`String`.** JS property keys are Unicode strings; `BitArray` was an arc-interpreter artifact. | §2.4 |
| **Q9** | `KNative` dispatch key — `Int` tag or sum-type? | **`NativeToken` sum-type.** `KNative(tag: NativeToken)`; `dispatch_native` matches exhaustively. | R10 / G20 |
| **Q10** | Add `TupleSet` IR TermOp? | **Yes — add it.** Needed for M18 state-machine locals-snapshot mutation without rebuilding the whole tuple. | §7.M9 |
| **Q11** | Indirect-eval / `$262.evalScript` hook shape? | **`JsStore.ops.eval_hook: fn(InstanceState,String) -> #(JsVal,InstanceState)`**, seeded by M20 harness with parse+compile+load. Direct `eval` remains D15 `Unsupported`. | §7.M19 |

### Low-confidence risks (spec-review PART 7)

Not blocking; each has a cheap verification step before the owning module lands.

1. **`erlang:fun_info(F, env)` stability** (M2 `refs_in_term`) — documented-stable OTP API for LOCAL funs; verified `emit_core.gleam:1515` emits `CFun` (local, env visible) not `fun M:F/A` (external, env=[]). Re-verify on any `emit_make_closure` rewrite.
2. **`scope.is_boxed` extension** (M10) — arc's `is_boxed` marks captured mutables only, NOT same-frame reassigns. M10 must walk `Assignment(Identifier)` targets during `enter_function` and force-box (or SSA-rebind) those slots. 3-line check: an unboxed `let x=1; x=2; x` round-trips.
3. **JsOps import-cycle** (D17) — `rt_js_val` importing `rt_js_types.{JsOps}` while `rt_js_types` names function types over `JsVal` is fine (Gleam allows mutually-recursive types in one module); the risk is `rt_js_store` importing a module that imports `rt_js_store`. 3-line check: `gleam build` after M1a+M1b+M3 land with `store.ops` typed.
4. **`classify/1` perf** (D16) — ~15-clause Erlang pattern-match on the hot path of every coercion. Expected fine (BEAM compiles to jump table); if profiling shows it hot, split into `is_object`/`is_number` fast-path guards + `classify_slow`.
