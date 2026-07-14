# u-call-abi — JS CALL ABI (FINAL)

## Ground-truth refs

**2core Threading return convention** — `{Package, State'}`, State LAST in tuple, State FIRST in params:
- `emit_core.gleam:1559` `Threading(cur) -> CTuple([pkg, CVar(cur)])`
- `emit_core.gleam:3467` `CApply(FName(fn_name, arity+1), [CVar(cur), ..cargs])`
- `emit_core.gleam:3508-3509` unpack `PTuple([PVar(pkg), PVar(st)])`
- `emit_core.gleam:651` `params = [st0, ..f.params]`
- `twocore_rt_gc_ffi.erl:206-208` `t_call_ref(St, {_,Closure}, Args, RC) -> {Pkg,St2}=Closure(St,Args)`
→ **Task brief's `{State',Result}` is corrected to `{Result, State'}`** to match all existing 2core threaded machinery.

**MakeClosure/CallClosure state-neutral today** — MUST change (M9):
- `emit_core.gleam:1490-1492` "closure body is UN-threaded direct call … state-threaded build … out of scope"
- `emit_core.gleam:1530-1535` "State-NEUTRAL under Threading … native BEAM fun does not carry our InstanceState"

**Threaded+EH unsupported today** — MUST change (M9):
- `emit_core.gleam:5127-5128` "Cell-only (T6): … no state travels in the throw — Threaded+EH is a categorised-unsupported combo"

**arc lexical quartet** (`src/arc/vm/lexical.gleam:21-35`): `RefThis, RefActiveFunc, RefHomeObject, RefNewTarget` in that fixed order (`all_lexical_refs`). Non-arrows OWN all four; arrows CAPTURE per-ref (`scope.gleam:248` `lexical_captures: Dict(LexicalRef,Int)`).

**arc call dispatch** (`src/arc/vm/exec/call.gleam:1619-1716` `call_value`): case on heap-read kind → `FunctionObject` / `NativeFunction` / `ProxyObject(callable)` / else TypeError. `do_construct` (1258-1400): IsConstructor gate → base (alloc `this` from `newTarget.prototype`) / derived (`this=JsUninitialized`) / bound (unwrap, rebind nt) / native / proxy.

**arc frame seed** (`frame.gleam:80-139`): `active_func` = callee ref, `home_object` = read from callee's `FunctionObject.home_object` field (set at DEFINE time), `this`+`new_target` = per-call from caller.

**arc cycle-break** (`state.gleam:645-684`): `state.call/construct/construct_with_target` are fn-pointers on `state.ctx`, read from state and applied — the reentrancy seam every rt_js builtin uses.

**BEAM fun identity** (verified via erl): `fun(X)->X end =:= fun(X)->X end` → `true`. Two distinct JS function-expressions with same body would compare `===` under raw-fun repr — **wrong**.

**arc FuncTemplate flags** (`value.gleam:198-241`): `name, arity, length, is_strict, is_arrow, is_derived_constructor, is_generator, is_async, is_constructor, is_class_constructor`.

---

## D1 — FUNCTION-VALUE REPR: **cell handle** (option b)

**A JS function value = `{js_cell, Int}` (a `Handle`)** whose `JsSlot` is `SObject{kind: KFunction{…} | KNative{…} | KBound{…}, properties, prototype: Some(function_proto_id), …}`. The BEAM `fun/3` is stored **inside** the cell's `ObjKind`, never observed directly by JS.

**Why not raw fun (option a):**
1. **Identity broken** — BEAM `=:=` on funs is by (module,index,uniq,env), so `(function(){}) === (function(){})` would be `true`. Verified above.
2. **Side-table keying impossible** — `fun_info(F,uniq)` is a code hash, not identity; two closures over different captures may collide or two same-capture funs are indistinguishable.
3. **Proxy/bound already force cell dispatch** — `new Proxy(f,{apply})` and `f.bind(x)` produce objects, not funs; `t_call` must case on cell kind anyway (arc `call_value` 1627-1715).
4. **Mutable props** — `f.foo=1`, `f.prototype=X`, `Object.defineProperty(f,'name',…)` all need per-function object storage.

**Why not hybrid side-table (option c):** GC must trace side-table Dict-of-funs; fun equality (per #1) makes the key unstable; every `t_fn_new` must both create cell AND insert into table; adds a second source of truth for "what is this function's props".

**typeof:** `rt_js_val.type_of(st, v)` reads cell → `"function"` when `ObjKind` is `KFunction | KNative | KBound | KProxy{callable:True}`. Raw-fun `is_function/1` guard is REMOVED from rt_js_ffi.erl:91,391.

**Fast-path escape hatch (M12, OPTIONAL, correctness-neutral):** when `emit_2core/expr` compiles `f(args)` where `scope.lookup("f")` yields `Binding{is_boxed:False, kind:FunctionDecl}` (never reassigned), emit `CallDirect("js_fn_N", [frame, args_list])` instead of `CallHost("js","call",…)`. `frame` still built with `active_func = f_handle` (the cell exists — declaration hoisting created it). Skipped in v1; the ABI is designed so adding it later changes no types.

---

## D2 — CTX/THIS TRANSPORT: **`Frame` 4-tuple as param 2**

**Compiled non-arrow signature:** `f(St, Frame, Args) -> {Result, St'}` where **`Frame = {ThisVal, ActiveFunc, HomeObj, NewTarget}`** — a plain Erlang 4-tuple in `all_lexical_refs` order (arc `lexical.gleam:30-35`). All four elements are `JsVal` terms (D16).

**Rationale:**
- **Rejects M7-A2 (fields on InstanceState):** arc does this (`call.gleam:1187-1188,1205-1206` save/set/restore `state.new_target`) and it's the most fragile part — every call site hand-writes save/restore, `finally` must restore, GC must trace it, and it makes InstanceState non-reentrant across `t_call`. 2 record-rebuilds per call.
- **Rejects M16 (`{this,nt}` + home-as-capture[0]):** `home_object` is a per-function-OBJECT constant, not a per-closure constant. Set by `t_class_define_method` AFTER the closure exists (arc `FunctionObject.home_object` value.gleam:3387). Can't be a capture. `active_func` = the cell wrapping the fun — allocated AFTER the fun; also can't be a capture.
- **4-tuple** is arc's exact model as one value: `frame.gleam:123-136` seeds `[this_val, JsObject(fn_ref), home, new_target]` in that order.
- **Per-call vs per-function split:** `this`+`new_target` come from the CALL SITE. `active_func`+`home_object` come from the CALLEE CELL. `t_call` reads the latter from the cell and assembles the tuple → compiled prologue destructures once. No save/restore.

**Compiled prologue (M14):** every non-arrow body opens with (in IR, ANF-let chain):
```
let LThis = TupleGet(Frame, 0)
let LAf   = TupleGet(Frame, 1)
let LHo   = TupleGet(Frame, 2)
let LNt   = TupleGet(Frame, 3)
```
then, for each of the four whose `FunctionInfo.lexical_boxed` bit is set (arc `scope.gleam:246`), wrap in a fresh box cell via `CallHost("js","cell_new",[LX])` so inner arrows can capture the handle. `emit_2core/expr` for `ThisExpression` / `Super` / `new.target` reads the corresponding local (or the arrow's captured cell).

**Arrows:** receive **`Frame = 'js_undefined'`** (unused; arity uniformity for `t_call`). Their body reads captured `LThis`/`LAf`/`LHo`/`LNt` from `MakeClosure` captures per `FunctionInfo.lexical_captures` (arc `scope.gleam:248`). `t_call` still passes a 4-tuple; arrow prologue ignores it.

**Builtins (M6):** identical signature `native(St, Frame, Args) -> {Result, St'}`. A native reading `new.target` (e.g. `Array` ctor, arc `call.gleam:1147`) does `let #(_, _, _, nt) = frame_unpack(frame)`.

---

## D3 — EXCEPTION ABI (state travels in throw payload)

**JS throw = `{wasm_exn, TagId("js_exn"), [St, ThrownVal]}`** raised ERROR-class via `rt_exn.throw_exn`. State is payload[0].

**M19 emits:** `TagDecl(name: "js_exn", params: [TTerm])` — ONE declared param (the thrown value). State is prepended by the backend under Threading, not declared.

**M9 backend changes** (this is the "Threaded+EH" support the `emit_core.gleam:5127` comment says is missing):
- `emit_throw` (5097-5106): under `Threading(cur)`, payload = `core_list([CVar(cur), ..cargs])`.
- `emit_catch_clause` (5226+): under `Threading(_)`, `OnTag` payload binds `[st_new, ..payload_names]`; handler body emitted under `Threading(st_new)` (not the try-entry `sc`).
- `emit_try` (5129): `body` under `Threading(cur)`, handler under `Threading(st_from_payload)`. Join point (materialize) already widened for state (`emit_core.gleam:2123-2125`).

**FFI catch shape** (`twocore_rt_js_call_ffi.erl`):
```erlang
apply_checked(Code, St, Frame, Args) ->
  try Code(St, Frame, Args) of
    {V, St2} -> {ok, {V, St2}}
  catch error:{wasm_exn, _Tag, [St2, Thrown]} -> {error, {Thrown, St2}}
  end.
```
Traps (`{wasm_trap,_}`) and non-`wasm_exn` errors are NOT caught — they propagate as engine bugs.

---

## D4 — MakeClosure/CallClosure under Threading (M9)

**`emit_make_closure` (emit_core.gleam:1493-1523) new arm:**
```
case sc, set.contains(ctx.fn_state_reaching, fn_name) {
  Threading(_), True -> {
    let #(st_p, s3) = fresh_var(state2)
    let target_arity = expected + 1   // St prepended by emit_function
    let body = CApply(FName(fn_name, target_arity),
                      [CVar(st_p), ..list.append(caps_c, list.map(param_names, CVar))])
    apply_cont(cont, [CFun([st_p, ..param_names], body)], sc, s3, ctx)
  }
  _, _ -> // today's untouched behavior
}
```
Emitted fun arity = `arity + 1` (St leading). For JS: IR `arity: 2` → BEAM `fun/3`.

**`emit_call_closure` (1536-1546) new arm:**
```
Threading(cur) -> {
  let applied = CApplyExpr(emit_value(callee), [CVar(cur), ..cargs])
  case cont { KReturn -> Ok(#(applied, state))
              _ -> emit_threaded_call_unpack(applied, 1, cont, state, ctx) }
}
```
(Follows `emit_call_direct` 3466-3471 exactly, but `CApplyExpr` on a value not `CApply` on a name.) **emit_2core does not emit `CallClosure`** — every JS call goes through `CallHost("js","call",…)`; this arm exists for symmetry + future fast-path.

---

## M1 `ObjKind` function variants (fills the truncated upstream `KFun…`)

```gleam
pub type ObjKind {
  // … KOrdinary, KArray, KArguments (upstream) …
  /// Compiled JS function. `code` is the BEAM fun/3: fun(St,Frame,Args)->{V,St'}.
  KFunction(
    code: CompiledFn,            // opaque (D16); applied only via t_call_protected FFI
    name: BitArray,              // .name (configurable, so also mirrored in properties on write)
    length: Int,                 // .length
    home: Option(Handle),        // [[HomeObject]], None for plain functions
    flags: FnFlags,              // bit-packed below
    fields_init: Option(CompiledFn), // M7: class instance-field initializer fun/3, None if not a class ctor
    private_brand: Option(Handle),   // M7: brand cell stamped on `this` at construct
  )
  /// rt_js builtin. Dispatched via M6's `dispatch_native(st, tag, this, args)` — sum-type token (R10).
  KNative(tag: NativeToken, name: BitArray, length: Int, constructible: Bool)
  /// Function.prototype.bind result. arc value.gleam:2950.
  KBound(target: Handle, bound_this: JsVal, bound_args: List(JsVal))
  /// Proxy exotic. arc value.gleam:1732-1744.
  KProxy(slots: Option(#(Int, Int)), callable: Bool, constructable: Bool)
  // … (rest of ObjKind) …
}

pub type FnFlags {  // arc FuncTemplate flags value.gleam:212-226
  FnFlags(
    strict: Bool,
    arrow: Bool,
    constructor: Bool,        // §7.2.4 has [[Construct]]
    class_constructor: Bool,  // §10.2.1 step 2 — throw on [[Call]]
    derived_constructor: Bool,
    generator: Bool,
    async: Bool,
  )
}
```

---

## `rt_js_call` — FULL MODULE SPEC

**File:** `../2core/src/twocore/runtime/rt_js_call.gleam`
**FFI:** `../2core/src/twocore_rt_js_call_ffi.erl`
**Depends on:** `rt_js_store` (M1), `rt_js_val` (M3 sentinels), `rt_js_obj` (M4 `t_get_prop`/`t_new_object_with_proto`/`proto_from_constructor`), `rt_state`, `rt_exn`. **Does NOT depend on** `rt_js_builtins` (M6) — cycle broken by M6 storing `KNative{dispatch}` funs on cells; `t_call` applies them opaquely.

### Types

```gleam
import twocore/runtime/rt_state.{type InstanceState}
import twocore/runtime/rt_js_types.{type Handle, type ObjKind, type FnFlags, type JsVal,
  type CompiledFn, type NativeToken, JsCell}

/// The `Frame` param — {ThisVal, ActiveFunc, HomeObj, NewTarget}. Plain 4-tuple; JsVal elements (D5/D16).
pub type Frame = #(JsVal, JsVal, JsVal, JsVal)

/// arc completion.gleam:14-17 shape, but state-paired.
pub type Checked = Result(#(JsVal, InstanceState), #(JsVal, InstanceState))
```

### Public functions (exact list + arity)

| fn | signature | notes |
|---|---|---|
| `frame_new` | `(this: JsVal, af: JsVal, ho: JsVal, nt: JsVal) -> Frame` | tuple ctor; `af`/`ho` are `{js_cell,N}` or `'js_undefined'` |
| `frame_this` / `frame_af` / `frame_ho` / `frame_nt` | `(Frame) -> JsVal` | element accessors for M6 builtins |
| `t_call` | `(st: InstanceState, callee: JsVal, this: JsVal, args: List(JsVal)) -> #(JsVal, InstanceState)` | **[[Call]]**. Propagates exceptions. `callee` may be any JS value. `nt='js_undefined'` |
| `t_construct` | `(st: InstanceState, callee: JsVal, args: List(JsVal), new_target: JsVal) -> #(JsVal, InstanceState)` | **[[Construct]]**. `new_target` is a `{js_cell,N}`. Returns the instance handle (or callee's object return) |
| `t_call_checked` | `(st, callee, this, args) -> Checked` | wraps `t_call` in FFI try; for M4/M6/M8 Gleam callers that recover |
| `t_construct_checked` | `(st, callee, args, nt) -> Checked` | wraps `t_construct` |
| `t_fn_new` | `(st, code: CompiledFn, name: BitArray, length: Int, flags: FnFlags, home: Option(Handle)) -> #(Handle, InstanceState)` | allocates `SObject{kind:KFunction, prototype:Some(realm.function_proto), properties:{name,length}}`; if `flags.constructor` also allocs `.prototype` obj with `.constructor` back-ref (arc `common.alloc_function_object`) |
| `t_native_new` | `(st, tag: NativeToken, name: BitArray, length: Int, constructible: Bool) -> #(Handle, InstanceState)` | for M6 realm bootstrap |
| `t_bound_new` | `(st, target: Handle, this: JsVal, args: List(JsVal)) -> #(Handle, InstanceState)` | Function.prototype.bind body; computes name/length per §20.2.3.2 (arc call.gleam:609-680) |
| `is_callable` | `(st, v: JsVal) -> Bool` | §7.2.3. `{js_cell,N}` whose kind is KFunction/KNative/KBound/KProxy{callable:True} |
| `is_constructor` | `(st, v: JsVal) -> Bool` | §7.2.4. arc object.gleam:3084-3109 port |
| `throw_js` | `(st: InstanceState, thrown: JsVal) -> a` | `rt_exn.throw_exn(js_exn_tag_id, [to_dyn(st), thrown])`. Never returns. THE one place state enters an exception |
| `throw_type_error` | `(st, msg: BitArray) -> a` | allocs TypeError via M6 realm intrinsic, then `throw_js` |

### `t_call` body (Gleam pseudocode; direct port of arc `call_value` 1619-1716)

```gleam
pub fn t_call(st, callee, this, args) {
  case rt_js_store.as_cell(callee) {
    Error(Nil) -> throw_type_error(st, not_a_function_msg(st, callee))
    Ok(JsCell(id)) -> case rt_js_store.t_cell_kind(st, id) {
      KFunction(code:, home:, flags:, ..) -> {
        use <- bool.lazy_guard(flags.class_constructor,
          fn() { throw_type_error(st, "Class constructor cannot be invoked without 'new'") })
        let frame = frame_new(bind_this(st, flags, this), callee, home_val(home), undefined())
        ffi_apply3(code, st, frame, args)  // {V, St'} — exception propagates
      }
      KNative(tag:, ..) ->
        // R10: token dispatch via JsOps → rt_js_builtins.dispatch_native (D17 fn-record)
        store_of(st).ops.dispatch_native(st, tag, this, args, undefined())
      KBound(target:, bound_this:, bound_args:) ->
        t_call(st, v_obj(target), bound_this, list.append(bound_args, args))
      KProxy(slots: None, ..) -> throw_type_error(st, "proxy revoked")
      KProxy(slots: Some(#(tgt, hnd)), callable: True, ..) -> {
        // §10.5.12: GetMethod(handler,"apply") … arc call.gleam:1661-1703
        let #(trap, st) = rt_js_obj.t_get_method(st, hnd, "apply")
        case trap {
          None -> t_call(st, cell_dyn(tgt), this, args)
          Some(tf) -> {
            let #(arr, st) = rt_js_obj.t_array_from_list(st, args)
            t_call(st, tf, cell_dyn(hnd), [cell_dyn(tgt), this, arr])
          }
        }
      }
      _ -> throw_type_error(st, not_a_function_msg(st, callee))
    }
  }
}
```
`bind_this(st, flags, this)` = §10.2.1.2 OrdinaryCallBindThis: `flags.strict` → this as-is; else undefined/null → realm.global_this; else primitive → box (arc `frame.gleam:145+`).

### `t_construct` body (port of arc `do_construct` 1258-1400)

Key steps: `is_constructor` gate → case on kind:
- `KFunction{flags.derived_constructor:True}` → `frame = {js_uninitialized, callee, home, nt}`; apply; result: if `V` is object → `V`, else if `js_uninitialized` → TypeError, else TypeError (arc's derived-ctor return rules).
- `KFunction{derived:False, constructor:True}` → `#(proto,st) = rt_js_obj.t_proto_from_constructor(st, nt, realm.object_proto)`; `#(inst,st) = t_new_object_with_proto(st, proto)`; `frame = {inst, callee, home, nt}`; apply; if `flags.class_constructor && fields_init: Some(fi)` run `fi(st, {inst,callee,home,nt}, [])` first (M7 field init); result: if `V` is object → `V` else `inst`.
- `KBound{target,bound_args}` → `nt' = if nt==callee then target else nt`; recurse.
- `KNative{constructible:True}` → apply with `frame = {undefined, callee, undefined, nt}`; native reads `frame_nt` to detect construct.
- `KProxy{constructable:True}` → §10.5.13.

### FFI (`twocore_rt_js_call_ffi.erl`)

```erlang
-module(twocore_rt_js_call_ffi).
-export([apply3/4, apply_checked/4]).

%% Apply a KFunction's stored fun/3. Exceptions PROPAGATE.
apply3(Code, St, Frame, Args) -> Code(St, Frame, Args).

%% Apply + catch js_exn → Result. Traps/other errors propagate.
apply_checked(Code, St, Frame, Args) ->
  try Code(St, Frame, Args) of {V, St2} -> {ok, {V, St2}}
  catch error:{wasm_exn, _T, [St2, Thrown]} -> {error, {Thrown, St2}} end.

%% KNative dispatch: token routed through rt_js_builtins:dispatch_native/5 (R10);
%% no fn-in-cell, no Dynamic coercion.
```

---

## M9 `resolve_js` entries for rt_js_call (StoreEffect = MutStoreValue: prepends St, destructures `{V,St'}`)

| op name | target | effect | args (post-St) |
|---|---|---|---|
| `"call"` | `rt_js_call:t_call` | `MutStoreValue` | `[callee, this, args_list]` |
| `"construct"` | `rt_js_call:t_construct` | `MutStoreValue` | `[callee, args_list, nt]` |
| `"fn_new"` | `rt_js_call:t_fn_new` | `MutStoreValue` | `[code_fun, name, length, flags_tuple, home_opt]` |
| `"is_callable"` | `rt_js_call:is_callable` | `ReadStore` | `[v]` |
| `"is_constructor"` | `rt_js_call:is_constructor` | `ReadStore` | `[v]` |
| `"throw"` | `rt_js_call:throw_js` | `MutStore` (bottom) | `[v]` — but see D3: emitted via IR `Throw`, not CallHost |

---

## Integration matrix (what each dependent module consumes)

| Module | consumes from rt_js_call |
|---|---|
| M3 rt_js_val | `is_callable` (for `type_of`) |
| M4 rt_js_obj | `t_call_checked` (getter/setter invoke, @@toPrimitive), `throw_type_error` |
| M5 rt_js_ops | `t_call_checked` (instanceof → @@hasInstance), `is_callable` |
| M6 rt_js_builtins | `t_call_checked` (Array.map cb, Promise executor, JSON.stringify replacer, …), `t_native_new`, `t_construct_checked`, `Frame` accessors |
| M7 rt_js_class | `t_fn_new` (ctor + methods), `t_construct` (super()), `KFunction.home`/`fields_init`/`private_brand` fields |
| M8 rt_js_async | `t_call_checked` (then-callback, resume), `throw_js` (reject path) |
| M12 emit_2core/expr | emits `CallHost("js","call",[callee,this,args])`, `CallHost("js","construct",[c,args,nt])` |
| M14 emit_2core/fn | emits `MakeClosure(…, arity:2)` then `CallHost("js","fn_new",[closure_fun, name, len, flags, home])`; prologue destructures `Frame` |
| M16 emit_2core/class | passes `home: Some(proto_handle)` to `fn_new` for methods; `LNt` local for `super()` |
| M17 emit_2core/exn | `throw e` → IR `Throw("js_exn", [e_val])`; M9 backend prepends St |

---

## Invariants rt_js_call upholds

1. **Never returns without state.** Every path either returns `#(JsVal, InstanceState)` or raises `{wasm_exn, _, [St, _]}` with a valid post-mutation St.
2. **`t_call` is the ONLY place a stored `KFunction.code` is applied.** Builtins/compiled code never `apply` a fun directly — enforces the Frame-assembly + class-ctor guard + this-binding in one place.
3. **No pdict.** Zero `erlang:put/get`. `new_target`/`active_func` never touch InstanceState fields.
4. **Cycle-free.** rt_js_call → rt_js_obj → rt_js_call is broken: rt_js_obj imports only `t_call_checked` + `throw_type_error` (leaf helpers); rt_js_call imports rt_js_obj's `t_get_method`/`t_proto_from_constructor`/`t_new_object_with_proto`/`t_array_from_list` (which do NOT call back). Matches arc's `state.ctx.call_fn` fn-pointer cycle-break (state.gleam:651) but via module split instead of stored fn-pointer.
5. **Tail-recursive on bound/proxy unwrap.** `KBound`/`KProxy(no-trap)` arms tail-call `t_call`/`t_construct` — a bind-chain of depth N is O(N) stack, matching arc's recursion at call.gleam:1361-1369, 1676-1683.

---

## Build order

rt_js_call is buildable after **M1 (JsStore/Handle/ObjKind)** and **M3 (sentinels)** exist as types, and **M4's** `t_get_method`/`t_proto_from_constructor`/`t_new_object_with_proto`/`t_array_from_list` signatures are fixed (bodies can be `todo`). M9's `emit_throw`/`emit_try`/`emit_make_closure` Threading arms are independent (pure emit_core work). M6/M7/M8/M12/M14/M16 all block on rt_js_call's public signatures — which are now fixed above.