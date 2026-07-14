# M7 `rt_js_class` — class runtime ops (threaded)

**File:** `../2core/src/twocore/runtime/rt_js_class.gleam`. No M7-local FFI: ctor invocation goes through `rt_js_call.t_call_protected/4` (§7.M-CALL), so there is no save/restore-around-apply to hand-write in Erlang. Pure Gleam over `JsStore`.

**Dependencies:** M1 `rt_js_store` (Handle, JsStore, t_cell_new/get/set, t_alloc_slot), M3 `rt_js_val` (sentinel atoms, `is_object`, error term builder), M4 `rt_js_obj` (JsSlot/ObjKind types, `t_new_ordinary_object`, `t_get_prop_with_receiver`, `t_set_prop_with_receiver`, `t_define_own_data`, `t_define_own_accessor`, `t_get_own_prop`, `t_get_proto_of`, `t_set_proto_of`, `is_extensible`, `intrinsics` record for `%Object.prototype%`/`%Function.prototype%`). No dependency on M5/M6/M8.

---

## A. Cross-cutting design decisions this module PINS (owned here, consumed by M4/M14/M16)

### A1. FunctionObject cell shape — the `KFunction` `ObjKind` variant (lives in M4's `JsSlot` type; spec'd here because M7 is the sole writer of the class-specific fields)
Port of arc `value.FunctionObject` (value.gleam:3384-3388) + `FuncTemplate` flags (value.gleam:198-241, only the runtime-relevant bits):
```gleam
// in rt_js_obj.gleam (M4)
pub type FnFlags { FnFlags(
  is_constructor: Bool,        // §7.2.4 [[Construct]] capability
  is_class_constructor: Bool,  // §10.2.1 step 2 — throws on [[Call]]
  is_derived_constructor: Bool // controls this-TDZ in t_construct
) }
KFunction(
  code: CompiledFn,                 // opaque BEAM fun/3: fun(St, Frame, Args) -> {V, St'} (SPEC §2.4 D4/D5)
  home_object: Option(Handle),      // §15.4.4 [[HomeObject]]; None for plain fns
  flags: FnFlags,
  name: BitArray, length: Int,      // surfaced as own props by M4
)
```
`KNative(tag: NativeToken, name: String, length: Int, constructible: Bool)` is the sibling for builtins (arc `NativeFunction` value.gleam:3395).

### A2. Call-frame lexicals ride on the `Frame` 4-tuple PARAMETER, NOT on `InstanceState` (SPEC §3.5 / D5 / R7)
Compiled shape is `Code(St, Frame, Args) -> {V, St'}` where `Frame = {ThisVal, ActiveFunc, HomeObject, NewTarget}` — a plain Erlang 4-tuple in `all_lexical_refs` order (arc `lexical.gleam:30-35`). `t_call`/`t_construct` (§7.M-CALL) BUILD the tuple from the callee cell (`active_func`=callee handle, `home_object`=cell's `KFunction.home_object`) + call site (`this`, `new_target`), then apply — no save/set/restore, no `InstanceState` fields. **The arc `state.new_target` save/restore pattern (call.gleam:1181-1206) is REJECTED** (u-call-abi_1.md D2).
M14's non-arrow prologue destructures via `TermOp(TupleGet(i), [Var("_frame")])` with **0-based** indices `this=0 / active_func=1 / home_object=2 / new_target=3` (R7):
```
let LThis = TupleGet(_frame, 0)   let LAf = TupleGet(_frame, 1)
let LHo   = TupleGet(_frame, 2)   let LNt = TupleGet(_frame, 3)
```
Arrows do NOT read `_frame`; M14 makes arrows CAPTURE the enclosing non-arrow's prologue-bound `LThis/LAf/LHo/LNt` locals per `FunctionInfo.lexical_captures` (arc `scope.gleam:248`, `is_arrow` gating value.gleam:231-234), so late-called arrows see creation-time values.

### A3. Private names are `PropertyKey`s, NOT a brand-set
**Deviation from the task's proposed `t_private_brand_add/check` Set-on-cell.** Arc's proven-at-97.4%-test262 model (key.gleam:160-210) stores private elements as own properties under a distinct key variant `Private(BitArray)` where the payload is `<<"#name", 0, UidText/binary>>`. Rationale: (1) one mechanism covers fields, methods, accessors uniformly (writable=False for methods, accessor descriptor for get/set — interpreter.gleam:3367-3428); (2) `#x in obj` is `has_own(Private(k))` (interpreter.gleam:3330-3350); (3) `PrivateGet`/`PrivateSet` reuse M4's own-prop lookup with no proto walk; (4) reflection filters via `is_private_key` (key.gleam:205). No per-instance Set field. **The uid counter lives on `JsStore` (`private_uid: Int`)**, replacing arc's `erlang:unique_integer` (value.gleam:3691) so it's threaded and deterministic across gc.

---

## B. Public types
```gleam
import twocore/runtime/rt_js_store.{type JsStore, type Handle, type St}
// St = InstanceState alias re-exported by rt_js_store for brevity
import twocore/runtime/rt_js_obj.{type FnFlags, type PropKey, type AccessorKind}

pub type MethodInstallKind { MIMethod  MIGetter  MISetter  MIStatic  MIStaticGetter  MIStaticSetter }   // R11 — SPEC §2.4:219

/// Return package of t_class_create — both handles are needed by M16 (ctor for
/// binding + statics; proto for instance-method target).
pub type ClassPair { ClassPair(ctor: Handle, proto: Handle) }
```
`PropKey` (M4) = `Named(BitArray) | Index(Int) | Sym(Handle) | Private(BitArray)` — port of arc key.gleam:36-48.

---

## C. Public functions (all `pub fn`; every one that touches the store takes `St` first and returns `#(_, St)`)

### C1. `t_new_private_name(st: St, source: BitArray) -> #(JsVal, St)`
Port of arc `NewPrivateName` (interpreter.gleam:3286) + `mint_private_key` (value.gleam:3700) + `private_key_text` (key.gleam:199). Bumps `st.js.private_uid`, returns `{js_private, <<Source/binary, 0, (integer_to_binary(Uid))/binary>>}` as an opaque `JsVal` (D16 — wire term is a 2-tuple so `is_private_key` is a tag check, and it can never collide with a user string). M16 emits one call per `#name` at class-evaluation time and binds the result to a class-scope const.

### C2. `t_class_create(st: St, ctor_code: CompiledFn, ctor_name: BitArray, ctor_len: Int, super: JsVal) -> #(ClassPair, St)`
Fusion of arc `make_closure` (interpreter.gleam:576-701, the .prototype allocation) + `SetupDerivedClass` (interpreter.gleam:3875-3958) + base-class `MakeMethod` (emit.gleam:7241-7249). One call replaces the arc `MakeClosure; SetupDerivedClass` / `MakeClosure; Dup; GetField prototype; Swap; MakeMethod` sequences.

- `super: JsVal` (D16) — `classify` yields `KHandle(_)` | `KNull` | a distinct sentinel for `js_no_heritage` (base class). Not `Option(Handle)` — because `class C extends null` is a distinct third case (interpreter.gleam:3933-3950).
- **Heritage validation** (before any alloc, so a throw leaks no cells): if `super` is a handle, `is_constructor(st, super)` must hold (arc object.gleam:3084 — reads `FnFlags.is_constructor` for `KFunction`, `constructible` for `KNative`, `constructable` for `ProxyObject`) else `raise {js_error, type_error, <<"Class extends value is not a constructor or null">>}`. Then `parent_proto = t_get_prop(st, super, Named("prototype"))` — must be a handle or `null` else TypeError (interpreter.gleam:3902-3910).
- **Alloc `proto`**: `t_new_ordinary_object(st, proto_parent)` where `proto_parent` = `parent_proto` (derived, may be `None` for `extends null`) | `Some(intrinsics.object_prototype)` (base).
- **Alloc `ctor`**: a `KFunction` slot: `code=ctor_code`, `home_object=Some(proto)` (§15.7.14 step 12 — interpreter.gleam:3913-3916), `flags=FnFlags(is_constructor:True, is_class_constructor:True, is_derived_constructor: super≠js_no_heritage)`, `name=ctor_name`, `length=ctor_len`. Cell's `[[Prototype]]` = `super` handle (derived, static inheritance — interpreter.gleam:3922) | `intrinsics.function_prototype` (base/extends-null). Own props: `prototype` = `{proto, W:False, E:False, C:False}` (arc make_closure — non-writable for classes, unlike plain fns), `name`/`length` = `{_, W:F, E:F, C:T}`.
- **Back-link**: `t_define_own_data(st, proto, Named("constructor"), ctor, W:True, E:False, C:True)`.
- Returns `#(ClassPair(ctor, proto), st)`.
- **Static-init order is NOT here** — M16 sequences `t_class_create` → computed-key evaluation → `t_define_method`×N (instance then static) → inner-name binding → static-init-fn call, exactly mirroring arc emit.gleam:7136-7151 / 7253-7288.

### C3. `t_define_method(st: St, target: Handle, key: ObjectKey, fun: Handle, kind: MethodInstallKind) -> #(Nil, St)`
Port of arc `DefineMethod`/`DefineMethodComputed`/`DefineAccessor`/`DefineAccessorComputed` (interpreter.gleam:3473-3600). `key: ObjectKey` — M16 emits `object_key_lit(pk)` for static keys or `host("to_property_key",[k])` for computed keys BEFORE this call (G9 — caller canonicalizes). Steps: (1) `t_make_method(st, fun, target)` — sets `fun.home_object = Some(target)` (interpreter.gleam:5507); (2) `set_computed_fn_name` if fun's name is empty (interpreter.gleam:3500,3517 — prefix `"get "`/`"set "` for accessors); (3) `MIMethod`/`MIStatic` → `t_define_own_data(.., W:T, E:F, C:T)`; `MIGetter`/`MISetter`/`MIStaticGetter`/`MIStaticSetter` → `t_define_own_accessor(.., kind, E:F, C:T)` which MERGES with an existing accessor half (arc `define_accessor` object.gleam:1833). (4) Throws TypeError if `key` names an existing non-configurable own prop on `target` (`static ['prototype']` — interpreter.gleam:3515). `MIStatic*` variants: caller passes `target = ctor`; instance variants: `target = proto` (arc `with_method_target` emit.gleam:7374).

### C4. `t_make_method(st: St, fun: Handle, home: Handle) -> St`
Port of `make_method` (interpreter.gleam:5504-5520). Mutates `fun`'s slot: if `KFunction(..)` → `home_object = Some(home)`; else no-op. Returned separately because M16 also uses it standalone for the field-init closure (emit.gleam:7365) and object-literal methods.

### C5. `t_define_private(st: St, target: Handle, priv_key: JsVal, val: JsVal, kind: MethodInstallKind) -> #(Nil, St)`
Port of `DefinePrivateField/Method/Accessor` (interpreter.gleam:3353-3428) + `check_private_add` (interpreter.gleam:5471-5501). `priv_key` is the `{js_private, Text}` from C1. Guard: if `t_get_own_prop(st, target, Private(text))` is `Some` → `{js_error, type_error, <<"Cannot initialize #… twice on the same object">>}`; if `!is_extensible(target)` → TypeError. Then: `MIMethod`→data `{W:F, E:F, C:F}` (non-writable so PrivateSet throws — interpreter.gleam:3375); `MIGetter`/`MISetter`→accessor half (merge with existing other half, but if the SAME half exists → TypeError double-init, interpreter.gleam:3391-3411). Private FIELDS do not use `MethodInstallKind` — they go through `t_private_define(st, obj, priv_key, v)` (data `{W:T, E:F, C:F}`, SPEC §7.M7). Does NOT set home_object (arc: private-method closures get `MakeMethod` separately at class-def time, emit.gleam:7430; the per-instance install here just copies the shared closure ref).

### C6. `t_private_get(st: St, obj: JsVal, priv_key: JsVal) -> #(JsVal, St)`
Port of `private_get_dyn` (interpreter.gleam:1734-1776). Non-object `obj` → TypeError. Own-only lookup of `Private(text)`: `None` → `{js_error, type_error, <<"Cannot read private member #… from an object whose class did not declare it">>}`; `Some(Accessor(get:None,..))` → TypeError "defined without a getter"; `Some(Data(v,..))` → `v`; `Some(Accessor(get:Some(g),..))` → `t_call(st, g, obj, [])` (may re-enter JS → threads St).

### C7. `t_private_set(st: St, obj: JsVal, priv_key: JsVal, val: JsVal) -> #(JsVal, St)`
Port of `private_put_found` (interpreter.gleam:1789-1817) + `PutPrivateFieldDyn` (interpreter.gleam:3303). Own-only lookup: `None` → TypeError; `Some(Data(W:False,..))` (a method) → TypeError; `Some(Accessor(set:None,..))` → TypeError; `Some(Data(W:True,..))` → overwrite; `Some(Accessor(set:Some(s),..))` → `t_call(st, s, obj, [val])`. Returns `val`.

### C8. `t_private_in(st: St, obj: JsVal, priv_key: JsVal) -> Int`
Port of `PrivateInDyn` (interpreter.gleam:3330-3350). Non-object → TypeError. Else `has_own(Private(text))` → i32 `1`/`0`. **JRead**: takes `st` to read the store but returns bare `Int`; `sc` unchanged at call site.

### C9. `t_super_get(st: St, home_object: Handle, key: ObjectKey, receiver: JsVal) -> #(JsVal, St)`
Port of `get_super_value` (interpreter.gleam:1846-1876) + `emit_super_base` (emit.gleam:2639). `base = t_get_proto_of(st, home_object)`; if `None` → `{js_error, type_error, <<"Cannot read super property when prototype is null">>}`; else `pk = t_to_property_key(st, key)` (may throw), then M4 `t_get_prop_with_receiver(st, base, pk, receiver)` (OrdinaryGet with explicit receiver — the getter runs with `this=receiver`). NB: M16 emits `home_object` as a let-bound-at-prologue value (`t_fn_home_object(st, active_func)`), NOT looked up per-access.

### C10. `t_super_set(st: St, home_object: Handle, key: ObjectKey, val: JsVal, receiver: JsVal, strict: Bool) -> #(JsVal, St)`
Port of `PutSuperValue` (interpreter.gleam:4332-4367). `base = proto_of(home_object)` (None → TypeError); `t_set_prop_with_receiver(st, base, pk, val, receiver) -> #(ok:Bool, st)`; if `!ok && strict` → TypeError "Cannot assign to read-only super property". Returns `val`. `strict` is a compile-time constant passed by M16 (class bodies are always strict — but object-literal super may be sloppy).

### C11. `t_construct(st: St, ctor: JsVal, args_list: List(JsVal), new_target: JsVal) -> #(JsVal, St)`
Port of `do_construct` (call.gleam:1258-1508) + constructor-return resolution (interpreter.gleam:3036-3070). This is the `new C(...)` / `super(...)` / `Reflect.construct` entry.

- **IsConstructor gate** (call.gleam:1271): `ctor` must be a handle whose slot's `is_constructor`/`constructible`/`constructable` is True; else TypeError `<<inspect(ctor), " is not a constructor">>`. Runs AFTER args evaluation (M16 evaluates args before the CallHost) — spec §13.3.7.2 step 5.
- **Dispatch by slot kind:**
  - `KFunction(code, flags:FnFlags(is_derived_constructor:True,..), ..)` → `this_val = atom js_uninitialized` (TDZ sentinel), `mode = Derived`.
  - `KFunction(.., is_derived_constructor:False, ..)` → `#(proto, st) = t_get_prototype_from_constructor(st, new_target, intrinsics.object_prototype)` (C13 below); `#(inst, st) = t_new_ordinary_object(st, Some(proto))`; `this_val = inst`, `mode = Base(inst)`.
  - `KBound(target, bound_this, bound_args)` → `nt' = if new_target==ctor then target else new_target`; tail-call `t_construct(st, target, bound_args++args, nt')` (call.gleam:1350-1369).
  - `ProxyObject(slots, ..)` → §10.5.13 trap dispatch (call.gleam:1389-1431) — deferred to M4's `t_proxy_construct` helper; M7 calls it.
  - `KNative(constructible:True, ..)` → M6's native-constructor dispatch (`t_construct_native(st, tag, args, new_target)`); M7 calls it. (String/Number/Boolean/Array/Error/… — call.gleam:1433-1508+.)
- **Invoke** (KFunction only): build `Frame = {this_val, ctor, cell.home_object ? undefined, new_target}` (§A2 / SPEC §3.5) and go through §7.M-CALL's `t_call_protected(St, code, Frame, args_list)` → `{{normal_completion, ret}, st'}` | `{{throw_completion, e}, st'}`. No `InstanceState` fields written; no save/restore. `t_call_protected` is the ONLY dynamic apply, and its target (`code: CompiledFn`) is a value we allocated, never user data (emit_core D3a).
- **Class-constructor [[Call]] guard**: NOT here — M4's `t_call` checks `is_class_constructor && new_target==undefined` → TypeError (call.gleam:101). `t_construct` always passes an object `new_target`.
- **Return resolution** (interpreter.gleam:3036-3070):
  - `Base(inst)`: `ret` is a handle → `ret`; else → `inst`.
  - `Derived`: `ret` is a handle → `ret`; `ret == undefined` → the callee's final `this` — **BUT** in compiled code there's no `read_this_local`. Solution: derived-ctor bodies are compiled by M14 to `return this` at every fall-through/`return;`/`return undefined` exit (M14 emits `Return([this_var])` not `Return([undefined])`), so `ret` here is never `undefined` for a derived ctor unless `super()` was never called → `ret == js_uninitialized` → `{js_error, reference_error, <<"Must call super constructor…">>}`. Any other primitive → `{js_error, type_error, <<"Derived constructors may only return object or undefined">>}`.
- **Field-init call is NOT here.** Arc runs it inside the ctor body (emit.gleam:4669-4672 after super, or at start for base — emit.gleam:7205-7209). M14/M16 emit the call inline in the ctor body; `t_construct` is oblivious.

### C12. `t_super_call(st: St, active_func: Handle, args_list: List(JsVal), new_target: JsVal) -> #(JsVal, St)`
Thin wrapper for M16's `super(...)` emission (arc emit.gleam:4633-4672 decomposed as `GetPrototypeOf(active_func); CallConstructor`). `parent = t_get_proto_of(st, active_func)` (must be Some — a derived ctor's `__proto__` was set by `t_class_create`); `t_construct(st, parent, args_list, new_target)`. M16 then emits the `PutLocalCheckInit(this_slot)` equivalent (double-super → ReferenceError) and the field-init call inline — those are emit-side, not runtime.

### C13. `t_get_prototype_from_constructor(st: St, new_target: Handle, intrinsic_default: Handle) -> #(Handle, St)`
Port of object.gleam:2536-2556. Fast path (`own_data_prototype` object.gleam:2568): if `new_target`'s own `"prototype"` is a plain data-prop holding a handle, return it (no observable Get). Else full `t_get_prop(st, new_target, Named("prototype"))`; result is a handle → it; else → `intrinsic_default`. Threads `st` because the slow path may hit a proxy `get` trap.

### C14. `t_fn_home_object(st: St, fun: Handle) -> JsVal` and `t_fn_flags(st: St, fun: Handle) -> FnFlags`
**JRead**: take `st` to read the store, return bare value; `sc` unchanged at call site. For M14's prologue emission and M16's `is_derived` check. `home_object` returns the handle or atom `undefined`.

### C15. `t_is_constructor(st: St, val: JsVal) -> Int`
Port of `is_constructor` (object.gleam:3084-3108) → i32 1|0. Used by `t_construct`'s guard, `t_class_create`'s heritage check, and M6 `Reflect.construct`. **JRead**: takes `st`, returns bare `Int`.

---

## D. Exception ABI (unifies with M17 / 2core `Try`)
Every guest error above raises `{wasm_exn, 0, [St, E]}` via `rt_js_exn.t_throw` (SPEC §4 / D6 — tag `js_exn`, payload `[St, JsVal]` state-first). `t_construct` does NOT hand-write try/catch: it invokes ctor bodies through §7.M-CALL's `t_call_protected/4` which catches `error:{wasm_exn,0,[St2,E]}` into `{{throw_completion,E},St2}`. On `throw_completion`, `t_construct` simply re-raises via `t_throw(St2, E)` — nothing to restore (Frame is a stack-local tuple, §A2; no `InstanceState` fields written). No M7-local FFI needed for this.

---

## E. `resolve_js` dispatch arms this module contributes (M9)
Current `resolve_js` (emit_core.gleam:3621) is state-neutral. For M9's threaded variant, each arm carries `(fn_atom, kind: JsOpKind, arity_without_st: Int)` where `JsOpKind = JMut | JMutUnit | JRead | JPure`. M7's arms:
```
"class_create"/4→t_class_create(JMut)  "define_method"/4→t_define_method(JMut)
"make_method"/2→t_make_method(JMutUnit)    "define_private"/4→t_define_private(JMut)
"private_get"/2→t_private_get(JMut)    "private_set"/3→t_private_set(JMut)
"private_in"/2→t_private_in(JRead)      "super_get"/3→t_super_get(JMut)
"super_set"/5→t_super_set(JMut)        "construct"/3→t_construct(JMut)
"super_call"/3→t_super_call(JMut)      "new_private_name"/1→t_new_private_name(JMut)
"proto_from_ctor"/2→t_get_prototype_from_constructor(JMut)
"fn_home_object"/1→t_fn_home_object(JRead)  "is_constructor"/1→t_is_constructor(JRead)
```
`JMut` = threads state: emitted as `let {V, St'} = call rt_js_class:F(St, ..args)` and rebinds `cur`. `JRead` = pure reader: `call rt_js_class:F(St, ..args)` returns bare `V`; `cur` unchanged (rt_state.gleam `t_global_get` pattern, emit_core.gleam:1838). `JMutUnit` = returns bare `St'`, no value.

---

## F. Invariants
1. Every `Handle` argument is trusted (compiler-generated); non-handle where a handle is required is a `{js_error, type_error}` (guest error), never a crash — matches arc's underflow→VmFailed vs type-mismatch→throw split.
2. `t_class_create` allocates exactly 2 cells; on heritage-validation failure it allocates 0 (validate-first-allocate-last).
3. `t_construct` writes NO call-frame fields on `InstanceState` (§A2) — the `Frame` 4-tuple is a stack-local value; there is nothing to save/restore on any exit path.
4. `home_object` is written only by `t_make_method`/`t_class_create`; read only by `t_fn_home_object`. Never appears in a property map.
5. Private-keyed props never surface via M4's `t_own_keys`/`t_for_in_keys` (M4 filters `Private(_)` — key.gleam:205).
6. GC (M2) traces `KFunction.home_object` via `refs_in_cell`. The in-flight `Frame` tuple's handles need no explicit root: GC only runs at turn boundaries (`call_depth == 0`, D11), never mid-call.

---

## G. Source-of-truth cross-references
| M7 fn | arc semantics source | arc emit source |
|---|---|---|
| t_class_create | interpreter.gleam:3875-3958 (SetupDerivedClass), :576-701 (make_closure) | emit.gleam:7227-7251 |
| t_define_method | interpreter.gleam:3473-3600, :5504-5520 | emit.gleam:7388-7536 |
| t_define_private | interpreter.gleam:3353-3428, :5471-5501 | emit.gleam:7404-7447, :7661-7674 |
| t_private_get/set/in | interpreter.gleam:1711-1817, :3299-3350 | — |
| t_super_get/set | interpreter.gleam:1846-1876, :4332-4367 | emit.gleam:2636-2679 |
| t_construct | call.gleam:1258-1508; interpreter.gleam:3036-3070 (return) | emit.gleam:4901-4910 |
| t_super_call | call.gleam:1258 (via CallConstructor) | emit.gleam:4633-4672 |
| t_new_private_name | interpreter.gleam:3286; value.gleam:3700; key.gleam:199 | emit.gleam:7131-7134 |
| t_get_prototype_from_constructor | ops/object.gleam:2536-2578 | — |
| t_is_constructor | ops/object.gleam:3084-3108 | — |
