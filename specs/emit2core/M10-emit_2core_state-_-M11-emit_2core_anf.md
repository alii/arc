# M10 `emit_2core/state` + M11 `emit_2core/anf` — full spec

## Source-of-truth files read
- `src/arc/compiler/emit.gleam` — Frame :125-160, Emitter :163-310, new_emitter :1295-1329, ScopeSave :1389-1391, enter_scope/leave_scope :1401-1442, fresh_label :2226, fresh_slot :2238, push_frame/push_loop/push_loop_iter/push_switch/push_barrier/pop_frame :2244-2314, block_child_scopes :1366, fn_info :1350, EmitError :393-430
- `src/arc/compiler/scope.gleam` — ScopeId :88 (`= Int`), ScopeTree :281-305, FunctionInfo :236-272, Resolution :340-343, Direct :317-321, lookup :2083, function_info, child_scopes, child_function_scopes, alloc_scratch, get_scope, is_function_kind :133
- `src/arc/parser/ast.gleam` — StmtWithLine, TryTail :307-309 (`TryFinally(finalizer: List(StmtWithLine))`)
- `../2core/src/twocore/ir.gleam` — Module :88-111, TagDecl :125, Function :421-428, Local :435, Value :471-495 (Var/ConstI32/ConstI64/ConstF32/ConstF64/ConstNull/ConstAtom/ConstBinary), Expr :541-965 (Values/Let/Block/Loop/If/Switch/Break/Continue/Return/CallHost/CallDirect/CallClosure/MakeClosure/TermOp/MapOp/TermTest/TermTag/NumTerm/Throw/Try/ThrowRef), ValType :317 (has TTerm), TermOp :1460-1468 (MakeTuple/TupleGet/MakeCons/ListHead/ListTail/IsEmptyList), TermKind :1509-1519, NumTermOp :1530-1539, CatchHandler :991, CatchTag :1006 (OnTag)
- `../2core/src/twocore/backend/emit_core.gleam` — StateChan :277-280, EmitState :297-304, fresh_var :308-315, fn_state_reaching :217/:379/:641, emit_call_host :3539-3596 (**today state-neutral** :3554), emit_make_closure :1493-1523, emit_call_closure :1536-1546 (**today state-neutral** :1530), emit_return :1551-1561 (Threading pairs `{Pkg, cur}`), resolve_js :3621-3657
- `../2core/src/twocore/runtime/rt_state.gleam` — InstanceState :134-144
- `../2core/src/twocore/runtime/rt_js.gleam` — current pdict-based ops :55-170

## IR-level calling convention (invariant M10+M11 uphold, M9+M14 depend on)

**At the `ir.Module` level** every compiled JS function is:
```
ir.Function(
  name:   String,                                       // from fresh_fn, e.g. "js_fn_7"
  params: [captures..., Local("__this", TTerm), Local("__args", TTerm)],
  result: [TTerm],
  locals: [],                                           // ANF uses Let, never ir.locals
  body:   Expr,
)
```
- **State is NOT an IR-level param.** 2core's `Threaded` state_strategy (`emit_core.gleam:641`) auto-prepends the leading `St` param and wraps `Return([v])` into `{v, St'}` (`emit_core.gleam:1559`) for every state-reaching function. **M9** makes `CallHost("js", …)` state-mutating and `CallClosure` state-threading (both are state-neutral today — see gotchas), which forces every JS function into `fn_state_reaching`. M10/M11 therefore emit state-unaware IR; the BEAM-level ABI `f(St, Captures…, This, Args) -> {Result, St'}` is materialised by emit_core.
- `MakeClosure(fn_name, captures, arity: 2)` — arity is always 2 (`__this`, `__args`). emit_core under Threaded (via M9) bumps the emitted fun to arity 3.
- `CallClosure(callee, [this_val, args_cons_list])` — 2 args at IR level.
- Every `CallHost("js", op, args)` yields exactly one `TTerm` (emit_core.gleam:3554 `apply_cont_call(…, 1, …)`).

---

## M10 — `arc/src/arc/emit_2core/state.gleam`

### Imports
```gleam
import arc/compiler/scope.{type ScopeId, type ScopeTree, root_scope_id}
import arc/parser/ast
import gleam/option.{type Option, None, Some}
import gleam/list
import twocore/ir
```

### Types

```gleam
/// Compile-time ir.Value constants for JS sentinel atoms (HANDOFF §2).
/// Built once by `realm_consts()`; carried on Emitter2 so every emit_* site
/// writes `e.rc.undef` instead of re-allocating `ir.ConstAtom("undefined")`.
pub type RealmConsts {
  RealmConsts(
    undef:   ir.Value,   // ConstAtom("undefined")
    null:    ir.Value,   // ConstAtom("null")
    true_:   ir.Value,   // ConstAtom("true")
    false_:  ir.Value,   // ConstAtom("false")
    nan:     ir.Value,   // ConstAtom("nan")
    pos_inf: ir.Value,   // ConstAtom("infinity")
    neg_inf: ir.Value,   // ConstAtom("neg_infinity")
    js_tag:  String,     // "js_exn" — the ir.TagDecl name M17/M19 register
  )
}

/// Cleanup a break/continue/return must emit when CROSSING a barrier
/// (before reaching its target). Re-emitted inline at each crossing site
/// (the "barrier-duplication rewrite" — no gosub in structured IR).
pub type BarrierCleanup {
  /// A `finally { … }` block: re-emit `body` via emit_2core/stmt at the
  /// crossing point, inside a fresh child of `saved_scope` (M17 owns the
  /// scope-cursor save/restore around duplication).
  FinallyBlock(body: List(ast.StmtWithLine), saved_scope: ScopeSave)
  /// for-of / for-await-of: `iter_var` holds the iterator value; crossing
  /// emits `CallHost("js", "iter_close", [Var(iter_var)])` (async variant
  /// awaits the result — M18 territory when `is_async`).
  IterClose(iter_var: String, is_async: Bool)
  /// try-catch with no finally: no cleanup to emit. IR `Try` is structured,
  /// so `Break(label)` from inside its body is valid Core Erlang; this
  /// variant exists only so M17 knows a Try boundary was crossed (for
  /// completion-value semantics).
  CatchOnly
}

/// Break/continue/return unwind stack. Port of arc emit.gleam:125-160
/// `Frame`, re-shaped for string labels + finally-duplication.
pub type Frame2 {
  /// Iteration statement. `break_label` names the enclosing ir.Block;
  /// `cont_label` names the ir.Loop head. `js_label` is the source-level
  /// label from `pending_label` (for `break foo`/`continue foo`).
  Loop2(break_label: String, cont_label: String, js_label: Option(String))
  /// switch OR labeled-block. Break target only; `continue` walks past.
  /// Merges arc's SwitchFrame + LabeledBlockFrame (both are "break-only").
  /// `is_switch: True` → unlabeled `break` targets it; `False` (labeled
  /// block) → only `break <js_label>` targets it (§14.8).
  Block2(break_label: String, js_label: Option(String), is_switch: Bool)
  /// try/catch/finally or for-of body. Never a target — only crossed.
  Barrier2(cleanup: BarrierCleanup)
}

/// Saved scope-cursor position (verbatim port of emit.gleam:1389-1391).
pub type ScopeSave {
  ScopeSave(scope: ScopeId, cursor: List(ScopeId), in_block: Bool)
}

/// The emitter state, threaded through every emit_* function as
/// `fn(…, e: Emitter2) -> #(ir.Expr, Emitter2)`. Port of emit.gleam:163-310
/// with bytecode-specific fields dropped and IR-specific fields added.
pub opaque type Emitter2 {
  Emitter2(
    // ── scope cursor (verbatim from arc, see emit.gleam:209-227) ──
    tree:        ScopeTree,
    fn_scope:    ScopeId,
    cur_scope:   ScopeId,
    cursor:      List(ScopeId),         // block-child scopes not yet entered
    fn_cursor:   List(ScopeId),         // function-child scopes not yet compiled (emit.gleam:309)
    in_block:    Bool,
    // ── name generation ──
    next_var:    Int,                   // gensym counter for ir.Var + ir labels ("_v42", "_L42")
    next_fn:     Int,                   // gensym counter for ir.Function names ("js_fn_42")
    // ── control-flow stacks ──
    frames:      List(Frame2),
    pending_label: Option(String),      // set by LabeledStatement, consumed by push_loop/push_block
    // ── function context (per-body; reset by enter_function) ──
    strict:      Bool,
    is_async:    Bool,
    is_generator: Bool,
    is_arrow:    Bool,
    with_stack:  List(String),          // ir.Var names of with-object holders (emit.gleam:273)
    private_env: List(String),          // "#x" names in scope (emit.gleam:280)
    field_init:  FieldInitMode,         // for M16 (emit.gleam:314-321, ported verbatim)
    // ── module-wide accumulators (survive enter_function) ──
    fns_acc:     List(ir.Function),     // ALL compiled functions, flat (ir.Module.functions)
    // ── mutual-recursion break (D13) ──
    dispatch:    EmitDispatch,
    // ── constants ──
    rc:          RealmConsts,
  )
}
// D2 (Arch-A): NO `state_var`/`st_cur` field. `InstanceState` threading is
// emit_core's job under `js_profile`; M10-M18 emit state-unaware IR.

pub type FieldInitMode { NoFieldInit  FieldInitAtStart  FieldInitAfterSuper }

/// D13: fn-record breaking the emit_2core module cycle. NOT opaque — M19's
/// `init_emitter` constructs it once with the concrete emit fns from each
/// module; every other module reads it via `e.dispatch.emit_*`. 7 fields:
pub type EmitDispatch {
  EmitDispatch(
    emit_expr:        fn(Emitter2, ast.Expression, Option(String)) -> EmitR,
    emit_stmts:       fn(Emitter2, List(ast.StmtWithLine), fn(Emitter2) -> Emit) -> EmitR,
    emit_pattern:     fn(Emitter2, ast.Pattern, ir.Value, BindMode) -> EmitR,
    emit_function:    fn(Emitter2, ast.FunctionLiteral, Option(String)) -> Result(#(ir.Value, Emitter2), EmitError),
    emit_class:       fn(Emitter2, ast.ClassLiteral, Option(String)) -> Result(#(ir.Value, Emitter2), EmitError),
    emit_async_body:  fn(Emitter2, List(ast.StmtWithLine)) -> EmitR,
    emit_destructure: fn(Emitter2, ast.Expression, ir.Value) -> EmitR,
  )
}
pub type BindMode { BindLet  BindConst  BindVar  BindAssign }

/// M17 EmitError port (arc emit.gleam:393-430). M10 declares it; every
/// emit_* module returns `Result(#(ir.Expr, Emitter2), EmitError)`.
pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  EarlySyntaxError(message: String)
  UnsupportedFeature(feature: String)    // direct eval, `with` if deferred
  ScopeCursorDesync(at: ScopeId)         // NEW: enter_scope popped [] unexpectedly
}
```

### Functions (all `pub`)

| fn | signature | body sketch | port of |
|---|---|---|---|
| `realm_consts` | `fn() -> RealmConsts` | literal record of `ir.ConstAtom(…)` | new |
| `new` | `fn(tree: ScopeTree, root: ScopeId, strict: Bool, dispatch: EmitDispatch) -> Emitter2` | `Emitter2(tree:, fn_scope: root, cur_scope: root, cursor: block_child_scopes(tree, root), fn_cursor: scope.child_function_scopes(tree, root), next_var: 0, next_fn: 0, frames: [], pending_label: None, strict:, is_async: False, is_generator: False, is_arrow: False, with_stack: [], private_env: [], field_init: NoFieldInit, in_block: False, fns_acc: [], dispatch:, rc: realm_consts())` | emit.gleam:1295-1329 |
| `fresh_var` | `fn(e) -> #(String, Emitter2)` | `#("_v" <> int.to_string(e.next_var), Emitter2(..e, next_var: e.next_var + 1))` | emit_core.gleam:308-315 shape |
| `fresh_label` | `fn(e) -> #(String, Emitter2)` | `#("_L" <> int.to_string(e.next_var), …next_var+1)` — shares counter with fresh_var (both are Core-var-namespace strings) | emit.gleam:2226 |
| `fresh_fn` | `fn(e) -> #(String, Emitter2)` | `#("js_fn_" <> int.to_string(e.next_fn), …next_fn+1)` | emit.gleam:2533 idx role |
| `fn_info` | `fn(e) -> scope.FunctionInfo` | `scope.function_info(e.tree, e.fn_scope)` | emit.gleam:1350 |
| `resolve` | `fn(e, name: String) -> scope.Resolution` | `scope.lookup(e.tree, e.cur_scope, name)` | wraps scope.gleam:2083 |
| `block_child_scopes` | `fn(tree, id) -> List(ScopeId)` | filter `scope.child_scopes` by `!is_function_kind` | emit.gleam:1366-1369 verbatim |
| `enter_scope` | `fn(e, in_block: Bool) -> #(Emitter2, ScopeSave)` | pop `cursor` head → move into it; empty-cursor case stays put (see emit.gleam:1419-1430 comment). **No binding-prologue emission here** — M13 owns TDZ-cell seeding via anf helpers. | emit.gleam:1401-1432 minus `emit_binding_prologue` |
| `leave_scope` | `fn(e, save: ScopeSave) -> Emitter2` | restore `cur_scope/cursor/in_block` | emit.gleam:1435-1442 verbatim |
| `enter_function` | `fn(e, child_id: ScopeId, strict, is_async, is_generator, is_arrow) -> #(Emitter2, FnSave)` | pops `fn_cursor` head (asserts `== child_id`), returns child emitter with `fn_scope: child_id, cur_scope: child_id, cursor: block_child_scopes(tree, child_id), fn_cursor: child_function_scopes(tree, child_id), frames: [], pending_label: None, next_var/next_fn/fns_acc PRESERVED` (module-wide), per-body flags reset. `FnSave` captures `{fn_scope, cur_scope, cursor, fn_cursor, in_block, frames, strict, is_async, is_generator, is_arrow, with_stack, private_env, field_init}`. | new (arc uses fresh `new_emitter` per child :1295; here fns_acc/next_* must survive so we save/restore instead) |
| `leave_function` | `fn(e, save: FnSave) -> Emitter2` | restore per-body fields; keep `next_var/next_fn/fns_acc/tree` | new |
| `add_function` | `fn(e, f: ir.Function) -> Emitter2` | `Emitter2(..e, fns_acc: [f, ..e.fns_acc])` | replaces emit.gleam:2533 |
| `take_functions` | `fn(e) -> List(ir.Function)` | `list.reverse(e.fns_acc)` — called once by M19 | new |
| `push_loop` | `fn(e, break_l: String, cont_l: String) -> Emitter2` | `push_frame(e, Loop2(break_l, cont_l, e.pending_label))` + clear pending_label | emit.gleam:2248-2262 |
| `push_block` | `fn(e, break_l: String, is_switch: Bool) -> Emitter2` | `push_frame(e, Block2(break_l, e.pending_label, is_switch))` + clear | emit.gleam:2283 |
| `push_barrier` | `fn(e, cleanup: BarrierCleanup) -> Emitter2` | prepend to `frames`; **does NOT clear pending_label** (emit.gleam:2291-2294 note) | emit.gleam:2295-2305 |
| `pop_frame` | `fn(e) -> Emitter2` | `let assert [_, ..rest] = e.frames` | emit.gleam:2311-2314 verbatim |
| `set_pending_label` | `fn(e, label: String) -> Emitter2` | `Emitter2(..e, pending_label: Some(label))` | emit.gleam LabeledStatement path |
| `find_break_target` | `fn(e, js_label: Option(String)) -> Result(#(String, List(BarrierCleanup)), EmitError)` | walk `frames`: collect `Barrier2.cleanup` crossed; stop at first `Loop2`/`Block2` matching (`js_label == None` → first Loop2 or Block2(is_switch: True); `Some(l)` → first frame with `js_label == Some(l)`). Return `(break_label, crossed_cleanups_in_order)`. Error `BreakOutsideLoop` if none. | new (arc's is imperative op-emitting; here we return data for M13/M17 to build the Expr) |
| `find_continue_target` | `fn(e, js_label: Option(String)) -> Result(#(String, List(BarrierCleanup)), EmitError)` | same walk; only `Loop2` is a target; `Block2` is transparent | new |

### Invariants M10 upholds
1. `next_var` / `next_fn` / `fns_acc` are **module-monotone** — never reset across `enter_function`. Guarantees globally-unique ir.Var names (required: `ir.Let` names must be unique within a Function body; sharing the counter across functions is over-strict but simple and matches emit_core.gleam:308's collision-avoidance intent).
2. `frames` is **per-function** — `enter_function` saves+clears it, `leave_function` restores. A `break` never crosses a function boundary.
3. `cursor` / `fn_cursor` consumption order == source order == `scope.finalize`'s numbering order (emit.gleam:224-227, :299-309). M12-M16 MUST call `enter_scope`/`enter_function` at exactly the AST positions the parser pushed scopes, else `ScopeCursorDesync`.
4. `pending_label` is consumed by exactly the next `push_loop`/`push_block`; `push_barrier` is transparent to it.

### Dropped from arc's Emitter (rationale)
- `code, constants_map, constants_list, next_const` — IR has inline `ConstAtom/ConstBinary/ConstF64`; no linear op stream (nested `ir.Expr` instead).
- `next_label: Int` → merged into `next_var` (IR labels are strings).
- `lexical_refs, references_arguments, code_kind` — arc propagated these UP to decide capture/boxing at the parent; in emit_2core, `scope.FunctionInfo.captures` (:247) already has the final capture list (computed by `scope.finalize`), so no upward propagation needed.
- `top_lex, deletable_global_vars, completion_var, param_scope_names, in_synth_default_ctor` — script/eval/REPL semantics; emit_2core targets module compilation only (M19).
- `ref_free, initialized` — bytecode slot-reuse / TDZ-checked-store optimizations. IR is SSA (fresh Let name per bind); TDZ is handled by M13 seeding let/const bindings as cell handles holding a `js_uninitialized` sentinel and `cell_get` checking it.

---

## M11 — `arc/src/arc/emit_2core/anf.gleam`

### Imports
```gleam
import arc/emit_2core/state.{type Emitter2, fresh_var}
import twocore/ir
import gleam/list
```

### Type aliases
```gleam
/// Every emit_* function's return shape. `ir.Expr` is the ANF tail
/// (right-nested Let chain terminating in Values/Return/Break/…);
/// Emitter2 carries fns_acc + counters forward.
pub type Emit = #(ir.Expr, Emitter2)
pub type EmitR = Result(Emit, state.EmitError)
```

### Core builders

```gleam
/// THE Let-chain builder. Evaluates `rhs`, binds it to a fresh var,
/// continues with `k` which receives that var as an ir.Value.
///   bind(e, CallHost("js","add",[a,b]), fn(sum, e) { … })
///   ==> Let(["_v7"], CallHost(…), <k body with Var("_v7")>)
pub fn bind(
  e: Emitter2,
  rhs: ir.Expr,
  k: fn(ir.Value, Emitter2) -> Emit,
) -> Emit {
  let #(v, e) = fresh_var(e)
  let #(body, e) = k(ir.Var(v), e)
  #(ir.Let([v], rhs, body), e)
}

/// Result-returning `bind` — `k` may fail. This is the workhorse; every
/// M12-M18 emit fn uses `use v, e <- anf.bind_try(e, rhs)`.
pub fn bind_try(
  e: Emitter2,
  rhs: ir.Expr,
  k: fn(ir.Value, Emitter2) -> EmitR,
) -> EmitR

/// Multi-result bind for rhs that yields a tuple (e.g. iterator
/// `{done, value}` from CallHost("js","iter_next",…)). Binds to ONE fresh
/// var then TupleGet-projects each field into `n` fresh vars.
///   bind_tuple(e, rhs, 2, fn([done, val], e) { … })
///   ==> Let([t], rhs,
///         Let([v0], TermOp(TupleGet(0),[t]),
///           Let([v1], TermOp(TupleGet(1),[t]), <k>)))
pub fn bind_tuple(
  e: Emitter2, rhs: ir.Expr, n: Int,
  k: fn(List(ir.Value), Emitter2) -> Emit,
) -> Emit

/// Sequence an effect-only expression (result discarded). Binds to "_".
///   seq(e, CallHost("js","cell_set",[c,v]), fn(e) { … })
///   ==> Let(["_"], CallHost(…), <k>)
/// Note: ir.Let names must be unique — "_" is special-cased by emit_core
/// as discard? NO (checked emit_core.gleam:308 — it gensyms). So seq uses
/// fresh_var and just ignores the value in k.
pub fn seq(e: Emitter2, effect: ir.Expr, k: fn(Emitter2) -> Emit) -> Emit

/// Lift an already-computed ir.Value into the Emit shape (terminal).
pub fn pure(e: Emitter2, v: ir.Value) -> Emit {
  #(ir.Values([v]), e)
}

/// Bind each element of `xs` left-to-right, then `k` receives all bound
/// Values. Used for argument lists, array elements.
pub fn bind_list(
  e: Emitter2,
  xs: List(a),
  each: fn(a, Emitter2) -> EmitR,
  k: fn(List(ir.Value), Emitter2) -> EmitR,
) -> EmitR
```

### JS-shaped helpers (built on `bind`)

```gleam
/// Shorthand for `bind(e, ir.CallHost("js", op, args), k)`.
/// EVERY rt_js call goes through this so M9's state-threading rewrite has
/// one seam. `args` are already-bound ir.Values. NO `St` param (D2 —
/// state threading is emit_core's job under `js_profile`).
pub fn host(e: Emitter2, op: String, args: List(ir.Value),
            k: fn(ir.Value, Emitter2) -> Emit) -> Emit

pub fn host_try(e, op, args, k) -> EmitR   // Result-returning variant

/// Emit the wire-encoded `ObjectKey` tuple for a STATIC (compile-time-known)
/// `ast.PropertyKey`. Returns an `ir.Expr` that evaluates to the exact term
/// `rt_js_obj` expects as a key (`{named, Binary}` / `{indexed, Int}` /
/// `{private, Uid}` per §2.3 wire encoding). Used by M12 (member/object-lit),
/// M15 (destructure), M16 (class) so key canonicalisation lives in ONE place.
/// Computed keys (`KeyComputed`) do NOT go through here — they are lowered
/// via `host("to_property_key", [v])` at the use site.
///   object_key_lit(KeyIdentifier("x", _))
///   ==> TermOp(MakeTuple, [ConstAtom("named"), ConstBinary(<<"x">>)])
///   object_key_lit(KeyNumber(FiniteNumber(3.0), _))
///   ==> TermOp(MakeTuple, [ConstAtom("indexed"), <BoxInt(W64) of ConstI64(3)>])
pub fn object_key_lit(pk: ast.PropertyKey) -> ir.Expr

/// Build a BEAM cons list from already-bound Values. Since ir.Value has no
/// nil literal (checked ir.gleam:471-495), the terminator is
/// `CallHost("js","empty_list",[])` (resolve_js:3652). Returns via `k`
/// because the result is a chain of Let-bound MakeCons, not a Value.
///   cons_list(e, [a,b], k)
///   ==> Let([nil], CallHost("js","empty_list",[]),
///         Let([c1], TermOp(MakeCons,[b, nil]),
///           Let([c0], TermOp(MakeCons,[a, c1]), k(c0, e))))
pub fn cons_list(e: Emitter2, vals: List(ir.Value),
                 k: fn(ir.Value, Emitter2) -> Emit) -> Emit

/// `if (ToBoolean(cond)) then_k else else_k`. Wraps the JS truthy coercion.
///   ==> Let([t], CallHost("js","truthy",[cond]),   // -> i32 1|0 (rt_js.gleam:122)
///         If(Var(t), [TTerm], <then>, <else>))
pub fn if_truthy(e: Emitter2, cond: ir.Value,
                 then_k: fn(Emitter2) -> Emit,
                 else_k: fn(Emitter2) -> Emit) -> Emit

/// The HANDOFF §5 / ir.gleam:965 numeric-fast-path pattern:
///   Let([an], TermTest(IsNumber, a),
///     Let([bn], TermTest(IsNumber, b),
///       Let([both], Num(I32And, [an, bn]),          // i32 &
///         If(both, [TTerm],
///           Let([r], NumTerm(fast_op, a, b), Values([r])),
///           Let([r], CallHost("js", slow_op, [a, b]), Values([r]))))))
/// `fast_op: Option(NumTermOp)` — None means "no fast path" (e.g. `/`,`%`
/// per HANDOFF §5 always go to rt_js because BEAM /0 traps).
/// For compare ops (NLt/NLe/NGt/NGe/NEq → i32), the fast arm additionally
/// wraps: `If(cmp_i32, [TTerm], Values([rc.true_]), Values([rc.false_]))`.
pub fn guarded_binop(
  e: Emitter2,
  fast_op: Option(ir.NumTermOp),
  slow_op: String,
  a: ir.Value, b: ir.Value,
  k: fn(ir.Value, Emitter2) -> Emit,
) -> Emit

/// `Let([v], TermTest(kind, arg), If(v, [TTerm], then, else))` — used by
/// M12 typeof, M15 destructure iterable-check.
pub fn if_term_is(e: Emitter2, kind: ir.TermKind, arg: ir.Value,
                  then_k: fn(Emitter2) -> Emit,
                  else_k: fn(Emitter2) -> Emit) -> Emit
```

### Invariants M11 upholds
1. **Right-nested Let**: every builder returns `Let(_, rhs, <deeper>)` where `<deeper>` is what `k` produced. The final `k` at the leaf produces a terminal Expr (`Values/Return/Break/Continue/If/…`). ir.gleam:668 — `Let` body is `Expr`, so arbitrary nesting is well-typed.
2. **One fresh_var per bind**: no ir.Var name is bound twice within a Function body (emit_core would shadow-warn; Core Erlang forbids). `seq` uses a fresh var too, not literal `"_"`.
3. **`host` is the only CallHost("js",…) constructor** in emit_2core/*. This is the seam M9 relies on: after M9 lands, `emit_core.emit_call_host` for `"js"` capability rebinds `cur` (state); if any emit_* module bypasses `anf.host` and emits `ir.CallHost` directly, it still works (M9 change is in emit_core, not here) — but the rule keeps grep-ability.
4. **`object_key_lit` is the only static-key constructor.** Every compile-time-known property key (identifier/string/number/bigint literal) goes through it; only `KeyComputed` bypasses to `host("to_property_key", …)`. Keeps the ObjectKey wire encoding in one place.
5. **guarded_binop's `Num(I32And,…)`**: `TermTest` yields i32 (ir.gleam:939 doc "→i32"); combining two i32 guards uses `ir.Num` with the i32-and op. Verified `ir.Num(op, args)` exists (:547) — the specific `NumOp` for i32-and needs lookup in ir.gleam's NumOp enum (not read; M12 implementer greps `I32And` in ir.gleam — it's the WASM numeric layer per HANDOFF §3). Fallback if awkward: nest two `If`s instead of `&`.

---

## Dependencies (DAG position)
- **M10 depends on**: `arc/compiler/scope` (unchanged), `arc/parser/ast` (unchanged), `twocore/ir` (unchanged). **No dependency on any other new module.** Can start immediately.
- **M11 depends on**: M10 only. Can start immediately after M10's type signatures land (or in parallel — M11 imports only `state.{Emitter2, fresh_var, EmitError}`, so a stub M10 with those three suffices).
- **M12-M18 depend on**: M10 + M11.
- **M10/M11 do NOT depend on**: M1-M9 (rt_js side). The `host(e, op, args, k)` helper emits `CallHost("js", op, …)` by string name; the op names are the M9 allowlist (`resolve_js` :3621-3656 today, extended by M9). Wrong op name = `UnknownJsOp` at emit_core time, not at arc-compile time — acceptable coupling.

---

## Gotchas (things a from-scratch implementer would trip on)

1. **CallClosure/CallHost("js",…) are state-NEUTRAL in emit_core today** (emit_core.gleam:1530-1531, :3554-3555). M10/M11 emit them anyway; **M9 must land** for the threaded store to actually thread through JS calls. Until M9 lands, compiled JS runs but every closure call sees a stale store (i.e. mutations through a closure are lost). Test order: M9 before M20 differential tests.

2. **`fns_acc` is module-global, not per-function.** arc's `emit.gleam` uses a fresh `new_emitter` per child (:1295) and returns nested `List(CompiledChild)` (:71). `ir.Module.functions` (:95) is FLAT, so emit_2core keeps ONE Emitter2 threaded through nested function compilations — hence `enter_function`/`leave_function` save/restore per-body fields but preserve `fns_acc/next_var/next_fn`.

3. **No `ir.Value` for `[]`**. `cons_list` MUST bind `CallHost("js","empty_list",[])` first (rt_js.gleam confirmed op exists via resolve_js:3652). Alternative for M11 v2: add `ir.ConstNil` to ir.gleam (2-line change, coordinate with 2core team) — but spec here uses the CallHost route to keep ir.gleam untouched.

4. **`guarded_binop` compare-op result is i32, not TTerm** (ir.gleam:1525-1526). The fast arm for `<`/`<=`/`>`/`>=`/`===` must wrap the i32 in `If(cmp, [TTerm], Values([rc.true_]), Values([rc.false_]))` to yield a JS boolean. The arithmetic ops (NAdd/NSub/NMul :1523-1524) yield TTerm directly.

5. **`enter_scope` empty-cursor case** (emit.gleam:1419-1430 comment): must NOT re-read current scope's children — stay in place. Port that exact behaviour or M13's `{}`-elision breaks scope sync.

6. **`push_barrier` must NOT clear `pending_label`** (emit.gleam:2291-2294): a `label: for (using x of …)` pushes IterClose barrier THEN Loop2; the label attaches to Loop2.

7. **`ir.If` cond is `ir.Value`, not `ir.Expr`** (ir.gleam:683). So `if_truthy` MUST bind the `CallHost("js","truthy",…)` result to a var first, then `If(Var(t), …)`. Same for `TermTest` in `guarded_binop` — bind first.

8. **`ir.Function.locals`** (:426): emit_2core always emits `locals: []`. All temporaries are `Let`-bound. arc's `fresh_slot` (which mutates `FunctionInfo.local_count`) has NO analogue — `fresh_var` replaces it entirely.

9. **`BarrierCleanup.FinallyBlock` carries a `ScopeSave`**: because re-emitting the finally body at each crossing site needs to enter the finally's block scope, and the scope cursor at the crossing point is somewhere ELSE. M17 saves the cursor position when the try is entered, and `FinallyBlock` carries it so each duplication can `leave_scope`→re-`enter_scope` correctly. This is the trickiest cross-module contract between M10 and M17.