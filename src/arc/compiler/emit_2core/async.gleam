//// M18: compile-time coroutine state-machine transform (regenerator-style)
//// for async / generator / async-generator bodies. SPEC.md §7.M18 + §9.
//// Emits an outer `jsf_N` (calls host <kind>_start) + a `jsf_N__sm` state
//// machine whose Return values are Step wire-tuples consumed by rt_js_async
//// (M8). Per-arm ir.Try wrapper, loc-tuple hoisted-local restore/pack,
//// self-looping yield*, try/finally-across-split via TryRegion+pending slot.
////
//// u-sig-seam: `emit_coroutine_fn` returns `Result(#(ir.Expr, Emitter2), _)`
//// (state.gleam + func.gleam amended per R14 precedent) — the outer wrapper's
//// closure-site is a Let-chain ir.Expr; ir.Value has no Expr-carrying variant.
////
//// u-func-seam / D13: SPEC.md:448's solid `func --> asyncE` edge is OVERRIDDEN
//// by §19.2 "no emit module imports another emit module directly". func.gleam
//// reaches this module via `e.dispatch.emit_async_body` ONLY; the func.gleam
//// helpers this module needs (`cap_param_name`, `build_ir_params`,
//// `emit_closure_site`, `expected_length`, `atom_bool`) are copied locally
//// below — grep "D13" for each site. Neither module imports the other.
////
//// ── SCOPE-CURSOR INVARIANT (u-scope-cursor) ─────────────────────────────
//// state.enter_scope / state.pop_child_fn (state.gleam:597-640) advance a
//// SOURCE-PRE-ORDER cursor: each call pops the next block/fn child scope in
//// the order the analyzer recorded them. Ordinary emission walks the AST in
//// that same order so the cursor stays in sync. The state machine breaks
//// that: arm bodies are AST FRAGMENTS emitted non-contiguously — a catch/
//// finally/delegate arm's fragment sits later in source than the arm emitted
//// before it, and a single source block can span several arms.
////
//// Reconciliation: split-analysis (which already walks the body in source
//// pre-order) threads an `ArmCursor` shadow of the emitter's cursor state,
//// advancing it via `cursor_enter_scope`/`cursor_pop_child_fn` exactly where
//// stmt.gleam would call `state.enter_scope`/`state.pop_child_fn`. At each
//// arm's fragment ENTRY it snapshots the shadow into `ArmSpec.entry_cursor`.
//// `emit_arm_body` then `install_cursor`s that snapshot onto the Emitter2
//// before delegating to `e.dispatch.emit_stmts`, so every arm — regardless
//// of emission order — sees the cursor position its fragment would have had
//// in a straight-line walk. Monotone fields (next_var/next_fn/fns_acc) are
//// NOT snapshotted; only cur_scope/scope_cursor/child_fn_cursor are.

import arc/compiler/ast_util
import arc/compiler/emit_2core/anf
import arc/compiler/emit_2core/state.{type Emitter2}
import arc/compiler/scope.{type ScopeId, type ScopeTree}
import arc/parser/ast
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import twocore/ir

// ── scope-cursor snapshot/restore (u-scope-cursor) ──────────────────────────

/// Source-position snapshot of the emitter's scope-walk cursor. Captured
/// per-arm during split-analysis; installed before each arm's fragment
/// emission. See module docstring "SCOPE-CURSOR INVARIANT".
pub type ArmCursor {
  ArmCursor(
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    child_fn_cursor: List(ScopeId),
  )
}

/// Cursor at function-body entry. Mirrors state.enter_function's cursor
/// reset (state.gleam:713-716) — the starting point for split-analysis's
/// shadow walk.
pub fn root_cursor(tree: ScopeTree, fn_scope: ScopeId) -> ArmCursor {
  ArmCursor(
    cur_scope: fn_scope,
    scope_cursor: state.block_child_scopes(tree, fn_scope),
    child_fn_cursor: scope.child_function_scopes(tree, fn_scope),
  )
}

/// Shadow of state.enter_scope (state.gleam:607-640): descend into the next
/// block-child scope. Returns `#(inner, resume)` where `inner` is the cursor
/// inside the child and `resume` is the parent cursor AFTER the child (what
/// state.leave_scope would restore). split-analysis recurses with `inner`
/// and continues siblings with `resume`. child_fn_cursor is per-FUNCTION,
/// so it threads through unchanged (matches ScopeSave2 not saving it).
pub fn cursor_enter_scope(
  tree: ScopeTree,
  c: ArmCursor,
) -> #(ArmCursor, ArmCursor) {
  case c.scope_cursor {
    [child, ..rest] -> #(
      ArmCursor(
        cur_scope: child,
        scope_cursor: state.block_child_scopes(tree, child),
        child_fn_cursor: c.child_fn_cursor,
      ),
      ArmCursor(..c, scope_cursor: rest),
    )
    // empty-cursor → stay put (state.gleam:629-638 semantics).
    [] -> #(c, c)
  }
}

/// Shadow of state.pop_child_fn (state.gleam:597-601): advance past one
/// nested FunctionExpression/Declaration/Arrow. split-analysis calls this
/// at each fn-site so a fragment starting after that site sees the correct
/// child_fn_cursor tail. Tolerates exhaustion (real pop asserts; the shadow
/// walk over-approximates on already-consumed cursors).
pub fn cursor_pop_child_fn(c: ArmCursor) -> ArmCursor {
  case c.child_fn_cursor {
    [_, ..rest] -> ArmCursor(..c, child_fn_cursor: rest)
    [] -> c
  }
}

/// Resume the parent cursor after a child block, carrying the child's
/// child_fn_cursor forward (fn-cursor is function-scoped, not block-scoped).
/// Use as `let resume = cursor_leave_scope(resume, inner_after)` when
/// split-analysis returns from a recursed block.
pub fn cursor_leave_scope(
  resume: ArmCursor,
  inner_after: ArmCursor,
) -> ArmCursor {
  ArmCursor(..resume, child_fn_cursor: inner_after.child_fn_cursor)
}

/// Install an arm's entry cursor onto the emitter before emitting its
/// fragment via e.dispatch.emit_stmts. Only cursor fields; slot_vars is
/// re-seeded per-arm from restore_locals, monotone fields thread through.
pub fn install_cursor(e: Emitter2, c: ArmCursor) -> Emitter2 {
  state.Emitter2(
    ..e,
    cur_scope: c.cur_scope,
    scope_cursor: c.scope_cursor,
    child_fn_cursor: c.child_fn_cursor,
  )
}

/// Snapshot the emitter's live cursor. Paired with install_cursor around
/// each arm so a later arm never observes cursor mutations from an earlier
/// (source-order-unrelated) arm.
pub fn capture_cursor(e: Emitter2) -> ArmCursor {
  ArmCursor(
    cur_scope: e.cur_scope,
    scope_cursor: e.scope_cursor,
    child_fn_cursor: e.child_fn_cursor,
  )
}

// ── shared internal types (types-and-skeleton) ──────────────────────────────

pub type SplitKind {
  SkAwait
  SkYield
  SkYieldStar
  SkForAwait
}

pub type SplitPoint {
  SplitPoint(id: Int, kind: SplitKind, enclosing_try: Option(Int))
}

pub type TryEntry {
  TryEntry(
    id: Int,
    catch_state: Option(Int),
    finally_state: Option(Int),
    after_state: Int,
    pending_loc_idx: Int,
    caught_loc_idx: Int,
    /// Enclosing TryEntry.id (§18.5 nesting): a re-throw at finally-end
    /// propagates to this region's catch/finally; None → step_throw/return.
    outer: Option(Int),
    /// AST payloads for the catch/finally arms — split-analysis records these
    /// so the orchestrator can hand them to emit_catch_arm / emit_finally_arm
    /// without re-walking the body.
    handler: Option(ast.CatchClause),
    finalizer: Option(List(ast.StmtWithLine)),
    /// Scope-cursor snapshots at catch/finally entry (SCOPE-CURSOR INVARIANT).
    catch_cursor: Option(ArmCursor),
    finally_cursor: Option(ArmCursor),
    /// Split-spanning break/continue targets in scope AT this try — seeds
    /// SmCtx.sm_labels for the catch/finally arm bodies (§18.4.5).
    sm_labels: List(SmLabel),
  )
}

/// One `yield*` self-looping arm (§18.6). loc indices are resolved from
/// `layout.extras` at emit time; `region` routes the arm-try wrapper.
pub type DelegateSpec {
  DelegateSpec(state_id: Int, next_state: Int, region: Option(Int))
}

/// One `for await (left of right) body` loop (u-for-await). Three states:
/// `head` calls `async_iter_next` and `step_await`s → `check`; `check` reads
/// the awaited iterresult, branches on `done` (→ `after`) or binds `value`
/// to `left` and enters the body (whose tail loops to `head`); `after` is
/// the continuation past the loop. `body_cursor` is the scope-cursor
/// snapshot at loop-body entry (SCOPE-CURSOR INVARIANT).
pub type ForAwaitSpec {
  ForAwaitSpec(
    head: Int,
    check: Int,
    body_s: Int,
    after: Int,
    left: ast.ForInit,
    body_cursor: ArmCursor,
    region: Option(Int),
  )
}

pub type LocLayout {
  LocLayout(
    slot_to_idx: Dict(Int, Int),
    size: Int,
    extras: Dict(String, Int),
    /// State-0 MakeTuple payload for the outer wrapper's `loc0` (§9:1752).
    initial_values: List(ir.Value),
  )
}

/// How execution reaches an arm. Drives §18.4.2 mode-dispatch: only
/// `AeResume` arms consult `sent.{mode,value}`; entry/jump arms skip it.
pub type ArmEntry {
  /// State 0. SPEC invariant "state 0 ignores sent".
  AeInitial
  /// Resumed after a split (rt_js_async re-invoked sm with fresh `sent`).
  AeResume(kind: SplitKind)
  /// Reached via `Continue(Lresume,[N,loc'])` from another arm — a §18.4.5
  /// control-flow join (if-merge, loop head, after-loop). No mode-dispatch.
  AeJump
}

/// How the fragment AFTER a split consumes sent_v — determined by the
/// syntactic position the await/yield occupied. Recorded on the resume
/// arm's `ArmSpec.resume` by split-analysis; consumed by fragment-emit.
pub type ResumeWith {
  /// `await p;` — result discarded.
  ResumeDiscard
  /// `x = await p` / `let {a} = await p` — bind sent_v via emit_destructure.
  ResumeBind(pat: ast.Pattern, mode: state.BindMode)
  /// `return await p` — resumed arm is `step_return(sent_v)`.
  ResumeReturn
  /// `throw await p` — resumed arm re-raises sent_v via route_abrupt.
  ResumeThrow
  /// `with (await p) { body }` — resumed arm wraps `body` in with(sent_v).
  ResumeWithScope(body: ast.Statement, line: Int)
}

/// How a segment (arm's `body_fragment`) terminates. `emit_arm_body`
/// (u-fragment-emit) emits `body_fragment` via `e.dispatch.emit_stmts` then
/// realizes this tail as the arm's terminal ir.Expr. §18.4.3-5.
pub type SegTail {
  /// Unconditional state transition: pack loc' → `Continue(Lresume,[to,loc'])`.
  FallTo(to: Int)
  /// Split point: emit `arg`, pack loc', `Return([{kind,v,ns,loc'}])`. §18.4.4.
  SplitAt(kind: SplitKind, arg: Option(ast.Expression), ns: Int)
  /// Eval `cond`, `If(truthy, Continue→then_s, Continue→else_s)`. Loop head/if.
  /// `cond: None` = head expression WAS `await c` — this tail sits in the
  /// resume arm; `ctx.sent_v` is the resolved value (§18.4.5 head-expr split).
  CondBranch(cond: Option(ast.Expression), then_s: Int, else_s: Int)
  /// for(;;) update state: emit `update` (if any) then FallTo(head).
  ForUpdate(update: Option(ast.Expression), head: Int)
  /// for-of/for-in setup: eval `right` → get_iterator(sync) → store handle at
  /// loc[iter_key] → Continue→head. (u-ctrl-split: prior arm's tail; head arm
  /// then reads the handle via ForOfStep.)
  ForOfSetup(right: ast.Expression, iter_key: String, head: Int)
  /// for-of/for-in step: `next(iter)` (iter at loc extra `iter_key`); done →
  /// after, else bind `left := value` → body_s.
  ForOfStep(
    left: ast.ForInit,
    iter_key: String,
    body_s: Int,
    after: Int,
    is_await: Bool,
  )
  /// switch dispatch: chain `strict_eq(disc,testᵢ)` → stateᵢ; None=default:;
  /// no default → unmatched falls to `after`.
  /// `disc: None` = discriminant WAS `await d` — resume arm reads `ctx.sent_v`.
  SwitchDispatch(
    disc: Option(ast.Expression),
    tests: List(#(Option(ast.Expression), Int)),
    after: Int,
  )
  /// for-await-of setup: eval `right` → get_iterator(async) → store handle at
  /// loc[for_await_iter_key(head)] → Continue→head. Head/check arms are then
  /// emitted from `plan.for_awaits` (u-for-await).
  ForAwaitSetup(right: ast.Expression, head: Int)
  /// §18.7 async-gen intermediate arm's tail: `step_yield(sent_v, ns, loc')`.
  /// The prior arm awaited the yield operand; sent_v is now the resolved value.
  AsyncGenYieldSent(ns: Int)
  /// End of function body: `step_return(undef)`.
  BodyEnd
  /// Fragment self-terminates (last stmt was Return/Throw/Break/Continue).
  SegDone
}

/// One state-machine switch arm's plan. `entry_cursor` is the scope-cursor
/// snapshot at this arm's fragment entry — see SCOPE-CURSOR INVARIANT.
pub type ArmSpec {
  ArmSpec(
    state_id: Int,
    region: Option(Int),
    entry_kind: ArmEntry,
    entry_cursor: ArmCursor,
    /// How this arm binds `sent_v` on entry (only meaningful when
    /// `entry_kind = AeResume(_)`). None → sent_v is unused/discarded.
    resume: Option(ResumeWith),
    body_fragment: List(ast.StmtWithLine),
    tail: SegTail,
    /// §18.4.5: split-spanning break/continue targets in scope for this arm.
    /// `emit_arm_body` seeds `SmCtx.sm_labels` from this so M13's resolver +
    /// `on_goto` route break/continue to the right state.
    sm_labels: List(SmLabel),
  )
}

pub type SplitPlan {
  SplitPlan(
    n_states: Int,
    arms: List(ArmSpec),
    try_entries: List(TryEntry),
    delegates: List(DelegateSpec),
    for_awaits: List(ForAwaitSpec),
  )
}

/// A break/continue target (loop / labeled block / switch) whose body spans
/// ≥1 split point, so it is compiled as SM STATES rather than an ir.Loop/
/// ir.Block. Before delegating a split-free fragment, `with_abrupt_intercept`
/// pushes a `state.Frame2` for each so M13's `find_break_target`/
/// `find_continue_target` still resolve; the frame's `ir_break`/`ir_continue`
/// are the *sentinel* strings held here, and the installed `on_goto` hook
/// maps a resolved sentinel back to its SM target state. `enclosing_try` is
/// the TryEntry.id of the split-spanning region DIRECTLY around this label —
/// a goto to it walks `try_stack` only up to (exclusive) that id.
pub type SmLabel {
  SmLoop(
    js_label: Option(String),
    brk_sentinel: String,
    cont_sentinel: String,
    break_state: Int,
    continue_state: Int,
    enclosing_try: Option(Int),
  )
  SmSwitch(
    js_label: Option(String),
    brk_sentinel: String,
    break_state: Int,
    enclosing_try: Option(Int),
  )
  SmLabeled(
    js_label: String,
    brk_sentinel: String,
    break_state: Int,
    enclosing_try: Option(Int),
  )
}

/// Threaded emit context for the arm walker (u-emit-ctx). Immutable-record
/// threading (Emitter2-style): every helper returns a fresh SmCtx. The arm
/// walker carries ONE SmCtx per state; on return the orchestrator reads
/// `finish_arms(ctx)`. `try_stack`/`sm_labels` are innermost-first — derived
/// per-arm via `with_region` so `route_abrupt`/`sentinel_match` see the
/// regions enclosing THAT arm's fragment, not the whole plan.
pub type SmCtx {
  SmCtx(
    kind: state.CoroutineKind,
    layout: LocLayout,
    lresume: String,
    mode_v: ir.Value,
    sent_v: ir.Value,
    loc_v: ir.Value,
    /// TryEntry lookup by id — §18.5 outer-region propagation.
    try_entries: List(TryEntry),
    /// Monotone state-id allocator for arms allocated at EMIT time (rare —
    /// almost every state is pre-allocated by split-analysis into `Ana`).
    /// Seeded from `plan.n_states` so late ids never collide.
    next_state: Int,
    /// Accumulated ir.SwitchArm bodies, PREPENDED. `finish_arms` reverses.
    arms: List(ir.SwitchArm),
    /// Split-spanning try regions enclosing the CURRENT fragment, innermost
    /// first. `route_abrupt` walks it: the innermost entry with a
    /// `finally_state` captures the completion into its `pending` loc slot.
    try_stack: List(TryEntry),
    /// Split-spanning break/continue targets visible from the CURRENT
    /// fragment, innermost first (mirrors frame_stack ordering).
    sm_labels: List(SmLabel),
  )
}

// ── SmCtx threading helpers (u-emit-ctx) ────────────────────────────────────

/// Initial SmCtx at arm-walk entry. mode_v/sent_v/loc_v are the fixed Var
/// names `emit_sm_function` binds (`_mode`/`_sv`/`_loc_i`). `next_state` is
/// seeded from `plan.n_states` so any emit-time `sm_alloc_state` yields a
/// fresh id past every analysis-allocated one.
pub fn new_sm_ctx(
  kind: state.CoroutineKind,
  layout: LocLayout,
  lresume: String,
  plan: SplitPlan,
) -> SmCtx {
  SmCtx(
    kind: kind,
    layout: layout,
    lresume: lresume,
    mode_v: ir.Var("_mode"),
    sent_v: ir.Var("_sv"),
    loc_v: ir.Var("_loc_i"),
    try_entries: plan.try_entries,
    next_state: plan.n_states,
    arms: [],
    try_stack: [],
    sm_labels: [],
  )
}

/// Allocate a fresh emit-time state id (past every analysis-allocated one).
/// Split-analysis's `Ana.alloc_state` covers the common case; this exists for
/// helpers that need a scratch state during arm emission.
pub fn sm_alloc_state(ctx: SmCtx) -> #(Int, SmCtx) {
  #(ctx.next_state, SmCtx(..ctx, next_state: ctx.next_state + 1))
}

/// Accumulate one emitted arm. Prepends — see `finish_arms`.
pub fn push_arm(ctx: SmCtx, n: Int, body: ir.Expr) -> SmCtx {
  SmCtx(..ctx, arms: [ir.SwitchArm(n, body), ..ctx.arms])
}

/// Source-order arm list for `emit_sm_function`. ir.Switch dispatches by
/// match-int so order is semantically irrelevant; reversed for dump legibility.
pub fn finish_arms(ctx: SmCtx) -> List(ir.SwitchArm) {
  list.reverse(ctx.arms)
}

/// Enter a split-containing try region. Paired with `pop_try` — the arm
/// walker pushes on TryStatement entry, pops after emitting catch/finally
/// arms. While pushed, `route_abrupt` routes throw/return/goto to this
/// region's states (§18.4 step 2, §18.5).
pub fn push_try(ctx: SmCtx, region: TryEntry) -> SmCtx {
  SmCtx(..ctx, try_stack: [region, ..ctx.try_stack])
}

pub fn pop_try(ctx: SmCtx) -> SmCtx {
  case ctx.try_stack {
    [_, ..rest] -> SmCtx(..ctx, try_stack: rest)
    // Unbalanced pop is an M18 bug, not user-reachable.
    [] -> panic as "async.pop_try: try_stack empty"
  }
}

/// Innermost enclosing try region — `None` means throw/mode==1 escapes the
/// sm as `step_throw`; `Some(r)` means it Continues to r's catch/finally.
pub fn current_try(ctx: SmCtx) -> Option(TryEntry) {
  case ctx.try_stack {
    [top, ..] -> Some(top)
    [] -> None
  }
}

/// Enter a split-spanning break/continue target (loop/switch/labeled block).
/// Paired with `pop_label`. While pushed, `sentinel_match` resolves M13's
/// break/continue to this SmLabel's target state.
pub fn push_label(ctx: SmCtx, label: SmLabel) -> SmCtx {
  SmCtx(..ctx, sm_labels: [label, ..ctx.sm_labels])
}

pub fn pop_label(ctx: SmCtx) -> SmCtx {
  case ctx.sm_labels {
    [_, ..rest] -> SmCtx(..ctx, sm_labels: rest)
    [] -> panic as "async.pop_label: sm_labels empty"
  }
}

/// Derive the per-arm ctx: `try_stack` becomes the innermost-first chain of
/// TryEntries enclosing `region` (walking `.outer` up to the root). Called by
/// `build_switch_arms` for each ArmSpec/TryEntry/DelegateSpec so that
/// `route_abrupt` inside that arm's fragment sees the correct enclosing
/// regions (§18.5 nesting). Monotone fields (`next_state`/`arms`) thread
/// through; `sm_labels` is left as-is (populated separately per fragment).
pub fn with_region(ctx: SmCtx, region: Option(Int)) -> SmCtx {
  SmCtx(..ctx, try_stack: try_chain(ctx.try_entries, region))
}

/// Per-arm ctx for a CATCH body: a throw here must NOT re-enter this entry's
/// catch (that would loop) — it goes to this entry's `finally_state` if any,
/// else propagates to `entry.outer`. Built as chain(outer) with a finally-only
/// view of `entry` pushed on top when it has a finalizer.
pub fn with_catch_body(ctx: SmCtx, entry: TryEntry) -> SmCtx {
  let outer = try_chain(ctx.try_entries, entry.outer)
  let stack = case entry.finally_state {
    Some(_) -> [TryEntry(..entry, catch_state: None), ..outer]
    None -> outer
  }
  SmCtx(..ctx, try_stack: stack)
}

/// Per-arm ctx for a FINALLY body: throw/return/goto here propagate straight
/// to `entry.outer` — a finally never intercepts itself.
pub fn with_finally_body(ctx: SmCtx, entry: TryEntry) -> SmCtx {
  SmCtx(..ctx, try_stack: try_chain(ctx.try_entries, entry.outer))
}

/// Innermost-first TryEntry chain from `region` to the root, via `.outer`.
fn try_chain(entries: List(TryEntry), region: Option(Int)) -> List(TryEntry) {
  case region {
    None -> []
    Some(id) ->
      case list.find(entries, fn(t) { t.id == id }) {
        Ok(entry) -> [entry, ..try_chain(entries, entry.outer)]
        Error(_) -> []
      }
  }
}

// ── u-abrupt-intercept: return/break/continue routing in delegated segments ──
//
// A split-free stmt batch is delegated wholesale to `dispatch.emit_stmts`
// (M13). Without this mechanism, `return v` inside it would emit a bare
// `ir.Return([v])` (leaking `v` as the sm's Step) and `break`/`continue`
// targeting a split-spanning loop would fail `find_break_target`. Fix:
// (1) push sentinel `state.Frame2`s for each `SmLabel` so M13's resolver
// walks past fragment-local frames and finds them; (2) install
// `state.SmAbrupt` hooks on Emitter2 — `on_return` routes via `route_abrupt`
// (step_return or pending+Continue-to-finally), `on_goto` checks the resolved
// ir label against the sentinel set and, if matched, routes via
// `route_abrupt`; unmatched (fragment-local) labels return None so M13
// emits its normal `ir.Break`.

/// A completion carried through a split-spanning `finally`'s pending slot
/// (§18.5; regenerator's `context.completion`). Wire encoding matches
/// rt_js_async's atom tags so `emit_finally_arm`'s pending-dispatch reads it.
pub type PendingKind {
  PkReturn(ir.Value)
  PkThrow(ir.Value)
  /// break/continue that resolved to SM state `target`.
  PkGoto(target: Int)
}

fn pending_tuple(pk: PendingKind) -> ir.Expr {
  case pk {
    PkReturn(v) -> ir.TermOp(ir.MakeTuple, [ir.ConstAtom("return"), v])
    PkThrow(v) -> ir.TermOp(ir.MakeTuple, [ir.ConstAtom("throw"), v])
    PkGoto(target) ->
      ir.TermOp(ir.MakeTuple, [ir.ConstAtom("goto"), ir.ConstI32(target)])
  }
}

/// SM `Continue(Lresume, [ConstI32(state), loc'])` — the ONLY way an arm
/// hands control to another state without suspending (§18.4).
fn sm_continue(ctx: SmCtx, target: Int, loc: ir.Value) -> ir.Expr {
  ir.Continue(ctx.lresume, [ir.ConstI32(target), loc])
}

/// Direct-CPS loc pack (u-step-and-loc-ir owns the anf.Build variant). Builds
/// a fresh `loc'` where slot i = overrides[i] if present, else the CURRENT
/// ssa var for a hoisted local (fragment may have reassigned it since
/// restore_locals), else copy-forward via `TupleGet(i, loc_v)`.
fn pack_loc_cps(
  e: Emitter2,
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  k: fn(Emitter2, ir.Value) -> ir.Expr,
) -> ir.Expr {
  pack_loc_from(e, ctx, overrides, 0, [], k)
}

fn pack_loc_from(
  e: Emitter2,
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
  k: fn(Emitter2, ir.Value) -> ir.Expr,
) -> ir.Expr {
  case i >= ctx.layout.size {
    True -> {
      let #(name, e) = state.fresh_var(e)
      ir.Let(
        [name],
        ir.TermOp(ir.MakeTuple, list.reverse(acc)),
        k(e, ir.Var(name)),
      )
    }
    False ->
      case dict.get(overrides, i) {
        Ok(v) -> pack_loc_from(e, ctx, overrides, i + 1, [v, ..acc], k)
        Error(_) ->
          case slot_at_loc_idx(ctx.layout, i) {
            Some(slot) -> {
              let v = ir.Var(state.get_slot_var(e, slot))
              pack_loc_from(e, ctx, overrides, i + 1, [v, ..acc], k)
            }
            None -> {
              let #(name, e) = state.fresh_var(e)
              ir.Let(
                [name],
                ir.TermOp(ir.TupleGet(i), [ctx.loc_v]),
                pack_loc_from(
                  e,
                  ctx,
                  overrides,
                  i + 1,
                  [ir.Var(name), ..acc],
                  k,
                ),
              )
            }
          }
      }
  }
}

fn slot_at_loc_idx(layout: LocLayout, idx: Int) -> Option(Int) {
  dict.fold(layout.slot_to_idx, None, fn(found, slot, at) {
    case found, at == idx {
      None, True -> Some(slot)
      _, _ -> found
    }
  })
}

/// Route an abrupt completion out of the current fragment (§18.5). Walks
/// `try_stack` (innermost first): the FIRST entry with `finally_state=Some(fs)`
/// captures the completion — pack `{kind,carry}` into its `pending` loc slot
/// and Continue to `fs`. A catch-only entry catches PkThrow (Continue to
/// catch_state with caught=v) but is transparent to return/goto. If nothing
/// intercepts: PkReturn→step_return, PkThrow→step_throw, PkGoto(t)→
/// pack_loc + Continue to t. `stop_at` bounds the walk for goto — only regions
/// STRICTLY inside the target label intercept it.
pub fn route_abrupt(
  e: Emitter2,
  ctx: SmCtx,
  pk: PendingKind,
  stop_at: Option(Int),
  sink: fn(Emitter2) -> Nil,
) -> ir.Expr {
  route_abrupt_walk(e, ctx, ctx.try_stack, pk, stop_at, sink)
}

fn route_abrupt_walk(
  e: Emitter2,
  ctx: SmCtx,
  stack: List(TryEntry),
  pk: PendingKind,
  stop_at: Option(Int),
  sink: fn(Emitter2) -> Nil,
) -> ir.Expr {
  case stack {
    [] -> route_abrupt_tail(e, ctx, pk, sink)
    [entry, ..rest] ->
      case stop_at == Some(entry.id) {
        True -> route_abrupt_tail(e, ctx, pk, sink)
        False ->
          case entry.finally_state {
            Some(fs) -> {
              let #(pv, e) = state.fresh_var(e)
              ir.Let([pv], pending_tuple(pk), {
                let over =
                  dict.from_list([#(entry.pending_loc_idx, ir.Var(pv))])
                pack_loc_cps(e, ctx, over, fn(e, loc) {
                  let _ = sink(e)
                  sm_continue(ctx, fs, loc)
                })
              })
            }
            None ->
              case pk {
                PkThrow(v) ->
                  case entry.catch_state {
                    Some(cs) -> {
                      let over = dict.from_list([#(entry.caught_loc_idx, v)])
                      pack_loc_cps(e, ctx, over, fn(e, loc) {
                        let _ = sink(e)
                        sm_continue(ctx, cs, loc)
                      })
                    }
                    None -> route_abrupt_walk(e, ctx, rest, pk, stop_at, sink)
                  }
                _ -> route_abrupt_walk(e, ctx, rest, pk, stop_at, sink)
              }
          }
      }
  }
}

fn route_abrupt_tail(
  e: Emitter2,
  ctx: SmCtx,
  pk: PendingKind,
  sink: fn(Emitter2) -> Nil,
) -> ir.Expr {
  case pk {
    PkReturn(v) -> {
      let _ = sink(e)
      step_return(v)
    }
    PkThrow(v) -> {
      let _ = sink(e)
      step_throw(v)
    }
    PkGoto(target) ->
      pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
        let _ = sink(e)
        sm_continue(ctx, target, loc)
      })
  }
}

fn sentinel_match(labels: List(SmLabel), ir_label: String) -> Option(SmLabel) {
  list.find(labels, fn(l) {
    case l {
      SmLoop(brk_sentinel: b, cont_sentinel: c, ..) ->
        ir_label == b || ir_label == c
      SmSwitch(brk_sentinel: b, ..) | SmLabeled(brk_sentinel: b, ..) ->
        ir_label == b
    }
  })
  |> option.from_result
}

fn sentinel_target(l: SmLabel, ir_label: String) -> #(Int, Option(Int)) {
  case l {
    SmLoop(
      brk_sentinel: b,
      break_state: bs,
      continue_state: cs,
      enclosing_try: et,
      ..,
    ) ->
      case ir_label == b {
        True -> #(bs, et)
        False -> #(cs, et)
      }
    SmSwitch(break_state: bs, enclosing_try: et, ..) -> #(bs, et)
    SmLabeled(break_state: bs, enclosing_try: et, ..) -> #(bs, et)
  }
}

fn make_on_return(
  ctx: SmCtx,
  sink: fn(Emitter2) -> Nil,
) -> fn(Emitter2, ir.Value) -> Result(ir.Expr, state.EmitError) {
  fn(e, v) { Ok(route_abrupt(e, ctx, PkReturn(v), None, sink)) }
}

fn make_on_goto(
  ctx: SmCtx,
  sink: fn(Emitter2) -> Nil,
) -> fn(Emitter2, String) -> Option(Result(ir.Expr, state.EmitError)) {
  fn(e, ir_label) {
    case sentinel_match(ctx.sm_labels, ir_label) {
      None -> None
      Some(label) -> {
        let #(target, stop) = sentinel_target(label, ir_label)
        Some(Ok(route_abrupt(e, ctx, PkGoto(target), stop, sink)))
      }
    }
  }
}

/// Push sentinel Frame2s for every split-spanning target in `ctx.sm_labels`
/// (so M13 resolves break/continue), install `state.SmAbrupt` hooks routing
/// return + resolved sentinels through `route_abrupt`, then run `body`. `body`
/// receives the prepared emitter and a `restore` fn that removes exactly what
/// was installed. Frames are pushed OUTERMOST FIRST so frame_stack head is
/// innermost (matching M13's walk order).
pub fn with_abrupt_intercept(
  e: Emitter2,
  ctx: SmCtx,
  body: fn(Emitter2, fn(Emitter2) -> Emitter2) -> a,
) -> a {
  // SmAbrupt hooks return bare ir.Expr (state.gleam:220-224) but allocate
  // fresh_vars via pack_loc_cps; sink the leaf next_var so `restore` can merge
  // it and later arms don't reallocate the same _tN names (ir.gleam:419-420).
  let hook_cell = make_ref()
  let _ = pdict_put(hook_cell, e.next_var)
  let sink = fn(ef: Emitter2) -> Nil {
    let cur: Int = pdict_erase(hook_cell)
    let nv = case ef.next_var > cur {
      True -> ef.next_var
      False -> cur
    }
    let _ = pdict_put(hook_cell, nv)
    Nil
  }
  let #(e, n_pushed) = push_sm_frames(e, ctx.sm_labels)
  let e =
    state.set_sm_abrupt(
      e,
      state.SmAbrupt(
        on_return: make_on_return(ctx, sink),
        on_goto: make_on_goto(ctx, sink),
      ),
    )
  let restore = fn(e: Emitter2) {
    let hook_nv: Int = pdict_erase(hook_cell)
    let e = case hook_nv > e.next_var {
      True -> state.Emitter2(..e, next_var: hook_nv)
      False -> e
    }
    pop_n_frames(state.clear_sm_abrupt(e), n_pushed)
  }
  body(e, restore)
}

fn push_sm_frames(e: Emitter2, labels: List(SmLabel)) -> #(Emitter2, Int) {
  // sm_labels is innermost-first; push_frame prepends → fold from LIST TAIL
  // (outermost) so frame_stack head ends up innermost.
  list.fold(list.reverse(labels), #(e, 0), fn(acc, lab) {
    let #(e, n) = acc
    let frame = case lab {
      SmLoop(js_label:, brk_sentinel:, cont_sentinel:, ..) ->
        state.Loop2(
          ir_break: brk_sentinel,
          ir_continue: cont_sentinel,
          js_label:,
          carried: [],
          iter_close: None,
        )
      SmSwitch(js_label:, brk_sentinel:, ..) ->
        state.Switch2(ir_break: brk_sentinel, js_label:, carried: [])
      SmLabeled(js_label:, brk_sentinel:, ..) ->
        state.Labeled2(ir_break: brk_sentinel, js_label:, carried: [])
    }
    #(state.push_frame(e, frame), n + 1)
  })
}

fn pop_n_frames(e: Emitter2, n: Int) -> Emitter2 {
  case n {
    0 -> e
    _ -> pop_n_frames(state.pop_frame(e), n - 1)
  }
}

// ── §18.2 pass-1 leaf predicates (u-split-analysis) ─────────────────────────
// Pure `has_split` over the FULL ast.Expression / ast.Statement variant set.
// Nested function/class-method/static-block bodies are OPAQUE — an await
// inside a nested async fn is that fn's split, not ours.

/// True iff `e` (or any sub-expression NOT crossing a function boundary)
/// contains an await/yield split point.
pub fn expr_has_split(e: ast.Expression) -> Bool {
  case e {
    ast.AwaitExpression(..) | ast.YieldExpression(..) -> True
    ast.Identifier(..)
    | ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.ThisExpression(..)
    | ast.SuperExpression(..)
    | ast.MetaProperty(..)
    | ast.RegExpLiteral(..)
    | ast.IntrinsicTemplateObject(..) -> False
    // function boundaries — do NOT descend into body
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) -> False
    ast.UnaryExpression(argument: a, ..)
    | ast.UpdateExpression(argument: a, ..)
    | ast.SpreadElement(argument: a, ..)
    | ast.ParenthesizedExpression(expression: a, ..) -> expr_has_split(a)
    ast.BinaryExpression(left: l, right: r, ..)
    | ast.LogicalExpression(left: l, right: r, ..)
    | ast.AssignmentExpression(left: l, right: r, ..) ->
      expr_has_split(l) || expr_has_split(r)
    ast.MemberExpression(object: o, property: p, ..)
    | ast.OptionalMemberExpression(object: o, property: p, ..) ->
      expr_has_split(o) || member_prop_has_split(p)
    ast.CallExpression(callee: c, arguments: args, ..)
    | ast.OptionalCallExpression(callee: c, arguments: args, ..)
    | ast.NewExpression(callee: c, arguments: args, ..) ->
      expr_has_split(c) || list.any(args, expr_has_split)
    ast.ConditionalExpression(condition: c, consequent: t, alternate: f, ..) ->
      expr_has_split(c) || expr_has_split(t) || expr_has_split(f)
    ast.SequenceExpression(expressions: xs, ..) -> list.any(xs, expr_has_split)
    ast.ArrayExpression(elements: xs, ..) ->
      list.any(xs, fn(o) { opt_expr_has_split(o) })
    ast.ObjectExpression(properties: ps, ..) -> list.any(ps, prop_has_split)
    ast.TemplateLiteral(parts: parts, ..) ->
      list.any(ast.template_expressions(parts), expr_has_split)
    ast.TaggedTemplateExpression(tag: t, parts: parts, ..) ->
      expr_has_split(t)
      || list.any(ast.template_expressions(parts), expr_has_split)
    ast.ImportExpression(source: s, options: o, ..) ->
      expr_has_split(s) || opt_expr_has_split(o)
    // class: computed keys / field inits / superclass are in the ENCLOSING
    // coroutine's scope; method/accessor/static-block bodies are opaque.
    ast.ClassExpression(super_class: sc, body: elems, ..) ->
      opt_expr_has_split(sc) || list.any(elems, class_elem_has_split)
  }
}

fn member_prop_has_split(p: ast.MemberProperty) -> Bool {
  case p {
    ast.Dot(..) -> False
    ast.Bracket(expression: e) -> expr_has_split(e)
  }
}

fn opt_expr_has_split(o: Option(ast.Expression)) -> Bool {
  case o {
    Some(e) -> expr_has_split(e)
    None -> False
  }
}

fn prop_has_split(p: ast.Property) -> Bool {
  case p {
    ast.InitProperty(key: k, value: v, ..) ->
      key_has_split(k) || expr_has_split(v)
    ast.MethodProperty(key: k, ..) | ast.AccessorProperty(key: k, ..) ->
      key_has_split(k)
    ast.SpreadProperty(argument: a) -> expr_has_split(a)
  }
}

fn key_has_split(k: ast.PropertyKey) -> Bool {
  case k {
    ast.KeyComputed(expression: e) -> expr_has_split(e)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> False
  }
}

fn class_elem_has_split(ce: ast.ClassElement) -> Bool {
  case ce {
    ast.ClassMethod(key: k, ..) -> key_has_split(k)
    // ES §15.7: field initializers are wrapped in an implicit function
    // ([~Await] context) — opaque to the ENCLOSING coroutine. Only the
    // computed key is evaluated in this scope.
    ast.ClassField(key: k, ..) -> key_has_split(k)
    ast.StaticBlock(..) -> False
  }
}

fn pattern_has_split(p: ast.Pattern) -> Bool {
  case p {
    ast.IdentifierPattern(..) -> False
    ast.AssignmentPattern(left: l, right: r) ->
      pattern_has_split(l) || expr_has_split(r)
    ast.RestElement(argument: a) -> pattern_has_split(a)
    ast.ArrayPattern(elements: xs) ->
      list.any(xs, fn(o) {
        case o {
          Some(x) -> pattern_has_split(x)
          None -> False
        }
      })
    ast.ObjectPattern(properties: ps) ->
      list.any(ps, fn(pp) {
        case pp {
          ast.PatternProperty(key: k, value: v, ..) ->
            key_has_split(k) || pattern_has_split(v)
          ast.RestProperty(..) -> False
        }
      })
  }
}

fn for_init_has_split(fi: ast.ForInit) -> Bool {
  case fi {
    ast.ForInitExpression(e) -> expr_has_split(e)
    ast.ForInitDeclaration(declarations: ds, ..) ->
      list.any(ds, declarator_has_split)
    ast.ForInitPattern(p) -> pattern_has_split(p)
  }
}

fn declarator_has_split(d: ast.VariableDeclarator) -> Bool {
  pattern_has_split(d.id) || opt_expr_has_split(d.init)
}

/// True iff `s` (or any sub-statement/-expression NOT crossing a function
/// boundary) contains an await/yield/for-await split point.
pub fn stmt_has_split(s: ast.Statement) -> Bool {
  case s {
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..) -> False
    ast.FunctionDeclaration(..) -> False
    ast.ClassDeclaration(super_class: sc, body: elems, ..) ->
      opt_expr_has_split(sc) || list.any(elems, class_elem_has_split)
    ast.ExpressionStatement(expression: e, ..)
    | ast.ThrowStatement(argument: e) -> expr_has_split(e)
    ast.ReturnStatement(argument: a) -> opt_expr_has_split(a)
    ast.BlockStatement(body: b) -> stmts_have_split(b)
    ast.VariableDeclaration(declarations: ds, ..) ->
      list.any(ds, declarator_has_split)
    ast.IfStatement(condition: c, consequent: t, alternate: f) ->
      expr_has_split(c)
      || stmt_has_split(t)
      || case f {
        Some(a) -> stmt_has_split(a)
        None -> False
      }
    ast.WhileStatement(condition: c, body: b)
    | ast.DoWhileStatement(condition: c, body: b) ->
      expr_has_split(c) || stmt_has_split(b)
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      case i {
        Some(fi) -> for_init_has_split(fi)
        None -> False
      }
      || opt_expr_has_split(c)
      || opt_expr_has_split(u)
      || stmt_has_split(b)
    ast.ForInStatement(left: l, right: r, body: b) ->
      for_init_has_split(l) || expr_has_split(r) || stmt_has_split(b)
    ast.ForOfStatement(left: l, right: r, body: b, is_await: aw) ->
      aw || for_init_has_split(l) || expr_has_split(r) || stmt_has_split(b)
    ast.SwitchStatement(discriminant: d, cases: cs) ->
      expr_has_split(d)
      || list.any(cs, fn(c: ast.SwitchCase) {
        opt_expr_has_split(c.condition) || stmts_have_split(c.consequent)
      })
    ast.TryStatement(block: b, tail: t) ->
      stmts_have_split(b) || try_tail_has_split(t)
    ast.LabeledStatement(body: b, ..) -> stmt_has_split(b)
    ast.WithStatement(object: o, body: b) ->
      expr_has_split(o) || stmt_has_split(b)
  }
}

fn try_tail_has_split(t: ast.TryTail) -> Bool {
  case t {
    ast.TryCatch(handler: h) -> catch_has_split(h)
    ast.TryFinally(finalizer: f) -> stmts_have_split(f)
    ast.TryCatchFinally(handler: h, finalizer: f) ->
      catch_has_split(h) || stmts_have_split(f)
  }
}

fn catch_has_split(c: ast.CatchClause) -> Bool {
  case c.param {
    Some(p) -> pattern_has_split(p)
    None -> False
  }
  || stmts_have_split(c.body)
}

fn stmts_have_split(ss: List(ast.StmtWithLine)) -> Bool {
  list.any(ss, fn(s: ast.StmtWithLine) { stmt_has_split(s.statement) })
}

// ── §18.2 pass-1: analyze_splits (u-split-analysis) ─────────────────────────

/// Threaded accumulator for the source-pre-order walk. `cur` is the shadow
/// scope-cursor (SCOPE-CURSOR INVARIANT) at the CURRENT source position.
/// §18.4.5 (u-ctrl-split): the walk maintains an OPEN segment (`open_*` +
/// `frag_rev`) — split-free stmts append to `frag_rev`; a split or a
/// split-containing control-flow construct closes the open segment (→ arms)
/// and opens the next one at the target state.
type Ana {
  Ana(
    tree: ScopeTree,
    kind: state.CoroutineKind,
    next_state: Int,
    next_try: Int,
    /// Unique-sentinel counter for SmLabel brk/cont sentinel strings.
    next_sentinel: Int,
    try_stack: List(Int),
    /// §18.4.5 loop_stack: split-spanning break/continue targets in scope at
    /// the CURRENT source position (innermost first). Snapshotted into each
    /// arm's `sm_labels` at close time.
    sm_labels: List(SmLabel),
    cur: ArmCursor,
    splits: List(SplitPoint),
    tries: List(TryEntry),
    arms: List(ArmSpec),
    delegates: List(DelegateSpec),
    for_awaits: List(ForAwaitSpec),
    // ── open segment (u-ctrl-split) ──
    frag_rev: List(ast.StmtWithLine),
    open_state: Int,
    open_region: Option(Int),
    open_entry: ArmEntry,
    open_cursor: ArmCursor,
    open_labels: List(SmLabel),
    open_resume: Option(ResumeWith),
  )
}

fn ana_region(a: Ana) -> Option(Int) {
  case a.try_stack {
    [top, ..] -> Some(top)
    [] -> None
  }
}

fn alloc_state(a: Ana) -> #(Int, Ana) {
  #(a.next_state, Ana(..a, next_state: a.next_state + 1))
}

// ── open-segment helpers (u-ctrl-split) ─────────────────────────────────────

/// Mint a unique sentinel ir-label string for an SmLabel brk/cont target.
/// Distinct namespace from state.fresh_label so plan-time sentinels never
/// collide with emit-time labels.
fn alloc_sentinel(a: Ana) -> #(String, Ana) {
  #(
    "_Lsm" <> int.to_string(a.next_sentinel),
    Ana(..a, next_sentinel: a.next_sentinel + 1),
  )
}

/// Append a split-free statement to the open segment's fragment.
fn frag_push(a: Ana, sl: ast.StmtWithLine) -> Ana {
  Ana(..a, frag_rev: [sl, ..a.frag_rev])
}

/// Close the open segment with `tail`, push it to `arms`, and open a fresh
/// empty segment at `new_state` with entry-cursor = the CURRENT live cursor
/// and sm_labels snapshot = the CURRENT loop_stack.
fn close_open(a: Ana, tail: SegTail, new_state: Int, entry: ArmEntry) -> Ana {
  let arm =
    ArmSpec(
      state_id: a.open_state,
      region: a.open_region,
      entry_kind: a.open_entry,
      entry_cursor: a.open_cursor,
      resume: a.open_resume,
      body_fragment: list.reverse(a.frag_rev),
      tail: tail,
      sm_labels: a.open_labels,
    )
  Ana(
    ..a,
    arms: [arm, ..a.arms],
    frag_rev: [],
    open_state: new_state,
    open_region: ana_region(a),
    open_entry: entry,
    open_cursor: a.cur,
    open_labels: a.sm_labels,
    open_resume: None,
  )
}

fn push_sm_label(a: Ana, l: SmLabel) -> Ana {
  // Re-snapshot open_labels: every plan_ctrl_* opens the body's first segment
  // via close_open BEFORE pushing this label, so the open segment must see it.
  let sm_labels = [l, ..a.sm_labels]
  Ana(..a, sm_labels:, open_labels: sm_labels)
}

fn pop_sm_label(a: Ana) -> Ana {
  case a.sm_labels {
    [_, ..rest] -> Ana(..a, sm_labels: rest)
    [] -> a
  }
}

/// Record one split of `kind` at the current source position: close the open
/// segment with `SplitAt(kind, arg, ns)` and open the resume arm at `ns`
/// (`entry_kind = AeResume(kind)`, entry_cursor = CURRENT shadow cursor).
/// `arg` is the await/yield operand — threaded to `SplitAt.arg` so
/// `emit_seg_tail` evaluates the REAL operand before suspending (§9).
/// `resume` is stamped onto the OPENED arm's `open_resume`.
fn record_split(
  a: Ana,
  kind: SplitKind,
  arg: Option(ast.Expression),
  resume: Option(ResumeWith),
) -> #(Int, Ana) {
  let #(ns, a) = alloc_state(a)
  let region = ana_region(a)
  let sp = SplitPoint(id: ns, kind:, enclosing_try: region)
  let a = Ana(..a, splits: [sp, ..a.splits])
  let a = close_open(a, SplitAt(kind:, arg:, ns:), ns, AeResume(kind))
  #(ns, Ana(..a, open_resume: resume))
}

/// §18.6 `yield*`: allocate self-looping delegate state Nd and follow-on
/// state; close the pre-delegate segment with a setup tail targeting Nd;
/// push a DelegateSpec; open the follow segment. Nd itself is NOT a fragment
/// arm — build_switch_arms emits it from `plan.delegates`.
fn record_delegate(
  a: Ana,
  arg: Option(ast.Expression),
  resume: Option(ResumeWith),
) -> Ana {
  let #(nd, a) = alloc_state(a)
  let #(follow, a) = alloc_state(a)
  let region = ana_region(a)
  let sp = SplitPoint(id: nd, kind: SkYieldStar, enclosing_try: region)
  let a = Ana(..a, splits: [sp, ..a.splits])
  // Setup tail evaluates `arg` and enters Nd; resume segment opens at follow.
  let a =
    close_open(
      a,
      SplitAt(kind: SkYieldStar, arg:, ns: nd),
      follow,
      AeResume(SkYieldStar),
    )
  let d = DelegateSpec(state_id: nd, next_state: follow, region:)
  Ana(..a, delegates: [d, ..a.delegates], open_resume: resume)
}

/// Shadow-enter one block-scope, run `f` with the inner cursor, then
/// resume with the parent cursor (carrying child_fn_cursor forward).
/// SCOPE-CURSOR × open-segment: if the currently-open segment is EMPTY,
/// re-snapshot its `open_cursor` to `inner` so a fragment starting inside
/// this scope emits with the right cur_scope; on exit, if `open_cursor`
/// still points inside this scope, resync it to the resumed parent cursor
/// (a `close_open(..., after)` inside `f` would otherwise leave the after-
/// arm's entry_cursor pointing at the inner scope). Non-empty on entry ⇒
/// the fragment spans two scopes; `close_open(FallTo)` a fresh state so each
/// scope gets its own arm/entry_cursor.
fn with_scope(a: Ana, f: fn(Ana) -> Ana) -> Ana {
  let #(inner, resume) = cursor_enter_scope(a.tree, a.cur)
  let a = case a.frag_rev {
    [] -> Ana(..a, open_cursor: inner)
    [_, ..] -> {
      let #(fresh, a) = alloc_state(a)
      close_open(a, FallTo(fresh), fresh, AeJump)
    }
  }
  let a_in = f(Ana(..a, cur: inner))
  let resumed = cursor_leave_scope(resume, a_in.cur)
  let open_cursor = case a_in.open_cursor.cur_scope == inner.cur_scope {
    True -> resumed
    False -> a_in.open_cursor
  }
  Ana(..a_in, cur: resumed, open_cursor:)
}

/// `with_scope` gated on `cond` — mirrors stmt.gleam's CONDITIONAL scope
/// entry (block_has_declarations / for_classic_init_is_lex). When False the
/// analyzer pruned the node (scope.gleam sb_close_block), so consuming a
/// cursor entry here would steal the next sibling's scope id.
fn with_scope_if(a: Ana, cond: Bool, f: fn(Ana) -> Ana) -> Ana {
  case cond {
    True -> with_scope(a, f)
    False -> f(a)
  }
}

/// Shadow-cursor walk of a catch clause — mirrors stmt.emit_catch_handler /
/// exn.emit_catch_arm exactly: `catch(p){b}` enters the Catch scope then
/// conditionally the body block; `catch{b}` enters NO Catch scope.
fn walk_catch_cur(a: Ana, h: ast.CatchClause, f: fn(Ana) -> Ana) -> Ana {
  let body_has = ast_util.block_has_declarations(h.body)
  case h.param {
    Some(_) -> with_scope(a, fn(a) { with_scope_if(a, body_has, f) })
    None -> with_scope_if(a, body_has, f)
  }
}

fn ana_opt_expr(a: Ana, o: Option(ast.Expression), tail) -> Ana {
  case o {
    Some(e) -> ana_expr(a, e, tail)
    None -> a
  }
}

/// Walk one expression subtree in evaluation order, recording every split.
/// `tail` is the enclosing statement-list tail AFTER the statement holding
/// this expression — it becomes the resume arm's body_fragment.
fn ana_expr(a: Ana, e: ast.Expression, tail: List(ast.StmtWithLine)) -> Ana {
  case e {
    ast.AwaitExpression(argument: arg, ..) -> {
      let a = ana_expr(a, arg, tail)
      let #(_, a) = record_split(a, SkAwait, Some(arg), None)
      a
    }
    ast.YieldExpression(argument: arg, is_delegate: del, ..) -> {
      let a = ana_opt_expr(a, arg, tail)
      case del {
        // §18.6: self-looping delegate state Nd + follow-on done-state.
        True -> record_delegate(a, arg, None)
        False ->
          case a.kind {
            state.CorAsyncGen -> {
              // §18.7: `yield x` = Await(x) THEN Yield(awaited). Await-phase
              // arm carries `arg`; the intermediate arm (empty fragment) has
              // tail AsyncGenYieldSent(ns2) → step_yield(sent_v, ns2, loc').
              let #(_, a) = record_split(a, SkAwait, arg, None)
              let #(ns2, a) = alloc_state(a)
              let sp =
                SplitPoint(id: ns2, kind: SkYield, enclosing_try: ana_region(a))
              let a = Ana(..a, splits: [sp, ..a.splits])
              close_open(a, AsyncGenYieldSent(ns: ns2), ns2, AeResume(SkYield))
            }
            state.CorAsync | state.CorGenerator -> {
              let #(_, a) = record_split(a, SkYield, arg, None)
              a
            }
          }
      }
    }
    // leaves
    ast.Identifier(..)
    | ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.ThisExpression(..)
    | ast.SuperExpression(..)
    | ast.MetaProperty(..)
    | ast.RegExpLiteral(..)
    | ast.IntrinsicTemplateObject(..) -> a
    // function boundaries: opaque body, but DO advance child_fn_cursor.
    ast.FunctionExpression(..) | ast.ArrowFunctionExpression(..) ->
      Ana(..a, cur: cursor_pop_child_fn(a.cur))
    ast.UnaryExpression(argument: x, ..)
    | ast.UpdateExpression(argument: x, ..)
    | ast.SpreadElement(argument: x, ..)
    | ast.ParenthesizedExpression(expression: x, ..) -> ana_expr(a, x, tail)
    ast.BinaryExpression(left: l, right: r, ..)
    | ast.LogicalExpression(left: l, right: r, ..)
    | ast.AssignmentExpression(left: l, right: r, ..) ->
      ana_expr(ana_expr(a, l, tail), r, tail)
    ast.MemberExpression(object: o, property: p, ..)
    | ast.OptionalMemberExpression(object: o, property: p, ..) -> {
      let a = ana_expr(a, o, tail)
      case p {
        ast.Bracket(expression: pe) -> ana_expr(a, pe, tail)
        ast.Dot(..) -> a
      }
    }
    ast.CallExpression(callee: c, arguments: args, ..)
    | ast.OptionalCallExpression(callee: c, arguments: args, ..)
    | ast.NewExpression(callee: c, arguments: args, ..) ->
      list.fold(args, ana_expr(a, c, tail), fn(a, x) { ana_expr(a, x, tail) })
    ast.ConditionalExpression(condition: c, consequent: t, alternate: f, ..) ->
      ana_expr(ana_expr(ana_expr(a, c, tail), t, tail), f, tail)
    ast.SequenceExpression(expressions: xs, ..) ->
      list.fold(xs, a, fn(a, x) { ana_expr(a, x, tail) })
    ast.ArrayExpression(elements: xs, ..) ->
      list.fold(xs, a, fn(a, o) { ana_opt_expr(a, o, tail) })
    ast.ObjectExpression(properties: ps, ..) ->
      list.fold(ps, a, fn(a, p) {
        case p {
          ast.InitProperty(key: k, value: v, ..) ->
            ana_expr(ana_key(a, k, tail), v, tail)
          ast.MethodProperty(key: k, ..) | ast.AccessorProperty(key: k, ..) -> {
            // method body is opaque; still consumes a child_fn slot.
            let a = ana_key(a, k, tail)
            Ana(..a, cur: cursor_pop_child_fn(a.cur))
          }
          ast.SpreadProperty(argument: x) -> ana_expr(a, x, tail)
        }
      })
    ast.TemplateLiteral(parts: parts, ..) ->
      list.fold(ast.template_expressions(parts), a, fn(a, x) {
        ana_expr(a, x, tail)
      })
    ast.TaggedTemplateExpression(tag: t, parts: parts, ..) ->
      list.fold(ast.template_expressions(parts), ana_expr(a, t, tail), fn(a, x) {
        ana_expr(a, x, tail)
      })
    ast.ImportExpression(source: s, options: o, ..) ->
      ana_opt_expr(ana_expr(a, s, tail), o, tail)
    ast.ClassExpression(super_class: sc, body: elems, ..) -> {
      let a = ana_opt_expr(a, sc, tail)
      list.fold(elems, a, fn(a, ce) { ana_class_elem(a, ce, tail) })
    }
  }
}

fn ana_key(a: Ana, k: ast.PropertyKey, tail) -> Ana {
  case k {
    ast.KeyComputed(expression: e) -> ana_expr(a, e, tail)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyPrivate(..) -> a
  }
}

fn ana_class_elem(a: Ana, ce: ast.ClassElement, tail) -> Ana {
  case ce {
    ast.ClassMethod(key: k, ..) -> {
      let a = ana_key(a, k, tail)
      Ana(..a, cur: cursor_pop_child_fn(a.cur))
    }
    // ES §15.7: field initializer is an implicit-function body ([~Await]) —
    // opaque to this coroutine's scope AND child_fn_cursor. Only the key is
    // evaluated in the enclosing scope.
    ast.ClassField(key: k, ..) -> ana_key(a, k, tail)
    ast.StaticBlock(..) -> Ana(..a, cur: cursor_pop_child_fn(a.cur))
  }
}

fn ana_pattern(a: Ana, p: ast.Pattern, tail) -> Ana {
  case p {
    ast.IdentifierPattern(..) -> a
    ast.AssignmentPattern(left: l, right: r) ->
      ana_expr(ana_pattern(a, l, tail), r, tail)
    ast.RestElement(argument: x) -> ana_pattern(a, x, tail)
    ast.ArrayPattern(elements: xs) ->
      list.fold(xs, a, fn(a, o) {
        case o {
          Some(x) -> ana_pattern(a, x, tail)
          None -> a
        }
      })
    ast.ObjectPattern(properties: ps) ->
      list.fold(ps, a, fn(a, pp) {
        case pp {
          ast.PatternProperty(key: k, value: v, ..) ->
            ana_pattern(ana_key(a, k, tail), v, tail)
          ast.RestProperty(..) -> a
        }
      })
  }
}

fn ana_for_init(a: Ana, fi: ast.ForInit, tail) -> Ana {
  case fi {
    ast.ForInitExpression(e) -> ana_expr(a, e, tail)
    ast.ForInitDeclaration(declarations: ds, ..) ->
      list.fold(ds, a, fn(a, d: ast.VariableDeclarator) {
        ana_opt_expr(ana_pattern(a, d.id, tail), d.init, tail)
      })
    ast.ForInitPattern(p) -> ana_pattern(a, p, tail)
  }
}

fn ana_stmts(a: Ana, ss: List(ast.StmtWithLine)) -> Ana {
  case ss {
    [] -> a
    [sl, ..rest] -> ana_stmts(ana_stmt(a, sl, rest), rest)
  }
}

/// Wrap a bare Statement as a one-element StmtWithLine list so `ana_stmts`
/// can recurse on `if`/loop bodies (which are `Statement`, not a list).
fn one_stmt(line: Int, s: ast.Statement) -> List(ast.StmtWithLine) {
  [ast.StmtWithLine(line:, statement: s)]
}

/// Walk one statement. `tail` is the remaining stmts in the SAME list
/// (threaded to `ana_expr` for its cursor walk). §18.4.5 (u-ctrl-split): a
/// split-free stmt is `frag_push`ed verbatim; a split-containing control-flow
/// construct is broken into states via the `plan_ctrl_*` helpers.
fn ana_stmt(a: Ana, sl: ast.StmtWithLine, tail: List(ast.StmtWithLine)) -> Ana {
  let ast.StmtWithLine(line:, statement: s) = sl
  case stmt_has_split(s) {
    // Split-free → append to open fragment; still shadow-walk for cursor sync
    // (nested fns/classes advance child_fn_cursor; blocks consume scope slots).
    False -> frag_push(ana_stmt_cursor_only(a, s, tail), sl)
    True ->
      case s {
        // ── §18.4.5 control-flow constructs whose branch has_split ──────────
        ast.IfStatement(condition: c, consequent: t, alternate: f) ->
          plan_ctrl_if(a, line, c, t, f, tail)
        ast.BlockStatement(body: b) ->
          with_scope_if(a, ast_util.block_has_declarations(b), fn(a) {
            ana_stmts(a, b)
          })
        ast.LabeledStatement(label:, body: b) ->
          plan_ctrl_labeled(a, line, label, b, tail)
        ast.WhileStatement(condition: c, body: b) ->
          plan_ctrl_while(a, line, None, c, b, tail)
        ast.DoWhileStatement(condition: c, body: b) ->
          plan_ctrl_do_while(a, line, None, c, b, tail)
        ast.ForStatement(init: i, condition: c, update: u, body: b) ->
          plan_ctrl_for(a, line, None, i, c, u, b, tail)
        ast.ForOfStatement(left: l, right: r, body: b, is_await: False) ->
          plan_ctrl_for_of(a, line, None, l, r, b, False, tail)
        ast.ForOfStatement(left: l, right: r, body: b, is_await: True) ->
          plan_ctrl_for_of(a, line, None, l, r, b, True, tail)
        ast.ForInStatement(left: l, right: r, body: b) ->
          plan_ctrl_for_of(a, line, None, l, r, b, False, tail)
        ast.SwitchStatement(discriminant: d, cases: cs) ->
          plan_ctrl_switch(a, line, None, d, cs, tail)
        // ── non-control-flow split-bearing stmts (u-split-analysis) ─────────
        // hoist_one splits `sl` into HiSplit boundaries so the split's operand
        // reaches SplitAt.arg AND the resume arm binds sent_v (§9 findings
        // #1/#2/#5) — the split-bearing stmt is NEVER frag_push'd verbatim.
        ast.ExpressionStatement(..)
        | ast.ThrowStatement(..)
        | ast.ReturnStatement(..)
        | ast.VariableDeclaration(..) -> ana_hoisted(a, hoist_one(sl), tail)
        ast.ClassDeclaration(super_class: sc, body: elems, ..) -> {
          let a = ana_opt_expr(a, sc, tail)
          let a = list.fold(elems, a, fn(a, ce) { ana_class_elem(a, ce, tail) })
          frag_push(a, sl)
        }
        ast.WithStatement(object: o, body: b) ->
          // finding #5: `with (await x) { body }` — the resume arm must wrap
          // `body` in `with(sent_v)`, not run it bare. Recognise a top-level
          // split object via `split_of` and record ResumeWithScope so
          // fragment-emit re-wraps. Nested-split objects and split-in-body
          // remain a v1 gap (with-scope-across-states requires loc-slot env).
          case split_of(o) {
            Some(#(kind, operand)) -> {
              let a = ana_opt_expr(a, operand, tail)
              let rw = Some(ResumeWithScope(body: b, line:))
              case kind {
                SkYieldStar -> record_delegate(a, operand, rw)
                _ -> {
                  let #(_, a) = record_split(a, kind, operand, rw)
                  a
                }
              }
            }
            None -> {
              let a = ana_expr(a, o, tail)
              ana_stmts(a, one_stmt(line, b))
            }
          }
        ast.TryStatement(block: blk, tail: tt) -> ana_try(a, blk, tt, tail)
        // Split-free by construction — unreachable in the True branch.
        ast.EmptyStatement
        | ast.DebuggerStatement
        | ast.BreakStatement(..)
        | ast.ContinueStatement(..)
        | ast.FunctionDeclaration(..) -> frag_push(a, sl)
      }
  }
}

/// Fold `hoist_one` output for a split-bearing simple statement (§9 wiring
/// for findings #1/#2/#5): each HiSplit becomes a `record_split` carrying its
/// real operand + resume-binding — the stmt is NEVER frag_push'd verbatim, so
/// the resume arm never re-emits the host("await") placeholder. HiStmt items
/// (v1-gap deep-nesting shapes hoist_one didn't recognise) fall back to the
/// old cursor-walk + frag_push path so any inner splits still get state ids.
fn ana_hoisted(
  a: Ana,
  items: List(HoistedItem),
  tail: List(ast.StmtWithLine),
) -> Ana {
  list.fold(items, a, fn(a, item) {
    case item {
      HiStmt(s) -> frag_push(ana_stmt_cursor_only(a, s.statement, tail), s)
      HiSplit(_, kind, operand, resume) -> {
        // Walk operand for cursor-sync + any nested splits (v1 gap: nested
        // await inside operand still routes through the host placeholder).
        let a = ana_opt_expr(a, operand, tail)
        let rw = Some(resume)
        case kind {
          SkYieldStar -> record_delegate(a, operand, rw)
          SkYield ->
            case a.kind {
              // §18.7: async-gen `yield x` = Await(x) THEN Yield(sent_v).
              state.CorAsyncGen -> {
                let #(_, a) = record_split(a, SkAwait, operand, None)
                let #(ns2, a) = alloc_state(a)
                let sp =
                  SplitPoint(
                    id: ns2,
                    kind: SkYield,
                    enclosing_try: ana_region(a),
                  )
                let a = Ana(..a, splits: [sp, ..a.splits])
                let a =
                  close_open(
                    a,
                    AsyncGenYieldSent(ns: ns2),
                    ns2,
                    AeResume(SkYield),
                  )
                Ana(..a, open_resume: rw)
              }
              state.CorAsync | state.CorGenerator -> {
                let #(_, a) = record_split(a, SkYield, operand, rw)
                a
              }
            }
          SkAwait | SkForAwait -> {
            let #(_, a) = record_split(a, kind, operand, rw)
            a
          }
        }
      }
    }
  })
}

/// Shadow-cursor-only walk of a split-FREE statement: advances `cur` past any
/// nested fn/class/block scopes it contains WITHOUT touching the segment.
/// Keeps SCOPE-CURSOR INVARIANT for stmts appended verbatim to `frag_rev`.
fn ana_stmt_cursor_only(
  a: Ana,
  s: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  case s {
    ast.FunctionDeclaration(..) -> Ana(..a, cur: cursor_pop_child_fn(a.cur))
    ast.ClassDeclaration(super_class: sc, body: elems, ..) -> {
      let a = ana_opt_expr(a, sc, tail)
      list.fold(elems, a, fn(a, ce) { ana_class_elem(a, ce, tail) })
    }
    ast.ExpressionStatement(expression: e, ..)
    | ast.ThrowStatement(argument: e) -> ana_expr(a, e, tail)
    ast.ReturnStatement(argument: arg) -> ana_opt_expr(a, arg, tail)
    ast.VariableDeclaration(declarations: ds, ..) ->
      list.fold(ds, a, fn(a, d: ast.VariableDeclarator) {
        ana_opt_expr(ana_pattern(a, d.id, tail), d.init, tail)
      })
    ast.BlockStatement(body: b) ->
      with_scope_if(a, ast_util.block_has_declarations(b), fn(a) {
        list.fold(b, a, fn(a, sl: ast.StmtWithLine) {
          ana_stmt_cursor_only(a, sl.statement, tail)
        })
      })
    ast.IfStatement(condition: c, consequent: t, alternate: f) -> {
      let a = ana_expr(a, c, tail)
      let a = ana_stmt_cursor_only(a, t, tail)
      case f {
        Some(alt) -> ana_stmt_cursor_only(a, alt, tail)
        None -> a
      }
    }
    ast.WhileStatement(condition: c, body: b)
    | ast.DoWhileStatement(condition: c, body: b) -> {
      let a = ana_expr(a, c, tail)
      ana_stmt_cursor_only(a, b, tail)
    }
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      with_scope_if(a, ast_util.for_classic_init_is_lex(i), fn(a) {
        let a = case i {
          Some(fi) -> ana_for_init(a, fi, tail)
          None -> a
        }
        let a = ana_opt_expr(a, c, tail)
        let a = ana_opt_expr(a, u, tail)
        ana_stmt_cursor_only(a, b, tail)
      })
    ast.ForInStatement(left: l, right: r, body: b)
    | ast.ForOfStatement(left: l, right: r, body: b, ..) ->
      with_scope_if(a, ast_util.for_classic_init_is_lex(Some(l)), fn(a) {
        let a = ana_for_init(a, l, tail)
        let a = ana_expr(a, r, tail)
        ana_stmt_cursor_only(a, b, tail)
      })
    ast.SwitchStatement(discriminant: d, cases: cs) -> {
      let a = ana_expr(a, d, tail)
      with_scope(a, fn(a) {
        list.fold(cs, a, fn(a, c: ast.SwitchCase) {
          let a = ana_opt_expr(a, c.condition, tail)
          list.fold(c.consequent, a, fn(a, sl: ast.StmtWithLine) {
            ana_stmt_cursor_only(a, sl.statement, tail)
          })
        })
      })
    }
    ast.LabeledStatement(body: b, ..) -> ana_stmt_cursor_only(a, b, tail)
    ast.WithStatement(object: o, body: b) -> {
      let a = ana_expr(a, o, tail)
      ana_stmt_cursor_only(a, b, tail)
    }
    ast.TryStatement(block: blk, tail: tt) -> {
      let body_fold = fn(a, ss: List(ast.StmtWithLine)) {
        list.fold(ss, a, fn(a, sl: ast.StmtWithLine) {
          ana_stmt_cursor_only(a, sl.statement, tail)
        })
      }
      let block_cur = fn(a, ss) {
        with_scope_if(a, ast_util.block_has_declarations(ss), fn(a) {
          body_fold(a, ss)
        })
      }
      let a = block_cur(a, blk)
      case tt {
        ast.TryCatch(handler: h) ->
          walk_catch_cur(a, h, fn(a) { body_fold(a, h.body) })
        ast.TryFinally(finalizer: f) -> block_cur(a, f)
        ast.TryCatchFinally(handler: h, finalizer: f) ->
          block_cur(walk_catch_cur(a, h, fn(a) { body_fold(a, h.body) }), f)
      }
    }
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..) -> a
  }
}

// ── §18.4.5 per-construct planners (u-ctrl-split) ───────────────────────────
// Each: alloc after_state (+ head/test/update as needed); close current arm
// with the entry-transition tail; push loop_stack (SmLabel) mapping label →
// (continue_state = head/test/update, break_state = after); recurse into
// branch bodies via ana_stmts (splits inside allocate their own states);
// close each branch with FallTo(back-edge or after); pop; resume at after.

/// Plan a construct's HEAD expression (if-cond / while-cond / for-cond /
/// switch-disc). If `expr` IS a top-level `await x` / `yield x`, record the
/// split (operand→SplitAt.arg) and return `None` — the SegTail then reads
/// `ctx.sent_v` instead of re-emitting `await x` via the placeholder host op.
/// Deep-expression splits (`x && await c`) fall through to `Some(expr)` (v1
/// gap: emit_seg_tail re-emits via dispatch.emit_expr; expr.gleam:318 covers).
fn plan_head_expr(
  a: Ana,
  expr: ast.Expression,
  tail: List(ast.StmtWithLine),
) -> #(Option(ast.Expression), Ana) {
  case split_of(expr) {
    Some(#(kind, operand)) ->
      case kind {
        // §18.6 delegate + §18.7 async-gen two-phase: defer to ana_expr so
        // record_delegate / Await-then-AsyncGenYieldSent run — record_split
        // alone would drop the DelegateSpec / second phase.
        SkYieldStar -> #(Some(expr), ana_expr(a, expr, tail))
        SkYield ->
          case a.kind {
            state.CorAsyncGen -> #(Some(expr), ana_expr(a, expr, tail))
            state.CorAsync | state.CorGenerator -> {
              let a = ana_opt_expr(a, operand, tail)
              let #(_, a) = record_split(a, SkYield, operand, None)
              #(None, a)
            }
          }
        SkAwait | SkForAwait -> {
          let a = ana_opt_expr(a, operand, tail)
          let #(_, a) = record_split(a, kind, operand, None)
          #(None, a)
        }
      }
    // Always ana_expr (even split-free) — cursor-sync for nested fn/class.
    None -> #(Some(expr), ana_expr(a, expr, tail))
  }
}

/// If: alloc after; close with CondBranch(cond, then_s, else_s); each branch
/// ends FallTo(after). No loop_stack entry (break/continue pass through).
fn plan_ctrl_if(
  a: Ana,
  line: Int,
  cond: ast.Expression,
  cons: ast.Statement,
  alt: Option(ast.Statement),
  tail: List(ast.StmtWithLine),
) -> Ana {
  // If cond itself has a split (`if (await c)`), record it FIRST so the
  // CondBranch tail sits in the resume arm and reads ctx.sent_v (§18.4.5).
  let #(cond, a) = plan_head_expr(a, cond, tail)
  let #(then_s, a) = alloc_state(a)
  let #(after, a) = alloc_state(a)
  let #(else_s, a) = case alt {
    Some(_) -> alloc_state(a)
    None -> #(after, a)
  }
  let a = close_open(a, CondBranch(cond:, then_s:, else_s:), then_s, AeJump)
  let a = ana_stmts(a, one_stmt(line, cons))
  let a = close_open(a, FallTo(after), else_s, AeJump)
  case alt {
    None -> a
    Some(alt_stmt) -> {
      let a = ana_stmts(a, one_stmt(line, alt_stmt))
      close_open(a, FallTo(after), after, AeJump)
    }
  }
}

/// While: head evals cond → body_s | after; body ends FallTo(head).
/// continue → head, break → after. §18.4.5 "loop back-edge crossing a split".
fn plan_ctrl_while(
  a: Ana,
  line: Int,
  label: Option(String),
  cond: ast.Expression,
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(head, a) = alloc_state(a)
  let #(body_s, a) = alloc_state(a)
  let #(after, a) = alloc_state(a)
  let #(brk, a) = alloc_sentinel(a)
  let #(cont, a) = alloc_sentinel(a)
  let a = close_open(a, FallTo(head), head, AeJump)
  // Head arm: cond may itself split (`while (await c)`).
  let #(cond, a) = plan_head_expr(a, cond, tail)
  let a =
    close_open(
      a,
      CondBranch(cond:, then_s: body_s, else_s: after),
      body_s,
      AeJump,
    )
  let sm_label =
    SmLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: head,
      enclosing_try: ana_region(a),
    )
  let a = push_sm_label(a, sm_label)
  let a = ana_stmts(a, one_stmt(line, body))
  let a = pop_sm_label(a)
  close_open(a, FallTo(head), after, AeJump)
}

/// DoWhile: body_s runs body → test_s; test_s evals cond → body_s | after.
/// continue → test_s (R15: continue re-tests, does NOT re-run body top).
fn plan_ctrl_do_while(
  a: Ana,
  line: Int,
  label: Option(String),
  cond: ast.Expression,
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(body_s, a) = alloc_state(a)
  let #(test_s, a) = alloc_state(a)
  let #(after, a) = alloc_state(a)
  let #(brk, a) = alloc_sentinel(a)
  let #(cont, a) = alloc_sentinel(a)
  let a = close_open(a, FallTo(body_s), body_s, AeJump)
  let sm_label =
    SmLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: test_s,
      enclosing_try: ana_region(a),
    )
  let a = push_sm_label(a, sm_label)
  let a = ana_stmts(a, one_stmt(line, body))
  let a = pop_sm_label(a)
  let a = close_open(a, FallTo(test_s), test_s, AeJump)
  let #(cond, a) = plan_head_expr(a, cond, tail)
  close_open(a, CondBranch(cond:, then_s: body_s, else_s: after), after, AeJump)
}

/// For(;;): init runs in prior arm; head=test; update_s runs update →
/// FallTo(head). continue → update_s (R15: runs update+test).
fn plan_ctrl_for(
  a: Ana,
  line: Int,
  label: Option(String),
  init: Option(ast.ForInit),
  cond: Option(ast.Expression),
  update: Option(ast.Expression),
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  with_scope_if(a, ast_util.for_classic_init_is_lex(init), fn(a) {
    // init: append to open fragment as an ordinary stmt (runs once).
    let a = case init {
      None -> a
      Some(fi) -> {
        let a = ana_for_init(a, fi, tail)
        case fi {
          ast.ForInitExpression(e) ->
            frag_push(
              a,
              ast.StmtWithLine(
                line:,
                statement: ast.ExpressionStatement(e, None),
              ),
            )
          ast.ForInitDeclaration(kind: vk, declarations:) ->
            frag_push(
              a,
              ast.StmtWithLine(
                line:,
                statement: ast.VariableDeclaration(kind: vk, declarations:),
              ),
            )
          ast.ForInitPattern(_) -> a
        }
      }
    }
    let #(head, a) = alloc_state(a)
    let #(body_s, a) = alloc_state(a)
    let #(update_s, a) = alloc_state(a)
    let #(after, a) = alloc_state(a)
    let #(brk, a) = alloc_sentinel(a)
    let #(cont, a) = alloc_sentinel(a)
    let a = close_open(a, FallTo(head), head, AeJump)
    // Head: cond → body_s | after (absent cond = unconditional body_s).
    let a = case cond {
      Some(c) -> {
        let #(c, a) = plan_head_expr(a, c, tail)
        close_open(
          a,
          CondBranch(cond: c, then_s: body_s, else_s: after),
          body_s,
          AeJump,
        )
      }
      None -> close_open(a, FallTo(body_s), body_s, AeJump)
    }
    let sm_label =
      SmLoop(
        js_label: label,
        brk_sentinel: brk,
        cont_sentinel: cont,
        break_state: after,
        continue_state: update_s,
        enclosing_try: ana_region(a),
      )
    let a = push_sm_label(a, sm_label)
    let a = ana_stmts(a, one_stmt(line, body))
    let a = pop_sm_label(a)
    let a = close_open(a, FallTo(update_s), update_s, AeJump)
    // Update state (may itself split — `for(;; await u())`).
    let #(update, a) = case update {
      Some(u) -> plan_head_expr(a, u, tail)
      None -> #(None, a)
    }
    close_open(a, ForUpdate(update:, head:), after, AeJump)
  })
}

/// ForOf / ForIn / ForAwaitOf: prior arm evals `right` + get_iterator, stores
/// handle at loc extra `iter_<head>`; head=step (ForOfStep tail); body ends
/// FallTo(head). continue → head, break → after. `is_await:True` marks the
/// step as a split (u-fragment-emit awaits the next() result).
fn plan_ctrl_for_of(
  a: Ana,
  line: Int,
  label: Option(String),
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  is_await: Bool,
  tail: List(ast.StmtWithLine),
) -> Ana {
  with_scope_if(a, ast_util.for_classic_init_is_lex(Some(left)), fn(a) {
    case is_await {
      True -> plan_ctrl_for_await(a, line, label, left, right, body, tail)
      False -> {
        let #(head, a) = alloc_state(a)
        let #(body_s, a) = alloc_state(a)
        let #(after, a) = alloc_state(a)
        let #(brk, a) = alloc_sentinel(a)
        let #(cont, a) = alloc_sentinel(a)
        let ikey = iter_key(head)
        // Prior arm evals `right` + get_iterator, stores handle at loc[ikey],
        // Continues→head (u-ctrl-split #8).
        let a =
          close_open(a, ForOfSetup(right:, iter_key: ikey, head:), head, AeJump)
        let a =
          close_open(
            a,
            ForOfStep(left:, iter_key: ikey, body_s:, after:, is_await: False),
            body_s,
            AeJump,
          )
        let sm_label =
          SmLoop(
            js_label: label,
            brk_sentinel: brk,
            cont_sentinel: cont,
            break_state: after,
            continue_state: head,
            enclosing_try: ana_region(a),
          )
        let a = push_sm_label(a, sm_label)
        // u-ctrl-split #5: LHS destructure defaults with a split are a v1 gap;
        // walking here would record the split in body_s (wrong arm).
        let a = ana_stmts(a, one_stmt(line, body))
        let a = pop_sm_label(a)
        close_open(a, FallTo(head), after, AeJump)
      }
    }
  })
}

/// for-await-of (u-for-await): setup tail evals rhs → get_iterator(async) →
/// stores handle → Continue(head). Head/check arms are emitted from
/// `plan.for_awaits` (NOT plan.arms) so no ArmSpec is close_open'd for them
/// here — the open segment jumps straight to body_s. body_s + after ARE
/// ordinary plan.arms so body-internal splits work. §18.2 SkForAwait split
/// is at `check` (the resume-after-await point).
fn plan_ctrl_for_await(
  a: Ana,
  line: Int,
  label: Option(String),
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(head, a) = alloc_state(a)
  let #(check, a) = alloc_state(a)
  let #(body_s, a) = alloc_state(a)
  let #(after, a) = alloc_state(a)
  let #(brk, a) = alloc_sentinel(a)
  let #(cont, a) = alloc_sentinel(a)
  let region = ana_region(a)
  let sp = SplitPoint(id: check, kind: SkForAwait, enclosing_try: region)
  let a = Ana(..a, splits: [sp, ..a.splits])
  let spec =
    ForAwaitSpec(
      head:,
      check:,
      body_s:,
      after:,
      left:,
      body_cursor: a.cur,
      region:,
    )
  let a = Ana(..a, for_awaits: [spec, ..a.for_awaits])
  // Close pre-loop with ForAwaitSetup; open body_s directly (head/check are
  // emitted from plan.for_awaits in build_switch_arms, not plan.arms).
  let a = close_open(a, ForAwaitSetup(right:, head:), body_s, AeJump)
  let sm_label =
    SmLoop(
      js_label: label,
      brk_sentinel: brk,
      cont_sentinel: cont,
      break_state: after,
      continue_state: head,
      enclosing_try: region,
    )
  let a = push_sm_label(a, sm_label)
  let a = ana_for_init(a, left, tail)
  let a = ana_stmts(a, one_stmt(line, body))
  let a = pop_sm_label(a)
  close_open(a, FallTo(head), after, AeJump)
}

/// Switch: alloc after + one state per case; prior arm SwitchDispatch; each
/// case body ends FallTo(next case) implementing JS fall-through. break →
/// after. Unlabeled `continue` walks past (SmSwitch has no continue target).
fn plan_ctrl_switch(
  a: Ana,
  _line: Int,
  label: Option(String),
  disc: ast.Expression,
  cases: List(ast.SwitchCase),
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(disc, a) = plan_head_expr(a, disc, tail)
  let #(after, a) = alloc_state(a)
  let #(brk, a) = alloc_sentinel(a)
  with_scope(a, fn(a) {
    // Alloc one state per case, in source order.
    let #(case_states_rev, a) =
      list.fold(cases, #([], a), fn(acc, _c) {
        let #(sts, a) = acc
        let #(s, a) = alloc_state(a)
        #([s, ..sts], a)
      })
    let case_states = list.reverse(case_states_rev)
    let tests =
      list.map2(cases, case_states, fn(c, s) {
        let ast.SwitchCase(condition:, ..) = c
        #(condition, s)
      })
    // §13.12.9: default is entered only after every non-default test misses;
    // switch_chain treats #(None, _) as terminal, so re-order it LAST.
    let #(defs, non_defs) =
      list.partition(tests, fn(t) {
        case t {
          #(None, _) -> True
          _ -> False
        }
      })
    let tests = list.append(non_defs, defs)
    let first = case case_states {
      [s, ..] -> s
      [] -> after
    }
    let a = close_open(a, SwitchDispatch(disc:, tests:, after:), first, AeJump)
    let sm_label =
      SmSwitch(
        js_label: label,
        brk_sentinel: brk,
        break_state: after,
        enclosing_try: ana_region(a),
      )
    let a = push_sm_label(a, sm_label)
    let a = plan_ctrl_switch_cases(a, cases, case_states, after)
    pop_sm_label(a)
  })
}

fn plan_ctrl_switch_cases(
  a: Ana,
  cases: List(ast.SwitchCase),
  states: List(Int),
  after: Int,
) -> Ana {
  case cases, states {
    [], [] -> a
    [ast.SwitchCase(consequent:, ..), ..cs], [_s, ..ss] -> {
      // u-ctrl-split #13: test exprs are evaluated in the DISPATCH arm (already
      // closed above); walking them here would record `case await x:` splits in
      // the case-BODY arm. Splits in case tests are a v1 gap.
      let a = ana_stmts(a, consequent)
      let next = case ss {
        [n, ..] -> n
        [] -> after
      }
      let a = close_open(a, FallTo(next), next, AeJump)
      plan_ctrl_switch_cases(a, cs, ss, after)
    }
    _, _ -> a
  }
}

/// Labeled: label directly on a loop/switch → thread label into that
/// construct's planner. Labeled non-iteration block → alloc after; push
/// SmLabeled(break_state=after); recurse; body-end FallTo(after).
fn plan_ctrl_labeled(
  a: Ana,
  line: Int,
  label: String,
  body: ast.Statement,
  tail: List(ast.StmtWithLine),
) -> Ana {
  case body {
    ast.WhileStatement(condition: c, body: b) ->
      plan_ctrl_while(a, line, Some(label), c, b, tail)
    ast.DoWhileStatement(condition: c, body: b) ->
      plan_ctrl_do_while(a, line, Some(label), c, b, tail)
    ast.ForStatement(init: i, condition: c, update: u, body: b) ->
      plan_ctrl_for(a, line, Some(label), i, c, u, b, tail)
    ast.ForOfStatement(left: l, right: r, body: b, is_await:) ->
      plan_ctrl_for_of(a, line, Some(label), l, r, b, is_await, tail)
    ast.ForInStatement(left: l, right: r, body: b) ->
      plan_ctrl_for_of(a, line, Some(label), l, r, b, False, tail)
    ast.SwitchStatement(discriminant: d, cases: cs) ->
      plan_ctrl_switch(a, line, Some(label), d, cs, tail)
    ast.LabeledStatement(label: inner, body: b) -> {
      // Stacked labels `a: b: while(…)` — both target the same construct.
      let #(after, a) = alloc_state(a)
      let #(brk, a) = alloc_sentinel(a)
      let alias =
        SmLabeled(
          js_label: label,
          brk_sentinel: brk,
          break_state: after,
          enclosing_try: ana_region(a),
        )
      let a = push_sm_label(a, alias)
      let a = plan_ctrl_labeled(a, line, inner, b, tail)
      let a = pop_sm_label(a)
      close_open(a, FallTo(after), after, AeJump)
    }
    _ -> {
      // Labeled non-iteration statement (`foo: { … break foo; … }`).
      let #(after, a) = alloc_state(a)
      let #(brk, a) = alloc_sentinel(a)
      let sm_label =
        SmLabeled(
          js_label: label,
          brk_sentinel: brk,
          break_state: after,
          enclosing_try: ana_region(a),
        )
      let a = push_sm_label(a, sm_label)
      let a = ana_stmts(a, one_stmt(line, body))
      let a = pop_sm_label(a)
      close_open(a, FallTo(after), after, AeJump)
    }
  }
}

fn ana_try(
  a: Ana,
  block: List(ast.StmtWithLine),
  tt: ast.TryTail,
  tail: List(ast.StmtWithLine),
) -> Ana {
  let #(handler, finalizer) = case tt {
    ast.TryCatch(handler: h) -> #(Some(h), None)
    ast.TryFinally(finalizer: f) -> #(None, Some(f))
    ast.TryCatchFinally(handler: h, finalizer: f) -> #(Some(h), Some(f))
  }
  let contains_split =
    stmts_have_split(block)
    || case handler {
      Some(h) -> catch_has_split(h)
      None -> False
    }
    || case finalizer {
      Some(f) -> stmts_have_split(f)
      None -> False
    }
  let block_has = ast_util.block_has_declarations(block)
  case contains_split {
    // 0 splits → NOT a TryEntry (§18.2); walk children only for cursor sync.
    False -> {
      let a = with_scope_if(a, block_has, fn(a) { ana_stmts(a, block) })
      let a = case handler {
        Some(h) -> walk_catch_cur(a, h, fn(a) { ana_stmts(a, h.body) })
        None -> a
      }
      case finalizer {
        Some(f) ->
          with_scope_if(a, ast_util.block_has_declarations(f), fn(a) {
            ana_stmts(a, f)
          })
        None -> a
      }
    }
    True -> {
      // Allocate TryEntry id; push region; walk block INSIDE the region so
      // splits there record enclosing_try = this id (matches §9 example:
      // block splits get ids 1..N, THEN catch/finally/after states).
      let try_id = a.next_try
      let outer = ana_region(a)
      let entry_sm_labels = a.sm_labels
      let a = Ana(..a, next_try: try_id + 1)
      // u-ctrl-split #11: close the pre-try segment (region=outer) and open a
      // fresh segment INSIDE the try region so a sync throw before the first
      // split routes via wrap_arm_try(region=try_id) to this catch/finally.
      let #(block_entry, a) = alloc_state(a)
      let a = close_open(a, FallTo(block_entry), block_entry, AeJump)
      let a =
        Ana(..a, try_stack: [try_id, ..a.try_stack], open_region: Some(try_id))
      let a = with_scope_if(a, block_has, fn(a) { ana_stmts(a, block) })
      // Pop region (catch/finally bodies are NOT inside this region for
      // arm-catch routing — they're inside any OUTER region).
      let a =
        Ana(..a, try_stack: case a.try_stack {
          [_, ..rest] -> rest
          [] -> []
        })
      // Allocate catch/finally/after states BEFORE closing the try-block tail
      // so its FallTo target is known. (§9 finding #7: without close_open the
      // catch/finally bodies frag_push into the block-tail arm and its normal-
      // completion transition is never emitted.)
      let #(catch_state, catch_cursor, a) = case handler {
        Some(_) -> {
          let #(cs, a) = alloc_state(a)
          #(Some(cs), Some(a.cur), a)
        }
        None -> #(None, None, a)
      }
      let #(finally_state, a) = case finalizer {
        Some(_) -> {
          let #(fs, a) = alloc_state(a)
          #(Some(fs), a)
        }
        None -> #(None, a)
      }
      let #(after_state, a) = alloc_state(a)
      let fall_to = case finally_state {
        Some(fs) -> fs
        None -> after_state
      }
      // Close the try-block's post-last-split arm with FallTo(fin|after); open
      // an unreachable sink so the catch/finally cursor-walks below have a
      // segment to spill into without polluting any live arm's body_fragment.
      let #(sink, a) = alloc_state(a)
      let a = close_open(a, FallTo(fall_to), sink, AeJump)
      // Walk catch/finally bodies for cursor sync (build_switch_arms emits the
      // catch_state/finally_state arms from TryEntry.{handler,finalizer}).
      // Split-bearing catch/finally bodies are a v1 gap (their splits produce
      // unreachable arms; emit_catch_arm/emit_finally_arm re-emit inline).
      let a = case handler {
        Some(h) -> walk_catch_cur(a, h, fn(a) { ana_stmts(a, h.body) })
        None -> a
      }
      let #(finally_cursor, a) = case finalizer {
        Some(_) -> #(Some(a.cur), a)
        None -> #(None, a)
      }
      let a = case finalizer {
        Some(f) ->
          with_scope_if(a, ast_util.block_has_declarations(f), fn(a) {
            ana_stmts(a, f)
          })
        None -> a
      }
      // Close the sink (dead) and open at after_state so the post-try tail
      // frag_pushes into the correct segment.
      let a = close_open(a, SegDone, after_state, AeJump)
      // loc indices are pass-2's job (u-loc-layout) — 0 sentinel here.
      let entry =
        TryEntry(
          id: try_id,
          catch_state:,
          finally_state:,
          after_state:,
          pending_loc_idx: 0,
          caught_loc_idx: 0,
          outer:,
          handler:,
          finalizer:,
          catch_cursor:,
          finally_cursor:,
          sm_labels: entry_sm_labels,
        )
      Ana(..a, tries: [entry, ..a.tries])
    }
  }
}

/// §18.2 pass-1: walk `body` in source pre-order assigning monotone state
/// ids 1..K (0=entry) at each Await / Yield / YieldDelegate / ForAwait-step,
/// allocating a `TryEntry` per try-statement whose subtree contains ≥1 split.
/// Threads the `ArmCursor` shadow (SCOPE-CURSOR INVARIANT) so each ArmSpec
/// carries the scope-cursor snapshot at its resume point. Pure AST→data.
fn analyze_splits(
  tree: ScopeTree,
  fn_scope: ScopeId,
  body: state.FnBody,
  kind: state.CoroutineKind,
) -> SplitPlan {
  let cur0 = root_cursor(tree, fn_scope)
  let init =
    Ana(
      tree:,
      kind:,
      next_state: 1,
      next_try: 0,
      next_sentinel: 0,
      try_stack: [],
      sm_labels: [],
      cur: cur0,
      splits: [],
      tries: [],
      arms: [],
      delegates: [],
      for_awaits: [],
      // Open segment = state 0 (AeInitial) at function-body entry.
      frag_rev: [],
      open_state: 0,
      open_region: None,
      open_entry: AeInitial,
      open_cursor: cur0,
      open_labels: [],
      open_resume: None,
    )
  let a = case body {
    state.StmtBody(ss) -> ana_stmts(init, ss)
    state.ExprBody(e) -> ana_expr(init, e, [])
  }
  // Close the trailing open segment (BodyEnd → step_return(undef)). §18.4.5:
  // arms accumulated by close_open + this final one form the complete plan.
  let a = close_open(a, BodyEnd, a.next_state, AeJump)
  SplitPlan(
    n_states: a.next_state,
    arms: list.reverse(a.arms),
    try_entries: list.reverse(a.tries),
    delegates: list.reverse(a.delegates),
    for_awaits: list.reverse(a.for_awaits),
  )
}

// ── stubs for sibling units (bodies land in loc-layout / step-and-loc-ir /
// ── sm-skeleton-emit / outer-fn-emit / arm-shell / yield-star-arm /
// ── finally-arm / fragment-emit) ────────────────────────────────────────────

// ── §18.3 pass-2: loc-tuple layout (u-loc-layout) ───────────────────────────

/// Extras-key builders — the string is the ONLY identity; every producer and
/// consumer routes through these so the spelling is defined once.
pub fn pending_key(try_id: Int) -> String {
  "pending_" <> int.to_string(try_id)
}

pub fn caught_key(try_id: Int) -> String {
  "caught_" <> int.to_string(try_id)
}

pub fn iter_key(state_id: Int) -> String {
  "iter_" <> int.to_string(state_id)
}

pub fn inner_key(state_id: Int) -> String {
  "inner_" <> int.to_string(state_id)
}

pub fn delegate_result_key(state_id: Int) -> String {
  "delegate_result_" <> int.to_string(state_id)
}

/// u-for-await: loc slot for a `for await` async-iterator handle. Keyed by
/// the head-state id so nested for-awaits get distinct slots.
pub fn for_await_iter_key(head: Int) -> String {
  "iter_fa_" <> int.to_string(head)
}

/// §18.3: assign every hoisted local slot a stable loc-tuple index, then
/// reserve extra indices for try-region pending/caught and yield*-delegate
/// bookkeeping. Conservative v1: when the body has ANY split (`n_states>1`)
/// hoist EVERY local slot 0..local_count-1 — precise live-across-split is a
/// later optimisation the SPEC permits skipping. Zero splits ⇒ empty layout.
fn compute_loc_layout(info: scope.FunctionInfo, plan: SplitPlan) -> LocLayout {
  let hoist_count = case plan.n_states > 1 {
    True -> info.local_count
    False -> 0
  }
  let slot_to_idx = index_identity_map(hoist_count)
  // Extras laid out contiguously past the hoisted-locals block, in
  // deterministic order (try pending → try caught → per-delegate iter/inner/
  // result). Determinism matters — the layout is baked into every arm's IR.
  let #(extras, next) =
    alloc_try_extras(plan.try_entries, dict.new(), hoist_count)
  let #(extras, next) = alloc_delegate_extras(plan.delegates, extras, next)
  let #(extras, next) = alloc_for_await_extras(plan.for_awaits, extras, next)
  let #(extras, size) = alloc_for_of_extras(plan.arms, extras, next)
  let initial_values = build_initial_loc(size, pending_index_set(extras, plan))
  LocLayout(slot_to_idx:, size:, extras:, initial_values:)
}

/// slot i → idx i for i ∈ [0, n). Identity mapping keeps §9's "loc[0] is x"
/// intuition and makes restore/pack diffs readable.
fn index_identity_map(n: Int) -> Dict(Int, Int) {
  identity_map_loop(0, n, dict.new())
}

fn identity_map_loop(i: Int, n: Int, acc: Dict(Int, Int)) -> Dict(Int, Int) {
  case i < n {
    False -> acc
    True -> identity_map_loop(i + 1, n, dict.insert(acc, i, i))
  }
}

/// One `pending_<id>` per finally-bearing region, one `caught_<id>` per
/// catch-bearing region (§18.5). A try/catch/finally allocates both.
fn alloc_try_extras(
  entries: List(TryEntry),
  extras: Dict(String, Int),
  next: Int,
) -> #(Dict(String, Int), Int) {
  use #(extras, next), entry <- list.fold(entries, #(extras, next))
  let #(extras, next) = case entry.finally_state {
    Some(_) -> #(dict.insert(extras, pending_key(entry.id), next), next + 1)
    None -> #(extras, next)
  }
  case entry.catch_state {
    Some(_) -> #(dict.insert(extras, caught_key(entry.id), next), next + 1)
    None -> #(extras, next)
  }
}

/// Three slots per §18.6 self-looping delegate arm: the get_iterator handle,
/// the inner iterator object, and the arm's result carry.
fn alloc_delegate_extras(
  delegates: List(DelegateSpec),
  extras: Dict(String, Int),
  next: Int,
) -> #(Dict(String, Int), Int) {
  use #(extras, next), d <- list.fold(delegates, #(extras, next))
  let sid = d.state_id
  let extras =
    extras
    |> dict.insert(iter_key(sid), next)
    |> dict.insert(inner_key(sid), next + 1)
    |> dict.insert(delegate_result_key(sid), next + 2)
  #(extras, next + 3)
}

/// One slot per for-await loop: the async-iterator handle, live from setup
/// through every head/check iteration until `after` (u-for-await).
fn alloc_for_await_extras(
  for_awaits: List(ForAwaitSpec),
  extras: Dict(String, Int),
  next: Int,
) -> #(Dict(String, Int), Int) {
  use #(extras, next), fap <- list.fold(for_awaits, #(extras, next))
  #(dict.insert(extras, for_await_iter_key(fap.head), next), next + 1)
}

/// One slot per split-bearing for-of/for-in head: the iterator handle at
/// `iter_<head>` (ForOfStep.iter_key), live from setup through every step
/// until `after`. Skips keys already allocated (delegates share `iter_<sid>`).
fn alloc_for_of_extras(
  arms: List(ArmSpec),
  extras: Dict(String, Int),
  next: Int,
) -> #(Dict(String, Int), Int) {
  use #(extras, next), arm <- list.fold(arms, #(extras, next))
  let alloc = fn(k) {
    case dict.has_key(extras, k) {
      True -> #(extras, next)
      False -> #(dict.insert(extras, k, next), next + 1)
    }
  }
  case arm.tail {
    ForOfStep(iter_key: k, ..) -> alloc(k)
    ForOfSetup(iter_key: k, ..) -> alloc(k)
    _ -> #(extras, next)
  }
}

/// Loc indices holding a `pending` completion record — those alone start as
/// ConstAtom("normal") so the finally-arm's IsAtom test (§9 SwitchArm 2)
/// reads "normal" on the no-abrupt path. Every other slot starts undefined.
fn pending_index_set(extras: Dict(String, Int), plan: SplitPlan) -> Set(Int) {
  use acc, entry <- list.fold(plan.try_entries, set.new())
  case entry.finally_state {
    None -> acc
    Some(_) ->
      case dict.get(extras, pending_key(entry.id)) {
        Ok(idx) -> set.insert(acc, idx)
        Error(Nil) -> acc
      }
  }
}

fn build_initial_loc(size: Int, pending_idxs: Set(Int)) -> List(ir.Value) {
  initial_loc_loop(0, size, pending_idxs, [])
}

fn initial_loc_loop(
  i: Int,
  size: Int,
  pending: Set(Int),
  acc: List(ir.Value),
) -> List(ir.Value) {
  case i < size {
    False -> list.reverse(acc)
    True -> {
      let v = case set.contains(pending, i) {
        True -> ir.ConstAtom("normal")
        False -> ir.ConstAtom("undefined")
      }
      initial_loc_loop(i + 1, size, pending, [v, ..acc])
    }
  }
}

/// Rewrite `entries` with `pending_loc_idx`/`caught_loc_idx` filled from
/// `layout.extras`. analyze_splits leaves them 0; downstream arm emitters
/// read them off the record. Fields with no matching key stay as-is.
pub fn enrich_try_entries(
  entries: List(TryEntry),
  layout: LocLayout,
) -> List(TryEntry) {
  use entry <- list.map(entries)
  let pending_loc_idx =
    dict.get(layout.extras, pending_key(entry.id))
    |> idx_or(entry.pending_loc_idx)
  let caught_loc_idx =
    dict.get(layout.extras, caught_key(entry.id))
    |> idx_or(entry.caught_loc_idx)
  TryEntry(..entry, pending_loc_idx:, caught_loc_idx:)
}

fn idx_or(r: Result(Int, Nil), default: Int) -> Int {
  case r {
    Ok(v) -> v
    Error(Nil) -> default
  }
}

fn step_return(v: ir.Value) -> ir.Expr {
  ir.Let(
    ["_step"],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("return"), v]),
    ir.Return([ir.Var("_step")]),
  )
}

fn step_throw(v: ir.Value) -> ir.Expr {
  ir.Let(
    ["_step"],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("throw"), v]),
    ir.Return([ir.Var("_step")]),
  )
}

fn step_yield(v: ir.Value, ns: Int, loc: ir.Value) -> ir.Expr {
  ir.Let(
    ["_step"],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("yield"), v, ir.ConstI32(ns), loc]),
    ir.Return([ir.Var("_step")]),
  )
}

fn step_await(v: ir.Value, ns: Int, loc: ir.Value) -> ir.Expr {
  ir.Let(
    ["_step"],
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("await"), v, ir.ConstI32(ns), loc]),
    ir.Return([ir.Var("_step")]),
  )
}

/// §18.4 loc pack (anf.Build variant): fresh MakeTuple of `layout.size` where
/// slot i = `overrides[i]` if present, else copy-forward `TupleGet(i, loc_v)`.
/// Plain copy-forward — the fragment-emit path uses `pack_loc_cps` which also
/// picks up reassigned SSA vars for hoisted slots.
fn pack_loc(ctx: SmCtx, overrides: Dict(Int, ir.Value)) -> anf.Build(ir.Value) {
  pack_loc_build(ctx, overrides, 0, [])
}

fn pack_loc_build(
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
) -> anf.Build(ir.Value) {
  case i < ctx.layout.size {
    False -> anf.make_tuple(list.reverse(acc))
    True ->
      case dict.get(overrides, i) {
        Ok(v) -> pack_loc_build(ctx, overrides, i + 1, [v, ..acc])
        Error(_) ->
          anf.then(anf.bind(anf.tuple_get(ctx.loc_v, i)), fn(v) {
            pack_loc_build(ctx, overrides, i + 1, [v, ..acc])
          })
      }
  }
}

/// §18.4.1 arm-entry local restore: bind `TupleGet(idx, loc_v)` for every
/// hoisted slot; return slot→Var so the caller re-seeds `state.set_slot_var`.
/// Bound in ascending slot order for reproducible IR.
fn restore_locals(ctx: SmCtx) -> anf.Build(Dict(Int, ir.Value)) {
  let entries =
    ctx.layout.slot_to_idx
    |> dict.to_list
    |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  restore_locals_fold(ctx, entries, dict.new())
}

fn restore_locals_fold(
  ctx: SmCtx,
  entries: List(#(Int, Int)),
  acc: Dict(Int, ir.Value),
) -> anf.Build(Dict(Int, ir.Value)) {
  case entries {
    [] -> anf.pure(acc)
    [#(slot, idx), ..rest] ->
      anf.then(anf.bind(anf.tuple_get(ctx.loc_v, idx)), fn(v) {
        restore_locals_fold(ctx, rest, dict.insert(acc, slot, v))
      })
  }
}

/// §18.1 switch-default: `Return {throw, new_error("invalid gen state")}`.
/// Uses anf.host (invariant #3) so returns the threaded Emitter2.
fn sm_default_arm(e: Emitter2) -> #(ir.Expr, Emitter2) {
  let msg = ir.ConstBinary(bit_array.from_string("invalid gen state"))
  anf.run_to(anf.host("new_error", [msg]), e, fn(_e, err) { step_throw(err) })
}

// ── §18.1 state-machine ir.Function shell (u-sm-skeleton-emit) ──────────────

/// D13: local copy of func.gleam:226 `cap_param_name` — do NOT import
/// func.gleam. Must stay byte-identical so the outer `jsf_N` and this
/// `jsf_N__sm` closure agree on capture-param positions.
fn cap_param_name(i: Int) -> String {
  "cap_" <> int.to_string(i)
}

/// Build the sm function's ir param list: `[cap_0..cap_{ncap-1}, _rs, _sent,
/// _loc]` (SPEC §18.1 / M8 wire ABI). Mirrors func.gleam:284-288 shape.
fn build_sm_params(i: Int, ncap: Int) -> List(ir.Local) {
  case i < ncap {
    True -> [
      ir.Local(cap_param_name(i), ir.TTerm),
      ..build_sm_params(i + 1, ncap)
    ]
    False -> [
      ir.Local("_rs", ir.TTerm),
      ir.Local("_sent", ir.TTerm),
      ir.Local("_loc", ir.TTerm),
    ]
  }
}

/// Build and register the `jsf_N__sm` ir.Function shell (SPEC §18.1). The
/// caller generates `lresume` via `state.fresh_label` and threads it through
/// `SmCtx` BEFORE building `arms`/`default` (arm bodies `ir.Continue` to it
/// for state transitions), then passes it here so the Loop label matches.
///
/// Shape (ir.gleam:422-428/668/675-680/686-691/1296-1302):
///   fn <sm_name>(cap_0..cap_{ncap-1}, _rs, _sent, _loc) -> [TTerm] {
///     let [_mode] = TupleGet(0, _sent)
///     let [_sv]   = TupleGet(1, _sent)
///     loop <lresume>(_rs_i = _rs, _loc_i = _loc) -> [TTerm] {
///       let [_rsi32] = Convert(UnboxInt(W32), _rs_i)
///       switch _rsi32 -> [TTerm] { arms.. ; default }
///     }
///   }
///
/// `_mode`/`_sv`/`_loc_i` are the fixed names `SmCtx.mode_v`/`sent_v`/`loc_v`
/// reference — do not fresh-var them.
fn emit_sm_function(
  e: Emitter2,
  sm_name: String,
  ncap: Int,
  lresume: String,
  arms: List(ir.SwitchArm),
  default: ir.Expr,
) -> Emitter2 {
  let sent = ir.Var("_sent")
  let loop_body =
    ir.Let(
      ["_rsi32"],
      ir.Convert(ir.UnboxInt(ir.W32), ir.Var("_rs_i")),
      ir.Switch(ir.Var("_rsi32"), [ir.TTerm], arms, default),
    )
  let body =
    ir.Let(
      ["_mode"],
      ir.TermOp(ir.TupleGet(0), [sent]),
      ir.Let(
        ["_sv"],
        ir.TermOp(ir.TupleGet(1), [sent]),
        ir.Loop(
          lresume,
          [
            ir.LoopParam("_rs_i", ir.TTerm, ir.Var("_rs")),
            ir.LoopParam("_loc_i", ir.TTerm, ir.Var("_loc")),
          ],
          [ir.TTerm],
          loop_body,
        ),
      ),
    )
  state.add_function(
    e,
    ir.Function(
      name: sm_name,
      params: build_sm_params(0, ncap),
      result: [ir.TTerm],
      locals: [],
      body: body,
    ),
  )
}

// ── §18.1 outer wrapper + parent-frame closure site (u-outer-fn-emit) ───────

/// D5 uniform IR-param shape: [cap_0.., _frame, _args]. Local copy of
/// func.gleam:284-288 (D13: no cross-emit-module imports).
fn build_outer_params(i: Int, n: Int) -> List(ir.Local) {
  case i < n {
    False -> [ir.Local("_frame", ir.TTerm), ir.Local("_args", ir.TTerm)]
    True -> [
      ir.Local(cap_param_name(i), ir.TTerm),
      ..build_outer_params(i + 1, n)
    ]
  }
}

/// The outer's own cap-param Values, forwarded verbatim as sm's captures.
fn cap_vars(i: Int, n: Int) -> List(ir.Value) {
  case i < n {
    False -> []
    True -> [ir.Var(cap_param_name(i)), ..cap_vars(i + 1, n)]
  }
}

fn atom_bool(rc: state.RealmConsts, b: Bool) -> ir.Value {
  case b {
    True -> rc.true_
    False -> rc.false_
  }
}

/// §15.1.5 ExpectedArgumentCount — leading params before the first default.
/// Local copy of func.gleam:788-797 (D13).
fn expected_length(fixed: List(ast.Pattern)) -> Int {
  fixed
  |> list.take_while(fn(p) {
    case p {
      ast.AssignmentPattern(..) -> False
      _ -> True
    }
  })
  |> list.length
}

fn start_op(kind: state.CoroutineKind) -> String {
  case kind {
    state.CorAsync -> "async_start"
    state.CorGenerator -> "gen_start"
    state.CorAsyncGen -> "asyncgen_start"
  }
}

fn kind_is_async(kind: state.CoroutineKind) -> Bool {
  case kind {
    state.CorAsync | state.CorAsyncGen -> True
    state.CorGenerator -> False
  }
}

fn kind_is_gen(kind: state.CoroutineKind) -> Bool {
  case kind {
    state.CorGenerator | state.CorAsyncGen -> True
    state.CorAsync -> False
  }
}

// pdict seam (mirrors anf.gleam:68-94 / func.gleam:119-145). Re-entrant
// (fresh make_ref per call). Shared by run_rk / emit_arm_body / run_terminal.

type Ref

type Erased

@external(erlang, "erlang", "make_ref")
fn make_ref() -> Ref

@external(erlang, "erlang", "put")
fn pdict_put(k: Ref, v: a) -> Erased

@external(erlang, "erlang", "erase")
fn pdict_erase(k: Ref) -> a

/// Emit the outer `jsf_N` wrapper (body: loc0=MakeTuple(initial); sm=
/// MakeClosure(sm_name,caps,3); h=host(<kind>_start,[sm,_frame,_args,loc0]);
/// Return([h])) into e.fns_acc, then return the PARENT-frame closure-site
/// Let-chain (FnFlags + MakeClosure(outer,captures,2) + host("fn_new",…)).
/// Mirrors func.gleam:838-873 (body) + :745-784 (closure site) locally per D13.
fn emit_outer_function(
  e: Emitter2,
  kind: state.CoroutineKind,
  sm_name: String,
  fn_scope_id: ScopeId,
  params: List(ast.Pattern),
  captures: List(ir.Value),
  layout: LocLayout,
  js_name: Option(String),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let ncap = list.length(captures)
  let #(outer_name, e) = state.fresh_fn_name(e)
  let #(e_child, save) =
    state.enter_function(
      e,
      fn_scope_id,
      strict: e.strict,
      is_async: kind_is_async(kind),
      is_generator: kind_is_gen(kind),
      is_arrow: False,
    )
  // Outer body — SPEC.md:1490-1494 / §9:1752-1756. All caps forwarded to sm.
  let #(body_expr, e_child) =
    anf.run_to(
      {
        use loc0 <- anf.then(anf.make_tuple(layout.initial_values))
        use sm <- anf.then(
          anf.bind(ir.MakeClosure(sm_name, cap_vars(0, ncap), 3)),
        )
        anf.host(start_op(kind), [sm, ir.Var("_frame"), ir.Var("_args"), loc0])
      },
      e_child,
      fn(_e, h) { ir.Return([h]) },
    )
  let e_child =
    state.add_function(
      e_child,
      ir.Function(
        name: outer_name,
        params: build_outer_params(0, ncap),
        result: [ir.TTerm],
        locals: [],
        body: body_expr,
      ),
    )
  let e = state.leave_function(e_child, save)
  // Closure site in PARENT frame — replicates func.gleam:745-784 locally (D13).
  Ok(emit_closure_site(e, outer_name, kind, js_name, params, captures))
}

/// Local replica of func.gleam:745-784 emit_closure_site (D13). Coroutines
/// are never constructors; is_arrow/is_method are not threaded through the
/// dispatch signature (state.gleam:237-244) so are False here — u-sig-seam
/// may widen the seam if async arrows/methods need distinct FnFlags.
fn emit_closure_site(
  e: Emitter2,
  outer_name: String,
  kind: state.CoroutineKind,
  js_name: Option(String),
  params: List(ast.Pattern),
  captures: List(ir.Value),
) -> #(ir.Expr, Emitter2) {
  let rc = e.consts
  // FnFlags wire tuple — MUST match rt_js_types.FnFlags field order exactly
  // (ctor, class_ctor, derived, arrow, method, gen, async). Gleam-tagged.
  let flags = [
    ir.ConstAtom("fn_flags"),
    rc.false_,
    rc.false_,
    rc.false_,
    rc.false_,
    rc.false_,
    atom_bool(rc, kind_is_gen(kind)),
    atom_bool(rc, kind_is_async(kind)),
  ]
  let name_bin = case js_name {
    Some(n) -> ir.ConstBinary(bit_array.from_string(n))
    None -> rc.empty_bin
  }
  let #(fixed, _) = ast_util.split_trailing_rest(params)
  let exp_len = expected_length(fixed)
  anf.run(
    {
      use fun <- anf.then(anf.bind(ir.MakeClosure(outer_name, captures, 2)))
      use flags_t <- anf.then(anf.make_tuple(flags))
      use caps_l <- anf.then(anf.cons_list(captures))
      anf.host("fn_new", [
        fun,
        flags_t,
        name_bin,
        ir.ConstI32(exp_len),
        caps_l,
        ir.ConstAtom("none"),
      ])
    },
    e,
  )
}

// ── §18.4 per-arm shell (u-arm-shell) ───────────────────────────────────────

/// CPS-build a fresh loc tuple where slot `i = overrides[i]` if present, else
/// carried forward via `TupleGet(i, ctx.loc_v)`. Uses deterministic per-index
/// names (`_pk<i>`, `_locp`) so it needs no Emitter2 — safe because callers
/// place it inside a catch-handler / mode-dispatch branch (a fresh Let scope).
fn pack_loc_expr(
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  k: fn(ir.Value) -> ir.Expr,
) -> ir.Expr {
  pack_loc_expr_go(ctx, overrides, 0, [], k)
}

fn pack_loc_expr_go(
  ctx: SmCtx,
  overrides: Dict(Int, ir.Value),
  i: Int,
  acc: List(ir.Value),
  k: fn(ir.Value) -> ir.Expr,
) -> ir.Expr {
  case i < ctx.layout.size {
    False ->
      ir.Let(
        ["_locp"],
        ir.TermOp(ir.MakeTuple, list.reverse(acc)),
        k(ir.Var("_locp")),
      )
    True ->
      case dict.get(overrides, i) {
        Ok(v) -> pack_loc_expr_go(ctx, overrides, i + 1, [v, ..acc], k)
        Error(_) -> {
          let name = "_pk" <> int.to_string(i)
          ir.Let(
            [name],
            ir.TermOp(ir.TupleGet(i), [ctx.loc_v]),
            pack_loc_expr_go(ctx, overrides, i + 1, [ir.Var(name), ..acc], k),
          )
        }
      }
  }
}

/// §18.4 arm-catch route for a JS-thrown value `ev` while in `region`.
/// catch_state → stash `ev` at caught_loc_idx and Continue there; else
/// finally_state → stash pending={throw,ev} and Continue there; else no
/// enclosing try-region → step_throw. Catch takes priority when both exist.
fn route_throw(ctx: SmCtx, region: Option(TryEntry), ev: ir.Value) -> ir.Expr {
  case region {
    Some(TryEntry(catch_state: Some(cs), caught_loc_idx: ci, ..)) ->
      pack_loc_expr(ctx, dict.from_list([#(ci, ev)]), fn(locp) {
        ir.Continue(ctx.lresume, [ir.ConstI32(cs), locp])
      })
    Some(TryEntry(finally_state: Some(fs), pending_loc_idx: pi, ..)) ->
      ir.Let(
        ["_pend"],
        ir.TermOp(ir.MakeTuple, [ir.ConstAtom("throw"), ev]),
        pack_loc_expr(ctx, dict.from_list([#(pi, ir.Var("_pend"))]), fn(locp) {
          ir.Continue(ctx.lresume, [ir.ConstI32(fs), locp])
        }),
      )
    _ -> step_throw(ev)
  }
}

/// §18.4 mode==2 route for injected `.return(v)`. Walks `region` outward via
/// `.outer` (mirroring route_abrupt_walk's PkReturn arm): the FIRST enclosing
/// entry with a `finally_state` captures pending={return,v}; catch-only
/// regions are skipped (a return never routes to catch_state); no interceptor
/// → step_return.
fn route_return(ctx: SmCtx, region: Option(TryEntry), v: ir.Value) -> ir.Expr {
  case region {
    Some(TryEntry(finally_state: Some(fs), pending_loc_idx: pi, ..)) ->
      ir.Let(
        ["_pend"],
        ir.TermOp(ir.MakeTuple, [ir.ConstAtom("return"), v]),
        pack_loc_expr(ctx, dict.from_list([#(pi, ir.Var("_pend"))]), fn(locp) {
          ir.Continue(ctx.lresume, [ir.ConstI32(fs), locp])
        }),
      )
    Some(entry) -> route_return(ctx, find_try(ctx.try_entries, entry.outer), v)
    None -> step_return(v)
  }
}

/// §18.4 wrap: every SwitchArm body sits inside exactly one `ir.Try` whose
/// handler catches the R2 `js_exn` tag and routes the caught value per
/// `route_throw`. Non-split trys nest as ordinary M17 `ir.Try` under `inner`.
fn wrap_arm_try(
  ctx: SmCtx,
  _n: Int,
  region: Option(TryEntry),
  inner: ir.Expr,
) -> ir.Expr {
  ir.Try(result: [ir.TTerm], body: inner, handlers: [
    ir.CatchHandler(
      on: ir.OnTag("js_exn"),
      payload: ["_e"],
      exnref: None,
      handler: route_throw(ctx, region, ir.Var("_e")),
    ),
  ])
}

/// §18.4 step 2: at the head of every AeResume arm, dispatch on `ctx.mode_v`
/// (0=next, 1=throw, 2=return per rt_js_async sent_* consts). mode 1 →
/// `route_throw(sent_v)`; mode 2 → `route_return(sent_v)`; else `normal`.
/// AeInitial/AeJump skip dispatch: `_mode`/`_sv` are bound OUTSIDE the Loop
/// so a Continue-entered arm sees STALE values from the last resume — re-
/// dispatching would re-throw an already-handled injection. `ir.If` cond is a
/// Value (gotcha #7) so each `IEq` is Let-bound first.
fn emit_mode_dispatch(
  ctx: SmCtx,
  entry: ArmEntry,
  region: Option(TryEntry),
  normal: ir.Expr,
) -> ir.Expr {
  case entry {
    AeInitial | AeJump -> normal
    AeResume(_) ->
      ir.Let(
        ["_i32m"],
        ir.Convert(ir.UnboxInt(ir.W32), ctx.mode_v),
        ir.Let(
          ["_is_thr"],
          ir.Num(ir.IEq(ir.W32), [ir.Var("_i32m"), ir.ConstI32(1)]),
          ir.If(
            ir.Var("_is_thr"),
            [ir.TTerm],
            route_throw(ctx, region, ctx.sent_v),
            ir.Let(
              ["_is_ret"],
              ir.Num(ir.IEq(ir.W32), [ir.Var("_i32m"), ir.ConstI32(2)]),
              ir.If(
                ir.Var("_is_ret"),
                [ir.TTerm],
                route_return(ctx, region, ctx.sent_v),
                normal,
              ),
            ),
          ),
        ),
      )
  }
}

// ── §18.6 yield* delegation (u-yield-star-arm) ──────────────────────────────

/// Run a Build(ir.Expr) whose leaves are terminal (Return/Continue). anf.run
/// only handles Build(ir.Value); this reuses the module's pdict seam.
fn run_terminal(b: anf.Build(ir.Expr), e: Emitter2) -> #(ir.Expr, Emitter2) {
  let cell = make_ref()
  let _ = pdict_put(cell, e)
  let tree =
    b(e, fn(ef, expr) {
      let _ = pdict_put(cell, ef)
      expr
    })
  #(tree, pdict_erase(cell))
}

/// ir.If where both arms are terminal ir.Expr (Return/Continue). Threads
/// Emitter2 through arms sequentially so fresh_var bumps do not collide.
fn if_terminal(
  cond: ir.Value,
  t: anf.Build(ir.Expr),
  f: anf.Build(ir.Expr),
) -> anf.Build(ir.Expr) {
  fn(e, k) {
    let #(t_tree, e) = run_terminal(t, e)
    let #(f_tree, e) = run_terminal(f, e)
    k(e, ir.If(cond, [ir.TTerm], t_tree, f_tree))
  }
}

/// Boxed state-id for `Continue(lresume, [rs_i, loc_i])` — LoopParam `_rs_i`
/// is TTerm (emit_sm_function), so the raw ConstI32 must be BoxInt-wrapped.
fn rs_box(n: Int) -> anf.Build(ir.Value) {
  anf.bind(ir.Convert(ir.BoxInt(ir.W32), ir.ConstI32(n)))
}

/// Static named-string wire key `{string_key, {named, <s>}}`.
fn key_named(s: String) -> anf.Build(ir.Value) {
  use inner <- anf.then(
    anf.make_tuple([
      ir.ConstAtom("named"),
      ir.ConstBinary(bit_array.from_string(s)),
    ]),
  )
  anf.make_tuple([ir.ConstAtom("string_key"), inner])
}

/// Dynamic named-string wire key from a runtime binary Value (mname).
fn key_named_dyn(bin: ir.Value) -> anf.Build(ir.Value) {
  use inner <- anf.then(anf.make_tuple([ir.ConstAtom("named"), bin]))
  anf.make_tuple([ir.ConstAtom("string_key"), inner])
}

fn iter_hint(kind: state.CoroutineKind) -> ir.Value {
  case kind {
    state.CorAsyncGen -> ir.ConstAtom("async")
    state.CorGenerator | state.CorAsync -> ir.ConstAtom("sync")
  }
}

/// §18.6(a) SETUP fragment — runs in the PRIOR state's arm at the `yield* e`
/// site. Acquires the iterator record, stores `iter_h`/`inner` into `loc`,
/// then `Continue`s to the self-looping delegate state `nd`. Entry into `nd`
/// sees this invocation's (mode, sent), which the arm forwards verbatim.
fn emit_delegate_setup(
  e: Emitter2,
  ctx: SmCtx,
  expr_v: ir.Value,
  nd: Int,
  iter_idx: Int,
  inner_idx: Int,
) -> #(ir.Expr, Emitter2) {
  let b = {
    use iter_h <- anf.then(
      anf.host("get_iterator", [expr_v, iter_hint(ctx.kind)]),
    )
    use k_iter <- anf.then(key_named("iterator"))
    use inner <- anf.then(anf.host("get_prop", [iter_h, k_iter]))
    let ov = dict.from_list([#(iter_idx, iter_h), #(inner_idx, inner)])
    // pack_loc_cps (not pack_loc): setup runs at seg-tail post-fragment, so
    // hoisted-local reassignments must repack from current slot_vars (§18.4.4).
    use loc2 <- anf.then(fn(e, k) { pack_loc_cps(e, ctx, ov, k) })
    use rs <- anf.then(rs_box(nd))
    anf.pure(ir.Continue(ctx.lresume, [rs, loc2]))
  }
  run_terminal(b, e)
}

/// §18.6(b) self-looping delegate arm body for state `nd`. NOT gated by
/// emit_mode_dispatch — this arm dispatches on `mode` itself to forward
/// next/throw/return to the inner iterator. Every path is terminal
/// (step_*/Continue), so the caller wraps only in `wrap_arm_try`.
fn emit_delegate_arm(
  e: Emitter2,
  ctx: SmCtx,
  nd: Int,
  next_state: Int,
  iter_idx: Int,
  inner_idx: Int,
  result_idx: Int,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let undef = ir.ConstAtom("undefined")
  let b = {
    // (1) restore iter_h / inner from loc.
    use iter_h <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, iter_idx)))
    use inner <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, inner_idx)))
    // (2) mname via Switch on unboxed mode → "next"/"throw"/"return".
    use mode_i32 <- anf.then(
      anf.bind(ir.Convert(ir.UnboxInt(ir.W32), ctx.mode_v)),
    )
    let mbin = fn(s) { ir.Values([ir.ConstBinary(bit_array.from_string(s))]) }
    use mname <- anf.then(
      anf.bind(ir.Switch(
        mode_i32,
        [ir.TTerm],
        [
          ir.SwitchArm(0, mbin("next")),
          ir.SwitchArm(1, mbin("throw")),
          ir.SwitchArm(2, mbin("return")),
        ],
        mbin("next"),
      )),
    )
    // (3) key = {string_key, {named, mname}}; (4) meth = inner[key].
    use key <- anf.then(key_named_dyn(mname))
    use meth <- anf.then(anf.host("get_prop", [inner, key]))
    // (5) missing = (mode≠0) ∧ (meth === undefined) — both i32, IAnd → i32.
    use mode_ne0 <- anf.then(
      anf.bind(ir.Num(ir.INe(ir.W32), [mode_i32, ir.ConstI32(0)])),
    )
    use is_undef_t <- anf.then(anf.host("strict_eq", [meth, undef]))
    use is_undef <- anf.then(anf.host("truthy", [is_undef_t]))
    use missing <- anf.then(
      anf.bind(ir.Num(ir.IAnd(ir.W32), [mode_ne0, is_undef])),
    )
    use is_throw <- anf.then(
      anf.bind(ir.Num(ir.IEq(ir.W32), [mode_i32, ir.ConstI32(1)])),
    )
    use is_return <- anf.then(
      anf.bind(ir.Num(ir.IEq(ir.W32), [mode_i32, ir.ConstI32(2)])),
    )
    // ── missing throw/return method ─────────────────────────────────────────
    let on_missing =
      if_terminal(
        is_throw,
        // mode==1: close inner (abrupt), then step_throw(sent_v).
        {
          use _ <- anf.then(
            anf.host_unit("iter_close", [
              iter_h,
              ir.ConstAtom("true"),
            ]),
          )
          anf.pure(step_throw(ctx.sent_v))
        },
        // mode==2: SPEC §18.6 → result := sent_v, continue past the yield*.
        {
          use loc2 <- anf.then(pack_loc(
            ctx,
            dict.from_list([#(result_idx, ctx.sent_v)]),
          ))
          use rs <- anf.then(rs_box(next_state))
          anf.pure(ir.Continue(ctx.lresume, [rs, loc2]))
        },
      )
    // ── method present (or mode==0): call it, branch on {done, value} ───────
    let on_call = {
      use argl <- anf.then(anf.cons_list([ctx.sent_v]))
      use res <- anf.then(anf.host("call", [meth, inner, argl]))
      use k_done <- anf.then(key_named("done"))
      use done_t <- anf.then(anf.host("get_prop", [res, k_done]))
      use done <- anf.then(anf.host("truthy", [done_t]))
      use k_val <- anf.then(key_named("value"))
      use v <- anf.then(anf.host("get_prop", [res, k_val]))
      if_terminal(
        done,
        // done: mode==2 → step_return(v); else result := v, continue past.
        if_terminal(is_return, anf.pure(step_return(v)), {
          use loc2 <- anf.then(pack_loc(ctx, dict.from_list([#(result_idx, v)])))
          use rs <- anf.then(rs_box(next_state))
          anf.pure(ir.Continue(ctx.lresume, [rs, loc2]))
        }),
        // not done: yield v; resumption re-enters `nd` with caller's next
        // (mode, sent), forwarding it verbatim (SPEC §18.6 last clause).
        anf.pure(step_yield(v, nd, ctx.loc_v)),
      )
    }
    if_terminal(missing, on_missing, on_call)
  }
  Ok(run_terminal(b, e))
}

// ── §18.5 try/finally-across-split (u-finally-arm) ──────────────────────────
//
// A TryEntry allocates dedicated state ids for its catch handler and/or
// finalizer. `emit_catch_arm` / `emit_finally_arm` build the *bodies* of
// those SwitchArms — the per-arm ir.Try wrapper and mode-dispatch prefix are
// applied by wrap_arm_try / emit_mode_dispatch (u-arm-shell) around what is
// returned here. Outer nesting: a completion at finally-end propagates to
// entry.outer's handler (throw → its catch_state else its finally_state;
// return → its finally_state, walking further out past catch-only regions),
// bottoming out at step_throw / step_return when no outer region remains.

fn find_try_entry(ctx: SmCtx, id: Int) -> Option(TryEntry) {
  list.find(ctx.try_entries, fn(t) { t.id == id }) |> option.from_result
}

fn outer_entry(ctx: SmCtx, entry: TryEntry) -> Option(TryEntry) {
  case entry.outer {
    None -> None
    Some(oid) -> find_try_entry(ctx, oid)
  }
}

/// Restore every hoisted local from ctx.loc_v (Let-chain of TupleGet) and
/// seed e.slot_vars so downstream e.dispatch.emit_stmts reads the loc-
/// sourced Vars. `k` receives the seeded Emitter2 under the Let-chain.
fn restore_and_seed(
  e: Emitter2,
  ctx: SmCtx,
  k: fn(Emitter2) -> Result(ir.Expr, state.EmitError),
) -> Result(ir.Expr, state.EmitError) {
  restore_and_seed_go(e, ctx, dict.to_list(ctx.layout.slot_to_idx), k)
}

fn restore_and_seed_go(
  e: Emitter2,
  ctx: SmCtx,
  slots: List(#(Int, Int)),
  k: fn(Emitter2) -> Result(ir.Expr, state.EmitError),
) -> Result(ir.Expr, state.EmitError) {
  case slots {
    [] -> k(e)
    [#(slot, idx), ..rest] -> {
      let #(name, e) = state.fresh_var(e)
      let e = state.set_slot_var(e, slot, name)
      use body <- result.map(restore_and_seed_go(e, ctx, rest, k))
      ir.Let([name], ir.TermOp(ir.TupleGet(idx), [ctx.loc_v]), body)
    }
  }
}

/// Continue(lresume, [ConstI32(ns), loc']) with loc' = pack_loc(ctx, ovr).
/// Runs the pack Build via anf.run_to so its fresh_var allocations thread out.
fn jump_state(
  e: Emitter2,
  ctx: SmCtx,
  ns: Int,
  overrides: Dict(Int, ir.Value),
) -> #(ir.Expr, Emitter2) {
  anf.run_to(pack_loc(ctx, overrides), e, fn(_e, loc2) {
    ir.Continue(ctx.lresume, [ir.ConstI32(ns), loc2])
  })
}

/// Like `jump_state` but packs loc via `pack_loc_cps`, so hoisted-local slots
/// read the CURRENT `state.get_slot_var` (picking up reassignments made by the
/// preceding emit_stmts) instead of copy-forwarding from arm-entry `loc_v`.
fn jump_state_leaf(
  e: Emitter2,
  ctx: SmCtx,
  ns: Int,
  overrides: Dict(Int, ir.Value),
) -> #(ir.Expr, Emitter2) {
  cps_pair(e, fn(e, k) {
    pack_loc_cps(e, ctx, overrides, fn(e, loc) {
      k(e, sm_continue(ctx, ns, loc))
    })
  })
}

/// Throw-completion terminal for `carry`, respecting outer-region nesting:
/// outer catch → jump there with caught=carry; else outer finally → jump
/// there with pending={throw,carry}; else step_throw.
fn dispatch_throw(
  e: Emitter2,
  ctx: SmCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter2) {
  case outer {
    None -> #(step_throw(carry), e)
    Some(o) ->
      case o.catch_state {
        Some(cs) ->
          jump_state_leaf(
            e,
            ctx,
            cs,
            dict.from_list([#(o.caught_loc_idx, carry)]),
          )
        None ->
          case o.finally_state {
            Some(fs) -> {
              let #(pn, e) = state.fresh_var(e)
              let #(jump, e) =
                jump_state_leaf(
                  e,
                  ctx,
                  fs,
                  dict.from_list([#(o.pending_loc_idx, ir.Var(pn))]),
                )
              let pend = ir.TermOp(ir.MakeTuple, [ir.ConstAtom("throw"), carry])
              #(ir.Let([pn], pend, jump), e)
            }
            // A TryEntry with neither handler is never allocated (§18.2);
            // guard by walking further out rather than crashing.
            None -> dispatch_throw(e, ctx, outer_entry(ctx, o), carry)
          }
      }
  }
}

/// Return-completion terminal: only finalizers intercept it (a catch-only
/// outer region is transparent — walk further out).
fn dispatch_return(
  e: Emitter2,
  ctx: SmCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter2) {
  case outer {
    None -> #(step_return(carry), e)
    Some(o) ->
      case o.finally_state {
        Some(fs) -> {
          let #(pn, e) = state.fresh_var(e)
          let #(jump, e) =
            jump_state_leaf(
              e,
              ctx,
              fs,
              dict.from_list([#(o.pending_loc_idx, ir.Var(pn))]),
            )
          let pend = ir.TermOp(ir.MakeTuple, [ir.ConstAtom("return"), carry])
          #(ir.Let([pn], pend, jump), e)
        }
        None -> dispatch_return(e, ctx, outer_entry(ctx, o), carry)
      }
  }
}

/// Goto-completion terminal (`{goto, ConstI32(target)}` deferred by an inner
/// finally): only outer finalizers intercept it (catch-only regions are
/// transparent). Bottoming out: unbox the boxed target int and Continue
/// directly to that state (`carry` came from `pending_tuple(PkGoto(t))`).
fn dispatch_goto(
  e: Emitter2,
  ctx: SmCtx,
  outer: Option(TryEntry),
  carry: ir.Value,
) -> #(ir.Expr, Emitter2) {
  case outer {
    None ->
      cps_pair(e, fn(e, k) {
        let #(ns, e) = state.fresh_var(e)
        ir.Let(
          [ns],
          ir.Convert(ir.UnboxInt(ir.W32), carry),
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, ir.Continue(ctx.lresume, [ir.Var(ns), loc]))
          }),
        )
      })
    Some(o) ->
      case o.finally_state {
        Some(fs) -> {
          let #(pn, e) = state.fresh_var(e)
          let #(jump, e) =
            jump_state_leaf(
              e,
              ctx,
              fs,
              dict.from_list([#(o.pending_loc_idx, ir.Var(pn))]),
            )
          let pend = ir.TermOp(ir.MakeTuple, [ir.ConstAtom("goto"), carry])
          #(ir.Let([pn], pend, jump), e)
        }
        None -> dispatch_goto(e, ctx, outer_entry(ctx, o), carry)
      }
  }
}

/// Pending-dispatch tail placed after the finalizer body (SPEC §9:1782-1786).
/// Built inside emit_finally_arm's k_tail with the LEAF emitter so every
/// `pack_loc_cps` under it (via jump_state_leaf / dispatch_*) picks up
/// hoisted-local reassignments the finalizer body made. Shape:
///   If(TermTest(IsAtom, pend), Continue(after_state, loc'),
///      Let kind=TupleGet(0,pend); Let carry=TupleGet(1,pend);
///      If(kind=='goto', dispatch_goto,
///         If(kind=='throw', dispatch_throw, dispatch_return)))
fn build_pending_dispatch(
  e: Emitter2,
  ctx: SmCtx,
  entry: TryEntry,
  pend: ir.Value,
) -> #(ir.Expr, Emitter2) {
  let outer = outer_entry(ctx, entry)
  let #(normal_jump, e) = jump_state_leaf(e, ctx, entry.after_state, dict.new())
  let #(kind_n, e) = state.fresh_var(e)
  let #(carry_n, e) = state.fresh_var(e)
  let carry = ir.Var(carry_n)
  let #(goto_tree, e) = dispatch_goto(e, ctx, outer, carry)
  let #(throw_tree, e) = dispatch_throw(e, ctx, outer, carry)
  let #(return_tree, e) = dispatch_return(e, ctx, outer, carry)
  // R8: strict_eq JPure → TTerm bool atom; truthy JPure → i32 for If.cond.
  let #(eqg_n, e) = state.fresh_var(e)
  let #(eqgi_n, e) = state.fresh_var(e)
  let #(eqt_n, e) = state.fresh_var(e)
  let #(eqti_n, e) = state.fresh_var(e)
  let throw_or_return =
    ir.Let(
      [eqt_n],
      ir.CallHost("js", "strict_eq", [ir.Var(kind_n), ir.ConstAtom("throw")]),
      ir.Let(
        [eqti_n],
        ir.CallHost("js", "truthy", [ir.Var(eqt_n)]),
        ir.If(ir.Var(eqti_n), [ir.TTerm], throw_tree, return_tree),
      ),
    )
  let kind_branch =
    ir.Let(
      [eqg_n],
      ir.CallHost("js", "strict_eq", [ir.Var(kind_n), ir.ConstAtom("goto")]),
      ir.Let(
        [eqgi_n],
        ir.CallHost("js", "truthy", [ir.Var(eqg_n)]),
        ir.If(ir.Var(eqgi_n), [ir.TTerm], goto_tree, throw_or_return),
      ),
    )
  let tuple_branch =
    ir.Let(
      [kind_n],
      ir.TermOp(ir.TupleGet(0), [pend]),
      ir.Let([carry_n], ir.TermOp(ir.TupleGet(1), [pend]), kind_branch),
    )
  let #(isatom_n, e) = state.fresh_var(e)
  #(
    ir.Let(
      [isatom_n],
      ir.TermTest(ir.IsAtom, pend),
      ir.If(ir.Var(isatom_n), [ir.TTerm], normal_jump, tuple_branch),
    ),
    e,
  )
}

/// Result-CPS runner (mirrors stmt.gleam:284-297). Recovers the leaf Emitter2
/// alongside the built ir.Expr; pre-seeds so a body that diverges before
/// `done` still yields a valid Emitter2 (module-monotone invariant #1).
fn run_rk(
  e: Emitter2,
  body: fn(Emitter2, fn(Emitter2, ir.Expr) -> Result(ir.Expr, state.EmitError)) ->
    Result(ir.Expr, state.EmitError),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let cell = make_ref()
  let _ = pdict_put(cell, e)
  let done = fn(ef: Emitter2, tree: ir.Expr) {
    let _ = pdict_put(cell, ef)
    Ok(tree)
  }
  use tree <- result.map(body(e, done))
  #(tree, pdict_erase(cell))
}

/// §18.5 finally-state arm body: restore locals; pend=loc[pending_idx];
/// with_abrupt_intercept so `return`/cross-state break in the finalizer route
/// via `route_abrupt` (never bare ir.Return); emit finalizer via
/// e.dispatch.emit_stmts; tail = build_pending_dispatch built INSIDE K with
/// the LEAF emitter so its loc packs pick up finalizer-body reassignments.
fn emit_finally_arm(
  e: Emitter2,
  ctx: SmCtx,
  entry: TryEntry,
  finalizer: List(ast.StmtWithLine),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  run_rk(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let #(pend_n, e) = state.fresh_var(e)
    let pend = ir.Var(pend_n)
    let leaf_cell = make_ref()
    let _ = pdict_put(leaf_cell, e)
    let seed = e
    use #(body, e2) <- result.try(
      with_abrupt_intercept(e, ctx, fn(e, restore) {
        let k_tail = fn(e_leaf: Emitter2) -> ir.Expr {
          let #(tail, e_final) =
            build_pending_dispatch(e_leaf, ctx, entry, pend)
          let _ = pdict_put(leaf_cell, restore(e_final))
          tail
        }
        use #(body, e2) <- result.map(e.dispatch.emit_stmts(
          e,
          finalizer,
          k_tail,
        ))
        case k_tail_ran(leaf_cell, seed) {
          True -> Nil
          False -> {
            let _ = pdict_put(leaf_cell, restore(e2))
            Nil
          }
        }
        #(body, e2)
      }),
    )
    let _ = e2
    let e_out: Emitter2 = pdict_erase(leaf_cell)
    done(
      e_out,
      ir.Let(
        [pend_n],
        ir.TermOp(ir.TupleGet(entry.pending_loc_idx), [ctx.loc_v]),
        body,
      ),
    )
  })
}

/// §18.5 catch-state arm body: restore locals; caught=loc[caught_idx];
/// [enter catch scope + destructure param]; with_abrupt_intercept so `return`
/// and cross-state break in the handler route via `route_abrupt`; emit
/// handler body; tail = Continue to this try's finally (pending="normal") if
/// present, else to after_state — built INSIDE K with the LEAF emitter (via
/// pack_loc_cps) so hoisted-local reassignments in the catch body carry
/// forward. Mirrors stmt.emit_catch_handler:1584-1606 for scope entry.
fn emit_catch_arm(
  e: Emitter2,
  ctx: SmCtx,
  entry: TryEntry,
  handler: ast.CatchClause,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let ast.CatchClause(param:, body: catch_body) = handler
  run_rk(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let #(caught_n, e) = state.fresh_var(e)
    let caught = ir.Var(caught_n)
    let leaf_cell = make_ref()
    let _ = pdict_put(leaf_cell, e)
    let seed = e
    use #(handler_tree, e2) <- result.try(
      with_abrupt_intercept(e, ctx, fn(e, restore) {
        let k_tail = fn(e_leaf: Emitter2) -> ir.Expr {
          let #(tail, e_final) = case entry.finally_state {
            Some(fs) ->
              jump_state_leaf(
                e_leaf,
                ctx,
                fs,
                dict.from_list([
                  #(entry.pending_loc_idx, ir.ConstAtom("normal")),
                ]),
              )
            None -> jump_state_leaf(e_leaf, ctx, entry.after_state, dict.new())
          }
          let _ = pdict_put(leaf_cell, restore(e_final))
          tail
        }
        use #(tree, e2) <- result.try(case param {
          Some(p) -> {
            let #(e, save) = state.enter_scope(e, in_block: e.in_block)
            use #(dtree, e) <- result.try(e.dispatch.emit_destructure(
              e,
              p,
              caught,
              state.BindLet,
            ))
            let #(dn, e) = state.fresh_var(e)
            use #(body_tree, e) <- result.map(e.dispatch.emit_stmts(
              e,
              catch_body,
              k_tail,
            ))
            #(ir.Let([dn], dtree, body_tree), state.leave_scope(e, save))
          }
          None -> e.dispatch.emit_stmts(e, catch_body, k_tail)
        })
        case k_tail_ran(leaf_cell, seed) {
          True -> Nil
          False -> {
            let _ = pdict_put(leaf_cell, restore(e2))
            Nil
          }
        }
        Ok(#(tree, e2))
      }),
    )
    let _ = e2
    let e_out: Emitter2 = pdict_erase(leaf_cell)
    done(
      e_out,
      ir.Let(
        [caught_n],
        ir.TermOp(ir.TupleGet(entry.caught_loc_idx), [ctx.loc_v]),
        handler_tree,
      ),
    )
  })
}

// ── §18.4.1+3+4 core per-state fragment emitter (u-fragment-emit) ───────────

/// Emit one "normal" arm's body (§18.4). FIRST installs `arm.entry_cursor`
/// (SCOPE-CURSOR INVARIANT) so e.dispatch.emit_stmts pops the correct block/fn
/// scopes. Then:
///  1. `restore_and_seed`: `Let([xᵢ], TupleGet(idxᵢ, loc), …)` per hoisted
///     slot + `state.set_slot_var` so downstream emit_expr/emit_stmts read the
///     loc-sourced Vars (§18.4.1).
///  2. `with_abrupt_intercept`: push sentinel Frame2s for `arm.sm_labels` and
///     install SmAbrupt hooks so `return`/cross-state `break`/`continue` in
///     the fragment route via `route_abrupt` (§18.4.5).
///  3. `dispatch.emit_stmts(body_fragment, k_tail)` — fragment is split-free.
///  4. `k_tail` (state.K, non-Result) builds `arm.tail` with the LEAF emitter
///     so `pack_loc_cps` picks up any fragment reassignments (§18.4.4). The
///     leaf Emitter2 and any operand-eval error are smuggled out via pdict
///     (state.K can't return Result) — same pattern as run_rk/anf.run_to.
/// Mode-dispatch (§18.4.2) and the arm-Try wrapper are layered by the caller
/// (build_switch_arms) AROUND this result.
fn emit_arm_body(
  e: Emitter2,
  ctx: SmCtx,
  arm: ArmSpec,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let e = install_cursor(e, arm.entry_cursor)
  // Per-arm ctx: try_stack from arm.region; sm_labels from arm.sm_labels.
  let ctx = SmCtx(..with_region(ctx, arm.region), sm_labels: arm.sm_labels)
  // Leaf-emitter + error cells (state.K returns bare ir.Expr; smuggle via
  // pdict so k_tail's fresh_var allocations and any dispatch.emit_expr error
  // reach the caller). Re-entrant: fresh make_ref per call.
  let leaf_cell = make_ref()
  let err_cell = make_ref()
  let _ = pdict_put(leaf_cell, e)
  let _ = pdict_put(err_cell, none_err())
  let tree_r =
    restore_and_seed(e, ctx, fn(e) {
      let _ = pdict_put(leaf_cell, e)
      with_abrupt_intercept(e, ctx, fn(e, restore) {
        // §18.4.2: bind sent_v per `arm.resume` BEFORE emitting the fragment.
        // ResumeReturn/Throw short-circuit — the fragment tail is dead code
        // after `return await p` / `throw await p`.
        case arm.resume {
          Some(ResumeReturn) ->
            Ok(
              route_abrupt(e, ctx, PkReturn(ctx.sent_v), None, fn(ef) {
                let _ = pdict_put(leaf_cell, restore(ef))
                Nil
              }),
            )
          Some(ResumeThrow) ->
            Ok(
              route_abrupt(e, ctx, PkThrow(ctx.sent_v), None, fn(ef) {
                let _ = pdict_put(leaf_cell, restore(ef))
                Nil
              }),
            )
          _ -> {
            use #(prelude, e) <- result.try(case arm.resume {
              Some(ResumeBind(pat, mode)) ->
                e.dispatch.emit_destructure(e, pat, ctx.sent_v, mode)
              _ -> Ok(#(ir.Values([e.consts.undef]), e))
            })
            let #(pre_n, e) = state.fresh_var(e)
            let k_tail = fn(e_leaf: Emitter2) -> ir.Expr {
              case emit_seg_tail(e_leaf, ctx, arm.tail) {
                Ok(#(tail, e_final)) -> {
                  let _ = pdict_put(leaf_cell, restore(e_final))
                  tail
                }
                Error(err) -> {
                  let _ = pdict_put(err_cell, Some(err))
                  let _ = pdict_put(leaf_cell, restore(e_leaf))
                  // Dummy — caller discards after reading err_cell.
                  step_return(e_leaf.consts.undef)
                }
              }
            }
            use #(frag, e2) <- result.map(e.dispatch.emit_stmts(
              e,
              arm.body_fragment,
              k_tail,
            ))
            // If k_tail ran, leaf_cell holds restore(e_final). If the fragment
            // diverged (SegDone: last stmt was Return/Throw/Break/Continue),
            // k_tail never ran — take emit_stmts's captured leaf and restore it.
            case k_tail_ran(leaf_cell, e) {
              True -> Nil
              False -> {
                let _ = pdict_put(leaf_cell, restore(e2))
                Nil
              }
            }
            case arm.resume {
              Some(ResumeBind(..)) -> ir.Let([pre_n], prelude, frag)
              _ -> frag
            }
          }
        }
      })
    })
  let e_final: Emitter2 = pdict_erase(leaf_cell)
  let err: Option(state.EmitError) = pdict_erase(err_cell)
  case err {
    Some(err) -> Error(err)
    None -> result.map(tree_r, fn(t) { #(t, e_final) })
  }
}

fn none_err() -> Option(state.EmitError) {
  None
}

/// True iff k_tail wrote to leaf_cell (its next_var advanced past the seeded
/// pre-intercept snapshot). erase+re-put so the cell survives the check.
fn k_tail_ran(leaf_cell: Ref, seed: Emitter2) -> Bool {
  let cur: Emitter2 = pdict_erase(leaf_cell)
  let _ = pdict_put(leaf_cell, cur)
  cur.next_var > seed.next_var
}

/// Build the arm's terminal ir.Expr from its `SegTail` (§18.4.4). Runs with
/// the LEAF emitter (post-fragment) so `pack_loc_cps` reads current slot_vars.
fn emit_seg_tail(
  e: Emitter2,
  ctx: SmCtx,
  tail: SegTail,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  case tail {
    BodyEnd -> Ok(#(step_return(e.consts.undef), e))
    // Fragment self-terminated — k_tail is unreachable in practice; emit a
    // safe fallback so the ir.Switch arm is well-typed if it IS reached.
    SegDone -> Ok(#(step_return(e.consts.undef), e))
    FallTo(to) ->
      Ok(
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, to, loc))
          })
        }),
      )
    SplitAt(kind, arg, ns) -> {
      use #(operand_tree, e) <- result.try(case arg {
        Some(ex) -> e.dispatch.emit_expr(e, ex)
        None -> Ok(#(ir.Values([e.consts.undef]), e))
      })
      let #(v_n, e) = state.fresh_var(e)
      case kind {
        // §18.6 setup: acquire iterator, stash iter_h/inner in loc extras,
        // Continue→ns (== nd per record_delegate) so emit_delegate_arm's
        // TupleGet(iter_idx/inner_idx) reads populated slots.
        SkYieldStar -> {
          let iter_idx = extra_idx(ctx.layout, iter_key(ns))
          let inner_idx = extra_idx(ctx.layout, inner_key(ns))
          let #(setup, e) =
            emit_delegate_setup(e, ctx, ir.Var(v_n), ns, iter_idx, inner_idx)
          Ok(#(ir.Let([v_n], operand_tree, setup), e))
        }
        SkAwait | SkForAwait | SkYield -> {
          let step = fn(v, loc) {
            case kind {
              SkAwait | SkForAwait -> step_await(v, ns, loc)
              SkYield | SkYieldStar -> step_yield(v, ns, loc)
            }
          }
          Ok(
            cps_pair(e, fn(e, k) {
              ir.Let(
                [v_n],
                operand_tree,
                pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
                  k(e, step(ir.Var(v_n), loc))
                }),
              )
            }),
          )
        }
      }
    }
    CondBranch(cond, then_s, else_s) -> {
      // cond=None: head expr WAS `await c` — this arm is its resume; the
      // resolved value is ctx.sent_v.
      use #(cond_tree, e) <- result.try(case cond {
        Some(ex) -> e.dispatch.emit_expr(e, ex)
        None -> Ok(#(ir.Values([ctx.sent_v]), e))
      })
      let #(cv_n, e) = state.fresh_var(e)
      let #(ti_n, e) = state.fresh_var(e)
      // Bind cond FIRST, pack loc INSIDE — so any hoisted-local reassignments
      // in `cond` are in scope for pack_loc's slot_var reads (§18.4.4).
      Ok(
        cps_pair(e, fn(e, k) {
          ir.Let(
            [cv_n],
            cond_tree,
            ir.Let(
              [ti_n],
              ir.CallHost("js", "truthy", [ir.Var(cv_n)]),
              pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
                k(
                  e,
                  ir.If(
                    ir.Var(ti_n),
                    [ir.TTerm],
                    sm_continue(ctx, then_s, loc),
                    sm_continue(ctx, else_s, loc),
                  ),
                )
              }),
            ),
          )
        }),
      )
    }
    ForUpdate(update, head) -> {
      use #(upd_tree, e) <- result.try(case update {
        Some(ex) -> e.dispatch.emit_expr(e, ex)
        None -> Ok(#(ir.Values([e.consts.undef]), e))
      })
      let #(tmp, e) = state.fresh_var(e)
      Ok(
        cps_pair(e, fn(e, k) {
          ir.Let(
            [tmp],
            upd_tree,
            pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
              k(e, sm_continue(ctx, head, loc))
            }),
          )
        }),
      )
    }
    ForOfStep(left, iter_key, body_s, after, is_await) ->
      emit_for_of_step(e, ctx, left, iter_key, body_s, after, is_await)
    ForOfSetup(right, iter_key, head) -> {
      use #(rhs_tree, e) <- result.try(e.dispatch.emit_expr(e, right))
      let iter_idx = extra_idx(ctx.layout, iter_key)
      let #(rhs_n, e) = state.fresh_var(e)
      let #(iter_n, e) = state.fresh_var(e)
      Ok(
        cps_pair(e, fn(e, k) {
          ir.Let(
            [rhs_n],
            rhs_tree,
            ir.Let(
              [iter_n],
              ir.CallHost("js", "get_iterator", [
                ir.Var(rhs_n),
                ir.ConstAtom("sync"),
              ]),
              pack_loc_cps(
                e,
                ctx,
                dict.from_list([#(iter_idx, ir.Var(iter_n))]),
                fn(e, loc) { k(e, sm_continue(ctx, head, loc)) },
              ),
            ),
          )
        }),
      )
    }
    ForAwaitSetup(right, head) -> {
      use #(rhs_tree, e) <- result.try(e.dispatch.emit_expr(e, right))
      let iter_idx = extra_idx(ctx.layout, for_await_iter_key(head))
      let #(rhs_n, e) = state.fresh_var(e)
      let #(iter_n, e) = state.fresh_var(e)
      Ok(
        cps_pair(e, fn(e, k) {
          ir.Let(
            [rhs_n],
            rhs_tree,
            ir.Let(
              [iter_n],
              ir.CallHost("js", "get_iterator", [
                ir.Var(rhs_n),
                ir.ConstAtom("async"),
              ]),
              pack_loc_cps(
                e,
                ctx,
                dict.from_list([#(iter_idx, ir.Var(iter_n))]),
                fn(e, loc) { k(e, sm_continue(ctx, head, loc)) },
              ),
            ),
          )
        }),
      )
    }
    AsyncGenYieldSent(ns) ->
      Ok(
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, step_yield(ctx.sent_v, ns, loc))
          })
        }),
      )
    SwitchDispatch(disc, tests, after) ->
      emit_switch_dispatch(e, ctx, disc, tests, after)
  }
}

/// Run a CPS builder capturing the final Emitter2 (pdict seam, re-entrant).
fn cps_pair(
  e: Emitter2,
  f: fn(Emitter2, fn(Emitter2, ir.Expr) -> ir.Expr) -> ir.Expr,
) -> #(ir.Expr, Emitter2) {
  let cell = make_ref()
  let _ = pdict_put(cell, e)
  let tree =
    f(e, fn(ef, tail) {
      let _ = pdict_put(cell, ef)
      tail
    })
  #(tree, pdict_erase(cell))
}

/// for-of/for-await step tail: `iter_h = loc[extras[iter_key]]`; `res =
/// host("iter_next",[iter_h])` (or sent_v for the awaited resume);
/// `done = truthy(get_prop(res,"done"))`; If(done, Continue→after,
/// bind `left := get_prop(res,"value")` then Continue→body_s).
fn emit_for_of_step(
  e: Emitter2,
  ctx: SmCtx,
  left: ast.ForInit,
  iter_key: String,
  body_s: Int,
  after: Int,
  is_await: Bool,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let iter_idx = extra_idx(ctx.layout, iter_key)
  run_rk(e, fn(e, done) {
    let #(iter_n, e) = state.fresh_var(e)
    let #(res_n, e) = state.fresh_var(e)
    // for-await: sent_v IS the awaited iter.next() result (§18.2 SkForAwait).
    let res_rhs = case is_await {
      True -> ir.Values([ctx.sent_v])
      False -> ir.CallHost("js", "iter_next", [ir.Var(iter_n)])
    }
    let #(done_t, e) = state.fresh_var(e)
    let #(done_i, e) = state.fresh_var(e)
    let #(val_n, e) = state.fresh_var(e)
    let #(dk_n, e) = state.fresh_var(e)
    let #(vk_n, e) = state.fresh_var(e)
    // DONE branch: pack loc with the PRE-bind emitter (loop var unchanged).
    let #(done_branch, e) =
      cps_pair(e, fn(e, k) {
        pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
          k(e, sm_continue(ctx, after, loc))
        })
      })
    // NOT-DONE branch: bind `left := value` FIRST (mutates slot_vars for the
    // loop var), pack loc INSIDE `bind_tree`'s Let so the packed slot references
    // the freshly-bound SSA name and is in-scope in the emitted IR.
    use #(bind_tree, e) <- result.try(bind_for_lhs(e, left, ir.Var(val_n)))
    let #(tmp, e) = state.fresh_var(e)
    let #(body_branch, e) =
      cps_pair(e, fn(e, k) {
        ir.Let(
          [val_n],
          ir.CallHost("js", "get_prop", [ir.Var(res_n), ir.Var(vk_n)]),
          ir.Let(
            [tmp],
            bind_tree,
            pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
              k(e, sm_continue(ctx, body_s, loc))
            }),
          ),
        )
      })
    let branch = ir.If(ir.Var(done_i), [ir.TTerm], done_branch, body_branch)
    done(
      e,
      ir.Let(
        [iter_n],
        ir.TermOp(ir.TupleGet(iter_idx), [ctx.loc_v]),
        ir.Let(
          [res_n],
          res_rhs,
          ir.Let(
            [dk_n],
            named_key_tuple("done"),
            ir.Let(
              [vk_n],
              named_key_tuple("value"),
              ir.Let(
                [done_t],
                ir.CallHost("js", "get_prop", [ir.Var(res_n), ir.Var(dk_n)]),
                ir.Let(
                  [done_i],
                  ir.CallHost("js", "truthy", [ir.Var(done_t)]),
                  branch,
                ),
              ),
            ),
          ),
        ),
      ),
    )
  })
}

/// Wire `{string_key,{named,<s>}}` — inline (matches anf.object_key_lit.s
/// output for KeyIdentifier) so ForOfStep needs no anf.Build threading.
/// Let-name derived from `s` so multiple calls in one scope never collide.
fn named_key_tuple(s: String) -> ir.Expr {
  let inner_n = "_nk_" <> s
  ir.Let(
    [inner_n],
    ir.TermOp(ir.MakeTuple, [
      ir.ConstAtom("named"),
      ir.ConstBinary(bit_array.from_string(s)),
    ]),
    ir.TermOp(ir.MakeTuple, [ir.ConstAtom("string_key"), ir.Var(inner_n)]),
  )
}

/// Bind a for-of/in LHS to `v` — mirrors stmt.gleam:1162-1200 shape locally
/// (D13). Destructuring/member LHS route through dispatch.emit_destructure.
fn bind_for_lhs(
  e: Emitter2,
  left: ast.ForInit,
  v: ir.Value,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let via = fn(pat, mode) { e.dispatch.emit_destructure(e, pat, v, mode) }
  case left {
    ast.ForInitDeclaration(kind:, declarations: [d]) ->
      via(d.id, bind_mode_of(kind))
    ast.ForInitDeclaration(..) ->
      Error(state.EarlySyntaxError("for-of/in: multiple declarators"))
    ast.ForInitPattern(p) -> via(p, state.BindAssign)
    ast.ForInitExpression(ast.Identifier(span:, name:)) ->
      via(ast.IdentifierPattern(name:, span:), state.BindAssign)
    ast.ForInitExpression(_) ->
      // Member-expression LHS — v1 gap (needs full lvalue path).
      Error(state.UnsupportedFeature("for-of member LHS in coroutine"))
  }
}

fn bind_mode_of(kind: ast.VariableKind) -> state.BindMode {
  case kind {
    ast.Var -> state.BindVar
    ast.Const -> state.BindConst
    ast.Let | ast.Using | ast.AwaitUsing -> state.BindLet
  }
}

/// switch dispatch tail: eval `disc`, then chain `If(truthy(strict_eq(disc,
/// testᵢ)), Continue→stateᵢ, …else…)`; a `None` test = `default:` (unconditional
/// jump); no default → unmatched falls to `after`.
fn emit_switch_dispatch(
  e: Emitter2,
  ctx: SmCtx,
  disc: Option(ast.Expression),
  tests: List(#(Option(ast.Expression), Int)),
  after: Int,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  use #(disc_tree, e) <- result.try(case disc {
    Some(d) -> e.dispatch.emit_expr(e, d)
    None -> Ok(#(ir.Values([ctx.sent_v]), e))
  })
  let #(dv_n, e) = state.fresh_var(e)
  let dv = ir.Var(dv_n)
  use #(chain, e) <- result.try(switch_chain(e, ctx, dv, tests, after))
  Ok(#(ir.Let([dv_n], disc_tree, chain), e))
}

fn switch_chain(
  e: Emitter2,
  ctx: SmCtx,
  dv: ir.Value,
  tests: List(#(Option(ast.Expression), Int)),
  after: Int,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  case tests {
    [] ->
      Ok(
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, after, loc))
          })
        }),
      )
    [#(None, target), ..] ->
      // default: — unconditional (§13.12.9: default only reached after all
      // non-default tests miss; split-analysis orders it last).
      Ok(
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, target, loc))
          })
        }),
      )
    [#(Some(case_test), target), ..rest] -> {
      use #(test_tree, e) <- result.try(e.dispatch.emit_expr(e, case_test))
      let #(tv_n, e) = state.fresh_var(e)
      let #(eq_n, e) = state.fresh_var(e)
      let #(eqi_n, e) = state.fresh_var(e)
      use #(else_tree, e) <- result.try(switch_chain(e, ctx, dv, rest, after))
      let #(hit, e) =
        cps_pair(e, fn(e, k) {
          pack_loc_cps(e, ctx, dict.new(), fn(e, loc) {
            k(e, sm_continue(ctx, target, loc))
          })
        })
      Ok(#(
        ir.Let(
          [tv_n],
          test_tree,
          ir.Let(
            [eq_n],
            ir.CallHost("js", "strict_eq", [dv, ir.Var(tv_n)]),
            ir.Let(
              [eqi_n],
              ir.CallHost("js", "truthy", [ir.Var(eq_n)]),
              ir.If(ir.Var(eqi_n), [ir.TTerm], hit, else_tree),
            ),
          ),
        ),
        e,
      ))
    }
  }
}

// ── hoist_splits_to_stmts: v1 "explode" pre-pass (u-fragment-emit) ──────────
// Rewrite a flat statement list so every await/yield sits at a statement
// boundary, yielding List(HoistedItem). split-analysis MAY use this to
// simplify its walk: partition at each HiSplit into ArmSpec.{body_fragment,
// tail:SplitAt, next-arm.entry}. v1 recognises the common statement-position
// shapes; deeper expression-position nesting (`if (await p)`, `f(await p)`)
// passes through as HiStmt — a documented gap (expr.gleam:318-342 emits the
// host("await",…) placeholder for those; use expr_has_split to detect).

/// One item in the hoisted stream.
pub type HoistedItem {
  /// A statement guaranteed split-free at TOP level (may still contain a
  /// nested split in expression position — v1 gap; use expr_has_split to
  /// detect). Emitted verbatim via dispatch.emit_stmts.
  HiStmt(ast.StmtWithLine)
  /// A split boundary. `operand` = the await/yield's argument (None for bare
  /// `yield`); `resume` = how the NEXT arm consumes sent_v.
  HiSplit(
    line: Int,
    kind: SplitKind,
    operand: Option(ast.Expression),
    resume: ResumeWith,
  )
}

/// Hoist every recognised await/yield in `stmts` to a HiSplit boundary.
pub fn hoist_splits_to_stmts(
  stmts: List(ast.StmtWithLine),
) -> List(HoistedItem) {
  list.flat_map(stmts, hoist_one)
}

fn hoist_one(located: ast.StmtWithLine) -> List(HoistedItem) {
  let ast.StmtWithLine(line, stmt) = located
  case stmt {
    ast.ExpressionStatement(expression: ex, ..) ->
      case split_of(ex) {
        Some(#(kind, operand)) -> [HiSplit(line, kind, operand, ResumeDiscard)]
        None ->
          case ex {
            // `a, await b, c` → three statements, re-hoisted.
            ast.SequenceExpression(_, parts) ->
              list.flat_map(parts, fn(p) {
                hoist_one(ast.StmtWithLine(
                  line,
                  ast.ExpressionStatement(p, None),
                ))
              })
            // `x = await p` (simple assignment RHS is the split).
            ast.AssignmentExpression(_, ast.Assign, lhs, rhs) ->
              case split_of(rhs), lhs_to_pattern(lhs) {
                Some(#(kind, operand)), Some(pat) -> [
                  HiSplit(
                    line,
                    kind,
                    operand,
                    ResumeBind(pat, state.BindAssign),
                  ),
                ]
                _, _ -> [HiStmt(located)]
              }
            _ -> [HiStmt(located)]
          }
      }
    // `let x = await p` (single declarator, split RHS).
    ast.VariableDeclaration(kind, [ast.VariableDeclarator(pat, Some(init))]) ->
      case split_of(init) {
        Some(#(skind, operand)) -> [
          HiSplit(line, skind, operand, ResumeBind(pat, bind_mode_of(kind))),
        ]
        None -> [HiStmt(located)]
      }
    // Multiple declarators → split into singletons, re-hoist each.
    ast.VariableDeclaration(kind, decls) ->
      list.flat_map(decls, fn(d) {
        hoist_one(ast.StmtWithLine(line, ast.VariableDeclaration(kind, [d])))
      })
    ast.ReturnStatement(Some(ex)) ->
      case split_of(ex) {
        Some(#(kind, operand)) -> [HiSplit(line, kind, operand, ResumeReturn)]
        None -> [HiStmt(located)]
      }
    ast.ThrowStatement(ex) ->
      case split_of(ex) {
        Some(#(kind, operand)) -> [HiSplit(line, kind, operand, ResumeThrow)]
        None -> [HiStmt(located)]
      }
    // Everything else passes through — split-analysis recurses into nested
    // stmt LISTS (BlockStatement/if/loop/try bodies) with a fresh hoist call.
    _ -> [HiStmt(located)]
  }
}

/// If `ex` IS an await/yield (optionally under parentheses), return its split
/// kind + operand. Not recursive — only top-level match.
fn split_of(
  ex: ast.Expression,
) -> Option(#(SplitKind, Option(ast.Expression))) {
  case ex {
    ast.AwaitExpression(_, arg) -> Some(#(SkAwait, Some(arg)))
    ast.YieldExpression(_, arg, is_delegate: False) -> Some(#(SkYield, arg))
    ast.YieldExpression(_, arg, is_delegate: True) -> Some(#(SkYieldStar, arg))
    ast.ParenthesizedExpression(_, inner) -> split_of(inner)
    _ -> None
  }
}

/// AssignmentExpression LHS → Pattern for ResumeBind. Only simple identifier
/// targets convert; MemberExpression / destructuring-as-expression are v1
/// gaps (return None → passes through as HiStmt).
fn lhs_to_pattern(lhs: ast.Expression) -> Option(ast.Pattern) {
  case lhs {
    ast.Identifier(span, name) -> Some(ast.IdentifierPattern(name:, span:))
    ast.ParenthesizedExpression(_, inner) -> lhs_to_pattern(inner)
    _ -> None
  }
}

// ── public entry (wired as EmitDispatch.emit_async_body) ────────────────────

fn find_try(entries: List(TryEntry), region: Option(Int)) -> Option(TryEntry) {
  case region {
    None -> None
    Some(id) -> list.find(entries, fn(t) { t.id == id }) |> option.from_result
  }
}

fn extra_idx(layout: LocLayout, key: String) -> Int {
  case dict.get(layout.extras, key) {
    Ok(i) -> i
    Error(_) -> panic as { "M18: loc-layout extras missing key " <> key }
  }
}

/// Step (6): build every ir.SwitchArm — fragment arms via emit_arm_body →
/// emit_mode_dispatch → wrap_arm_try; then catch/finally arms per TryEntry;
/// then yield*-delegate self-looping arms. SmCtx threads through (u-emit-ctx):
/// arms accumulate via `push_arm`; per-arm `try_stack` is derived via
/// `with_region`/`with_catch_body`/`with_finally_body` so `route_abrupt`
/// inside each fragment sees the correct enclosing regions (§18.5 nesting).
fn build_switch_arms(
  e: Emitter2,
  ctx: SmCtx,
  plan: SplitPlan,
) -> Result(#(List(ir.SwitchArm), Emitter2), state.EmitError) {
  // fragment arms
  use #(ctx, e) <- result.try(
    list.try_fold(plan.arms, #(ctx, e), fn(st, arm) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, arm.region)
      let region = current_try(ctx)
      use #(inner, e) <- result.map(emit_arm_body(e, ctx, arm))
      let dispatched = emit_mode_dispatch(ctx, arm.entry_kind, region, inner)
      let wrapped = wrap_arm_try(ctx, arm.state_id, region, dispatched)
      #(push_arm(ctx, arm.state_id, wrapped), e)
    }),
  )
  // catch/finally arms per split-bearing try — a throw in catch/finally routes
  // to the OUTER region; entered via Continue so no mode dispatch.
  use #(ctx, e) <- result.try(
    list.try_fold(plan.try_entries, #(ctx, e), fn(st, entry) {
      let #(ctx, e) = st
      let outer_region = find_try(plan.try_entries, entry.outer)
      use #(ctx, e) <- result.try(case entry.catch_state, entry.handler {
        Some(cs), Some(h) -> {
          let ctx =
            SmCtx(..with_catch_body(ctx, entry), sm_labels: entry.sm_labels)
          // A sync throw in the catch body must still hit THIS entry's
          // finalizer (if any) before propagating outward — with_catch_body
          // pushes the finally-only view; wrap against that, not entry.outer.
          let catch_wrap_region = current_try(ctx)
          let e = case entry.catch_cursor {
            Some(c) -> install_cursor(e, c)
            None -> e
          }
          use #(inner, e) <- result.map(emit_catch_arm(e, ctx, entry, h))
          let wrapped = wrap_arm_try(ctx, cs, catch_wrap_region, inner)
          #(push_arm(ctx, cs, wrapped), e)
        }
        _, _ -> Ok(#(ctx, e))
      })
      case entry.finally_state, entry.finalizer {
        Some(fs), Some(fin) -> {
          let ctx =
            SmCtx(..with_finally_body(ctx, entry), sm_labels: entry.sm_labels)
          let e = case entry.finally_cursor {
            Some(c) -> install_cursor(e, c)
            None -> e
          }
          use #(inner, e) <- result.map(emit_finally_arm(e, ctx, entry, fin))
          let wrapped = wrap_arm_try(ctx, fs, outer_region, inner)
          #(push_arm(ctx, fs, wrapped), e)
        }
        _, _ -> Ok(#(ctx, e))
      }
    }),
  )
  // yield* delegate arms — mode dispatch is IN the arm body (§18.6 forwards
  // mode/sent to the inner iterator), so no emit_mode_dispatch here.
  use #(ctx, e) <- result.try(
    list.try_fold(plan.delegates, #(ctx, e), fn(st, d) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, d.region)
      let region = current_try(ctx)
      let sid = int.to_string(d.state_id)
      use #(inner, e) <- result.map(emit_delegate_arm(
        e,
        ctx,
        d.state_id,
        d.next_state,
        extra_idx(ctx.layout, "iter_" <> sid),
        extra_idx(ctx.layout, "inner_" <> sid),
        extra_idx(ctx.layout, "delegate_result_" <> sid),
      ))
      let wrapped = wrap_arm_try(ctx, d.state_id, region, inner)
      #(push_arm(ctx, d.state_id, wrapped), e)
    }),
  )
  // for-await-of head+check arms (u-for-await). head is Continue-entered
  // (AeJump — no mode-dispatch); check is post-await resume (mode-dispatch).
  // body_s + after are emitted by the plan.arms fold above.
  use #(ctx, e) <- result.map(
    list.try_fold(plan.for_awaits, #(ctx, e), fn(st, fap) {
      let #(ctx, e) = st
      let ctx = with_region(ctx, fap.region)
      let region = current_try(ctx)
      let #(head_body, e) = emit_for_await_head(e, ctx, fap)
      let ctx =
        push_arm(ctx, fap.head, wrap_arm_try(ctx, fap.head, region, head_body))
      use #(check_body, e) <- result.map(emit_for_await_check(e, ctx, fap))
      let check_wrapped =
        wrap_arm_try(
          ctx,
          fap.check,
          region,
          emit_mode_dispatch(ctx, AeResume(SkAwait), region, check_body),
        )
      #(push_arm(ctx, fap.check, check_wrapped), e)
    }),
  )
  #(finish_arms(ctx), e)
}

/// EmitDispatch.emit_async_body entry. Compiles a coroutine body into an
/// outer `jsf_N` wrapper + `jsf_N__sm` state machine (both appended to
/// `e.fns_acc`) and returns the parent-frame closure-site Let-chain.
pub fn emit_coroutine_fn(
  e: Emitter2,
  kind: state.CoroutineKind,
  params: List(ast.Pattern),
  body: state.FnBody,
  fn_scope_id: ScopeId,
  captures: List(ir.Value),
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  // (1) child FunctionInfo + capture arity
  let info = scope.function_info(e.tree, fn_scope_id)
  let ncap = list.length(captures)

  // (2) allocate the sm name; outer name is minted inside emit_outer_function
  let #(sm_base, e) = state.fresh_fn_name(e)
  let sm_name = sm_base <> "__sm"

  // (3) enter the coroutine body's function scope for arm emission
  let #(e, save) =
    state.enter_function(
      e,
      fn_scope_id,
      strict: e.strict,
      is_async: kind_is_async(kind),
      is_generator: kind_is_gen(kind),
      is_arrow: False,
    )

  // (4) two analysis passes — split points, then stable loc-tuple layout
  let plan = analyze_splits(e.tree, fn_scope_id, body, kind)
  let layout = compute_loc_layout(info, plan)
  // analyze_splits leaves TryEntry.{pending,caught}_loc_idx as placeholder 0 —
  // fill them from layout.extras before ctx/arm-emit read them.
  let plan =
    SplitPlan(..plan, try_entries: enrich_try_entries(plan.try_entries, layout))

  // (5) resume-loop label + shared arm context (binder names match
  //     emit_sm_function's fixed Let/Loop names)
  let #(lresume, e) = state.fresh_label(e)
  let ctx = new_sm_ctx(kind, layout, lresume, plan)

  // (6) build every SwitchArm
  use #(arms, e) <- result.try(build_switch_arms(e, ctx, plan))

  // (7) assemble + register the sm ir.Function
  let #(default, e) = sm_default_arm(e)
  let e = emit_sm_function(e, sm_name, ncap, lresume, arms, default)

  // (8) restore parent frame
  let e = state.leave_function(e, save)

  // (9) outer wrapper ir.Function + parent-frame closure-site tree.
  //     js_name is not threaded through the dispatch seam; func.gleam applies
  //     NamedEvaluation on the returned handle when needed.
  emit_outer_function(
    e,
    kind,
    sm_name,
    fn_scope_id,
    params,
    captures,
    layout,
    None,
  )
}

// ── for-await-of + §18.7 async-gen yield emission (u-for-await) ─────────────
// SPEC §18.2 lists ForAwait-step as a split kind; §18.7 pins async-gen
// `yield x` = Await(x) then Yield (tc39/ecma262#2819) — TWO consecutive
// splits. Owned by async.gleam's own AST walk (NOT stmt.gleam:349 todo —
// for-await-of only appears inside coroutine bodies, which route here).
// Setup + §18.7 are realised as SegTail variants (ForAwaitSetup /
// AsyncGenYieldSent) so they flow through the plan.arms machinery; only the
// head/check arms are emitted here (build_switch_arms's for_awaits fold).

/// for-await HEAD arm body (state `fap.head`, entered via Continue only):
/// read iter handle from loc; call `async_iter_next` (§8 op —
/// Promise<IteratorResult>); pack loc unchanged; `step_await(p, check, loc')`.
fn emit_for_await_head(
  e: Emitter2,
  ctx: SmCtx,
  fap: ForAwaitSpec,
) -> #(ir.Expr, Emitter2) {
  let iter_idx = extra_idx(ctx.layout, for_await_iter_key(fap.head))
  let b = {
    use iter_h <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, iter_idx)))
    use p <- anf.then(anf.host("async_iter_next", [iter_h]))
    use loc2 <- anf.then(pack_loc(ctx, dict.new()))
    anf.pure(step_await(p, fap.check, loc2))
  }
  run_terminal(b, e)
}

/// for-await CHECK arm body (state `fap.check`, entered post-await with
/// `ctx.sent_v` = resolved IteratorResult): read `.done`/`.value`; if done →
/// close iterator (async, non-abrupt) and `Continue(after)`; else bind
/// `value` to `left` (via `bind_for_lhs` — handles ForInitExpression identifier
/// targets) then `Continue(body_s)`. The loop body itself is a plan.arms
/// ArmSpec so body-internal splits work.
fn emit_for_await_check(
  e: Emitter2,
  ctx: SmCtx,
  fap: ForAwaitSpec,
) -> Result(#(ir.Expr, Emitter2), state.EmitError) {
  let iter_idx = extra_idx(ctx.layout, for_await_iter_key(fap.head))
  run_rk(e, fn(e, done) {
    use e <- restore_and_seed(e, ctx)
    let e = install_cursor(e, fap.body_cursor)
    // done-branch: close iterator (non-abrupt) then jump to `after`.
    let #(done_branch, e) =
      anf.run_to(
        {
          use iter_h <- anf.then(anf.bind(anf.tuple_get(ctx.loc_v, iter_idx)))
          use _ <- anf.then(
            anf.host_unit("iter_close", [iter_h, ir.ConstAtom("false")]),
          )
          pack_loc(ctx, dict.new())
        },
        e,
        fn(_e, loc2) {
          ir.Continue(ctx.lresume, [ir.ConstI32(fap.after), loc2])
        },
      )
    // not-done: bind `left := value` then Continue→body_s (body emitted via
    // plan.arms; its tail loops FallTo(head)).
    let #(val_name, e) = state.fresh_var(e)
    use #(bind_tree, e) <- result.try(bind_for_lhs(
      e,
      fap.left,
      ir.Var(val_name),
    ))
    let #(drop, e) = state.fresh_var(e)
    let #(body_jump, e) = jump_state(e, ctx, fap.body_s, dict.new())
    let not_done = ir.Let([drop], bind_tree, body_jump)
    // Outer chain: sent_v is the iterresult; project done/value; branch.
    let #(chain, e) =
      run_terminal(
        {
          use dk <- anf.then(key_named("done"))
          use done_jv <- anf.then(anf.host("get_prop", [ctx.sent_v, dk]))
          use done_i <- anf.then(anf.host("truthy", [done_jv]))
          use vk <- anf.then(key_named("value"))
          use value <- anf.then(anf.host("get_prop", [ctx.sent_v, vk]))
          anf.pure(ir.Let(
            [val_name],
            ir.Values([value]),
            ir.If(done_i, [ir.TTerm], done_branch, not_done),
          ))
        },
        e,
      )
    done(e, chain)
  })
}
