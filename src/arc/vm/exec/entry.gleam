// ============================================================================
// Public API for the VM — run scripts, modules, REPL sessions
// ============================================================================

import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/promise as builtins_promise
import arc/vm/completion.{
  type Completion, Completed, NormalCompletion, Suspended, ThrowCompletion,
}
import arc/vm/exec/event_loop
import arc/vm/exec/generators
import arc/vm/exec/interpreter
import arc/vm/exec/promises
import arc/vm/heap
import arc/vm/internal/job_queue
import arc/vm/internal/tuple_array
import arc/vm/realm
import arc/vm/host_hooks
import arc/vm/state.{type Heap, type State, type VmError, RealmCtx, State}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, AsyncFunctionSlot, JsObject,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

// ============================================================================
// Public types
// ============================================================================

/// Result of module evaluation -- includes locals for export extraction.
/// `jobs` carries any promise jobs still queued when the supplied `finish`
/// driver returned — empty for draining drivers (`event_loop.finish`), the
/// leftover microtasks for a non-draining driver (dynamic import evaluates
/// modules inside an already-running event loop and must hand jobs back to
/// the host queue instead of draining them nested, where host continuations
/// would run against the wrong lexical context).
pub type ModuleResult(host) {
  ModuleOk(
    value: JsValue,
    heap: Heap(host),
    locals: tuple_array.TupleArray(JsValue),
    jobs: List(value.Job),
  )
  ModuleThrow(value: JsValue, heap: Heap(host), jobs: List(value.Job))
  ModuleError(error: VmError)
  /// The module body is parked on top-level await and the supplied `finish`
  /// driver did not settle it. `promise_data_ref` is the module's
  /// [[TopLevelCapability]] promise data — Evaluate() step 4: a re-import
  /// must chain onto this same promise rather than re-run the body.
  ModulePending(promise_data_ref: Ref, heap: Heap(host), jobs: List(value.Job))
}

/// Persistent REPL environment carried between evaluations.
pub type ReplEnv {
  ReplEnv(
    global_object: Ref,
    lexical_globals: dict.Dict(String, value.LexicalGlobal),
    symbol_registry: dict.Dict(String, value.SymbolId),
    /// Realm builtins registry, keyed by RealmSlot ref.
    /// Persisted so $262.evalScript/createRealm work across REPL evaluations.
    realms: dict.Dict(Ref, Builtins),
  )
}

/// Fresh REPL environment with empty registries.
pub fn new_repl_env(global_object: Ref) -> ReplEnv {
  ReplEnv(
    global_object:,
    lexical_globals: dict.new(),
    symbol_registry: dict.new(),
    realms: dict.new(),
  )
}

// ============================================================================
// Public functions
// ============================================================================

/// Run a function template with a globalThis object, then drain microtasks.
/// No macrotask loop — for that, pass a driver to `run_with`.
/// Boots with `host_hooks.default_host_hooks()` (no host capabilities); embedders
/// that need them use `run_with_hooks`.
///
/// The settled outcome is `Ok(value)` for a normal top-level return and
/// `Error(thrown)` for an uncaught throw — the only two ways a top-level run
/// can end (see `settle`) — paired with the drained heap.
pub fn run(
  func: FuncTemplate,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
) -> Result(#(Result(JsValue, JsValue), Heap(host)), VmError) {
  run_with(func, heap, builtins, global_object, event_loop.finish)
}

/// Like `run` but the caller supplies the post-script driver. `finish`
/// receives the State after the top-level script returns and is expected
/// to drain microtasks plus whatever macrotask loop the embedder owns
/// (e.g. `arc/beam.run`). Core's `event_loop.finish` is the no-macrotask
/// default. Boots with `host_hooks.default_host_hooks()`.
pub fn run_with(
  func: FuncTemplate,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  finish: fn(State(host)) -> State(host),
) -> Result(#(Result(JsValue, JsValue), Heap(host)), VmError) {
  run_with_hooks(
    func,
    heap,
    builtins,
    global_object,
    host_hooks.default_host_hooks(),
    finish,
  )
}

/// `run_with` with the embedder's `host_hooks.HostHooks` (host capabilities such
/// as the Atomics blocking wait / wake delivery). The hooks are supplied
/// once, at State construction, and carried on the realm context
/// (`RealmCtx.host_hooks`), so every derived State — child realms, eval and
/// Function realms, module bodies — inherits them; there is no per-call-site
/// install step to forget. `run`/`run_with` pass `host_hooks.default_host_hooks()`
/// (no capabilities — a blocking `Atomics.wait` then throws per DoWait).
pub fn run_with_hooks(
  func: FuncTemplate,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  host_hooks: host_hooks.HostHooks,
  finish: fn(State(host)) -> State(host),
) -> Result(#(Result(JsValue, JsValue), Heap(host)), VmError) {
  let executed =
    interpreter.init_state(
      func,
      heap,
      builtins,
      global_object,
      False,
      host_hooks,
    )
    |> interpreter.execute_to_completion("run_with_hooks")
  use #(settled, drained) <- result.map(settle(executed, finish))
  #(settled, drained.heap)
}

/// Run a module template with its binding cells seeded into local slots.
/// `seeds` are (slot index, BoxSlot ref) pairs: import bindings in capture
/// slots 0..N-1 (each the exporting module's live cell) plus this module's own
/// export cells in their declared slots — ES live bindings (§16.2). Reads/writes
/// go through GetBoxed/PutBoxed. Module `this` is undefined per ES §16.2.1.5.2.
///
/// `host_hooks` are the embedder's host capabilities (Atomics blocking
/// wait / wake delivery), supplied at State construction and carried on the
/// realm context. Each module body boots a fresh State, so the hooks must be
/// threaded here rather than installed after the fact; pass
/// `host_hooks.default_host_hooks()` for none.
pub fn run_module(
  func: FuncTemplate,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  seeds: List(#(Int, JsValue)),
  host_hooks: host_hooks.HostHooks,
  finish: fn(State(host)) -> State(host),
) -> ModuleResult(host) {
  let locals = interpreter.init_module_locals(func, seeds)
  let state =
    interpreter.new_state(
      func,
      locals,
      heap,
      builtins,
      global_object,
      dict.new(),
      dict.new(),
      host_hooks,
    )
  case interpreter.execute_inner(state) {
    Error(vm_err) -> ModuleError(error: vm_err)
    // A module body is the ONE non-function frame that can legitimately
    // suspend: top-level await (§16.2.1.5.3).
    Ok(#(Suspended(completion.Await, awaited_value), suspended)) ->
      drive_top_level_await(func, awaited_value, suspended, finish)
    Ok(#(Completed(NormalCompletion(val)), final_state)) -> {
      let drained = finish(final_state)
      ModuleOk(
        value: val,
        heap: drained.heap,
        locals: drained.locals,
        jobs: remaining_jobs(drained),
      )
    }
    Ok(#(Completed(ThrowCompletion(val)), final_state)) -> {
      let drained = finish(final_state)
      ModuleThrow(value: val, heap: drained.heap, jobs: remaining_jobs(drained))
    }
    // `yield` cannot occur outside a generator body — a module top level
    // yielding is an engine bug, reported on the VmError channel.
    Ok(#(Suspended(completion.Yield, _), _)) ->
      ModuleError(error: state.SuspensionLeak(
        site: "run_module",
        kind: completion.Yield,
      ))
  }
}

/// Drive a module body suspended on top-level await to completion — the spec's
/// ExecuteAsyncModule (§16.2.1.5.3): the module body behaves like an async
/// function whose result lands in a fresh promise capability (cf. QuickJS,
/// where a TLA module evaluates to a promise the host drains jobs against).
/// The suspension is parked in an AsyncFunctionSlot and the awaited value is
/// hooked with the same resume machinery async functions use, so settling the
/// awaited promise resumes the body (re-suspending on each further await)
/// until it completes and settles the capability. After draining, the
/// capability tells us how evaluation ended.
fn drive_top_level_await(
  func: FuncTemplate,
  awaited_value: JsValue,
  suspended: State(host),
  finish: fn(State(host)) -> State(host),
) -> ModuleResult(host) {
  let #(h, promise_ref, data_ref) =
    builtins_promise.create_promise(
      suspended.heap,
      suspended.builtins.promise.prototype,
    )
  // The host always inspects this capability below — mark it handled so a
  // rejection isn't also reported as an unhandled promise rejection.
  let h = builtins_promise.mark_handled(h, data_ref)
  let #(h, resolve, reject) =
    builtins_promise.create_resolving_functions(
      h,
      suspended.builtins.function.prototype,
      promise_ref,
      data_ref,
    )
  let #(h, async_data_ref) =
    heap.alloc(
      h,
      AsyncFunctionSlot(
        promise_data_ref: data_ref,
        resolve:,
        reject:,
        func_template: func,
        saved_pc: suspended.pc,
        saved_locals: suspended.locals,
        saved_stack: suspended.stack,
        saved_try_stack: generators.save_stacks(suspended.try_stack),
      ),
    )
  let wait_state = {
    use is_reject <- promises.setup_await(
      State(..suspended, heap: h, stack: [], call_stack: [], try_stack: []),
      awaited_value,
    )
    value.AsyncResume(async_data_ref:, is_reject:)
  }
  let drained = finish(wait_state)
  case heap.read_promise_state(drained.heap, data_ref) {
    Some(value.PromiseFulfilled(val)) ->
      ModuleOk(
        value: val,
        heap: drained.heap,
        locals: drained.locals,
        jobs: remaining_jobs(drained),
      )
    Some(value.PromiseRejected(reason)) ->
      ModuleThrow(
        value: reason,
        heap: drained.heap,
        jobs: remaining_jobs(drained),
      )
    // Still pending after `finish` returned. For a draining driver this
    // means an awaited promise can never settle; for a non-draining driver
    // (dynamic import) the module is legitimately mid-flight. Either way the
    // module must NOT be marked evaluated (Evaluate()/ExecuteAsyncModule —
    // importers would run against uninitialized export cells); the caller
    // decides whether pending is an error (static entry points) or a promise
    // to chain onto (ContinueDynamicImport).
    Some(value.PromisePending) ->
      ModulePending(
        promise_data_ref: data_ref,
        heap: drained.heap,
        jobs: remaining_jobs(drained),
      )
    // The capability slot we allocated above is gone — internal invariant
    // breach, never a successful evaluation. Report it on the VmError
    // channel (like every other engine bug), not as a guest throw.
    None ->
      ModuleError(error: state.InternalError(
        "drive_top_level_await",
        "promise capability slot missing",
      ))
  }
}

/// Jobs still queued on a state after its `finish` driver ran — what a
/// non-draining driver leaves behind for the host's own event loop.
fn remaining_jobs(state: State(host)) -> List(value.Job) {
  do_remaining_jobs(state.job_queue, [])
}

fn do_remaining_jobs(
  queue: job_queue.JobQueue(value.Job),
  acc: List(value.Job),
) -> List(value.Job) {
  case job_queue.pop(queue) {
    None -> list.reverse(acc)
    Some(#(job, rest)) -> do_remaining_jobs(rest, [job, ..acc])
  }
}

/// Like run, but persists globals across calls.
/// Used by the REPL so var declarations and function definitions survive.
/// Boots with `host_hooks.default_host_hooks()`; embedders that need host
/// capabilities use `run_and_drain_repl_with`.
///
/// Hands back the settled outcome (`Ok(value)` / `Error(thrown)`), the drained
/// heap, and the persistent REPL environment for the next evaluation.
pub fn run_and_drain_repl(
  func: FuncTemplate,
  heap: Heap(host),
  builtins: Builtins,
  env: ReplEnv,
) -> Result(#(Result(JsValue, JsValue), Heap(host), ReplEnv), VmError) {
  run_and_drain_repl_with(
    func,
    heap,
    builtins,
    env,
    host_hooks.default_host_hooks(),
    event_loop.drain_jobs,
  )
}

/// `run_and_drain_repl` with embedder hooks: `host_hooks` carries the host
/// capabilities (Atomics blocking wait / wake delivery) into the freshly
/// booted State's realm context — supplied once at construction and
/// inherited by every derived State (child realms, eval realms, module
/// bodies) — and `finish` is the post-script driver (e.g. a
/// notify-consuming embedder loop like `beam.settle_pending_wakes` instead
/// of the default microtask drain). Used by the test262 harness, whose
/// per-test worker processes drive the event loop directly.
pub fn run_and_drain_repl_with(
  func: FuncTemplate,
  heap: Heap(host),
  builtins: Builtins,
  env: ReplEnv,
  host_hooks: host_hooks.HostHooks,
  finish: fn(State(host)) -> State(host),
) -> Result(#(Result(JsValue, JsValue), Heap(host), ReplEnv), VmError) {
  // §16.1.6 ScriptEvaluation sets envs to globalEnv; script `this` resolves via §9.1.1.4.11 GetThisBinding to [[GlobalThisValue]].
  let this_val = JsObject(env.global_object)
  let locals = interpreter.init_top_level_locals(func, this_val)
  let base =
    interpreter.new_state(
      func,
      locals,
      heap,
      builtins,
      env.global_object,
      env.lexical_globals,
      env.symbol_registry,
      host_hooks,
    )
  let run_state = State(..base, ctx: RealmCtx(..base.ctx, realms: env.realms))
  use #(settled, drained) <- result.map(settle(
    interpreter.execute_to_completion(run_state, "run_and_drain_repl"),
    finish,
  ))
  let new_env =
    ReplEnv(
      global_object: drained.ctx.global_object,
      lexical_globals: drained.ctx.lexical_globals,
      symbol_registry: drained.ctx.symbol_registry,
      realms: drained.ctx.realms,
    )
  #(settled, shrink_for_handoff(settled, drained), new_env)
}

/// Heaps above this many live slots get a mark-and-sweep before being handed
/// back to the caller. An allocation-heavy script (e.g. test262's
/// dst-offset-caching family allocates ~2.4M short-lived Dates) otherwise
/// returns a heap dict of millions of dead slots; the test runner copies the
/// result as a message into a worker capped at 10M words (max_heap_size),
/// and the copy alone kills the worker. Typical scripts stay far below this
/// threshold and skip the sweep entirely.
const handoff_gc_min_slots = 65_536

/// GC the finished script's heap down to what the caller can still reach —
/// the exact set `handoff_roots` enumerates (settled value, global object,
/// lexical globals, realms, template objects, host hooks, leftover
/// jobs/waiters/rejections, eval env, top-level locals and stack). Only used
/// after execution has fully settled (empty stack/call stack), so there are
/// no hidden VM roots. No-op below `handoff_gc_min_slots`.
fn shrink_for_handoff(
  settled: Result(JsValue, JsValue),
  state: State(host),
) -> Heap(host) {
  case heap.size(state.heap) >= handoff_gc_min_slots {
    False -> state.heap
    True -> heap.compact(state.heap, handoff_roots(settled, state))
  }
}

/// Every slot id the caller of a settled run can still reach from outside
/// the heap. The persistent root set (builtins, global, realm slots) is
/// added by `heap.compact` itself. ENGINE state hanging off `ctx.host_hooks`
/// (the dynamic-import hook's function object) comes from
/// `state.host_hook_roots`, which is exhaustive over the hook record so a new
/// ref-carrying hook cannot silently miss this set.
fn handoff_roots(
  settled: Result(JsValue, JsValue),
  state: State(host),
) -> set.Set(Int) {
  let acc =
    set.new()
    |> set.insert(state.ctx.global_object.id)
    |> add_value_root(case settled {
      Ok(v) -> v
      Error(thrown) -> thrown
    })
  let acc =
    dict.fold(state.ctx.lexical_globals, acc, fn(a, _name, g) {
      add_value_root(a, value.lexical_global_value(g))
    })
  let acc =
    dict.fold(state.ctx.template_objects, acc, fn(a, _site, ref: Ref) {
      set.insert(a, ref.id)
    })
  let acc =
    dict.fold(state.ctx.realms, acc, fn(a, ref: Ref, _b) {
      set.insert(a, ref.id)
    })
  let acc =
    list.fold(state.host_hook_roots(state.ctx.host_hooks), acc, add_value_root)
  let acc =
    list.fold(state.unhandled_rejections, acc, fn(a, ref: Ref) {
      set.insert(a, ref.id)
    })
  let acc = list.fold(remaining_jobs(state), acc, add_job_roots)
  let acc =
    list.fold(state.atomics_waiters, acc, fn(a, w: value.AtomicsWaiter) {
      a
      |> set.insert(w.buffer.id)
      |> set.insert(w.promise_data.id)
      |> set.insert(w.promise.id)
    })
  let acc = case state.eval_env {
    Some(ref) -> set.insert(acc, ref.id)
    None -> acc
  }
  // Top-level locals (`this`, function declarations' cells) and any values
  // left on the operand stack. The call stack is empty once settled.
  let acc = list.fold(tuple_array.to_list(state.locals), acc, add_value_root)
  list.fold(state.stack, acc, add_value_root)
}

fn add_value_root(acc: set.Set(Int), val: JsValue) -> set.Set(Int) {
  case val {
    JsObject(value.Ref(id)) -> set.insert(acc, id)
    _ -> acc
  }
}

fn add_job_roots(acc: set.Set(Int), job: value.Job) -> set.Set(Int) {
  case job {
    value.PromiseReactionJob(handler:, arg:, resolve:, reject:) -> {
      let acc = case handler {
        value.Handler(fun:) -> add_value_root(acc, fun)
        value.IdentityPassThrough | value.ThrowerPassThrough -> acc
      }
      acc
      |> add_value_root(arg)
      |> add_value_root(resolve)
      |> add_value_root(reject)
    }
    value.PromiseResolveThenableJob(thenable:, then_fn:, resolve:, reject:) ->
      acc
      |> add_value_root(thenable)
      |> add_value_root(then_fn)
      |> add_value_root(resolve)
      |> add_value_root(reject)
    value.HostJob(run:) -> add_value_root(acc, run)
  }
}

/// Call a function value with `this` and `args`, then run the `finish` driver
/// to drain. The counterpart to `run`/`run_with` for a value you already hold —
/// e.g. a `receive` export read off a module namespace — letting an embedder
/// invoke it without re-evaluating a script. The host-call-then-drain pattern
/// (cf. Node's MakeCallback, QuickJS `JS_Call` + `JS_ExecutePendingJob`).
///
/// Built on the lossless `interpreter.call_root`, so it shares its shape with
/// `run`/`run_with`: the settled outcome is `Ok(value)` for a normal return
/// and `Error(thrown)` for an uncaught throw, while an engine `VmError`
/// surfaces as the outer `Error` (not a panic — the embedder is outside the VM
/// and can handle it). Draining happens once, at this outermost call.
///
/// `host_hooks` carries the embedder's host capabilities into the fresh root
/// State (pass `host_hooks.default_host_hooks()` for none).
pub fn run_export(
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  host_hooks: host_hooks.HostHooks,
  finish: fn(State(host)) -> State(host),
) -> Result(#(Result(JsValue, JsValue), Heap(host)), VmError) {
  use #(settled, drained) <- result.map(settle(
    interpreter.call_root(
      callee,
      this_val,
      args,
      heap,
      builtins,
      global_object,
      host_hooks,
    ),
    finish,
  ))
  #(settled, drained.heap)
}

/// Drain microtasks/macrotasks via `finish`, then narrow a top-level completion
/// to its settled outcome — `Ok(value)` for a normal completion,
/// `Error(thrown)` for a throw — paired with the drained State. A top-level
/// script or embedder call can only finish Normal or Throw: the `run*` family
/// drives the step loop through `interpreter.execute_to_completion`, which
/// turns a leaked Yield/Await into a `VmError` before it ever reaches here.
fn settle(
  executed: Result(#(Completion, State(host)), VmError),
  finish: fn(State(host)) -> State(host),
) -> Result(#(Result(JsValue, JsValue), State(host)), VmError) {
  use #(completion, final_state) <- result.try(executed)
  let drained = finish(final_state)
  case completion {
    NormalCompletion(val) -> Ok(#(Ok(val), drained))
    ThrowCompletion(val) -> Ok(#(Error(val), drained))
  }
}

/// How a promise JsValue has settled: `Some(Ok(value))` if fulfilled,
/// `Some(Error(reason))` if rejected, `None` if it is not a promise or is
/// still pending. The Ok/Error split matters — callers asserting on a
/// rejection must not silently accept a fulfillment with the same value.
pub fn promise_settlement(
  h: Heap(host),
  val: JsValue,
) -> Option(Result(JsValue, JsValue)) {
  use ref <- option.then(case val {
    JsObject(ref) -> Some(ref)
    _ -> None
  })
  use data_ref <- option.then(heap.read_promise_data_ref(h, ref))
  case heap.read_promise_state(h, data_ref) {
    Some(value.PromiseFulfilled(v)) -> Some(Ok(v))
    Some(value.PromiseRejected(r)) -> Some(Error(r))
    _ -> None
  }
}

/// Build a $262 object with evalScript, createRealm, gc methods and a global
/// property. Delegates to realm.build_262.
pub fn build_262(
  h: Heap(host),
  b: Builtins,
  global_ref: Ref,
  realm_ref: Ref,
) -> #(Heap(host), Ref) {
  realm.build_262(h, b, global_ref, realm_ref)
}
