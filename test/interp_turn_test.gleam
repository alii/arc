//// End to end through the engine turn entry points (`entry.script_turn` /
//// `entry.call_turn`): the top-level run, then the epilogue that collects
//// and drains with the completion value rooted. Under a tiny GC threshold
//// the drain collects between jobs many times over; the heap stays bounded,
//// the microtasks all run, and the value handed back is still live.

import arc/compiler
import arc/interp/entry
import arc/module
import arc/module_host
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion}
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/types.{
  type Agent, type JsVal, Agent, JInt, JsStore, KHandle, PromiseFulfilled,
  classify, mk_number, mk_undefined,
}
import gleam/option.{Some}
import gleam/set
import gleam/string
import rt_helpers

const threshold = 256

fn small_agent() -> Agent {
  let st = rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
  let st = rt_gc.t_collect(st, [])
  Agent(..st, store: JsStore(..st.store, gc_threshold: threshold))
}

fn script(st: Agent, source: String, finish: entry.Finish) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
  let assert Ok(template) = compiler.compile(body, sb)
  entry.script_turn(st, template, finish)
}

fn show(st: Agent, v: JsVal) -> String {
  rt_inspect.inspect(st, v)
}

fn global(st: Agent, name: String) -> String {
  let #(v, st) = rt_helpers.global(st, name)
  show(st, v)
}

fn is_live_and_unpinned(st: Agent, v: JsVal) -> Bool {
  let assert KHandle(h) = classify(v)
  rt_gc.t_is_live(st, h) && !set.contains(st.store.pinned_roots, h.id)
}

/// A long promise chain whose every job allocates: nothing has run when the
/// script body returns, everything has by the end of the turn, and the
/// object only the completion value holds survives the collecting drain.
pub fn script_turn_drains_and_keeps_completion_value_test() {
  let st = small_agent()
  let base = rt_gc.stats(st).live
  let source =
    "
    function alloc(i) { return { a: [i, i + 1], s: 'x' + i, o: { i: i } }; }
    var p = Promise.resolve(0);
    for (let i = 0; i < 2000; i++) { p = p.then(v => { alloc(v); return v + 1; }); }
    p.then(v => { globalThis.done = v; });
    ({ tag: 'kept', seen: typeof globalThis.done })
    "
  let #(completion, st) = script(st, source, rt_async.drain)
  let assert NormalCompletion(v) = completion
  assert is_live_and_unpinned(st, v)
  let #(tag, st) = rt_helpers.get(st, v, "tag")
  let #(seen, st) = rt_helpers.get(st, v, "seen")
  assert show(st, tag) == "'kept'"
  assert show(st, seen) == "'undefined'"
  assert global(st, "done") == "2000"
  // Tens of thresholds' worth was minted over the turn; about one
  // threshold's worth of garbage is all that is left of it.
  let stats = rt_gc.stats(st)
  assert stats.next >= base + 40 * threshold
  assert stats.live <= base + 4 * threshold
}

/// A thrown completion is rooted through the drain just the same.
pub fn script_turn_keeps_thrown_value_test() {
  let st = small_agent()
  let source =
    "
    var p = Promise.resolve(0);
    for (let i = 0; i < 500; i++) { p = p.then(v => ({ v: [v, v] })); }
    throw new RangeError('kept');
    "
  let #(completion, st) = script(st, source, rt_async.drain)
  let assert ThrowCompletion(e) = completion
  assert is_live_and_unpinned(st, e)
  assert string.contains(rt_inspect.format_error(st, e), "RangeError: kept")
}

/// The embedder's driver runs exactly once per turn, after the body, and
/// what it drains is what the body queued.
pub fn script_turn_runs_the_given_driver_once_test() {
  let st = small_agent()
  let source = "Promise.resolve(7).then(v => { globalThis.hit = v; }); 1"
  let #(completion, st) =
    script(st, source, fn(st) {
      rt_helpers.record(global(st, "hit"))
      let st = rt_async.drain(st)
      rt_helpers.record(global(st, "hit"))
      st
    })
  let assert NormalCompletion(_) = completion
  assert rt_helpers.recorded() == ["undefined", "7"]
  assert global(st, "hit") == "7"
}

/// `call_turn` on a held bytecode function: the call allocates past the
/// threshold and queues allocating jobs; its fresh return value survives the
/// epilogue, the jobs run, and the heap stays bounded across many turns.
pub fn call_turn_drains_and_keeps_return_value_test() {
  let st = small_agent()
  let source =
    "
    globalThis.runs = 0;
    function work(n) {
      let acc = [];
      for (let i = 0; i < n; i++) { acc.push({ i: i, s: 'v' + i }); }
      Promise.resolve(n).then(v => { for (let i = 0; i < v; i++) ({ j: [i] }); globalThis.runs++; });
      return { n: n, last: acc[n - 1].s };
    }
    "
  let #(_, st) = script(st, source, rt_async.drain)
  let #(work, st) = rt_helpers.global(st, "work")
  let base = rt_gc.stats(st).live
  let st = call_many(st, work, 20)
  assert global(st, "runs") == "20"
  assert rt_gc.stats(st).live <= base + 4 * threshold
}

fn call_many(st: Agent, work: JsVal, left: Int) -> Agent {
  case left {
    0 -> st
    _ -> {
      let n = 300 + left
      let #(completion, st) =
        entry.call_turn(
          st,
          work,
          mk_undefined(),
          [mk_number(JInt(n))],
          rt_async.drain,
        )
      let assert NormalCompletion(v) = completion
      assert is_live_and_unpinned(st, v)
      let #(last, st) = rt_helpers.get(st, v, "last")
      assert show(st, last) == "'v" <> string.inspect(n - 1) <> "'"
      call_many(st, work, left - 1)
    }
  }
}

/// A throwing callee and a non-callable both come back as throw
/// completions of a live error, never a raise past the engine.
pub fn call_turn_folds_throws_test() {
  let st = small_agent()
  let #(_, st) =
    script(
      st,
      "function boom() { throw new TypeError('kept ' + [1,2,3].join('')); }",
      rt_async.drain,
    )
  let #(boom, st) = rt_helpers.global(st, "boom")
  let #(completion, st) =
    entry.call_turn(st, boom, mk_undefined(), [], rt_async.drain)
  let assert ThrowCompletion(e) = completion
  assert is_live_and_unpinned(st, e)
  assert string.contains(rt_inspect.format_error(st, e), "TypeError: kept 123")
  let #(completion, st) =
    entry.call_turn(st, mk_number(JInt(3)), mk_undefined(), [], rt_async.drain)
  let assert ThrowCompletion(e) = completion
  assert string.contains(rt_inspect.format_error(st, e), "TypeError")
}

/// A module body's epilogue is the same turn end: the error a body throws
/// after queuing allocating jobs is held only in Gleam while the drain
/// collects, and comes back live.
pub fn module_body_epilogue_keeps_thrown_value_test() {
  let st = small_agent()
  let source =
    "
    var p = Promise.resolve(0);
    for (let i = 0; i < 500; i++) { p = p.then(v => ({ v: [v, v, v] })); }
    export const x = 1;
    throw new SyntaxError('kept ' + x);
    "
  let assert Ok(bundle) =
    module.compile_bundle(
      "/main.js",
      source,
      module_host.forbid_resolve,
      module_host.forbid_load,
    )
  let base = rt_gc.stats(st).next
  let assert #(st, Error(module.EvaluationError(value: e))) =
    module.evaluate_bundle(bundle, st, rt_async.drain)
  // The drain ran (and so collected several times over) before we got here.
  assert rt_gc.stats(st).next >= base + 4 * threshold
  assert is_live_and_unpinned(st, e)
  assert string.contains(rt_inspect.format_error(st, e), "SyntaxError: kept 1")
}

/// A native callee goes through the same turn: `Promise.resolve` called as
/// a held value returns a promise the drain then leaves fulfilled.
pub fn call_turn_accepts_native_callees_test() {
  let st = small_agent()
  let #(promise_ctor, st) = rt_helpers.global(st, "Promise")
  let #(resolve, st) = rt_helpers.get(st, promise_ctor, "resolve")
  let #(completion, st) =
    entry.call_turn(
      st,
      resolve,
      promise_ctor,
      [mk_number(JInt(9))],
      rt_async.drain,
    )
  let assert NormalCompletion(p) = completion
  let assert Some(promise) = rt_async.as_promise(st, p)
  let assert #(_, PromiseFulfilled(v), _) = rt_async.promise_data(st, promise)
  assert show(st, v) == "9"
}
