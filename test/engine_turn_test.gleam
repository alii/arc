import arc/engine.{type Engine, ModuleThrew, Returned, Threw}
import arc/host.{Context}
import arc/module/loader
import arc/rt/async as rt_async
import arc/rt/gc as rt_gc
import arc/rt/types.{
  type Agent, type JsVal, Agent, KHandle, Store, classify, mk_int, mk_undefined,
}
import gleam/set
import gleam/string
import rt_helpers

const threshold = 256

fn small_engine() -> Engine(Nil) {
  let eng = engine.new() |> engine.with_host_hooks(rt_helpers.quiet_hooks())
  let #(Nil, eng) =
    engine.with_context(eng, fn(ctx) {
      let st = rt_gc.t_collect(ctx.agent, [])
      let st = Agent(..st, store: Store(..st.store, gc_threshold: threshold))
      #(Nil, Context(..ctx, agent: st))
    })
  eng
}

fn show(eng: Engine(host), v: JsVal) -> String {
  engine.inspect(eng, v)
}

fn global(eng: Engine(host), name: String) -> JsVal {
  let #(v, _) = rt_helpers.global(engine.agent(eng), name)
  v
}

fn stats(eng: Engine(host)) -> rt_gc.GcStats {
  rt_gc.stats(engine.agent(eng))
}

fn is_live_and_unpinned(eng: Engine(host), v: JsVal) -> Bool {
  let st: Agent = engine.agent(eng)
  let assert KHandle(h) = classify(v)
  rt_gc.t_is_live(st, h) && !set.contains(st.store.pinned_roots, h.id)
}

pub fn eval_is_bounded_drains_and_keeps_its_value_test() {
  let eng = small_engine()
  let base = stats(eng).live_count
  let source =
    "
    function alloc(i) { return { a: [i, i + 1], s: 'x' + i, o: { i: i } }; }
    let acc = 0;
    for (let i = 0; i < 4000; i++) { acc = acc + alloc(i).a[0]; }
    var p = Promise.resolve(0);
    for (let i = 0; i < 1000; i++) { p = p.then(v => { alloc(v); return v + 1; }); }
    p.then(v => { globalThis.done = v; });
    ({ tag: 'kept:' + acc, seen: typeof globalThis.done })
    "
  let assert Ok(#(Returned(v), eng)) = engine.eval(eng, source)
  assert is_live_and_unpinned(eng, v)
  let #(tag, _) = rt_helpers.get(engine.agent(eng), v, "tag")
  let #(seen, _) = rt_helpers.get(engine.agent(eng), v, "seen")
  assert show(eng, tag) == "'kept:7998000'"
  assert show(eng, seen) == "'undefined'"
  assert show(eng, global(eng, "done")) == "1000"
  // cells that die old wait for the next major
  assert stats(eng).live_count <= base + rt_gc.minors_per_major * threshold
}

pub fn eval_keeps_thrown_value_test() {
  let eng = small_engine()
  let source =
    "
    var p = Promise.resolve(0);
    for (let i = 0; i < 500; i++) { p = p.then(v => ({ v: [v, v] })); }
    p.then(v => { globalThis.ran = 'yes'; });
    throw new RangeError('kept');
    "
  let assert Ok(#(Threw(e), eng)) = engine.eval(eng, source)
  assert show(eng, global(eng, "ran")) == "'yes'"
  assert is_live_and_unpinned(eng, e)
  assert string.contains(engine.format_error(eng, e), "RangeError: kept")
}

pub fn call_is_bounded_drains_and_keeps_its_value_test() {
  let eng = small_engine()
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
  let assert Ok(#(Returned(_), eng)) = engine.eval(eng, source)
  let work = global(eng, "work")
  let base = stats(eng).live_count
  let eng = call_many(eng, work, 20)
  assert show(eng, global(eng, "runs")) == "20"
  assert stats(eng).live_count <= base + 4 * threshold
}

fn call_many(eng: Engine(host), work: JsVal, left: Int) -> Engine(host) {
  case left {
    0 -> eng
    _ -> {
      let n = 300 + left
      let #(outcome, eng) = engine.call(eng, work, mk_undefined(), [mk_int(n)])
      let assert Returned(v) = outcome
      assert is_live_and_unpinned(eng, v)
      let #(last, _) = rt_helpers.get(engine.agent(eng), v, "last")
      assert show(eng, last) == "'v" <> string.inspect(n - 1) <> "'"
      call_many(eng, work, left - 1)
    }
  }
}

pub fn call_folds_throws_test() {
  let eng = small_engine()
  let assert Ok(#(Returned(_), eng)) =
    engine.eval(
      eng,
      "function boom() { throw new TypeError('kept ' + [1,2,3].join('')); }",
    )
  let #(outcome, eng) =
    engine.call(eng, global(eng, "boom"), mk_undefined(), [])
  let assert Threw(e) = outcome
  assert is_live_and_unpinned(eng, e)
  assert string.contains(engine.format_error(eng, e), "TypeError: kept 123")
  let #(outcome, eng) = engine.call(eng, mk_int(3), mk_undefined(), [])
  let assert Threw(e) = outcome
  assert string.contains(engine.format_error(eng, e), "TypeError")
}

pub fn eval_module_keeps_thrown_value_test() {
  let eng = small_engine()
  let source =
    "
    var p = Promise.resolve(0);
    for (let i = 0; i < 500; i++) { p = p.then(v => ({ v: [v, v, v] })); }
    p.then(v => { globalThis.ran = 'yes'; });
    export const x = 1;
    throw new SyntaxError('kept ' + x);
    "
  let assert Ok(#(ModuleThrew(e), eng)) =
    engine.eval_module(
      eng,
      "/main.js",
      source,
      loader.forbid_resolve,
      loader.forbid_load,
    )
  assert show(eng, global(eng, "ran")) == "'yes'"
  assert is_live_and_unpinned(eng, e)
  assert string.contains(engine.format_error(eng, e), "SyntaxError: kept 1")
}

pub fn drain_runs_once_per_turn_test() {
  let eng = small_engine()
  let recording = fn(st) {
    rt_helpers.record("drain")
    rt_async.drain(st)
  }
  let assert Ok(#(Returned(_), eng)) =
    engine.eval_with(
      eng,
      "Promise.resolve(7).then(v => { globalThis.hit = v; })",
      recording,
    )
  assert rt_helpers.recorded() == ["drain"]
  assert show(eng, global(eng, "hit")) == "7"
  let #(_, eng) =
    engine.call_with(eng, global(eng, "Object"), mk_undefined(), [], recording)
  assert rt_helpers.recorded() == ["drain"]
  let assert Ok(#(_, eng)) =
    engine.eval_module_with(
      eng,
      "/m.js",
      "export const y = await Promise.resolve(2);",
      loader.forbid_resolve,
      loader.forbid_load,
      recording,
    )
  assert rt_helpers.recorded() == ["drain"]
  let #(Nil, _) =
    engine.with_context_with(eng, fn(ctx) { #(Nil, ctx) }, recording)
  assert rt_helpers.recorded() == ["drain"]
}
