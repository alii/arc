import arc/engine.{type Engine, Returned}
import arc/host.{Context}
import arc/rt/gc as rt_gc
import arc/rt/types.{Agent, Store}
import rt_helpers

const threshold = 256

// a small young generation, so a few thousand objects mean several full gcs
fn small_engine() -> Engine(Nil) {
  let eng = engine.new() |> engine.with_host_hooks(rt_helpers.quiet_hooks())
  let #(Nil, eng) =
    engine.with_context(eng, fn(ctx) {
      let st = rt_gc.collect(ctx.agent, [])
      let st = Agent(..st, store: Store(..st.store, gc_threshold: threshold))
      #(Nil, Context(..ctx, agent: st))
    })
  eng
}

fn run(eng: Engine(Nil), source: String) -> #(String, Engine(Nil)) {
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  #(engine.inspect(eng, value), eng)
}

fn full_gc(eng: Engine(Nil)) -> Engine(Nil) {
  let #(Nil, eng) =
    engine.with_context(eng, fn(ctx) {
      #(Nil, Context(..ctx, agent: rt_gc.collect(ctx.agent, [])))
    })
  eng
}

fn live(eng: Engine(Nil)) -> Int {
  rt_gc.stats(engine.agent(eng)).live_count
}

const shared = "
  var shared = { name: 'shared', list: [1, 2, 3], made: [] };
  var cache = new WeakMap();
  function churn(n) { var last; for (var i = 0; i < n; i++) last = { i: i, pad: [i, i] }; return last.i; }
  'loaded'
"

pub fn what_was_kept_survives_and_garbage_made_later_still_goes_test() {
  let #(_, eng) = run(small_engine(), shared)
  let eng = engine.keep_forever(eng)
  let kept = live(eng)
  // far more objects than the young generation holds, none of them kept
  let #(last, eng) = run(eng, "churn(20000)")
  assert last == "19999"
  let eng = full_gc(eng)
  assert live(eng) < kept + 4 * threshold
  let #(seen, _) = run(eng, "shared.name + shared.list.join('')")
  assert seen == "'shared123'"
}

pub fn a_kept_object_keeps_what_it_is_given_later_test() {
  let #(_, eng) = run(small_engine(), shared)
  let eng = engine.keep_forever(eng)
  let #(_, eng) =
    run(
      eng,
      "for (var i = 0; i < 50; i++) shared.made.push({ n: i, deep: { n: i * 2 } }); churn(20000)",
    )
  let eng = full_gc(eng)
  let #(_, eng) = run(eng, "churn(20000)")
  let eng = full_gc(eng)
  let #(seen, _) =
    run(
      eng,
      "shared.made.length + ':' + shared.made[49].deep.n + ':' + shared.made[0].n",
    )
  assert seen == "'50:98:0'"
}

pub fn dropping_what_a_kept_object_was_given_frees_it_test() {
  let #(_, eng) = run(small_engine(), shared)
  let eng = engine.keep_forever(eng)
  let #(_, eng) =
    run(eng, "for (var i = 0; i < 2000; i++) shared.made.push({ n: i }); 0")
  let eng = full_gc(eng)
  let held = live(eng)
  let #(_, eng) = run(eng, "shared.made.length = 0; 0")
  let eng = full_gc(eng)
  assert live(eng) < held - 1500
}

pub fn a_kept_weak_map_forgets_keys_that_died_later_test() {
  let #(_, eng) = run(small_engine(), shared)
  let eng = engine.keep_forever(eng)
  let #(_, eng) =
    run(
      eng,
      "var stays = { id: 'stays' }; cache.set(stays, 'kept value'); (function () { for (var i = 0; i < 500; i++) cache.set({ i: i }, { big: [i] }); })(); 0",
    )
  let eng = full_gc(eng)
  let held = live(eng)
  let #(_, eng) = run(eng, "churn(5000)")
  let eng = full_gc(eng)
  // the 500 keys and their values went with the first full gc
  assert live(eng) <= held + 8
  let #(seen, _) = run(eng, "cache.get(stays)")
  assert seen == "'kept value'"
}

pub fn keeping_twice_moves_the_line_test() {
  let #(_, eng) = run(small_engine(), shared)
  let eng = engine.keep_forever(eng)
  let #(_, eng) = run(eng, "var later = { name: 'later' }; churn(3000)")
  let eng = engine.keep_forever(eng)
  let #(_, eng) = run(eng, "churn(20000)")
  let eng = full_gc(eng)
  let #(seen, _) = run(eng, "later.name + shared.name")
  assert seen == "'latershared'"
}
