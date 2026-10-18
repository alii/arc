import arc/engine.{type Engine, Returned}
import arc/rt/gc as rt_gc

fn run(eng: Engine(Nil), source: String) -> #(String, Engine(Nil)) {
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  #(engine.inspect(eng, value), eng)
}

fn live(eng: Engine(Nil)) -> Int {
  rt_gc.stats(engine.agent(eng)).live_count
}

pub fn frees_what_small_collections_left_behind_test() {
  let eng = engine.collect_garbage(engine.new())
  // old enough to have outlived small collections, then dropped
  let #(_, eng) =
    run(
      eng,
      "var held = []; for (var i = 0; i < 20000; i++) held.push({ i: i }); var stays = { name: 'stays' }; 0",
    )
  let full = live(eng)
  let #(_, eng) = run(eng, "held = null; 0")
  let eng = engine.collect_garbage(eng)
  assert live(eng) < full - 19_000
  let #(seen, eng) = run(eng, "stays.name")
  assert seen == "'stays'"
  // nothing made since: the same engine comes back
  let again = engine.collect_garbage(eng)
  assert live(again) == live(eng)
}
