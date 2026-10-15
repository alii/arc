import arc/engine.{type Engine, type Script, AtLoad, OnFirstCall, Returned}
import arc/hints.{type Hints}
import arc/rt/async as rt_async
import arc/rt/types.{KStr, classify}
import gleam/dict

const source = "
  function used(a, b) { /* long enough to be worth leaving for later */ return a + b; }
  function unused(a) { /* long enough to be worth leaving for later */ return a; }
  var arrow = (a) => { /* long enough to be worth leaving for later */ return a + '!'; };
  arrow(used('a', 'b'))
"

fn load(source: String, except: Hints) -> #(Script, Engine(Nil)) {
  let assert Ok(#(Returned(value), script, eng)) =
    engine.eval_script(
      engine.new(),
      source,
      compile_functions: OnFirstCall(except:),
      drain: rt_async.drain,
    )
  assert classify(value) == KStr("ab!")
  #(script, eng)
}

// how many functions the engine compiled on a first call
fn compiled_late(eng: Engine(Nil)) -> Int {
  dict.size(engine.agent(eng).store.meta.lazy_code)
}

pub fn what_a_run_learned_is_compiled_at_load_next_time_test() {
  let #(script, eng) = load(source, hints.none())
  // used and arrow were called; unused never was
  assert compiled_late(eng) == 2
  let learned = engine.learned_hints(eng, script)

  let #(again, eng) = load(source, learned)
  assert compiled_late(eng) == 0
  // nothing new was learned, and what it was loaded with is kept
  assert engine.learned_hints(eng, again) == learned
}

pub fn hints_survive_being_kept_as_bytes_test() {
  let #(script, eng) = load(source, hints.none())
  let learned = engine.learned_hints(eng, script)
  let kept = hints.to_bit_array(learned)
  assert hints.from_bit_array(kept) == Ok(learned)
  assert hints.from_bit_array(hints.to_bit_array(hints.none()))
    == Ok(hints.none())

  let assert Ok(learned) = hints.from_bit_array(kept)
  let #(_, eng) = load(source, learned)
  assert compiled_late(eng) == 0
}

pub fn bytes_that_are_not_hints_are_refused_test() {
  assert hints.from_bit_array(<<>>) == Error(Nil)
  assert hints.from_bit_array(<<"arc hints 0":utf8, 0>>) == Error(Nil)
  // a number cut short
  assert hints.from_bit_array(<<"arc hints 1":utf8, 1, 9:64, 9:32, 200>>)
    == Error(Nil)
}

pub fn hints_for_another_source_are_ignored_test() {
  let #(script, eng) = load(source, hints.none())
  let learned = engine.learned_hints(eng, script)

  // the same functions, each one byte later
  let moved = " " <> source
  let #(script, eng) = load(moved, learned)
  assert compiled_late(eng) == 2
  // and what this run learned is about the source it ran
  let #(_, eng) = load(moved, engine.learned_hints(eng, script))
  assert compiled_late(eng) == 0
}

const library = "
  function first(a) { /* long enough to be worth leaving for later */ return a + 1; }
  function second(a) { /* long enough to be worth leaving for later */ return a + 2; }
  'ab!'
"

// loads the library, then runs `calls` as a script of its own
fn use_library(except: Hints, calls: String) -> #(Script, Engine(Nil)) {
  let #(script, eng) = load(library, except)
  let assert Ok(#(Returned(_), eng)) = engine.eval(eng, calls)
  #(script, eng)
}

pub fn merged_hints_name_the_functions_of_both_runs_test() {
  let #(script, eng) = use_library(hints.none(), "first(1)")
  let one = engine.learned_hints(eng, script)
  let #(script, eng) = use_library(hints.none(), "second(1)")
  let other = engine.learned_hints(eng, script)
  assert one != other

  let both = hints.merge(one, other)
  let #(_, eng) = use_library(both, "first(1) + second(1)")
  assert compiled_late(eng) == 0
  // either alone leaves the other function for its first call
  let #(_, eng) = use_library(one, "first(1) + second(1)")
  assert compiled_late(eng) == 1

  assert hints.merge(one, hints.none()) == one
  assert hints.merge(hints.none(), one) == one
}

pub fn merging_hints_for_another_source_adds_nothing_test() {
  let #(script, eng) = use_library(hints.none(), "first(1)")
  let ours = engine.learned_hints(eng, script)
  let #(script, eng) = load(source, hints.none())
  let theirs = engine.learned_hints(eng, script)
  assert hints.merge(ours, theirs) == ours
}

pub fn a_script_compiled_at_load_learns_nothing_test() {
  let assert Ok(#(Returned(_), script, eng)) =
    engine.eval_script(
      engine.new(),
      source,
      compile_functions: AtLoad,
      drain: rt_async.drain,
    )
  assert compiled_late(eng) == 0
  let learned = engine.learned_hints(eng, script)
  let #(_, eng) = load(source, learned)
  // so loading lazily with it leaves every function for its first call
  assert compiled_late(eng) == 2
}
