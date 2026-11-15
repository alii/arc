// work that should cost the same for each item however many there are. It is
// counted in reductions, which do not depend on the machine, and only the
// counts at two sizes are compared, so a faster or slower engine changes nothing.
// Time spent inside one bif or one =:= is not counted

import arc/engine.{type Engine, Returned}
import arc/host
import arc/rt/gc as rt_gc
import gleam/int

@external(erlang, "rt_helpers_ffi", "reductions")
fn reductions(run: fn() -> a) -> Int

const few = 1000

const many = 8000

fn cost(start: Engine(Nil), body: String, n: Int) -> Int {
  // collecting often brings out at these sizes what would take far larger ones
  let #(Nil, eng) = {
    use ctx <- engine.with_context(start)
    #(Nil, host.Context(..ctx, agent: rt_gc.set_threshold(ctx.agent, 64)))
  }
  let source =
    "function noop() {} (function (n) {"
    <> body
    <> "})("
    <> int.to_string(n)
    <> ")"
  use <- reductions
  let assert Ok(#(Returned(..), _)) = engine.eval(eng, source)
}

// eight times the items would be eight times the cost of each were it quadratic
fn assert_linear(body: String) -> Nil {
  assert_linear_from(engine.new(), body)
}

fn assert_linear_from(start: Engine(Nil), body: String) -> Nil {
  let _loads_what_it_needs = cost(start, body, 1)
  let fixed = cost(start, body, 0)
  let small = { cost(start, body, few) - fixed } / few
  let large = { cost(start, body, many) - fixed } / many
  assert large < small * 2
}

pub fn queueing_jobs_test() {
  assert_linear("for (let i = 0; i < n; i++) Promise.resolve(i).then(noop)")
}

pub fn jobs_that_queue_jobs_test() {
  assert_linear(
    "for (let i = 0; i < n; i++) Promise.resolve(i).then(v => [v]).then(noop)",
  )
}

pub fn reactions_on_one_promise_test() {
  assert_linear(
    "const p = new Promise(noop); for (let i = 0; i < n; i++) p.then(noop)",
  )
}

pub fn handling_rejections_late_test() {
  assert_linear(
    "const all = []; for (let i = 0; i < n; i++) all.push(Promise.reject(i));
     for (const p of all) p.catch(noop)",
  )
}

pub fn promise_all_test() {
  assert_linear(
    "const all = []; for (let i = 0; i < n; i++) all.push(Promise.resolve(i));
     Promise.all(all).then(noop)",
  )
}

pub fn filling_a_map_test() {
  assert_linear(
    "const m = new Map(); for (let i = 0; i < n; i++) m.set(i, { i })",
  )
}

pub fn filling_a_map_beside_much_that_is_kept_test() {
  let assert Ok(#(Returned(..), loaded)) =
    engine.eval(
      engine.new(),
      "var kept = []; for (var i = 0; i < 20000; i++) kept.push({ i })",
    )
  assert_linear_from(
    engine.keep_forever(loaded),
    "const m = new Map(); for (let i = 0; i < n; i++) m.set(i, { i })",
  )
}

pub fn overwriting_a_map_test() {
  assert_linear(
    "const m = new Map(); for (let i = 0; i < n; i++) m.set(i, 0);
     for (let i = 0; i < n; i++) m.set(i, { i })",
  )
}

pub fn filling_a_set_test() {
  assert_linear("const s = new Set(); for (let i = 0; i < n; i++) s.add({ i })")
}

pub fn filling_a_weak_map_test() {
  assert_linear(
    "const m = new WeakMap(), keys = [];
     for (let i = 0; i < n; i++) { const k = {}; keys.push(k); m.set(k, i) }",
  )
}

pub fn many_weak_containers_test() {
  assert_linear(
    "const all = [];
     for (let i = 0; i < n; i++) { const o = {}; all.push(o, new WeakRef(o), new WeakMap()) }",
  )
}

pub fn registering_finalizers_test() {
  assert_linear(
    "const r = new FinalizationRegistry(noop), all = [];
     for (let i = 0; i < n; i++) { const o = {}; all.push(o); r.register(o, i) }",
  )
}

pub fn queueing_on_an_async_generator_test() {
  assert_linear(
    "async function* g() { for (;;) yield [1] }
     const it = g(); for (let i = 0; i < n; i++) it.next()",
  )
}

pub fn deferring_disposals_test() {
  assert_linear(
    "const s = new DisposableStack();
     for (let i = 0; i < n; i++) s.defer(() => i); s.dispose()",
  )
}

pub fn for_in_inside_a_generator_test() {
  assert_linear(
    "const o = {}; for (let i = 0; i < n; i++) o['k' + i] = i;
     function* g() { for (const k in o) yield [k] } for (const x of g()) [x]",
  )
}

pub fn filling_an_object_test() {
  assert_linear("const o = {}; for (let i = 0; i < n; i++) o['k' + i] = { i }")
}

pub fn filling_an_array_test() {
  assert_linear("const a = []; for (let i = 0; i < n; i++) a.push({ i })")
}

pub fn making_garbage_beside_a_long_array_test() {
  assert_linear(
    "const a = []; for (let i = 0; i < n; i++) a.push(...Array(24).fill(0));
     for (let i = 0; i < n * 16; i++) [i]; a.length",
  )
}

pub fn making_garbage_beside_a_large_map_test() {
  assert_linear(
    "const m = new Map(); for (let i = 0; i < n * 8; i++) m.set(i, i);
     for (let i = 0; i < n * 32; i++) [i]; m.size",
  )
}

pub fn growing_a_string_test() {
  assert_linear("let s = ''; for (let i = 0; i < n; i++) s += 'ab'; s.length")
}

pub fn compiling_a_long_regexp_test() {
  assert_linear("new RegExp('ab'.repeat(n)).test('x')")
}

pub fn running_a_long_chain_of_reads_test() {
  assert_linear(
    "const o = {}; o.a = o;
     new Function('o', 'for (var i = 0; i < 20; i++) o = '
       + '('.repeat(n / 8) + 'o' + '.a)'.repeat(n / 8))(o)",
  )
}
