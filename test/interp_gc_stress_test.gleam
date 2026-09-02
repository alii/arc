import arc/compiler
import arc/interp/entry
import arc/interp/safepoint
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/store as rt_store
import arc/rt/types.{type Agent}
import rt_helpers

const threshold = 256

fn small_agent() -> Agent {
  let st = rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
  rt_gc.t_collect(st, [])
  |> rt_store.t_gc_settings(gc_threshold: threshold, names_sweep_min: 0)
}

fn run(st: Agent, source: String) -> #(rt_call.Completion, Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
  let assert Ok(template) = compiler.compile(body, sb)
  entry.run_script(st, template)
}

pub fn allocating_loop_is_bounded_and_keeps_frame_values_test() {
  let st = small_agent()
  let base = rt_gc.stats(st).live
  let source =
    "
    let keep = { tag: 42 };
    let acc = 0;
    function alloc(i) { return { a: [i, i + 1], s: 'x' + i, o: { i: i } }; }
    for (let i = 0; i < 4000; i++) { acc = acc + alloc(i).a[0]; }
    keep.tag + ':' + acc
    "
  let #(completion, st) = run(st, source)
  let assert NormalCompletion(v) = completion
  assert rt_inspect.inspect(st, v) == "'42:7998000'"
  assert rt_gc.stats(st).live <= base + 4 * threshold
  let st = safepoint.end_turn(st, [v])
  assert rt_gc.stats(st).live <= base + 4 * threshold
}

pub fn closures_made_in_the_loop_keep_their_captures_test() {
  let st = small_agent()
  let source =
    "
    let fns = [];
    function make(i) { let box = { v: i }; return function () { return box.v; }; }
    for (let i = 0; i < 2000; i++) { let f = make(i); if (i % 500 === 0) fns.push(f); }
    let total = 0;
    for (let j = 0; j < fns.length; j++) { total = total + fns[j](); }
    total
    "
  let #(completion, st) = run(st, source)
  let assert NormalCompletion(v) = completion
  assert rt_inspect.inspect(st, v) == "3000"
}
