import arc/compiler
import arc/interp/entry
import arc/interp/safepoint
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion} as rt_call
import arc/rt/gc as rt_gc
import arc/rt/inspect as rt_inspect
import arc/rt/types.{type Agent}
import rt_helpers

fn run(st: Agent, source: String) -> #(rt_call.Completion, Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
  let assert Ok(template) = compiler.compile(body, sb)
  entry.run_script(st, template)
}

pub fn parked_generator_eval_env_survives_collect_test() {
  let st = rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
  let st = rt_gc.t_collect(st, [])
  let #(first, st) =
    run(
      st,
      "
      function* g() {
        eval('var x = { v: 7 }; var y = [1, 2, 3]');
        yield 1;
        yield x.v + y.length;
      }
      var it = g();
      it.next().value
      ",
    )
  let assert NormalCompletion(v) = first
  assert rt_inspect.inspect(st, v) == "1"
  let st = rt_gc.t_collect(st, [])
  let st = safepoint.end_turn(st, [])
  let #(_, st) =
    run(st, "for (var k = 0; k < 3000; k++) { var o = { k: k, a: [k] }; } 0")
  let st = rt_gc.t_collect(st, [])
  let #(second, st) = run(st, "it.next().value")
  let assert NormalCompletion(v) = second
  assert rt_inspect.inspect(st, v) == "10"
}
