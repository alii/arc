import arc/engine.{type Engine, Returned}
import arc/host
import arc/rt/types.{mk_string}

fn new_engine() -> Engine(Nil) {
  engine.new()
  |> engine.define_fn("current", 0, fn(ctx, _args, _this) {
    #(Ok(host.async_context(ctx)), ctx)
  })
  |> engine.define_fn("swap", 1, fn(ctx, args, _this) {
    let #(previous, ctx) = host.swap_async_context(ctx, host.first_arg(args))
    #(Ok(previous), ctx)
  })
}

const prelude = "
var log = [];
function run(value, body) {
  var previous = swap(value);
  try { return body() } finally { swap(previous) }
}
"

fn eval_string(eng: Engine(Nil), source: String) -> #(String, Engine(Nil)) {
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  #(engine.inspect(eng, value), eng)
}

fn logged(source: String) -> String {
  let #(_, eng) = eval_string(new_engine(), prelude <> source)
  eval_string(eng, "log.join()").0
}

pub fn unset_context_is_undefined_test() {
  let #(seen, _) = eval_string(new_engine(), "String(current())")
  assert seen == "'undefined'"
}

pub fn then_runs_under_the_context_it_was_made_in_test() {
  assert logged(
      "
      run('a', () => { Promise.resolve().then(() => log.push('then:' + current())) });
      Promise.resolve().then(() => log.push('outside:' + current()));
      ",
    )
    == "'then:a,outside:undefined'"
}

pub fn await_keeps_the_context_test() {
  assert logged(
      "
      run('b', async () => {
        await null;
        log.push('first:' + current());
        await new Promise(resolve => resolve());
        log.push('second:' + current());
      });
      run('c', async () => { await null; log.push('other:' + current()) });
      ",
    )
    == "'first:b,other:c,second:b'"
}

pub fn pending_promise_reaction_keeps_its_context_test() {
  assert logged(
      "
      var settle;
      var pending = new Promise(resolve => { settle = resolve });
      run('d', () => { pending.then(() => log.push('late:' + current())) });
      run('e', () => settle());
      ",
    )
    == "'late:d'"
}

pub fn thenable_job_keeps_its_context_test() {
  assert logged(
      "
      var thenable = { then(resolve) { log.push('then:' + current()); resolve() } };
      run('f', () => { Promise.resolve(thenable) });
      ",
    )
    == "'then:f'"
}

pub fn context_is_restored_after_the_jobs_test() {
  let #(_, eng) =
    eval_string(
      new_engine(),
      prelude
        <> "run('g', () => { Promise.resolve().then(() => {}) }); swap('top')",
    )
  let #(seen, _) = eval_string(eng, "current()")
  assert seen == "'top'"
}

pub fn resumed_ticket_continues_in_the_awaiting_context_test() {
  let eng = new_engine()
  let #(#(promise, ticket), eng) =
    engine.with_context(eng, fn(ctx) {
      let #(promise, ticket, ctx) = host.suspend(ctx)
      #(#(promise, ticket), ctx)
    })
  let eng = engine.define_global(eng, "pending", promise)
  let #(_, eng) =
    eval_string(
      eng,
      prelude
        <> "run('h', async () => { log.push(await pending + ':' + current()) })",
    )
  let #(outcome, eng) =
    engine.with_context(eng, fn(ctx) {
      host.resume(ctx, ticket, Ok(mk_string("done")))
    })
  assert outcome == host.Resumed
  assert eval_string(eng, "log.join()").0 == "'done:h'"
  assert eval_string(eng, "String(current())").0 == "'undefined'"
}
