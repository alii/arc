import arc/compiler
import arc/host_hooks.{HostHooks}
import arc/interp/entry
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/inspect as rt_inspect
import arc/rt/realm as rt_realm
import arc/rt/types.{type Agent, type JsVal, JInt, KBool, KNum, KStr, classify}
import gleam/string
import rt_helpers

fn agent() -> Agent {
  rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
}

fn run_on(st: Agent, source: String) -> #(rt_call.Completion, Agent) {
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compiler.compile(body, sb)
    as { "compile failed: " <> source }
  let #(completion, st) = entry.run_script(st, template)
  #(completion, rt_async.drain(st))
}

fn out_of(st: Agent, source: String) -> #(JsVal, Agent) {
  case run_on(st, source) {
    #(NormalCompletion(_), st) -> rt_helpers.global(st, "out")
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

fn out(source: String) -> String {
  let #(v, st) = out_of(agent(), source)
  case classify(v) {
    KStr(s) -> s
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

fn eval(source: String) -> #(JsVal, Agent) {
  case run_on(agent(), source) {
    #(NormalCompletion(v), st) -> #(v, st)
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

fn eval_int(source: String) -> Int {
  let #(v, st) = eval(source)
  case classify(v) {
    KNum(JInt(n)) -> n
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

fn eval_string(source: String) -> String {
  let #(v, st) = eval(source)
  case classify(v) {
    KStr(s) -> s
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

fn eval_bool(source: String) -> Bool {
  let #(v, st) = eval(source)
  case classify(v) {
    KBool(b) -> b
    _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
  }
}

fn thrown(source: String) -> String {
  case run_on(agent(), source) {
    #(ThrowCompletion(e), st) -> rt_inspect.inspect(st, e)
    #(NormalCompletion(v), st) ->
      panic as { source <> " returned " <> rt_inspect.inspect(st, v) }
  }
}

const settle = "
  var q = Promise.resolve();
  for (var i = 0; i < 30; i++) q = q.then(() => 0);
  q.then(() => { out = log.join(',') });
"

pub fn generator_protocol_matrix_test() {
  assert eval_string(
      "function* g() { var a = yield 1; var b = yield a + 1; return a + b }
       var it = g(); var r = [];
       for (var s of [it.next('x'), it.next(10), it.next(20), it.next(30)]) r.push(s.value + ':' + s.done);
       r.join(',')",
    )
    == "1:false,11:false,30:true,undefined:true"
  assert eval_string(
      "var ran = false; function* g() { ran = true; yield 1 }
       var a = g(); var r1 = a.return('r'); var b = g(); var t;
       try { b.throw(new Error('e')) } catch (e) { t = e.message }
       [r1.value, r1.done, a.next().done, t, b.next().done, ran].join()",
    )
    == "r,true,true,e,true,false"
  assert eval_string(
      "function* g() { try { yield 1; yield 2 } catch (e) { yield 'caught ' + e } finally { yield 'fin' } return 'end' }
       var it = g(); var r = [];
       r.push(it.next().value); r.push(it.throw('T').value); r.push(it.next().value); r.push(it.next().value + ':' + it.next().done);
       var it2 = g(); it2.next();
       var a = it2.return('R'); var b = it2.next();
       r.push(a.value + ':' + a.done, b.value + ':' + b.done);
       r.join()",
    )
    == "1,caught T,fin,end:true,fin:false,R:true"
  assert eval_string(
      "function* g() { try { yield 1 } finally { return 'override' } }
       var it = g(); it.next(); var r = it.return('R'); r.value + ':' + r.done",
    )
    == "override:true"
  assert eval_string(
      "var it; function* g() { try { it.next() } catch (e) { yield e.constructor.name } }
       it = g(); it.next().value",
    )
    == "TypeError"
  assert eval_string(
      "var closed = 0;
       var src = { [Symbol.iterator]() { return { next() { return { value: 1, done: false } }, return() { closed++; return {} } } } };
       function* g() { for (var x of src) { yield x } }
       var it = g(); it.next(); var r = it.return('done');
       [r.value, r.done, closed].join()",
    )
    == "done,true,1"
}

pub fn generator_objects_and_this_test() {
  assert eval_bool(
    "function* g() {} var it = g();
     Object.getPrototypeOf(it) === g.prototype && it[Symbol.iterator]() === it
     && Object.prototype.toString.call(it) === '[object Generator]'",
  )
  assert eval_int(
      "var o = { n: 7, *g(k) { yield this.n * k; yield arguments.length } };
     var it = o.g(6, 0, 0); it.next().value + it.next().value",
    )
    == 45
  assert eval_string("[...(function*() { yield* [1, 2]; yield 3 })()].join()")
    == "1,2,3"
  assert eval_string(
      "function* g() { throw new Error('early'); yield 1 }
       var it = g(); var m; try { it.next() } catch (e) { m = e.message }
       m + ':' + it.next().done",
    )
    == "early:true"
}

pub fn yield_star_forwarding_test() {
  assert eval_string(
      "var gets = 0, calls = 0;
       var inner = { [Symbol.iterator]() { return this },
         get next() { gets++; return function (v) { calls++; return { done: calls > 2, value: 'i' + calls + ':' + v } } } };
       function* g() { var r = yield* inner; yield 'result ' + r }
       var it = g(); var r = [it.next('a').value, it.next('b').value, it.next('c').value];
       r.concat([gets, calls]).join()",
    )
    == "i1:undefined,i2:b,result i3:c,1,3"
  assert eval_string(
      "var log = [];
       var inner = { [Symbol.iterator]() { return this },
         next(v) { log.push('next ' + v); return { done: false, value: 'n' } },
         throw(e) { log.push('throw ' + e); return e === 'again' ? { done: true, value: 'T' } : { done: false, value: 'kept' } } };
       function* g() { var r = yield* inner; log.push('after ' + r); yield 'tail' }
       var it = g(); it.next();
       var a = it.throw('once'); var b = it.throw('again');
       log.concat([a.value, b.value]).join()",
    )
    == "next undefined,throw once,throw again,after T,kept,tail"
  assert eval_string(
      "var closed = 0;
       var inner = { [Symbol.iterator]() { return this }, next() { return { done: false, value: 1 } }, return() { closed++; return {} } };
       function* g() { try { yield* inner } catch (e) { yield e.constructor.name + closed } }
       var it = g(); it.next(); it.throw('x').value",
    )
    == "TypeError1"
  assert eval_string(
      "var log = [];
       var inner = { [Symbol.iterator]() { return this }, next() { return { done: false, value: 'n' } },
         return(v) { log.push('ret ' + v); return v === 'stop' ? { done: true, value: 'inner-' + v } : { done: false, value: 'still' } } };
       function* g() { try { yield* inner } finally { log.push('fin') } }
       var it = g(); it.next();
       var a = it.return('go'); var b = it.return('stop');
       log.concat([a.value + ':' + a.done, b.value + ':' + b.done]).join()",
    )
    == "ret go,ret stop,fin,still:false,inner-stop:true"
  assert eval_string(
      "var inner = { [Symbol.iterator]() { return this }, next() { return { done: false, value: 'n' } } };
       function* g() { try { yield* inner } finally { yield 'fin' } }
       var it = g(); it.next(); var a = it.return('R'); var b = it.next();
       [a.value, a.done, b.value, b.done].join()",
    )
    == "fin,false,R,true"
  assert eval_string(
      "var inner = { [Symbol.iterator]() { return this }, next() { return { done: false, value: 1 } }, get return() { throw 'getter' } };
       function* g() { try { yield* inner } catch (e) { return 'caught ' + e } }
       var it = g(); it.next(); var r = it.return('x'); r.value + ':' + r.done",
    )
    == "caught getter:true"
  assert eval_string(
      "var log = [];
       function* inner() { try { yield 1; yield 2 } finally { log.push('inner fin') } }
       function* outer() { try { yield* inner() } finally { log.push('outer fin') } }
       var it = outer(); it.next(); var r = it.return('R');
       log.concat([r.value, r.done]).join()",
    )
    == "inner fin,outer fin,R,true"
}

pub fn async_ordering_against_microtasks_test() {
  assert out("var log = [], out;
       async function a() { log.push('a1'); await null; log.push('a2'); await null; log.push('a3'); return 'A' }
       async function b() { log.push('b1'); await new Promise(r => r()); log.push('b2'); return 'B' }
       Promise.resolve().then(() => log.push('p1')).then(() => log.push('p2')).then(() => log.push('p3'));
       a().then(v => log.push(v));
       b().then(v => log.push(v));
       log.push('sync');" <> settle) == "a1,b1,sync,p1,a2,b2,p2,a3,B,p3,A"
  assert out("var log = [], out;
       async function f() { try { await Promise.reject('no') } catch (e) { log.push('caught ' + e); return 'ok' } finally { log.push('fin') } }
       f().then(v => log.push(v));" <> settle) == "caught no,fin,ok"
  assert out("var log = [], out;
       async function f() { await 0; throw new Error('bad') }
       async function g() { return { then(r) { r('adopted') } } }
       f().catch(e => log.push(e.message));
       g().then(v => log.push(v));" <> settle) == "bad,adopted"
  assert out("var log = [], out;
       var o = { k: 'K', async m(a, ...r) { await 0; var f = () => this.k + a + r.length + arguments.length; await 0; return f() } };
       o.m('A', 1, 2).then(v => log.push(v));
       (async (x) => { await 0; return x * 2 })(21).then(v => log.push(v));" <> settle) == "42,KA23"
  assert out("var log = [], out;
       async function sum(n) { var t = 0; for (var i = 0; i < n; i++) { t += await Promise.resolve(i) } return t }
       async function outer() { return [await sum(4), await sum(0), (await sum(3)) + 100].join('/') }
       outer().then(v => log.push(v));" <> settle) == "6/0/103"
}

pub fn coroutines_resume_in_their_own_realm_test() {
  let #(_, st) =
    run_on(
      agent(),
      "var r = new ShadowRealm(); globalThis.tag = 'outer';
       r.evaluate('globalThis.tag = \"inner\"; 0');
       r.evaluate('(async function () { globalThis.before = globalThis.tag; await 1; globalThis.after = globalThis.tag })(); 0');
       r.evaluate('(async function* () { await 1; globalThis.ag = globalThis.tag; yield 0 })().next(); 0');
       r.evaluate('var it = (async function* () { yield 1; globalThis.ag3 = globalThis.tag })(); it.next(); it.next(); 0');",
    )
  let #(inside, st) =
    out_of(
      st,
      "var out = r.evaluate('[globalThis.before, globalThis.after, globalThis.ag, globalThis.ag3].join()')",
    )
  assert classify(inside) == KStr("inner,inner,inner,inner")
  let #(outside, _) =
    out_of(
      st,
      "var out = [typeof globalThis.before, typeof globalThis.after, typeof globalThis.ag, typeof globalThis.ag3].join()",
    )
  assert classify(outside) == KStr("undefined,undefined,undefined,undefined")
  let st = agent()
  let #(_, st) = rt_realm.install_262(st, st.realm)
  let #(v, _) =
    out_of(
      st,
      "var other = $262.createRealm();
       other.evalScript('var tag = \"theirs\"; function* g() { while (true) yield tag + \":\" + ([] instanceof Array) }');
       var tag = 'ours', it = other.global.g();
       var out = it.next().value + '/' + it.next().value",
    )
  assert classify(v) == KStr("theirs:true/theirs:true")
}

pub fn async_generator_queue_test() {
  assert out("var log = [], out;
       async function* g() { log.push('g0'); var x = yield 1; log.push('g1:' + x); await null; log.push('g2'); x = yield 2; log.push('g3:' + x); return 3 }
       var it = g();
       it.next('n0').then(r => log.push('r0:' + r.value + r.done));
       it.next('n1').then(r => log.push('r1:' + r.value + r.done));
       it.next('n2').then(r => log.push('r2:' + r.value + r.done));
       it.next('n3').then(r => log.push('r3:' + r.value + r.done));
       Promise.resolve().then(() => log.push('p1')).then(() => log.push('p2'));
       log.push('sync');" <> settle) == "g0,sync,g1:n1,p1,r0:1false,g2,p2,g3:n2,r1:2false,r2:3true,r3:undefinedtrue"
  assert out("var log = [], out;
       async function* thrower() { try { yield 1; log.push('not here') } finally { log.push('fin'); await null; log.push('fin2') } }
       var t = thrower();
       t.next().then(r => log.push('t0:' + r.value));
       t.return(Promise.resolve('R')).then(r => log.push('t1:' + r.value + r.done));
       t.next().then(r => log.push('t2:' + r.done));
       var u = thrower();
       u.return('early').then(r => log.push('u:' + r.value + r.done));
       var w = thrower();
       w.next().then(() => w.throw(new Error('boom'))).then(null, e => log.push('w:' + e.message));" <> settle) == "t0:1,fin,u:earlytrue,fin,fin2,fin2,t1:Rtrue,t2:true,w:boom"
  assert out("var log = [], out;
       async function* g() { try { yield 1 } catch (e) { yield 'caught ' + e } yield Promise.reject('rej'); yield 'never' }
       var it = g();
       it.next().then(r => log.push(r.value));
       it.throw('T').then(r => log.push(r.value));
       it.next().then(r => log.push('?' + r.value), e => log.push('rejected ' + e));
       it.next().then(r => log.push('done ' + r.done));" <> settle) == "1,caught T,rejected rej,done true"
  assert out("var log = [], out;
       async function* g() {}
       var p = g().next.call({});
       log.push(typeof p.then);
       p.catch(e => log.push(e.constructor.name));" <> settle) == "function,TypeError"
}

pub fn async_yield_star_test() {
  assert out("var log = [], out;
       async function* inner() { yield 'i1'; yield 'i2'; return 'ret' }
       async function* outer() { var r = yield* inner(); yield* ['s1']; yield r }
       (async () => { for await (var v of outer()) log.push(v) })();" <> settle) == "i1,i2,s1,ret"
  assert out("var log = [], out;
       var inner = { [Symbol.asyncIterator]() { return this }, next() { return { done: false, value: 'n' } },
         return(v) { log.push('ret ' + v); return v === 'stop' ? { done: true, value: 'inner-' + v } : { done: false, value: 'still' } } };
       async function* g() { try { yield* inner } finally { log.push('fin') } }
       var it = g();
       it.next().then(r => log.push(r.value));
       it.return('go').then(r => log.push(r.value + ':' + r.done));
       it.return('stop').then(r => log.push(r.value + ':' + r.done));" <> settle) == "n,ret go,still:false,ret stop,fin,inner-stop:true"
  assert out("var log = [], out;
       var inner = { [Symbol.asyncIterator]() { return this }, next() { return { done: false, value: 'n' } },
         throw(e) { log.push('throw ' + e); if (e === 'rej') return Promise.reject('R'); return e === 'fin' ? { done: true, value: 'T' } : { done: false, value: 'kept' } } };
       async function* g() { try { var r = yield* inner; log.push('after ' + r); yield* inner } catch (e) { yield 'caught ' + e } }
       var it = g();
       it.next().then(r => log.push(r.value));
       it.throw('once').then(r => log.push(r.value));
       it.throw('fin').then(r => log.push(r.value));
       it.throw('rej').then(r => log.push(r.value));" <> settle) == "throw once,n,throw fin,kept,after T,throw rej,n,caught R"
  assert out("var log = [], out;
       var closed = 0;
       var noThrow = { [Symbol.asyncIterator]() { return this }, next() { return { done: false, value: 1 } }, return() { closed++; return {} } };
       var bare = { [Symbol.asyncIterator]() { return this }, next() { return { done: false, value: 1 } } };
       async function* g(src) { try { yield* src } catch (e) { yield e.constructor.name + closed } }
       var a = g(noThrow); a.next(); a.throw('x').then(r => log.push('a ' + r.value));
       var b = g(bare); b.next(); b.throw('x').then(r => log.push('b ' + r.value));
       async function* h() { try { yield* bare } finally { log.push('h fin') } }
       var c = h(); c.next(); c.return('R').then(r => log.push('c ' + r.value + r.done));" <> settle) == "b TypeError1,h fin,a TypeError1,c Rtrue"
  assert out("var log = [], out;
       var n = 0;
       var src = { get [Symbol.asyncIterator]() { log.push('get async'); return function () { return { get next() { log.push('get next'); return function () { n++; return { done: n > 2, value: 'v' + n } } } } } } };
       var sync = { get [Symbol.asyncIterator]() { log.push('get async2'); return null }, get [Symbol.iterator]() { log.push('get sync'); return function () { return { next() { return { done: true, value: 's' } } } } } };
       async function* g() { log.push(yield* src); log.push(yield* sync) }
       (async () => { for await (var v of g()) log.push(v) })();" <> settle) == "get async,get next,v1,v2,v3,get async2,get sync,s"
}

pub fn for_await_test() {
  assert out("var log = [], out;
       var closes = 0;
       var src = { [Symbol.asyncIterator]() { var n = 0; return { next() { n++; return Promise.resolve({ value: n, done: n > 3 }) }, return() { closes++; return {} } } } };
       async function all() { var r = []; for await (var v of src) r.push(v); return r.join('') }
       async function brk() { for await (var v of src) { if (v == 2) break } return 'b' }
       async function ret() { for await (var v of src) { return 'r' + v } }
       async function thr() { try { for await (var v of src) { throw 'boom' } } catch (e) { return 'c' + e } }
       async function sync() { var r = []; for await (var v of [10, Promise.resolve(20), 30]) r.push(v); return r.join('/') }
       async function dstr() { var it = (function*() { yield; yield })(); for await (var [,] of [it]) { return it.next().done } }
       Promise.all([all(), brk(), ret(), thr(), sync(), dstr()]).then(v => { log.push(v.join(), closes) });" <> settle) == "123,b,r1,cboom,10/20/30,true,3"
  assert out("var log = [], out;
       var it = { iterator: 42, next() { return { done: false, value: 1 } }, [Symbol.asyncIterator]() { return this },
         return() { log.push('ret'); return Promise.resolve({}).then(v => (log.push('settled'), v)) } };
       Promise.resolve().then(() => log.push('t1')).then(() => log.push('t2')).then(() => log.push('t3'));
       async function f() { try { for await (const x of it) { return 'f' + x } } finally { log.push('fin') } }
       async function g() { outer: for (var i of [1, 2]) { for await (const x of it) continue outer } return 'g' + it.done }
       f().then(v => { log.push(v); return g() }).then(v => log.push(v));" <> settle) == "t1,ret,t2,settled,t3,fin,f1,ret,settled,ret,settled,gundefined"
  assert out("var log = [], out;
       async function* doubled(xs) { for await (var x of xs) yield x * 2 }
       (async () => { for await (var y of doubled([1, 2, 3])) log.push(y) })();" <> settle) == "2,4,6"
}

pub fn unhandled_rejections_reported_by_drain_test() {
  let assert [] = rt_helpers.recorded()
  let st =
    rt_builtins.new_agent(
      HostHooks(..rt_helpers.quiet_hooks(), report_uncaught: rt_helpers.record),
    )
    |> entry.link
  let #(_, st) =
    run_on(
      st,
      "async function f() { await 0; throw new Error('handled') } f().catch(() => {});
       Promise.reject('also handled').then(null, () => {});",
    )
  let assert [] = rt_helpers.recorded()
  let #(_, _st) =
    run_on(
      st,
      "async function g() { throw new Error('nobody listens') } g();
       (async function* h() { throw 'gen' })().next();",
    )
  let reports: List(String) = rt_helpers.recorded()
  let assert [_, _] = reports
  let joined = string.join(reports, "|")
  assert string.contains(joined, "Uncaught (in promise) Error: nobody listens")
  assert string.contains(joined, "Uncaught (in promise) gen")
}

pub fn generator_errors_test() {
  assert string.contains(
    thrown("function* g() {} g.prototype.next.call({})"),
    "TypeError",
  )
  assert string.contains(thrown("function* g() {} new g()"), "TypeError")
  assert string.contains(thrown("async function* g() {} new g()"), "TypeError")
}
