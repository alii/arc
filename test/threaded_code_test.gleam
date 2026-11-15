import arc/engine.{Returned}
import arc/host
import arc/rt/gc as rt_gc
import arc/rt/store as rt_store
import arc/rt/types.{KStr}
import gleam/dict
import gleam/list

fn eval_string(source: String) -> String {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  let assert KStr(s) = types.classify(value)
  s
}

// each body runs well past the point its code is built a second time
fn warmed(setup: String, check: String) -> String {
  eval_string(setup <> "; for (let i = 0; i < 40; i++) warm(i);" <> check)
}

pub fn read_meets_other_shapes_test() {
  let out =
    warmed(
      "function get(o) { return o.x }
       function warm(i) { get({ x: i, y: 1 }) }",
      "[get({ x: 1, y: 2 }), get({ y: 2, x: 3 }), get({ y: 2 }),
        get(Object.create({ x: 4 })), get({ get x() { return 5 } }),
        get('s'), get([1])].join()",
    )
  assert out == "1,3,,4,5,,"
}

pub fn write_meets_other_shapes_test() {
  let out =
    warmed(
      "function put(o, v) { o.x = v; return o }
       function warm(i) { put({ x: i, y: 1 }, i) }",
      "let seen; const frozen = Object.freeze({ x: 1, y: 2 });
       const setter = Object.create({ set x(v) { seen = v } });
       [put({ x: 1, y: 2 }, 7).x, JSON.stringify(put({ y: 2, x: 3 }, 8)),
        JSON.stringify(put({ y: 2 }, 9)), put(frozen, 5).x,
        (put(setter, 6), seen)].join()",
    )
  assert out == "7,{\"y\":2,\"x\":8},{\"y\":2,\"x\":9},1,6"
}

pub fn inherited_read_sees_later_changes_test() {
  let out =
    warmed(
      "function P() { this.a = 1 } P.prototype.m = function () { return 'old' };
       function call(o) { return o.m() }
       function warm() { call(new P()) }",
      "const p = new P(), before = call(p);
       P.prototype.m = function () { return 'new' };
       const after = call(p); p.m = () => 'own';
       [before, after, call(p)].join()",
    )
  assert out == "old,new,own"
}

pub fn loop_goes_on_in_its_second_form_test() {
  let out =
    eval_string(
      "function sum(o, n) { let s = 0;
         for (let i = 0; i < n; i++) { try { s += o.v } finally { o.v++ } }
         return s }
       String(sum({ v: 1 }, 100))",
    )
  assert out == "5050"
}

pub fn recursion_spans_both_forms_test() {
  let out =
    eval_string(
      "function depth(n) { return n.next ? 1 + depth(n.next) : 0 }
       function last(n) { 'use strict'; if (!n.next) return n.v; return last(n.next) }
       let n = { v: 0, next: null };
       for (let i = 1; i <= 50; i++) n = { v: i, next: n };
       [depth(n), depth(n), last(n), last(n)].join()",
    )
  assert out == "50,50,0,0"
}

pub fn closures_and_callbacks_share_the_second_form_test() {
  let out =
    eval_string(
      "const make = k => function named(o) { return o.x + k };
       const items = Array.from({ length: 30 }, (_, x) => ({ x }));
       const a = make(1), b = make(2);
       const first = items.map(a).pop();
       class A { constructor(o) { this.x = o.x } }
       class B extends A { constructor(o) { super(o); this.y = o.x } }
       const made = items.map(o => new B(o)).pop();
       [first, b({ x: 1 }), make(3)({ y: 0, x: 1 }), b.name, made.x + made.y].join()",
    )
  assert out == "30,3,4,named,58"
}

pub fn short_regexp_runs_test() {
  use #(source, want) <- list.each([
    #("/^a+b?$/.test('aaab')", "true"),
    #("/(a|ab)(c|bcd)(d*)/.exec('abcd')", "abcd,a,bcd,"),
    #("/(?:x|y)*?z/.exec('xyxz')", "xyxz"),
    #("/\\bfoo\\b/.exec('a foo b').index", "2"),
    #("/^(\\d+)-(\\w*)$/.exec('12-')", "12-,12,"),
    #("/[^a-c]+/i.exec('ABCdefA')", "def"),
    #("/(z)|(y)/.exec('y')", "y,,y"),
    #("'a.b.c'.replace(/\\./g, '-')", "a-b-c"),
    #("/(a*)*b/.test('aaaaaaaaaaaaaaaaaaaaaaaac')", "false"),
    #("/é+/.exec('caféé')", "éé"),
    #("/^[eé]/.test('été')", "true"),
    #("/^(?:a|é)/.test('é')", "true"),
    #("/a(?![_é])/.test('aé')", "false"),
    #("/^[^\\x00-\\x7e]/.test('é')", "true"),
  ])
  assert eval_string("String(" <> source <> ")") == want
}

pub fn parsed_json_objects_test() {
  let out =
    eval_string(
      "const rows = JSON.parse('[{\"a\":1,\"b\":[]},{\"a\":2,\"b\":{}},{\"b\":3,\"a\":4},'
         + '{\"a\":1,\"a\":5},{\"1\":0,\"a\":6},{\"__proto__\":7},-0,1e21,\"\\\\u00e9\\\\n\"]');
       rows[1].c = 9; delete rows[0].a;
       JSON.stringify(rows) + Object.is(rows[6], -0) + Object.keys(rows[5])",
    )
  assert out
    == "[{\"b\":[]},{\"a\":2,\"b\":{},\"c\":9},{\"b\":3,\"a\":4},{\"a\":5},"
    <> "{\"1\":0,\"a\":6},{\"__proto__\":7},0,1e+21,\"é\\n\"]true__proto__"
}

pub fn engines_share_no_writes_test() {
  assert eval_string("Array.prototype.mine = 1; Math.max = 2; 'ok'") == "ok"
  assert eval_string("typeof [].mine + typeof Math.max") == "undefinedfunction"
}

pub fn names_given_at_run_time_stay_with_their_closure_test() {
  let out =
    eval_string(
      "const make = k => class { [k]() { return this.q } get [k + 'g']() { return this.q } }.prototype;
       const a = make('a'); for (let i = 0; i < 20; i++) a.a() + a.ag;
       const b = make('b');
       [b.b.name, Object.getOwnPropertyDescriptor(b, 'bg').get.name, a.a.name].join()",
    )
  assert out == "b,get bg,a"
}

pub fn fused_apply_is_a_tail_call_test() {
  let out =
    eval_string(
      "'use strict'; let n = 50000;
       function a() { const self = this; return --n ? a.apply(self) : 'a' }
       function b() { if (++n === 50000) return 'b'; return b.apply() }
       function c() { const self = this; if (--n === 0) return 'c'; return c.apply(self) }
       [(n = 3, a()), b(), c()].join()",
    )
  assert out == "a,b,c"
}

fn cells_left_by(source: String) -> Int {
  let cells = fn(eng) { rt_gc.stats(engine.agent(eng)).live_count }
  let start = engine.new()
  let assert Ok(#(Returned(..), eng)) = engine.eval(start, source)
  cells(eng) - cells(start)
}

pub fn loops_that_only_call_still_collect_test() {
  use source <- list.each([
    "Number.prototype.m = function () { return this.x };
     for (var i = 0; i < 30000; i++) i.m()",
    "var a = [1]; for (var i = 0; i < 30000; i++) a.slice()",
    "function* g() { while (true) yield 1 }
     var it = g(); for (var i = 0; i < 30000; i++) it.next()",
  ])
  assert cells_left_by(source) < 10_000
}

pub fn shapes_are_not_made_without_end_test() {
  let shapes = fn(source) {
    let assert Ok(#(Returned(..), eng)) = engine.eval(engine.new(), source)
    engine.agent(eng).store.next_shape
  }
  assert shapes(
      "for (let i = 0; i < 6000; i++) JSON.parse(`[{\"k${i}\":1},{\"k${i}\":2}]`)",
    )
    <= 4096
  assert shapes(
      "for (let i = 0; i < 500; i++) (0, eval)(`(o => o ? { k${i}: 1 } : 1)`)(false)",
    )
    < 100
}

pub fn a_getter_is_not_the_tail_call_test() {
  let out =
    eval_string(
      "const o = {};
       Object.defineProperty(o, 'm', {
         get: function () { const t = this; return () => t.length }.bind('prim'),
       });
       function f(x) { 'use strict'; return x.m() }
       String(f(o))",
    )
  assert out == "4"
}

pub fn sums_stay_within_safe_integers_test() {
  let out =
    eval_string(
      "var k = 1 << 26, h = -k * k, x = h + h;
       [new Map([[-(2 ** 53), 'hit']]).get(x), new Set([-(2 ** 53), x]).size].join()",
    )
  assert out == "hit,1"
}

pub fn an_old_generator_keeps_the_variables_eval_gave_it_test() {
  let out =
    eval_string(
      "function* g() { yield 0; eval('var q = 1'); yield q; q++; yield q }
       var it = g(), seen = [];
       for (var k = 0; k < 4; k++) {
         seen.push(it.next().value);
         for (var i = 0; i < 20000; i++) [i];
       }
       seen.join()",
    )
  assert out == "0,1,2,"
}

@external(erlang, "erts_debug", "flat_size")
fn words_when_copied(term: a) -> Int

pub fn code_does_not_multiply_when_copied_test() {
  let size = fn(source) {
    let assert Ok(#(Returned(..), eng)) = engine.eval(engine.new(), source)
    words_when_copied(engine.agent(eng))
  }
  let grown =
    size(
      "function f(it) { const [a, b, c, d, e, g, h, i, j, k, l, m, n, o, p, q, r, s] = it; return s }
       f(new Set([1]))",
    )
    - size("function f() {} f()")
  assert grown < 20_000
}

pub fn a_function_is_asked_for_its_json_test() {
  let out =
    eval_string(
      "function f() {} f.toJSON = () => 1; function g() {}
       JSON.stringify({ f, a: [f, g], g }) + JSON.stringify({ f, g }, null, 1)",
    )
  assert out == "{\"f\":1,\"a\":[1,null]}{\n \"f\": 1\n}"
}

pub fn maps_and_sets_bring_on_collections_test() {
  use source <- list.each([
    "for (var j = 0; j < 200; j++) {
       const m = new Map(); for (let i = 0; i < 500; i++) m.set(i, i) }",
    "for (var j = 0; j < 200; j++) {
       const s = new Set(); for (let i = 0; i < 500; i++) s.add(i) }",
  ])
  assert cells_left_by(source) < 100
}

pub fn dead_weak_keys_do_not_pile_up_test() {
  let #(Nil, eng) = {
    use ctx <- engine.with_context(engine.new())
    #(Nil, host.Context(..ctx, agent: rt_gc.set_threshold(ctx.agent, 16)))
  }
  let assert Ok(#(Returned(value:), eng)) =
    engine.eval(
      eng,
      "var m = new WeakMap(); for (var i = 0; i < 30000; i++) m.set({}, i); m",
    )
  let assert types.KHandle(map) = types.classify(value)
  let assert types.SObject(kind: types.WeakMapObj(entries:), ..) =
    rt_store.cell_get(engine.agent(eng), map)
  assert dict.size(entries) < 2000
}

pub fn a_builtin_call_counts_towards_generator_depth_test() {
  let out =
    eval_string(
      "let n = 0; function* r() { n++; yield r().next() }
       try { r().next() } catch (e) { n = e.name + n } n",
    )
  assert out == "RangeError5000"
}

pub fn a_value_a_caller_has_dropped_is_collected_test() {
  // a weak ref may hold on for the job it was made in
  let assert Ok(#(Returned(..), eng)) =
    engine.eval(engine.new(), "var held = {}, w = new WeakRef(held)")
  let assert Ok(#(Returned(value:), _)) =
    engine.eval(
      eng,
      "function churn() { for (let i = 0; i < 5000; i++) [i] }
       function f() {
         let o, s = '1';
         for (let i = 0; i < 3; i++) {
           o = held;
           if (!i) { s++; o = held = null; churn() }
         }
         return typeof w.deref()
       }
       f()",
    )
  assert types.classify(value) == KStr("undefined")
}
