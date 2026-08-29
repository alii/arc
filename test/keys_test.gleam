import arc/bytecode/key
import arc/compiler
import arc/engine.{type Engine, JsString, Returned}
import arc/internal/tuple_array
import arc/interp/load
import arc/parser
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/store as rt_store
import gleam/list
import gleam/string

fn run(eng: Engine(Nil), source: String) -> #(String, Engine(Nil)) {
  let assert Ok(#(Returned(v), eng)) = engine.eval(eng, source)
  let assert JsString(s) = engine.classify(v)
  #(s, eng)
}

fn js(source: String) -> String {
  run(engine.new(), source).0
}

pub fn object_keys_order_test() {
  assert js(
      "var o = {b: 1, 2: 1, a: 1, '4294967295': 1, 1: 1, '4294967294': 1, '01': 1, '-0': 1};
       Object.keys(o).join()",
    )
    == "1,2,4294967294,b,a,4294967295,01,-0"
}

pub fn canonical_index_keys_alias_test() {
  assert js(
      "var a = []; a['7'] = 'x'; a[8.0] = 'y';
       [a[7], a['8'], a.length, a['07'], a['7.0']].join()",
    )
    == "x,y,9,,"
}

pub fn reads_of_unseen_names_do_not_grow_the_table_test() {
  let eng = engine.new()
  let #(_, eng) =
    run(eng, "var o = {a: 1}, arr = [1], s = 'str', r = [], i, k; 'ok'")
  let before = engine.heap(eng).store.next_name
  let #(out, eng) =
    run(
      eng,
      "r = [];
       for (i = 0; i < 50; i++) {
         k = 'zz_never_' + i;
         r.push(o[k], k in o, o.hasOwnProperty(k), Object.hasOwn(o, k),
           Reflect.has(o, k), Reflect.get(o, k), arr[k], s[k],
           Object.getOwnPropertyDescriptor(o, k), o.propertyIsEnumerable(k));
       }
       String(r.every(function (x) { return x === undefined || x === false }))",
    )
  assert out == "true"
  assert engine.heap(eng).store.next_name == before
  let #(out, eng) = run(eng, "o['zz_never_0'] = 2; String(o.zz_never_0)")
  assert out == "2"
  assert engine.heap(eng).store.next_name == before + 1
}

pub fn unseen_name_read_through_proxy_still_traps_test() {
  assert js(
      "var seen = [];
       var p = new Proxy({}, { get(t, k) { seen.push(typeof k + ':' + k); return 7 },
                               has(t, k) { seen.push('has:' + k); return true } });
       var o = Object.create(p);
       [o.zq_unseen_1, o['zq_unseen_2'], 'zq_unseen_3' in o].join() + '|' + seen.join()",
    )
    == "7,7,true|string:zq_unseen_1,string:zq_unseen_2,has:zq_unseen_3"
}

pub fn json_round_trip_with_many_names_test() {
  let eng = engine.new()
  let #(_, eng) = run(eng, "'warm'")
  let before = engine.heap(eng).store.next_name
  let #(out, eng) =
    run(
      eng,
      "var o = {};
       for (var i = 0; i < 10000; i++) o['k' + i] = i;
       var text = JSON.stringify(o);
       var back = JSON.parse(text);
       String(JSON.stringify(back) === text && Object.keys(back).length === 10000
         && back.k9999 === 9999 && Object.keys(back)[42] === 'k42')",
    )
  assert out == "true"
  let grown = engine.heap(eng).store.next_name - before
  assert grown >= 10_000 && grown < 10_010
}

pub fn typed_array_string_keys_test() {
  assert js(
      "var ta = new Int8Array(4); ta[1] = 5;
       var r = [ta['1'], ta['-0'], ta['1.5'], ta['01'], '-0' in ta, '1.5' in ta, '01' in ta];
       ta['-0'] = 9; ta['01'] = 9; r.push(ta[0], ta['01'], Object.keys(ta).join());
       r.join()",
    )
    == "5,,,,false,false,false,0,9,0,1,2,3,01"
}

pub fn proxy_own_keys_gets_strings_test() {
  assert js(
      "var target = {a: 1, 5: 2};
       Object.defineProperty(target, 'fixed', {value: 1, configurable: false});
       var kinds = '';
       var p = new Proxy(target, { ownKeys(t) {
         kinds = Reflect.ownKeys(t).map(function (k) { return typeof k }).join('/');
         return ['a', 'a', 'fixed'] } });
       var thrown = '';
       try { Object.keys(p) } catch (e) { thrown = e.constructor.name }
       var p2 = new Proxy(target, { ownKeys(t) { return ['fixed', '5', 'zz_new'] } });
       [thrown, Reflect.ownKeys(p2).join('/'), kinds].join('|')",
    )
    == "TypeError|fixed/5/zz_new|string/string/string"
}

pub fn private_names_are_fresh_per_class_evaluation_test() {
  assert js(
      "function make() { return class { #x = 1; static read(o) { return o.#x } } }
       var A = make(), B = make();
       var out = [A.read(new A())];
       try { A.read(new B()) } catch (e) { out.push(e.constructor.name) }
       out.join()",
    )
    == "1,TypeError"
  let eng = engine.new()
  let #(_, eng) =
    run(
      eng,
      "function make() { return class { #x; m() {} } } make(); make(); 'ok'",
    )
  let st = engine.heap(eng)
  let #(a, st) = rt_store.t_new_private_key(st, "#x")
  let #(b, st) = rt_store.t_new_private_key(st, "#x")
  assert a != b
  assert key.is_private(a) && key.is_private(b)
  assert rt_store.t_key_text(st, a) == "#x"
}

pub fn load_pass_numbers_names_per_heap_test() {
  let compile = fn(source) {
    let assert Ok(#(body, sb)) = parser.parse_script(source)
    let assert Ok(t) = compiler.compile(body, sb)
    t
  }
  let one = compile("var o = {}; o.zz_shared_name = 1; o.zz_only_one = 2")
  let two = compile("var p = {}; p.zz_only_two = 3; p.zz_shared_name = 4")
  let st = engine.heap(engine.new())
  let #(l1, st) = load.template(st, one)
  let #(l2, st) = load.template(st, two)
  let number_of = fn(t: FuncTemplate(key.SourceKey), name) {
    let assert [#(i, _)] =
      tuple_array.to_list(t.keys)
      |> list.index_map(fn(k, i) { #(i, k) })
      |> list.filter(fn(pair) { pair.1 == key.SourceName(name) })
    i
  }
  let k1 = tuple_array.get_unchecked(number_of(one, "zz_shared_name"), l1.keys)
  let k2 = tuple_array.get_unchecked(number_of(two, "zz_shared_name"), l2.keys)
  assert k1 == k2
  assert rt_store.t_key_text(st, k1) == "zz_shared_name"
  assert tuple_array.get_unchecked(number_of(one, "zz_only_one"), l1.keys)
    != tuple_array.get_unchecked(number_of(two, "zz_only_two"), l2.keys)
  // the same unit in a fresh heap that already named something else
  let other = engine.heap(engine.new())
  let #(_, other) = rt_store.t_key(other, "zz_taken_first")
  let #(l3, other) = load.template(other, one)
  let k3 = tuple_array.get_unchecked(number_of(one, "zz_shared_name"), l3.keys)
  assert k3 != k1
  assert rt_store.t_key_text(other, k3) == "zz_shared_name"
}

pub fn snapshot_keeps_dynamic_names_test() {
  let eng = engine.new()
  let #(_, eng) =
    run(
      eng,
      "var o = {zz_dyn_a: 1}; o['zz_dyn_b'] = 2; globalThis.zz_g = o; 'ok'",
    )
  let assert Ok(bin) = engine.serialize(eng)
  let assert Ok(eng2) = engine.deserialize(bin)
  let #(out, _) =
    run(
      eng2,
      "[zz_g.zz_dyn_a, zz_g.zz_dyn_b, Object.keys(zz_g).join('/'), 'zz_dyn_b' in zz_g].join()",
    )
  assert out == "1,2,zz_dyn_a/zz_dyn_b,true"
}

pub fn global_names_and_eval_vars_test() {
  assert js(
      "var zz_gv = 1; let zz_gl = 2; zz_implicit = 3;
       function f() { return eval('var zz_ev = 4; zz_ev + zz_gv + zz_gl + zz_implicit') }
       var r = [f(), typeof zz_ev, typeof zz_nope, delete zz_implicit, typeof zz_implicit];
       try { zz_missing } catch (e) { r.push(e.message) }
       r.join()",
    )
    == "10,undefined,undefined,true,undefined,zz_missing is not defined"
}

pub fn key_text_of_all_kinds_test() {
  let st = engine.heap(engine.new())
  let #(n, st) = rt_store.t_key(st, "zz_plain")
  let #(i, st) = rt_store.t_key(st, "12")
  let #(p, st) = rt_store.t_new_private_key(st, "#hidden")
  assert key.is_name(n) && key.is_index(i) && key.is_private(p)
  assert i == key.index(12)
  assert list.map([n, i, p], rt_store.t_key_text(st, _))
    == ["zz_plain", "12", "#hidden"]
  assert string.inspect(rt_store.t_find_key(st, "zz_plain"))
    == "Some(" <> string.inspect(n) <> ")"
}
