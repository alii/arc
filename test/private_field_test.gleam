import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

const prelude = "
  var out = [];
  function attempt(f) { try { out.push(String(f())); } catch (e) { out.push(e.constructor.name); } }
  class Stamp { constructor(o) { return o; } }
  class P extends Stamp {
    #x = 1;
    #m() { return 'm'; }
    get #g() { return 'g' + this.#x; }
    set #s(v) { this.#x = v; }
    static #sx = 'static';
    static read(o) { return o.#x; }
    static write(o, v) { return o.#x = v; }
    static bump(o) { return o.#x++; }
    static add(o, v) { return o.#x += v; }
    static call(o) { return o.#m(); }
    static method(o) { return o.#m; }
    static writeMethod(o) { o.#m = 1; }
    static getter(o) { return o.#g; }
    static writeGetter(o) { o.#g = 1; }
    static setter(o, v) { return o.#s = v; }
    static readSetter(o) { return o.#s; }
    static readStatic(o) { return o.#sx; }
  }
"

pub fn fields_methods_and_accessors_test() {
  assert run(prelude <> "
  var p = new P({});
  attempt(() => P.read(p));
  attempt(() => P.write(p, 5));
  attempt(() => P.read(p));
  attempt(() => P.bump(p));
  attempt(() => P.add(p, 10));
  attempt(() => P.call(p));
  attempt(() => typeof P.method(p));
  attempt(() => P.writeMethod(p));
  attempt(() => P.getter(p));
  attempt(() => P.writeGetter(p));
  attempt(() => P.setter(p, 7));
  attempt(() => P.read(p));
  attempt(() => P.readSetter(p));
  attempt(() => P.readStatic(P));
  attempt(() => P.readStatic(p));
  out.join()
  ") == "'1,5,5,5,16,m,function,TypeError,g16,TypeError,7,7,TypeError,static,TypeError'"
}

pub fn receivers_without_the_field_throw_test() {
  assert run(prelude <> "
  var others = [{}, [], function () {}, new Proxy(new P({}), {}), Object.create(new P({})),
    1, 'str', true, null, undefined, Symbol('s'), 1n];
  for (var i = 0; i < others.length; i++) {
    let o = others[i];
    attempt(() => P.read(o));
    attempt(() => P.write(o, 1));
    attempt(() => P.call(o));
  }
  out.every(function (r) { return r === 'TypeError'; }) + ':' + out.length
  ") == "'true:36'"
}

pub fn any_object_can_be_stamped_with_the_field_test() {
  assert run(prelude <> "
  var trapped = [];
  var handler = { get(t, k) { trapped.push(k); return t[k]; }, set() { trapped.push('set'); return true; },
    has() { trapped.push('has'); return true; }, defineProperty() { trapped.push('define'); return true; },
    getOwnPropertyDescriptor() { trapped.push('gopd'); } };
  var hosts = [[1, 2], function () {}, new Proxy({}, handler), new Uint8Array(2), new String('s'),
    new Map(), { shaped: 1, like: 2 }, Object.create(null)];
  for (var i = 0; i < hosts.length; i++) {
    let o = new P(hosts[i]);
    attempt(() => o === hosts[i]);
    attempt(() => P.read(o));
    attempt(() => P.write(o, i));
    attempt(() => P.read(o));
    attempt(() => P.getter(o));
  }
  var keys = Object.getOwnPropertyNames(hosts[6]).join('+');
  out.join() + '|' + trapped.length + '|' + keys + '|' + JSON.stringify(hosts[6])
  ") == "'true,1,0,0,g0,true,1,1,1,g1,true,1,2,2,g2,true,1,3,3,g3,true,1,4,4,g4,true,1,5,5,g5,true,1,6,6,g6,true,1,7,7,g7|0|shaped+like|{\"shaped\":1,\"like\":2}'"
}

pub fn two_evaluations_of_a_class_have_different_names_test() {
  assert run(
      "
  function make() { return class { #x = 1; static read(o) { return o.#x; } }; }
  var A = make(), B = make();
  var out = [A.read(new A()), B.read(new B())];
  try { A.read(new B()); out.push('read'); } catch (e) { out.push(e.constructor.name); }
  out.join()
  ",
    )
    == "'1,1,TypeError'"
}
