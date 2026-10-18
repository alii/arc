import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

const prelude = "
  var out = [];
  function attempt(f) { try { out.push(String(f())); } catch (e) { out.push(e.constructor.name); } }
  function withGetter(get) { return Object.defineProperty({ tag: 'recv' }, 'g', { get: get, configurable: true }); }
"

pub fn every_kind_of_getter_function_test() {
  assert run(prelude <> "
  var kinds = [
    function () { return this.tag; },
    function () { 'use strict'; return this.tag; },
    () => typeof this,
    function () { return arguments.length; },
    function (a, b) { return String(a) + b; },
    (function () { return this.tag; }).bind({ tag: 'bound' }),
    new Proxy(function () { return this.tag; }, {}),
    async function () { return 1; },
    function* () { yield 1; },
    class { },
    Object.getOwnPropertyDescriptor(Map.prototype, 'size').get,
    function () { throw new RangeError('x'); },
    function () { return new.target === undefined; },
  ];
  for (var i = 0; i < kinds.length; i++) {
    let o = withGetter(kinds[i]);
    attempt(function () { return o.g; });
    attempt(function () { var local = o; return local.g; });
    attempt(function () { 'use strict'; return o.g; });
  }
  out.join()
  ") == "'recv,recv,recv,recv,recv,recv,object,object,object,0,0,0,undefinedundefined,undefinedundefined,undefinedundefined,bound,bound,bound,recv,recv,recv,[object Promise],[object Promise],[object Promise],[object Generator],[object Generator],[object Generator],TypeError,TypeError,TypeError,TypeError,TypeError,TypeError,RangeError,RangeError,RangeError,true,true,true'"
}

pub fn the_receiver_is_what_was_read_from_test() {
  assert run(prelude <> "
  class Base { get who() { return this.name; } static get s() { return this.name; } }
  class Derived extends Base { constructor() { super(); this.name = 'derived'; } get up() { return super.who + '!'; } }
  var d = new Derived(), child = Object.create(d); child.name = 'child';
  attempt(() => d.who); attempt(() => child.who); attempt(() => d.up); attempt(() => child.up);
  attempt(() => Derived.s); attempt(() => Base.s);
  var noGet = Object.defineProperty({}, 'g', { set(v) {} });
  attempt(() => noGet.g);
  var shadow = Object.create(withGetter(function () { return 'getter'; }));
  attempt(() => shadow.g);
  Object.defineProperty(shadow, 'g', { value: 'own' });
  attempt(() => shadow.g);
  out.join()
  ") == "'derived,child,derived!,child!,Derived,Base,undefined,getter,own'"
}

pub fn getters_nest_recurse_and_change_the_object_test() {
  assert run(prelude <> "
  var depth = 0;
  var forever = { get g() { depth++; return this.g; } };
  attempt(() => forever.g);
  var counted = { n: 0, get g() { return ++this.n; } };
  attempt(() => counted.g + counted.g * 10);
  var once = { get g() { Object.defineProperty(this, 'g', { value: 'cached' }); return 'first'; } };
  attempt(() => once.g + once.g);
  var chain = { get a() { return { get b() { return { get c() { return 'deep'; } }; } }; } };
  attempt(() => chain.a.b.c);
  var caught = { get g() { try { throw 1; } catch (e) { return 'caught' + e; } finally { out.push('finally'); } } };
  attempt(() => caught.g);
  var total = 0, churn = { get g() { return { pad: [1, 2, 3] }; } };
  for (var i = 0; i < 20000; i++) total += churn.g.pad.length;
  out.push(total);
  out.join() + ':' + (depth > 100)
  ") == "'RangeError,21,firstcached,deep,finally,caught1,60000:true'"
}

pub fn a_getters_error_names_the_getter_and_its_caller_test() {
  assert run(
      "
  var o = { get boom() { throw new Error('x'); } };
  function reader() { var v = o.boom; return v; }
  var stack = '';
  try { reader(); } catch (e) { stack = e.stack; }
  [/boom/.test(stack), /reader/.test(stack)].join()
  ",
    )
    == "'true,true'"
}
