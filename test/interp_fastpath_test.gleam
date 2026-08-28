// fused kernels must agree with the slow path
import arc/compiler
import arc/interp/entry
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion}
import arc/rt/inspect as rt_inspect
import arc/rt/types.{KStr, classify}
import rt_helpers

fn global_epoch_after(source: String) -> Int {
  let st = rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compiler.compile(body, sb)
    as { "compile failed: " <> source }
  let #(_, st) = entry.run_script(st, template)
  st.store.global_epoch
}

pub fn global_epoch_test() {
  let quiet = global_epoch_after("var o = {}; o.x = 1; o.y = 5")
  assert global_epoch_after("var o = {}; o.x = 1; g = 5") > quiet
  assert global_epoch_after("var o = {}; o.x = 1; globalThis.h = 5") > quiet
  assert global_epoch_after("var o = {}; o.x = 1; this[3] = 5") > quiet
  assert global_epoch_after(
      "var o = {}; o.x = 1; Object.defineProperty(globalThis, 'q', { value: 1 })",
    )
    > quiet
  assert global_epoch_after("var o = {}; o.x = 1; delete globalThis.NaN")
    == quiet
}

fn run_string(source: String) -> String {
  let st = rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compiler.compile(body, sb)
    as { "compile failed: " <> source }
  case entry.run_script(st, template) {
    #(NormalCompletion(v), st) ->
      case classify(v) {
        KStr(s) -> s
        _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
      }
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

pub fn put_elem_overwrite_fill_append_test() {
  assert run_string(
      "var a = []; for (var i = 0; i < 5; i++) a[i] = i * i; a[2] = 'x'; a.join() + '/' + a.length",
    )
    == "0,1,x,9,16/5"
  assert run_string("var h = [1,,3]; h[1] = 2; h.join() + '/' + h.length")
    == "1,2,3/3"
  assert run_string("var f = [1,2,3]; f[5] = 1; f.length + '/' + (4 in f)")
    == "6/false"
  assert run_string(
      "var s = []; s[2000] = 1; s[2001] = 2; s.length + '/' + (1999 in s) + '/' + s[2001]",
    )
    == "2002/false/2"
}

pub fn put_elem_inherited_setter_test() {
  assert run_string(
      "var hit = 'no';
       Object.defineProperty(Array.prototype, '3', { set: function (v) { hit = v }, configurable: true });
       var a = [1,2,3]; a[3] = 9;
       [hit, a.length, a[3]].join()",
    )
    == "9,3,"
  assert run_string(
      "var hit = 'no';
       Object.defineProperty(Object.prototype, '1', { set: function (v) { hit = v }, configurable: true });
       var a = [1,,3]; a[1] = 9;
       [hit, a.length, a.hasOwnProperty(1)].join()",
    )
    == "9,3,false"
}

pub fn put_elem_inherited_readonly_test() {
  assert run_string(
      "Object.defineProperty(Array.prototype, '2', { value: 7, writable: false, configurable: true });
       var a = [1,2]; a[2] = 9;
       [a.length, a[2], a.hasOwnProperty(2)].join()",
    )
    == "2,7,false"
  assert run_string(
      "'use strict';
       Object.defineProperty(Array.prototype, '2', { value: 7, writable: false, configurable: true });
       var a = [1,2];
       try { a[2] = 9; 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
}

pub fn put_elem_frozen_length_test() {
  assert run_string(
      "var a = [1,2]; Object.defineProperty(a, 'length', { writable: false });
       a[2] = 5; [a.length, a[2]].join()",
    )
    == "2,"
  assert run_string(
      "'use strict';
       var a = [1,2]; Object.defineProperty(a, 'length', { writable: false });
       try { a[2] = 5; 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
  assert run_string(
      "var e = Object.preventExtensions([1,,3]); e[1] = 5; [e.length, 1 in e].join()",
    )
    == "3,false"
}

pub fn put_elem_index_override_test() {
  assert run_string(
      "var a = [1,2,3];
       Object.defineProperty(a, '1', { value: 'ro', writable: false });
       a[1] = 'w';
       var g = 0; Object.defineProperty(a, '2', { get: function () { return 'g' }, set: function (v) { g = v } });
       a[2] = 'set';
       [a[1], a[2], g].join()",
    )
    == "ro,g,set"
}

pub fn get_elem_holes_and_keys_test() {
  assert run_string(
      "Array.prototype[1] = 'p'; var a = [1,,3]; var r = a[1]; delete Array.prototype[1]; r",
    )
    == "p"
  assert run_string(
      "var a = ['x','y']; var o = { k: 'v', 3: 'three' };
       [a['1'], a[1.0], String(a[-1]), String(a[1.5]), o['k'], o[3], o['3']].join()",
    )
    == "y,y,undefined,undefined,v,three,three"
  assert run_string(
      "var a = [1,2]; Object.defineProperty(a, '0', { get: function () { return 'got' } }); a[0]",
    )
    == "got"
}

pub fn get_field_chain_test() {
  assert run_string(
      "class A { get x() { return 'ax' } m() { return 'm' } }
       class B extends A {}
       var b = new B(); b.own = 'o';
       [b.x, b.m(), b.own, String(b.nope), [1,2,3].length, 'str'.length, (function f(a, b) {}).length].join()",
    )
    == "ax,m,o,undefined,3,3,2"
  assert run_string(
      "var t = { get v() { return this.w * 2 }, w: 21 }; var p = new Proxy({}, { get: function (_, k) { return 'P' + String(k) } });
       [t.v, p.any].join()",
    )
    == "42,Pany"
}

pub fn put_field_test() {
  assert run_string(
      "var o = { x: 1 }; o.x = 2; o.y = 3;
       var log = [];
       var s = { set v(n) { log.push(n) } }; s.v = 7;
       var c = Object.create(s); c.v = 8;
       Object.defineProperty(o, 'ro', { value: 'r', writable: false }); o.ro = 'changed';
       [o.x, o.y, log.join('+'), c.hasOwnProperty('v'), o.ro, Object.keys(o).join('')].join()",
    )
    == "2,3,7+8,false,r,xy"
  assert run_string(
      "'use strict'; var o = Object.freeze({ x: 1 }); try { o.x = 2; 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
  assert run_string(
      "var a = [1,2,3]; a.length = 1; var s = new String('abc');
       [a.join(), a.length, s.length].join()",
    )
    == "1,1,3"
}

pub fn put_field_proto_chain_change_test() {
  assert run_string(
      "function P(v) { this.v = v; this.w = v }
       var log = [];
       new P(1); new P(2);
       Object.defineProperty(P.prototype, 'v', { set: function (n) { log.push('s' + n) }, configurable: true });
       var a = new P(3);
       Object.defineProperty(Object.prototype, 'w', { value: 0, writable: false, configurable: true });
       var b = new P(4);
       delete Object.prototype.w;
       Object.defineProperty(P.prototype, 'v', { value: 9, writable: true, configurable: true });
       var c = new P(5);
       [log.join('+'), a.hasOwnProperty('v'), a.w, b.hasOwnProperty('w'), b.w, c.v, c.w].join()",
    )
    == "s3+s4,false,3,false,,5,5"
  assert run_string(
      "function P(v) { this.v = v }
       var log = [];
       new P(1);
       var q = { set v(n) { log.push(n) } };
       Object.setPrototypeOf(P.prototype, q);
       var a = new P(2);
       P.prototype.__proto__ = Object.prototype;
       var b = new P(3);
       function R(v) { this.__proto__ = v; this.k = 1 }
       new R({}); var r = new R(q);
       [log.join('+'), a.hasOwnProperty('v'), b.v, Object.getPrototypeOf(r) === q, r.hasOwnProperty('__proto__')].join()",
    )
    == "2,false,3,true,false"
  assert run_string(
      "class A { constructor() { this.x = 1 } }
       new A(); new A();
       class B extends A { constructor() { super(); this.y = 2 } set x(v) { this.z = v } }
       var b = new B();
       Object.freeze(A.prototype);
       var a = new A();
       [b.hasOwnProperty('x'), b.z, b.y, a.x].join()",
    )
    == "false,1,2,1"
}

pub fn constructed_receiver_shape_test() {
  assert run_string(
      "function P(a) { this.x = a; this.y = a + 1 }
       var p = new P(1), q = new P(2);
       q.z = 9; delete q.x; q[0] = 'i';
       Object.defineProperty(p, 'g', { get: function () { return this.x + 10 }, enumerable: true });
       var r = new P(3); r.y = 7;
       [Object.keys(p).join(''), Object.keys(q).join(''), JSON.stringify(r), p.g, q.x, 'y' in q, r.hasOwnProperty('x')].join()",
    )
    == "xyg,0yz,{\"x\":3,\"y\":7},11,,true,true"
  assert run_string(
      "class A { #p = 1; constructor(v) { this.v = v } get p() { return this.#p + this.v } }
       var a = new A(4), b = new A(5);
       var seen = []; for (var k in b) seen.push(k);
       class B { constructor() { Object.preventExtensions(this) } }
       var ok; try { new B().w = 1; ok = new B().w === undefined } catch (e) { ok = false }
       [a.p, b.p, seen.join(''), ok, Object.getOwnPropertyNames(a).join('')].join()",
    )
    == "5,6,v,true,v"
}

pub fn define_field_test() {
  assert run_string("var o = { a: 1, b: 2, a: 3 }; Object.keys(o).join() + o.a")
    == "a,b3"
  assert run_string(
      "class A { constructor() { Object.defineProperty(this, 'x', { value: 1, configurable: false }) } }
       class B extends A { x = 2 }
       try { String(new B().x) } catch (e) { e.constructor.name }",
    )
    == "TypeError"
  assert run_string(
      "class A { constructor() { Object.defineProperty(this, 'x', { get() { return 1 }, configurable: true }) } }
       class B extends A { x = 2 }
       JSON.stringify(Object.getOwnPropertyDescriptor(new B(), 'x'))",
    )
    == "{\"value\":2,\"writable\":true,\"enumerable\":true,\"configurable\":true}"
}

pub fn add_kernel_test() {
  assert run_string(
      "[1+2, 1.5+1, 'a'+1, 1+'a', 1+null, null+1, true+1, 1+undefined, 0.1+0.2, 9007199254740991+1, 1e308+1e308, 'a'+1n, [1]+[2]].join()",
    )
    == "3,2.5,a1,1a,1,1,2,NaN,0.30000000000000004,9007199254740992,Infinity,a1,12"
  assert run_string(
      "try { 1 + 1n; 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
  assert run_string(
      "try { 'a' + Symbol(); 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
}

pub fn negative_zero_kernel_test() {
  assert run_string(
      "[1/(-0), 1/(0*-1), 1/(-1%1), 1/(-4%2), 1/(0/-3), Object.is(0, -(0*-1)), 1/-(0)].join()",
    )
    == "-Infinity,-Infinity,-Infinity,-Infinity,-Infinity,true,-Infinity"
}

pub fn div_mod_kernel_test() {
  assert run_string(
      "[7/2, 6/3, 1/0, -1/0, 0/0, 5%0, -5%5, 5.5%2, Infinity/Infinity, 2**10].map(String).join()",
    )
    == "3.5,2,Infinity,-Infinity,NaN,NaN,0,1.5,NaN,1024"
}

pub fn compare_kernel_test() {
  assert run_string(
      "[1<2, 'a'<'b', 'B'<'a', 1<NaN, NaN>=NaN, Infinity>1e308, -Infinity<-1e308, 1n<2n, null<1, undefined<1, '10'<'9', 10<'9', [2]<3].join()",
    )
    == "true,true,true,false,false,true,true,true,true,false,true,false,true"
  assert run_string(
      "[null==undefined, null==0, 1=='1', NaN==NaN, 0==-0, 'a'=='a', 1n==1, Infinity=='Infinity', true==1, [1]==1, ({})=='[object Object]', null===undefined, 1===1.0].join()",
    )
    == "true,false,true,false,true,true,true,true,true,true,true,false,true"
}

pub fn bitwise_kernel_test() {
  assert run_string(
      "[5&3, 5|3, 5^3, 1<<31, -1>>>0, -8>>1, ~5, (2147483647+1)|0, 1.5|0, '3'|0, ~'x'].join()",
    )
    == "1,7,6,-2147483648,4294967295,-4,-6,-2147483648,1,3,-1"
}

pub fn inc_dec_local_test() {
  assert run_string(
      "(function () {
         var s = '1'; s++; var t = '1'; t--; var f = 0.5; f++; var m = 9007199254740991; m++;
         var u; u++; var o = { valueOf() { return 41 } }; o++;
         return [typeof s, s, t, f, m, u, o].join()
       })()",
    )
    == "number,2,0,1.5,9007199254740992,NaN,42"
}

pub fn cmp_local_jump_test() {
  assert run_string(
      "(function () {
         var n = 0; for (var i = 0; i < 10; i++) n++;
         var m = 0; for (var j = 'a'; j < 'aaaa'; j += 'a') m++;
         var calls = 0; var lim = { valueOf() { calls++; return 3 } };
         var k = 0; for (var x = 0; x < lim; x++) k++;
         var big = 0; for (var y = 0n; y < 3n; y = y + 1n) big++;
         return [n, m, k, calls, big].join()
       })()",
    )
    == "10,3,3,4,3"
  assert run_string(
      "(function () { try { for (let i = 0; i < 1; i++) { let z = z < 1; } return 'nothrow' } catch (e) { return e.constructor.name } })()",
    )
    == "ReferenceError"
}

pub fn cmp_local_equality_jump_test() {
  assert run_string(
      "(function () {
         var r = [];
         var x = 1; if (x === 1) r.push('a'); if (x !== 1) r.push('never'); if (x === '1') r.push('never');
         if (x == '1') r.push('b'); if (x != true) r.push('never');
         var n = null; while (n !== null) r.push('never'); var u; if (u == n) r.push('c'); if (u === n) r.push('never');
         var o = {}, p = o, q = {}; if (o === p) r.push('d'); if (o == q) r.push('never'); if (o != p) r.push('never');
         var calls = 0; var v = { valueOf() { calls++; return 7 } }; var seven = 7;
         if (v == seven) r.push('e'); if (seven != v) r.push('never'); if (v === seven) r.push('never');
         var nan = NaN; if (nan === nan) r.push('never'); if (nan != nan) r.push('f');
         var z = 0, mz = -0; if (z === mz) r.push('g');
         var big = 2n, two = 2; if (big == two) r.push('h'); if (big === two) r.push('never');
         var t; try { if (w === 1) {} let w = 0; t = 'nothrow' } catch (e) { t = e.constructor.name }
         var t2; try { if (x != w2) {} let w2 = 0; t2 = 'nothrow' } catch (e) { t2 = e.constructor.name }
         return r.join('') + calls + t + t2
       })()",
    )
    == "abcdefgh2ReferenceErrorReferenceError"
}

pub fn local_field_superinstruction_test() {
  assert run_string(
      "(function () {
         var r = [];
         var o = { a: 1, get g() { return this.a + 1 }, set s(v) { r.push('set' + v) }, m() { return this.a * 10 } };
         r.push(o.a, o.g, o.m(), o.missing === undefined);
         var p = new Proxy({}, { get(t, k) { return 'trap:' + String(k) }, set(t, k, v) { r.push('pset' + v); return true } });
         r.push(p.q); p.z = 3; o.s = 4;
         var str = 'abc'; r.push(str.length, str.toUpperCase());
         var n = null; try { n.x } catch (e) { r.push(e.constructor.name) }
         var u; try { u.m() } catch (e) { r.push(e.constructor.name) }
         try { n.x = 1; } catch (e) { r.push(e.constructor.name) }
         'use strict'; var f = Object.freeze({ k: 1 });
         try { (function () { 'use strict'; f.k = 2; })() } catch (e) { r.push('strict' + e.constructor.name) }
         f.k = 3; r.push(f.k);
         class B {} class D extends B { constructor() { try { this.x; } catch (e) { r.push('tdz' + e.constructor.name) } super(); this.y = 5; r.push(this.y) } }
         new D();
         r.push(eval('var q = {}; q.w = 7;'));
         function* gen(t) { t.v = yield 1; r.push(t.v) } var it = gen({}); it.next(); it.next(9);
         return r.join()
       })()",
    )
    == "1,2,10,true,trap:q,pset3,set4,3,ABC,TypeError,TypeError,TypeError,strictTypeError,1,tdzReferenceError,5,7,9"
}

pub fn prefix_inc_dec_local_test() {
  assert run_string(
      "(function () {
         var s = '1'; var a = ++s; var n = 3; var seen = []; while (--n >= 0) seen.push(n);
         var o = { valueOf() { return 41 } }; var b = --o;
         var t; try { ++z; let z = 0; t = 'nothrow' } catch (e) { t = e.constructor.name }
         return [typeof s, s, a, seen.join(''), o, b, t].join()
       })()",
    )
    == "number,2,2,210,40,40,ReferenceError"
}

pub fn branch_lowering_test() {
  assert run_string(
      "(function () {
         var log = ''; function t(x) { log += x; return x }
         var r = [];
         if (t(1) && t(0) && t(2)) r.push('a'); else r.push('b');
         if (!(t(0) || !t(3))) r.push('c');
         if (!t('')) r.push('d');
         var i = 5; do { r.push(i) } while (--i > 3);
         var j = 0; do { j++ } while (j < 2 || t(0));
         while (true) { if (null == t(null) && t(4) != undefined) break }
         for (; 0;) r.push('never');
         r.push(t({ valueOf() { return null } }) == null ? 'obj' : 'ok');
         return r.join() + '/' + log + '/' + j
       })()",
    )
    == "b,c,d,5,4,ok/10030null4null/2"
}

pub fn typeof_kernel_test() {
  assert run_string(
      "[typeof class {}, typeof function () {}, typeof new Proxy(function () {}, {}), typeof new Proxy({}, {}), typeof null, typeof undefined, typeof 1n, typeof Symbol(), typeof 'a'.at, typeof Math].join()",
    )
    == "function,function,function,object,object,undefined,bigint,symbol,function,object"
}

pub fn truthy_kernel_test() {
  assert run_string(
      "[0, -0, NaN, '', null, undefined, 0n, false, 1, 'a', {}, [], 1n, Symbol(), Infinity].map(function (v) { return v ? 1 : 0 }).join('')",
    )
    == "000000001111111"
}

pub fn get_global_kernel_test() {
  assert run_string(
      "let lx = 1; const cx = 2; var vx = 3;
       Object.defineProperty(globalThis, 'acc', { get: function () { return 4 }, configurable: true });
       [lx + cx + vx + acc, typeof nope, typeof vx, typeof lx, typeof Math, toString === Object.prototype.toString].join()",
    )
    == "10,undefined,number,number,object,true"
  assert run_string(
      "var out = [];
       try { tdz } catch (e) { out.push(e.constructor.name) }
       try { typeof tdz } catch (e) { out.push(e.constructor.name) }
       let tdz = 0;
       try { nope } catch (e) { out.push(e.constructor.name) }
       out.join()",
    )
    == "ReferenceError,ReferenceError,ReferenceError"
}

pub fn put_global_kernel_test() {
  assert run_string(
      "var v = 1; let l = 2; v = 10; l = 20; fresh = 30;
       var d = Object.getOwnPropertyDescriptor(globalThis, 'fresh');
       var strict = (function () { 'use strict'; try { undeclared = 1; return 'set' } catch (e) { return e.constructor.name } })();
       Object.defineProperty(globalThis, 'ro', { value: 1, writable: false }); ro = 5;
       [v, l, fresh, d.writable && d.enumerable && d.configurable, strict, ro, 'l' in globalThis].join()",
    )
    == "10,20,30,true,ReferenceError,1,false"
}

pub fn construct_fast_path_test() {
  assert run_string(
      "function K(a) { this.a = a } K.prototype = { z: 1 };
       var k = new K(5);
       function R() { this.q = 1; return { r: 2 } }
       function S() { this.q = 1; return 5 }
       var out = [k.a, k.z, Object.getPrototypeOf(k) === K.prototype, new R().r, new R().q, new S().q];
       K.prototype = null; out.push(Object.getPrototypeOf(new K(1)) === Object.prototype);
       class B { constructor() { this.b = 1 } } class D extends B { constructor() { super(); this.d = 2 } }
       var dd = new D(); out.push(dd.b + dd.d, dd instanceof B);
       try { new (() => 1)() } catch (e) { out.push(e.constructor.name) }
       var P = new Proxy(function () {}, { get: function (t, k) { return k === 'prototype' ? { viaProxy: true } : t[k] } });
       out.push(Reflect.construct(K, [], P).viaProxy);
       out.join()",
    )
    == "5,1,true,2,,1,true,3,true,TypeError,true"
}

pub fn instanceof_kernel_test() {
  assert run_string(
      "function G() {} var g = new G(); var out = [];
       out.push(g instanceof G, ({}) instanceof G, 5 instanceof G, [] instanceof Array, new Map instanceof Map, G instanceof Function, g instanceof Object);
       class B {} class D extends B {} out.push(new D instanceof B, new B instanceof D);
       out.push(g instanceof G.bind(null));
       var H = function () {}; Object.defineProperty(H, Symbol.hasInstance, { value: function () { return true } });
       out.push(1 instanceof H);
       function F() {} Object.setPrototypeOf(F, Object.create(Function.prototype, { [Symbol.hasInstance]: { value: function () { return true } } }));
       out.push(({}) instanceof F);
       try { g instanceof {} } catch (e) { out.push(e.constructor.name) }
       var A = () => 1; try { g instanceof A } catch (e) { out.push(e.constructor.name) }
       out.push(g instanceof new Proxy(G, {}));
       out.join()",
    )
    == "true,false,false,true,true,true,true,true,false,true,true,true,TypeError,TypeError,true"
}

pub fn elem_kernels_ordinary_and_arguments_test() {
  assert run_string(
      "var o = {}; o[3] = 'x'; o[4294967295] = 'big'; o['7'] = 's'; var k = 'name'; o[k] = 'n';
       var out = [o[3], o['3'], o[4294967295], o[7], o.name, o[9], Object.keys(o).join('|')];
       var P = { set 5(v) { out.push('setter' + v) } }; var c = Object.create(P); c[5] = 1; out.push(Object.keys(c).length);
       var ro = Object.create(Object.defineProperty({}, '6', { value: 0, writable: false })); ro[6] = 1; out.push(ro[6]);
       var fz = Object.freeze({}); fz[1] = 2; out.push(fz[1]);
       function A(a, b) { arguments[0] = 9; return [arguments[0], arguments[1], arguments[2], arguments.length].join('/') }
       out.push(A(1, 2));
       var arr = [1, 2, 3]; out.push(arr.length, arr['length']); arr.length = 1; out.push(arr.length, arr[1]);
       var xs = [1, 2]; xs[1] += 5; var oo = { q: 1 }; var qq = 'q'; oo[qq] += 1; out.push(xs[1], oo.q);
       out.join()",
    )
    == "x,x,big,s,n,,3|7|4294967295|name,setter1,0,0,,9/2//2,3,3,1,,7,2"
}

pub fn literal_closure_try_yield_arms_test() {
  assert run_string(
      "var out = [];
       var lit = { a: 1, b: 2, a: 3 }; out.push(Object.keys(lit).join('|'), lit.a);
       var fs = []; for (var i = 0; i < 3; i++) fs.push((function (x) { return function () { return x } })(i));
       out.push(fs.map(function (f) { return f() }).join('|'));
       function T() { try { throw 1 } catch (e) { return e + 1 } finally { out.push('fin') } } out.push(T());
       function* gen(k) { for (var j = 0; j < k; j++) yield j; return 'done' }
       var s = 0; for (var v of gen(4)) s += v; out.push(s);
       var it = gen(1); out.push(it.next().value, it.next().value, it.next().done);
       async function af() { var x = await 5; return x + 1 } var r; af().then(function (v) { r = v });
       out.push(typeof r);
       out.join()",
    )
    == "a|b,3,0|1|2,fin,2,6,0,done,true,undefined"
}

pub fn object_literal_head_test() {
  assert run_string(
      "var p = { z: 0 };
       var o = { b: 1, a: 2, b: 3, __proto__: p, '3': 4, c: function () {}, ['d']: 5, ...{ e: 6 }, f: 7 };
       var out = [Object.keys(o).join('|'), o.b, Object.getPrototypeOf(o) === p, o.c.name, o.z, o.hasOwnProperty('__proto__')];
       var q = { __proto__: p, x: 1 }; out.push(Object.getPrototypeOf(q) === p, Object.keys(q).join('|'));
       var s = { __proto__ }; var __proto__; out.push(s.hasOwnProperty('__proto__'));
       var d = Object.getOwnPropertyDescriptor({ k: 1 }, 'k');
       out.push(d.writable && d.enumerable && d.configurable, Object.keys({}).length);
       var big = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4 }; out.push(Object.keys(big).join('|'), big.a3);
       out.join()",
    )
    == "3|b|a|c|d|e|f,3,true,c,0,false,true,x,true,true,0,a0|a1|a2|a3|a4,3"
}

pub fn call_new_test() {
  assert run_string(
      "function F(a, b) { this.s = a + b } var out = [new F(1, 2).s, new F(...[3, 4]).s];
       out.push(new F(1, 2) instanceof F);
       function G() { return new.target === G } out.push(new G().constructor === G);
       F.prototype = { tag: 'swapped' }; out.push(new F(0, 0).tag);
       try { new Math.max() } catch (e) { out.push(e.constructor.name) }
       try { new (function* () {})() } catch (e) { out.push(e.constructor.name) }
       var n = 0; new (function () { n++ })(); new (function () { n++ }); out.push(n);
       out.join()",
    )
    == "3,7,true,true,swapped,TypeError,TypeError,2"
}

pub fn fused_field_store_test() {
  assert run_string(
      "var out = [];
       (function () { try { o.x = 1 } catch (e) { out.push(e.constructor.name) } let o = {} })();
       (function () { let o = {}; try { o.x = v } catch (e) { out.push(e.constructor.name) } let v = 1 })();
       var log = []; var t = { set s(v) { log.push('s' + v) } }; var one = 1; t.s = one; t.s = 2; out.push(log.join('|'));
       var fz = Object.freeze({ a: 1 }); (function () { var z = 5; fz.a = z; fz.a = null })(); out.push(fz.a);
       try { (function () { 'use strict'; var z = 5; fz.a = z })() } catch (e) { out.push('strict' + e.constructor.name) }
       var px = new Proxy({}, { set(t, k, v) { log.push('p' + k + v); return true } }); var w = 9; px.q = w; px.r = 0; out.push(log.join('|'));
       var prim = 'str'; (function () { var y = 1; prim.len = y })(); out.push(prim.len === undefined);
       function C(a) { this.a = a; this.b = null; this.c = 3 } var c = new C(7); out.push(Object.keys(c).join('|'), c.a, c.b, c.c);
       out.join()",
    )
    == "ReferenceError,ReferenceError,s1|s2,1,strictTypeError,s1|s2|pq9|pr0,true,a|b|c,7,,3"
}

pub fn fused_method_call_test() {
  assert run_string(
      "var out = [];
       var o = { v: 4, m() { return this.v }, get g() { out.push('get'); return function () { return this === o } } };
       out.push(o.m(), o.g());
       try { null.m() } catch (e) { out.push(e.constructor.name) }
       try { ({}).nope() } catch (e) { out.push(e.constructor.name) }
       function* gen() { yield 1 } var it = gen(); out.push(it.next().value);
       var p = new Proxy({}, { get(t, k) { return function () { return 'trap' + String(k) } } }); out.push(p.hi());
       class A { m() { return 'A' } } class B extends A { m() { return super.m() + 'B' } } out.push(new B().m());
       out.push('abc'.toUpperCase(), (255).toString(), [3, 1, 2].sort().join('|'));
       (function () { try { x.m() } catch (e) { out.push(e.constructor.name) } let x = {} })();
       var bound = { f: function () { return this.tag }.bind({ tag: 'bound' }) }; out.push(bound.f());
       var one = { id(x) { return x }, get g1() { out.push('g1'); return function (x) { return x + 1 } } };
       var a1 = 7; out.push(one.id(a1), one.g1(a1), 'abc'.indexOf(a1), p.q(a1));
       (function () { try { one.id(z) } catch (e) { out.push(e.constructor.name) } let z = 1 })();
       (function () { try { ({ get m() { out.push('read'); return null } }).m(w) } catch (e) { out.push(e.constructor.name) } let w = 1 })();
       out.join()",
    )
    == "get,4,true,TypeError,TypeError,1,traphi,AB,ABC,255,1|2|3,ReferenceError,bound,g1,7,8,-1,trapq,ReferenceError,read,ReferenceError"
}

pub fn folded_operand_ops_test() {
  assert run_string(
      "var out = [];
       function C() {}
       function f1() { var a = {valueOf(){ out.push('a'); return 2 }}, b = {valueOf(){ out.push('b'); return 3 }}; var c; c = a * b; return c }
       function f2() { try { var r = x + 1; let x = 5 } catch (e) { return e.constructor.name } }
       function f3() { try { let y = q * q; let q = 1 } catch (e) { return e.constructor.name } }
       function f4() { try { var r = z instanceof C; let z = 1; return r } catch (e) { return e.constructor.name } }
       function f5() { var o = { get 3() { return 'g' } }; var k = 3; var s = 'abc'; var k2 = 1; var arr = [1,,3]; var h = 1;
                       Object.prototype[1] = 'P'; var r = o[k] + s[k2] + arr[h]; delete Object.prototype[1]; return r }
       function f6() { var i = '5'; var j = i++; var o = {valueOf(){ return 7 }}; var p = o++; return [i, j, o, p].join() }
       function f7() { var s = 'a'; s = s + 1; var t; t = 'x' in {x:1}; var u; u = [] instanceof Array; return [s,t,u].join() }
       function f8(o) { return 1 + o.x }
       out.push(f1(), f2(), f3(), f4(), f5(), f6(), f7(), f8({x:2}), f8({get x() { return 10 }}), f8({x:{valueOf(){ return 5 }}}));
       try { f8(null) } catch (e) { out.push(e.constructor.name) }
       out.join('|')",
    )
    == "a|b|6|ReferenceError|ReferenceError|ReferenceError|gbP|6,5,8,7|a1,true,true|3|11|6|TypeError"
}

pub fn fused_branch_ops_test() {
  assert run_string(
      "var out = [], n = 0, calls = 0;
       var o = { valueOf() { calls++; return 3 } };
       for (var i = 0; i < 10; i++) { if (i % 4 === 0) n++; if (o > i) n += 10 }
       out.push(n, calls);
       var u; var v = null; var w = 0;
       out.push(u != null ? 'a' : 'b', v == undefined ? 'c' : 'd', w != null ? 'e' : 'f');
       var k = 'x'; try { for (var j = 0; j < 2; k++) { j++ } } catch (e) { out.push('threw') }
       out.push(String(k));
       out.join('|')",
    )
    == "33|10|b|c|e|NaN"
}

pub fn apply_arguments_forwarding_test() {
  assert run_string(
      "var log = [];
       function Base(a, b) { log.push('b' + a + b + arguments.length) }
       function D() { Base.apply(this, arguments) }
       new D(1, 2); D(3); D.call({}, 4, 5, 6);
       var weird = { apply: function (t, args) { log.push('w' + args.length + typeof args.callee); return args } };
       function W() { var a1 = weird.apply(this, arguments); var a2 = weird.apply(this, arguments); log.push(a1 === a2) }
       W(7, 8);
       function S() { 'use strict'; return weird.apply(null, arguments) }
       try { S(1).callee } catch (e) { log.push(e.constructor.name) }
       var orig = Function.prototype.apply;
       Function.prototype.apply = function (t, a) { log.push('p' + a.length); return orig.call(this, t, a) };
       function P() { return Base.apply(null, arguments) } P(9, 10);
       Function.prototype.apply = orig;
       function NC() { var o = { apply: orig }; return o.apply(this, arguments) }
       try { NC(1) } catch (e) { log.push(e.constructor.name) }
       function E() { var n = arguments.length; Base.apply(this, arguments); return n } log.push(E(1, 2));
       function A() { var f = () => Base.apply(this, arguments); f() } A('x', 'y');
       function R() { return Base.apply(this, arguments) } R('r', 's');
       log.join()",
    )
    == "b122,b3undefined1,b453,w2function,w2function,true,TypeError,p2,b9102,TypeError,b122,2,bxy2,brs2"
}
